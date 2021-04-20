module Bot.Socket (
  write,
  writeBS,
  listen,
  quitNetwork
) where
--- IMPORTS -----------------------------------------------------------------------------------
import Control.Monad.Loops (iterateUntil)
import Control.Monad       (unless)
import Data.Foldable       (fold)
import Data.Text.Encoding  (encodeUtf8)
import Control.Concurrent  (tryTakeMVar, forkIO, MVar, putMVar)
import Control.Exception.Base (throw)

import qualified Data.Text             as T
import qualified Data.Text.IO          as T
import qualified Network.Connection    as C
import qualified Data.ByteString.Char8 as BS
import qualified Network.HTTP.Client   as Client

import Bot.MessageParser
import Bot.MessageType
import Bot.NetworkType
import Bot.Message
import Bot.StateType
--- FUNCTIONS ---------------------------------------------------------------------------------

-- takes a Handle and an (Action, Args) tuple and sends to socket
write :: C.Connection -> (T.Text, T.Text) -> IO ()
write h (act,args) = C.connectionPut h (encodeUtf8 txt) >> T.putStr txt
  where
    txt = fold [act, " ", args, "\n"]

writeBS h (act, args) = C.connectionPut h txt >> BS.putStrLn txt
  where
    txt = fold [act, " ", args, "\r\n"]

-- simply listens to a socket forever
listen ::
  (C.Connection, MVar Quit) -> AllServers -> IRCNetwork -> T.Text -> GlobalState -> Client.Manager -> IO Quit
listen (h, quit) allServs network net state manager = do
  Just exit <- iterateUntil (/= Nothing) (resLoop quit)
  C.connectionClose h
  return exit
  where
    resLoop quit = do
      isConnected <- C.connectionWaitForInput h 251000 -- see if we hear anything in 4 mins

      unless isConnected                               -- if not then we are dcd
             (throw (C.HostNotResolved (show net)))    -- bubble an exception up to reconnect

      s <- C.connectionGetLine 10240 h

      forkIO (inout s net quit state)

      BS.putStrLn s
      print net

      tryTakeMVar quit

    inout s net quit state = do
      res <- respond s allServs (parseMessage s) net state network manager
      case res of
        Quit x     -> quitNetwork h >> putMVar quit x
        Response x -> write h x
        NoResponse -> return ()

quitNetwork h = write h ("QUIT", ":Exiting")
