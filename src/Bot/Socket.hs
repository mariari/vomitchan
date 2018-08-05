{-# LANGUAGE Haskell2010       #-}
{-# LANGUAGE OverloadedStrings #-}


--- MODULE DEFINITION -------------------------------------------------------------------------
module Bot.Socket (
  write,
  writeBS,
  listen
) where
--- IMPORTS ------------------------------------------------------------------------------ ----
import           Control.Monad
import           Control.Monad.Loops
import           Control.Monad.Reader
import           Control.Concurrent as C
import qualified Data.Text          as T
import qualified Data.Text.Format   as T
import qualified Data.Text.IO       as T
import qualified Network.Connection as C
import           Control.Monad.IO.Class
import           Control.Concurrent.STM
import           Data.Foldable
import qualified Data.Text.Encoding    as TE
import qualified Data.ByteString.Char8 as BS
import           Bot.MessageType
import           Bot.Message
import           Bot.StateType
--- FUNCTIONS ------------------------------------------------------------------------------ --

-- takes a Handle and an (Action, Args) tuple and sends to socket
write :: C.Connection -> (T.Text, T.Text) -> IO ()
write h (act,args) = C.connectionPut h (TE.encodeUtf8 . fold $ [act, " ", args, "\r\n"])
                  >> T.print "{} {}\n" [act,args]

writeBS h (act, args) = C.connectionPut h txt >> BS.putStrLn txt
  where txt = fold [act, " ", args, "\r\n"]

-- simply listens to a socket forever
listen :: C.Connection -> T.Text -> GlobalState -> IO Quit
listen h net state = do
  quit      <- C.newEmptyMVar
  Just exit <- iterateUntil (not . (== Nothing)) (resLoop quit)
  C.connectionClose h
  return exit
  where
    resLoop quit = do
      s <- TE.decodeUtf8 <$> C.connectionGetLine 10240 h

      T.putStrLn s
      print net

      forkIO (inout s net quit state)
      C.tryTakeMVar quit

    inout s net quit state = do
      res <- runReaderT (respond s) (toInfo s net state)
      case res of
        Quit x     -> quitNetwork h >> C.putMVar quit x
        Response x -> write h x
        NoResponse -> return ()


quitNetwork h = write h ("QUIT", ":Exiting")
