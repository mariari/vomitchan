{-# LANGUAGE Haskell2010       #-}
{-# LANGUAGE OverloadedStrings #-}


--- MODULE DEFINITION -------------------------------------------------------------------------
module Bot.MessageParser where

import Bot.MessageType

import           Prelude hiding (takeWhile)
import           Control.Applicative
import           Data.Attoparsec.ByteString
import           Data.Word
import qualified Data.Text                        as T
import qualified Data.ByteString                  as BS
import qualified Data.Set                         as S
import qualified Data.Attoparsec.ByteString.Char8 as C
import qualified Data.Text.Encoding               as TE
-- TYPES FOR PARSING---------------------------------------------------------------------------
data Prefix = ServerName Server
            | PUser UserI
            deriving Show

parseMessage :: BS.ByteString -> Either String Command
parseMessage = parseOnly (maybePrefix >>= command)

command prefix =  wordCommand prefix
              <|> numberCommand prefix

maybePrefix :: Parser (Maybe Prefix)
maybePrefix = optionally prefix

prefix :: Parser Prefix
prefix = word8 58 >> (parseUserI <|> parseServer) -- 58 = :


numberCommand prefix = do
  code <- C.decimal
  NUMBERS <$> numbers code prefix

wordCommand prefix = do
  word <- C.takeWhile1 C.isAlpha_ascii
  commandW word prefix

commandW :: BS.ByteString -> Maybe Prefix -> Parser Command
commandW word Nothing = f word
  where
    f "PING" = ping
    f _      = OTHER (TE.decodeUtf8 word) . OtherNoInfo <$> takeText

commandW word (Just (PUser userI)) = f word
  where
    f "PRIVMSG" = privMsg userI
    f "JOIN"    = join    userI
    f "QUIT"    = quit    userI
    f "PART"    = quit    userI
    f _         = OTHER (TE.decodeUtf8 word) . OtherUser userI <$> takeText

commandW word (Just (ServerName s)) = f word
  where
    f _ = OTHER (TE.decodeUtf8 word) . OtherNoInfo <$> takeText

numbers :: Int -> Maybe Prefix -> Parser Numbers
numbers code Nothing = f code
  where
    f _ = NOther code . OtherNoInfo <$> takeText

numbers code (Just (PUser userI)) = f code
  where
    f _ = NOther code . OtherUser userI <$> takeText

numbers code (Just (ServerName s)) = f code
  where
    f 354 = N354 s <$> takeText
    f 376 = N376 s <$> takeText
    f 904 = N904 s <$> takeText
    f 903 = N903 s <$> takeText
    f _   = NOther code . OtherServer s <$> takeText

-- COMMAND-------------------------------------------------------------------------------------

privMsg :: UserI -> Parser Command
privMsg userI = targetCmd f
  where
    f target = PRIVMSG . PrivMsg userI target

part userI = targetCmd f
  where
    f target = PART . Part userI target

ping :: Parser Command
ping = C.space >> PING . Ping <$> takeText

join userI = takeColon (JOIN . Join userI)

quit userI = takeColon (CQUIT . CQuit userI)


takeColon :: (T.Text -> b) -> Parser b
takeColon f = do
  C.space
  word8 58
  rest <- takeText
  return (f rest)

targetCmd :: (T.Text -> T.Text -> b) -> Parser b
targetCmd f = do
  C.space
  target  <- takeTill isWhiteSpace
  C.space
  word8 58 -- 58 = :
  content <- takeText
  return (f (TE.decodeUtf8 target) content)

-- PREFIX-------------------------------------------------------------------------------------
parseServer :: Parser Prefix
parseServer = do
  host <- parseHost
  C.space
  return (ServerName (TE.decodeUtf8 host))

parseUserI :: Parser Prefix
parseUserI = do
  nick <- parseNick
  user <- optionally (word8 33 >> parseUser) -- 33 = !
  host <- optionally (word8 64 >> parseHost) -- 64 = @
  C.space
  return (PUser (UserI (TE.decodeUtf8 nick) (TE.decodeUtf8 <$> user) (TE.decodeUtf8 <$> host)))


parseNick :: Parser BS.ByteString
parseNick = C.takeWhile1 (\x -> C.isAlpha_ascii x || C.isDigit x || x `S.member` specialNick)

parseUser :: Parser BS.ByteString
parseUser = takeTill (\x -> isWhiteSpace x || x == 64) -- 64 = @

parseHost :: Parser BS.ByteString
parseHost = takeTill isWhiteSpace

specialNick :: S.Set Char
specialNick = S.fromList "-[]`{}`"

whiteSpace :: S.Set Word8
whiteSpace = S.fromList [0xd, 0x0, 0xa]

isWhiteSpace :: Word8 -> Bool
isWhiteSpace x = C.isSpace_w8 x || x `S.member` whiteSpace

-- HELPERS-------------------------------------------------------------------------------------
optionally :: Alternative f => f a -> f (Maybe a)
optionally p = option Nothing (Just <$> p)

takeText :: Parser T.Text
takeText = TE.decodeUtf8 <$> takeTill C.isEndOfLine

-- :loli!loli@net-cqi4sn.cl7c.pujq.i9lmq6.IP PART #lainchan :WeeChat 2.2
-- :loli!loli@net-cqi4sn.cl7c.pujq.i9lmq6.IP JOIN :#lainchan
-- :ergo!4f67a633@net-cnc.itb.81eeq7.IP QUIT :Connection closed
-- :irc.fuwafuwa.moe 376 vomitchan :End of /MOTD command.
-- :drastikbot!drastik@drastik.org PRIVMSG #lainchan :Riot – Riot – open team collaboration
