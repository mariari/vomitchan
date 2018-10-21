module Bot.MessageParser where

import Bot.MessageType
import Bot.NetworkType

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
            | NoPrefix
            deriving Show

parseMessage :: BS.ByteString -> Either String Command
parseMessage = parseOnly (prefix >>= command)

command ::  Prefix -> Parser Command
command prefix =  wordCommand prefix
              <|> numberCommand prefix

prefix :: Parser Prefix
prefix = (word8 58 >> parseUserI <|> parseServer) -- 58 = :
      <|> return NoPrefix

numberCommand :: Prefix -> Parser Command
numberCommand prefix = do
  code <- C.decimal
  NUMBERS <$> numbers code prefix

wordCommand :: Prefix -> Parser Command
wordCommand prefix = do
  word <- C.takeWhile1 C.isAlpha_ascii
  commandW word prefix

commandW :: BS.ByteString -> Prefix -> Parser Command
commandW "PING"    NoPrefix      = ping
commandW "PRIVMSG" (PUser userI) = privMsg userI
commandW "JOIN"    (PUser userI) = join userI
commandW "QUIT"    (PUser userI) = quit userI
commandW "PART"    (PUser userI) = part userI
commandW word      prefix        = handleOther word prefix

numbers :: Int -> Prefix -> Parser Numbers
numbers 354 (ServerName s) = N354 s <$> takeText
numbers 376 (ServerName s) = N376 s <$> takeText
numbers 903 (ServerName s) = N903 s <$> takeText
numbers 904 (ServerName s) = N904 s <$> takeText
numbers cod prefix         = handleNOther cod prefix
-- COMMAND-------------------------------------------------------------------------------------

privMsg :: UserI -> Parser Command
privMsg userI = targetCmd f
  where
    f target = PRIVMSG . PrivMsg userI target

part :: UserI -> Parser Command
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
  optionally (word8 58)
  rest <- takeText
  return (f rest)

targetCmd :: (T.Text -> T.Text -> b) -> Parser b
targetCmd f = do
  C.space
  target  <- takeTill isWhiteSpace
  optionally (C.space >> word8 58)  -- 58 = :
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
specialNick = S.fromList "-[]`{}^_"

whiteSpace :: S.Set Word8
whiteSpace = S.fromList [0xd, 0x0, 0xa]

isWhiteSpace :: Word8 -> Bool
isWhiteSpace x = C.isSpace_w8 x || x `S.member` whiteSpace

-- HELPERS-------------------------------------------------------------------------------------
optionally :: Alternative f => f a -> f (Maybe a)
optionally p = option Nothing (Just <$> p)

takeText :: Parser T.Text
takeText = TE.decodeUtf8 <$> takeTill C.isEndOfLine

prefixToOther :: Prefix -> Content -> Other
prefixToOther (ServerName s) = OtherServer s
prefixToOther (PUser s)      = OtherUser s
prefixToOther NoPrefix       = OtherNoInfo

handleNOther :: Int -> Prefix -> Parser Numbers
handleNOther code prefix = NOther code . prefixToOther prefix <$> takeText

handleOther :: BS.ByteString -> Prefix -> Parser Command
handleOther word prefix = OTHER (TE.decodeUtf8 word) . prefixToOther prefix <$> takeText
