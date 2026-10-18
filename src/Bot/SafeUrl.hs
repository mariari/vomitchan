{-# LANGUAGE ScopedTypeVariables #-}

module Bot.SafeUrl
  ( isPublicHttpUrl
  , isPublicSockAddr
  , validatedCurlArgs
  ) where

import qualified Control.Exception   as Exception
import qualified Data.ByteString.Char8 as BS8
import           Data.List           (intercalate, nub)
import           Data.Maybe          (isJust)
import qualified Data.Text           as T
import           Data.Word           (Word8, Word16)
import           Numeric             (showHex)
import qualified Network.HTTP.Client as HTTP
import           Network.Socket
import           System.Timeout      (timeout)

-- | Resolve a HTTP(S) URL and accept it only when every address returned by
-- DNS is globally routable.
isPublicHttpUrl :: T.Text -> IO Bool
isPublicHttpUrl = fmap isJust . validatedCurlArgs

-- | Return curl arguments that pin the URL's hostname to the exact public
-- addresses checked here.  Besides rejecting mixed public/private DNS answers,
-- this prevents DNS rebinding between validation and connection.
validatedCurlArgs :: T.Text -> IO (Maybe [T.Text])
validatedCurlArgs url = Exception.catch check (\(_ :: Exception.SomeException) -> pure Nothing)
  where
    check = do
      request <- HTTP.parseRequest (T.unpack url)
      let lowerUrl = T.toLower url
          allowedScheme = "http://" `T.isPrefixOf` lowerUrl
                       || "https://" `T.isPrefixOf` lowerUrl
      if allowedScheme
        then do
          let hints = defaultHints { addrSocketType = Stream }
          result <- timeout 5000000 $
            (getAddrInfo (Just hints) (Just (BS8.unpack $ HTTP.host request)) Nothing :: IO [AddrInfo])
          pure $ case result of
            Just addresses
              | not (null addresses)
              , all (isPublicSockAddr . addrAddress) addresses ->
                  let ips = nub $ map (renderAddress . addrAddress) addresses
                      entry = BS8.unpack (HTTP.host request)
                           <> ":" <> show (HTTP.port request)
                           <> ":" <> intercalate "," ips
                  in Just ["--resolve", T.pack entry]
            _ -> Nothing
        else pure Nothing

renderAddress :: SockAddr -> String
renderAddress (SockAddrInet _ address) =
  let (a, b, c, d) = hostAddressToTuple address
  in intercalate "." (map show [a, b, c, d])
renderAddress (SockAddrInet6 _ _ address _) =
  let groups = hostAddress6ToTuple address
  in "[" <> intercalate ":" (map (`showHex` "") (tuple8ToList groups)) <> "]"
renderAddress _ = ""

tuple8ToList :: (a, a, a, a, a, a, a, a) -> [a]
tuple8ToList (a, b, c, d, e, f, g, h) = [a, b, c, d, e, f, g, h]

-- | True only for globally routable IPv4 and IPv6 unicast addresses.
isPublicSockAddr :: SockAddr -> Bool
isPublicSockAddr (SockAddrInet _ address) = publicIPv4 (hostAddressToTuple address)
isPublicSockAddr (SockAddrInet6 _ _ address _) = publicIPv6 (hostAddress6ToTuple address)
isPublicSockAddr _ = False

publicIPv4 :: (Word8, Word8, Word8, Word8) -> Bool
publicIPv4 (a, b, c, _)
  | a == 0                         = False
  | a == 10                        = False
  | a == 100 && b >= 64 && b <= 127 = False
  | a == 127                       = False
  | a == 169 && b == 254           = False
  | a == 172 && b >= 16 && b <= 31 = False
  | a == 192 && b == 0             = False
  | a == 192 && b == 168           = False
  | a == 198 && b >= 18 && b <= 19 = False
  | a == 198 && b == 51 && c == 100 = False
  | a == 203 && b == 0 && c == 113 = False
  | a >= 224                       = False
  | otherwise                      = True

publicIPv6 :: (Word16, Word16, Word16, Word16, Word16, Word16, Word16, Word16) -> Bool
publicIPv6 (a, b, _, _, _, _, _, _)
  | a < 0x2000 || a > 0x3fff       = False
  | a == 0x2001 && b == 0x0db8     = False
  | otherwise                      = True
