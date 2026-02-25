module Main where

import Test.Hspec

import qualified Data.Text as T
import qualified Data.List as L

import Bot.Examples
import Bot.MessageParser (parseMessage)
import Bot.MessageType
import Bot.Modifier      (applyEffects, Unit(..), Scope(..), TextUnit(..))
import Bot.StateType
import Bot.Database      (getUserQuantityOfVomitsConn, getLinkConn, getRouletteVomitConn, DBVomit(..))

main :: IO ()
main = hspec $ do
  parserSpec
  modifierSpec
  stateSpec
  globalStateSpec
  commandSpec
  databaseSpec

--------------------------------------------------------------------------------
-- Parser tests
--------------------------------------------------------------------------------

parserSpec :: Spec
parserSpec = describe "MessageParser" $ do
  it "all examples parse successfully" $
    mapM_ (\(name, result) ->
             case result of
               Left err -> expectationFailure $ name ++ " failed: " ++ err
               Right _  -> return ())
          parseAll

  it "PRIVMSG fields" $
    case parseMessage exPrivmsg of
      Right (PRIVMSG pm) -> do
        msgNick pm `shouldBe` "nick"
        msgChan pm `shouldBe` "#channel"
        msgContent pm `shouldBe` "hello world"
      other -> expectationFailure $ show other

  it "rejects empty input" $
    parseMessage "" `shouldSatisfy` isLeft
    where isLeft (Left _) = True
          isLeft _        = False

--------------------------------------------------------------------------------
-- Modifier tests (migrated from test/Modifier.hs)
--------------------------------------------------------------------------------

modifierSpec :: Spec
modifierSpec = describe "Modifier" $ do
  it "empty scope preserves text" $ do
    result <- applyEffects exPlain
    result `shouldBe` "Hello My Name is Bob"

  it "block Set wraps text with control chars" $ do
    result <- applyEffects exBold
    result `shouldBe` "\x2Hello My Name is Bob\x2"

  it "non-modifiable text passes through" $ do
    result <- applyEffects (Unit (Scope [] []) (NonModifiable "raw text"))
    result `shouldBe` "raw text"

  it "effects inject control codes" $
    mapM_ (\ex -> applyEffects ex >>= (`shouldSatisfy` (not . T.null)))
      [ exStartColor, exAnsiColor, exRainbow, exAlternating
      , exNoOverride, exFixOverride ]

--------------------------------------------------------------------------------
-- State tests
--------------------------------------------------------------------------------

stateSpec :: Spec
stateSpec = describe "State" $
  it "example states" $
    mapM_ (uncurry shouldBe)
      [ (stateDefault, HashStorage True  False False False)
      , (stateMuted,   HashStorage True  True  False False)
      , (stateAllOff,  HashStorage False False False False)
      , (stateYuki,    HashStorage True  False False True)
      ]

--------------------------------------------------------------------------------
-- GlobalState (STM map) tests
--------------------------------------------------------------------------------

globalStateSpec :: Spec
globalStateSpec = describe "GlobalState" $
  it "mkGlobalState + dumpState round-trips" $ do
    mapM_ (\entries -> do
      gs <- mkGlobalState entries
      pairs <- dumpState gs
      L.sort pairs `shouldBe` L.sort entries)
      [ []
      , sampleStateEntries
      , [("net#a", stateDefault), ("net#b", stateMuted)]
      ]

--------------------------------------------------------------------------------
-- Command integration tests (via mockEnv)
--------------------------------------------------------------------------------

commandSpec :: Spec
commandSpec = describe "Commands" $ do
  it "known commands return Response" $
    mapM_ (\msg -> testCmd (mkMsg msg) >>= (`shouldSatisfy` isResponse))
      [".bots", ".8ball"]

  it ".quit returns Quit for admin" $ do
    result <- testCmd (mkMsg ".quit")
    case result of
      Quit CurrentNetwork -> return ()
      other -> expectationFailure $ "Expected Quit CurrentNetwork, got: " ++ show other

  it "unknown command returns NoResponse" $
    testCmd (mkMsg "just chatting") >>= (`shouldSatisfy` isNoResponse)

  it "testCmdWith custom state" $
    testCmdWith [("irc.example.net#test", stateYuki)] (mkMsg ".bots")
      >>= (`shouldSatisfy` isResponse)

--------------------------------------------------------------------------------
-- Database tests — runs the example narrative, checking state between steps
--------------------------------------------------------------------------------

databaseSpec :: Spec
databaseSpec = describe "Database" $ do
  it "createSchema creates all 3 tables" $ withTestDb $ \conn -> do
    tables <- query_ conn
      "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name;" :: IO [Only String]
    L.sort (map fromOnly tables) `shouldBe` ["channels", "user", "vomits"]

  it "setup creates user with zero vomits" $ withTestDb $ \conn -> do
    exDbSetup conn
    getUserQuantityOfVomitsConn conn "nick" "#test" >>= (`shouldBe` 0)

  it "addVomit increments count and stores file" $ withTestDb $ \conn -> do
    exDbAddVomit conn
    getUserQuantityOfVomitsConn conn "nick" "#test" >>= (`shouldBe` 1)
    voms <- allVomits conn
    length voms `shouldBe` 1
    vomitMD5 (L.head voms) `shouldBe` "abc123"

  it "linkVomit caches the URL" $ withTestDb $ \conn -> do
    exDbLinkVomit conn
    getLinkConn conn "./data/logs/#test/nick/file.jpg"
      >>= (`shouldBe` Just "https://example.com/file.jpg")

  it "fixCounts corrects a drifted count" $ withTestDb $ \conn -> do
    exDbFixCounts conn
    getUserQuantityOfVomitsConn conn "nick" "#test" >>= (`shouldBe` 2)
    allVomits conn >>= ((`shouldBe` 2) . length)

  it "roulette picks from multiple users" $ withTestDb $ \conn -> do
    exDbAddSecondUser conn
    allUsers conn >>= ((`shouldBe` 2) . length)
    (path, _) <- getRouletteVomitConn conn "#test"
    path `shouldSatisfy` (not . null)

  it "nukeMD5 removes vomit and decrements count" $ withTestDb $ \conn -> do
    paths <- exDbNukeMD5 conn
    paths `shouldBe` ["./data/logs/#test/nick/file.jpg"]
    getUserQuantityOfVomitsConn conn "nick" "#test" >>= (`shouldBe` 0)
    allVomits conn >>= (`shouldBe` [])

  it "nukeUser removes user and all their vomits" $ withTestDb $ \conn -> do
    exDbNukeUser conn
    allUsers conn >>= (`shouldBe` [])
    allVomits conn >>= (`shouldBe` [])

  it "getRouletteVomit returns path and nick" $ withTestDb $ \conn -> do
    exDbAddSecondUser conn
    (path, nick) <- getRouletteVomitConn conn "#test"
    path `shouldSatisfy` (not . null)
    nick `shouldSatisfy` (`elem` ["nick", "alice"])

-- Helpers

isResponse, isNoResponse :: Response a -> Bool
isResponse (Response _) = True
isResponse _            = False
isNoResponse NoResponse = True
isNoResponse _          = False
