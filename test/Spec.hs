module Main where

import Test.Hspec

import qualified Data.Text as T
import qualified Data.List as L

import Bot.Examples
import Bot.MessageParser (parseMessage)
import Bot.MessageType
import Bot.Modifier      (applyEffects, Unit(..), Scope(..), TextUnit(..))
import Bot.StateType
import Bot.Database

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
-- Database tests (in-memory SQLite via withTestDb)
--------------------------------------------------------------------------------

databaseSpec :: Spec
databaseSpec = describe "Database" $ do
  it "createSchema creates all 3 tables" $ withTestDb $ \conn -> do
    tables <- query_ conn
      "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name;" :: IO [Only String]
    L.sort (map fromOnly tables) `shouldBe` ["channels", "user", "vomits"]

  it "addChannel + addUser + getUserQuantityOfVomits returns 0" $ withTestDb $ \conn -> do
    addChannelConn conn "#test"
    addUserConn conn "nick" "#test"
    count <- getUserQuantityOfVomitsConn conn "nick" "#test"
    count `shouldBe` 0

  it "addVomit + succUserQuantityOfVomits increments count" $ withTestDb $ \conn -> do
    seedTestDb conn
    succUserQuantityOfVomitsConn conn "nick" "#test"
    count <- getUserQuantityOfVomitsConn conn "nick" "#test"
    count `shouldBe` 1

  it "updateLink + getLink round-trip" $ withTestDb $ \conn -> do
    seedTestDb conn
    updateLinkConn conn "/data/logs/#test/nick/file.jpg" "https://example.com/file.jpg"
    link <- getLinkConn conn "/data/logs/#test/nick/file.jpg"
    link `shouldBe` Just "https://example.com/file.jpg"

  it "getRandomVomit returns inserted vomit" $ withTestDb $ \conn -> do
    seedTestDb conn
    voms <- getRandomVomitConn conn "nick" "#test"
    length voms `shouldBe` 1
    vomitPath (head voms) `shouldBe` "/data/logs/#test/nick/file.jpg"
    vomitMD5 (head voms) `shouldBe` "abc123"

  it "nukeVomitByMD5FixConn removes vomit and decrements count" $ withTestDb $ \conn -> do
    seedTestDb conn
    succUserQuantityOfVomitsConn conn "nick" "#test"
    paths <- nukeVomitByMD5FixConn conn "abc123"
    paths `shouldBe` ["/data/logs/#test/nick/file.jpg"]
    count <- getUserQuantityOfVomitsConn conn "nick" "#test"
    count `shouldBe` 0

  it "nukeVomitsOfUserFromDb clears all vomits" $ withTestDb $ \conn -> do
    seedTestDb conn
    addVomitConn conn "nick" "#test" "def456" "/data/logs/#test/nick/file2.png"
    nukeVomitsOfUserFromDbConn conn "nick" "#test"
    voms <- getRandomVomitConn conn "nick" "#test"
    voms `shouldBe` []

  it "nukeUserFromDb removes the user" $ withTestDb $ \conn -> do
    seedTestDb conn
    nukeVomitsOfUserFromDbConn conn "nick" "#test"
    nukeUserFromDbConn conn "nick" "#test"
    count <- getUserQuantityOfVomitsConn conn "nick" "#test"
    count `shouldBe` 0

  it "fixQuantityOfVomitsConn corrects counts" $ withTestDb $ \conn -> do
    seedTestDb conn
    addVomitConn conn "nick" "#test" "def456" "/data/logs/#test/nick/file2.png"
    -- count is still 0 since we never called succ
    fixQuantityOfVomitsConn conn
    count <- getUserQuantityOfVomitsConn conn "nick" "#test"
    count `shouldBe` 2

  it "getRouletteVomit works across users" $ withTestDb $ \conn -> do
    seedTestDb conn
    succUserQuantityOfVomitsConn conn "nick" "#test"
    addUserConn conn "alice" "#test"
    addVomitConn conn "alice" "#test" "xyz789" "/data/logs/#test/alice/pic.jpg"
    succUserQuantityOfVomitsConn conn "alice" "#test"
    (path, nick) <- getRouletteVomitConn conn "#test"
    path `shouldSatisfy` (not . null)
    nick `shouldSatisfy` (`elem` ["nick", "alice"])

-- Helpers

isResponse, isNoResponse :: Response a -> Bool
isResponse (Response _) = True
isResponse _            = False
isNoResponse NoResponse = True
isNoResponse _          = False
