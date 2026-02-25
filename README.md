# vomitchan
An IRC bot written in Haskell.  *cheek pinch*

---

![vomitchan running in IRC](https://puu.sh/BJHkI/08007ee669.png)

## Commands
- `.bots [vomitchan]` - prints bot info
- `.source vomitchan` - prints a link to bot source
- `.lewd <someone>` - lewds someone
- `*vomits* <someone>` - 'vomits' a link posted by you, or optionally someone else
- `*cheek pinch*` - turns off and on dreammode for a channel
- `*step*` - turns off and on fleecyMode for a channel
- `*yuki*` - turns off and on yukiMode for a channel
- `.lotg <someone>` - gives the luck of the grasshopper to a user
- `.8ball <question>` - acts like an eight ball!

### Admin Only
- Currently admins are stored in the admins list in Command.hs (will be read from a file later)
- `.quit` - kills vomitchan from the current server ;-;
- `.part` - an alias for quit
- `.quit all` - Kills vomitchan from all servers ;-;
- `.join <channel>` - vomitchan joins specified channel
- `.leave <channel>` - vomitchan leaves the spcified channel
- `.leave` - vomitchan leaves the current channel

## Building
### Requirements
- Stack
- Haskell packages (stack should install these for you):
  - bytestring
  - connection
  - directory >= 1.2.5
  - lens
  - monad-loops
  - network
  - random
  - stm
  - text
  - turtle
  - utf8-string
  - vector
  - containers
  - stm-containers
  - mtl
  - base64-bytestring
  - attoparsec


### Building on Linux
- clone this repo using `git clone https://github.com/mariari/vomitchan.git`
- run `cd vomitchan && stack setup && stack build` to compile

## Configuration
vomitchan requires `data/networks.json` to store information about IRC networks to connect to.
This file should look like the following:

**NOTE: one can forego writing T F F F for "dreammode", "mutemode", "fleecyMode", and YukiMode because they default to T F F F**

```json
[
    { "netServer"   : "irc.freenode.net"
      , "netPort"   : 6697
      , "netSSL"    : false
      , "netNick"   : "vomitchan"
      , "netPass"   : "PASS"
      , "netAdmins" : ["mariari@net-8bn.43k.64gvlf.IP"]
      , "netChans"  : [["#hardfought", {"chanKey" : "hardfoughter" }], ["#em.slashem.me", {}], ["#Haskell", {}]]
      , "netIgnore" : []
      , "netBans"   : []
      , "netState"  : [["#hardfought", {"dream"  : true,
                                       "mute"   : false,
                                       "fleecy" : false,
                                       "yuki"   : false}]
                      ,["#em.slashem.me", {"dream"  : true,
                                           "mute"   : false,
                                           "fleecy" : true,
                                           "yuki"   : false}]]
      , "netUpload" : {"uploadService" : "toro",
                        "uploadSecret"  : "my-secret",
                        "uploadUrl"     : "http://127.0.0.1:4000/upload.php"}
    },

    { "netServer"   : "other"
      , "netPort"   : 6697
      , "netSSL"    : false
      , "netNick"   : "vomitchan"
      , "netPass"   : ""
      , "netAdmins" : ["mariari@net.ip"]
      , "netChans"  : [["#bots", {}], ["#programming", {}]]
      , "netIgnore" : ["mariari"]
      , "netBans"   : ["4ed600d4c9f69bcaf14540e887c75bbe"]
      , "netState"  : []
    }
]
```

Omitting `netUpload` defaults to catbox with no secret. Supported `uploadService` values: `"catbox"`, `"neko"`, `"lain"`, or any pomf-compatible URL. For catbox, `uploadSecret` is the userhash for account-linked uploads. For pomf-compatible services, it is sent as a `secret` form field.

## Running
The bot can be started within ghci by running `stack ghci` in the project directory. Once at the prompt, run the `main` function.
`stack build` should also create an executable that can be started with `stack exec vomitchan-exe`

## Architecture

```
app/Main.hs              -- startup: genDb, init state/servers/connections
src/Bot/
  MessageParser.hs       -- attoparsec: ByteString -> Either String Command
  Message.hs             -- dispatch hub: parses command, routes PRIVMSG/NOTICE/PING
  Commands.hs            -- command dispatch, runs in ReaderT InfoPriv IO
  EffType.hs             -- Func = Response (Action, Modifier.T), CmdImp constraint
  State.hs               -- STM-based channel state, key = server <> channel
  StateType.hs           -- HashStorage (dream/mute/fleecy/yuki), GlobalState
  Database.hs            -- SQLite via sqlite-simple, *Conn variants for testability
  FileOps.hs             -- file downloads, uploads (catbox/pomf/neko/lain), user folders
  Modifier.hs            -- IRC text effects (bold/italic/color), closure-based
  NetworkType.hs         -- IRCNetwork, UploadConfig, ChanOptions
  Network.hs             -- IRC connection handling
  Servers.hs             -- multi-server management
  Socket.hs              -- raw socket/TLS I/O
  MessageType.hs         -- PrivMsg, UserI, Info types
  Examples.hs            -- sample data, mock env, DB examples for GHCi and tests
  Misc.hs                -- small utilities
test/Spec.hs             -- hspec suite
```

## Planned Features
- Per-channel rate limiting, to prevent spam
