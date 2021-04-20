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
    { "netServer" : "irc.freenode.net"
      , "netPort" : 6697
      , "netSSL"  : false
      , "netNick" : "vomitchan"
      , "netPass" : "PASS"
      , "admins"  : ["mariari"]
      , "netChans" : ["#hardfought", "#em.slashem.me", "#Haskell"]
      , "netState" : [["#hardfought", {"dream"  : true,
                                       "mute"   : false,
                                       "fleecy" : false,
                                       "yuki"   : false}]
                      ,["#em.slashem.me", {"dream"  : true,
                                           "mute"   : false,
                                           "fleecy" : true,
                                           "yuki"   : false}]]
    },

    { "netServer" : "other"
      , "netPort" : 6697
      , "netSSL"  : false
      , "netNick" : "vomitchan"
      , "netPass" : ""
      , "admins"  : ["mariari"]
      , "netChans" : ["#bots", "#programming"]
      , "netState" : []
    }
]

```

## Running
The bot can be started within ghci by running `stack ghci` in the project directory. Once at the prompt, run the `main` function.
`stack build` should also create an executable that can be started with `stack exec vomitchan-exe`

## Planned Features
- Versioning
- Per-channel rate limiting, to prevent spam
