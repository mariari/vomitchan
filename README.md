# vomitchan
An IRC bot written in Haskell.  *cheek pinch*

---

![vomitchan running in IRC](https://puu.sh/xfN35/2f9ee0615a.png)

## Commands
- `.bots [vomitchan]` - prints bot info
- `.source vomitchan` - prints a link to bot source
- `.lewd <someone>` - lewds someone
- `*vomits* <someone>` - 'vomits' a link posted by you, or optionally someone else
- `*cheek pinch*` - turns off and on dreammode for a channel
- `*step*` - turns off and on fleecyMode for a channel
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
  - regex-tdfa
  - stm
  - text
  - text-format
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

**NOTE: one can forego writing T F F for "dreammode", "mutemode", and "fleecyMode" because they default to T F F**

```json
[
    { "netServer" : "irc.freenode.net"
      , "netPort" : 6697
      , "netSSL"  : false
      , "netNick" : "vomitchan"
      , "netPass" : "PASS"
      , "netChans" : ["#lainchan", "##0chan", "#arisuchan", "#em.slashem.me", "#Haskell"]
      , "netState" : [["#lainchan", {"dream"  : true,
                                     "mute"   : false,
                                     "fleecy" : false}]
                      ,["#em.slashem.me", {"dream"  : true,
                                           "mute"   : false,
                                           "fleecy" : true}]]
    },

    { "netServer" : "other"
      , "netPort" : 6697
      , "netSSL"  : false
      , "netNick" : "vomitchan"
      , "netPass" : ""
      , "netChans" : ["#bots", "#programming"]
      , "netState" : []
    }
]

```

You can add multiple networks if you please.
**NOTE: Some networks may not get the join messages if one does not wait for authentication**

## Running
The bot can be started within ghci by running `stack ghci` in the project directory. Once at the prompt, run the `main` function.
`stack build` should also create an executable that can be started with `stack exec vomitchan-exe`

## Planned Features
- Versioning
- Per-channel rate limiting, to prevent spam