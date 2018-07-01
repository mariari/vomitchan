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
- '.lotg <someone>' - gives the luck of the grasshopper to a user
- '.8ball <question>' - acts like an eight ball!

### Admin Only
- `.quit` - kill vomitchan ;-;

## Building
### Requirements
- Stack
- Haskell packages (stack should install these for you):
  - network
  - text
  - text-format
  - regex-tdfa
  - bytestring
  - aeson
  - monad-loops
  - turtle
  - connection
  - bytestring
  - random
  - stm
  - hashtables

### Building on Linux
- clone this repo using `git clone https://gitla.in/nymphet/vomitchan.git`
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
      , "netChans" : ["#lainchan", "##loli", "##0chan", "#arisuchan", "#em.slashem.me"]
      , "netState" : [["#lainchan", {"dream"  : true,
                                     "mute"   : false,
                                     "fleecy" : false}]
                      ,["#em.slashem.me", {"dream"  : true,
                                           "mute"   : false,
                                           "fleecy" : true}]]
    },

    { "netServer" : "lainchan.org"
      , "netPort" : 6697
      , "netSSL"  : false
      , "netNick" : "vomitchan"
      , "netPass" : ""
      , "netChans" : ["#bots", "#lainchan"]
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
- Support for the 'major' IRC networks (Freenode, Rizon, IRCNet, QuakeNet, Etc.)
- Per-channel rate limiting, to prevent spam
- use parser combinators for handling messages