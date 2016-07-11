# vomitchan
An IRC bot written in Haskell.  
\*cheek pinch\*

![vomitchan running in IRC](https://u.pomf.is/fkwwgf.png)

## Building
### Requirements
- Stack
- Haskell packages (install with `stack install`):
  - network
  - text
  - text-format
  - bytestring
  - aeson

Note: you can build and run this without Stack of course, figure it out for yourself.

### Building on Linux
- clone this repo using `git clone https://gitla.in/MrDetonia/vomitchan.git`
- run `cd vomitchan && stack setup && stack build` to compile

## Configuration
vomitchan requires `data/networks.json` to store information about IRC networks to connect to.
This file should look like the following:

```json
[
  { "netServer" : "irc.freenode.net"
  , "netPort" : 6667
  , "netNick" : "vomitchan"
  , "netPass" : "password"
  , "netChans" : [ "#lainchan", "#extra-chan"]
  }
]
```

You can add multiple networks if you please.  
**NOTE: multiple network support is experimental - expect everything to break horribly**

## Running
The bot can be started within ghci by running `stack ghci` in the project directory. Once at the prompt, run the `main` function.  
`stack build` should also create an executable that can be started with `stack exec detoniabot-exe`

## Planned Features
- Versioning
- Support for the 'major' IRC networks (Freenode, Rizon, IRCNet, QuakeNet, Etc.)
- Nicer way of defining bot commands
- Per-channel rate limiting, to prevent spam
- Refactor the ugly handlePM function
