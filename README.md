# DetoniaBot
A basic IRC bot written in Haskell that does absolutely nothing.

![DetoniaBot running in IRC](https://u.pomf.is/fkwwgf.png)

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
- clone this repo using `git clone git@gitla.in:MrDetonia/detoniabot.git`
- run `cd detoniabot && stack setup && stack build` to compile

## Configuration
DetoniaBot requires `data/networks.json` to store information about IRC networks to connect to.
This file should look like the following:

```json
[
  { "netServer" : "irc.freenode.net"
  , "netPort" : 6667
  , "netNick" : "bot_nick"
  , "netPass" : "nickserv_password"
  , "netChans" : [ "#chans", "#to", "#join"]
  }
]
```

You can add multiple networks if you please.  
**NOTE: joining multiple networks is currently buggy - expect it to not work**

## Running
The bot can be started within ghci by running `stack ghci` in the project directory. Once at the prompt, run the `main` function.  
`stack build` should also create an executable that can be started with `stack exec detoniabot-exe`

## Planned Features
- Versioning
- Support for joining multiple networks
- Support for the 'major' IRC networks (Freenode, Rizon, IRCNet, QuakeNet, Etc.)
- Nicer way of defining bot commands
- Per-channel rate limiting, to prevent spam
