# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Vomitchan is a multi-network IRC bot written in Haskell. It connects to multiple IRC servers concurrently, manages per-channel state (dream/mute/fleecy/yuki modes), persists data in SQLite, and responds to user commands with styled text output.

## Build & Development

**Build tool**: Stack (with Hpack — `package.yaml` is the source of truth, `vomitchan.cabal` is generated)

```bash
stack build                  # compile
stack test                   # run hspec test suite
stack exec vomitchan-exe     # run the bot (requires data/networks.json)
stack ghci                   # REPL with OverloadedStrings enabled
```

**Resolver**: LTS 23.28 (see `stack.yaml`)

**Default extensions** (always on): `OverloadedStrings`, `DeriveGeneric`, `DeriveFunctor`

**GHC options**: The library uses `-Wno-x-partial`; the executable uses `-threaded -rtsopts -with-rtsopts=-N -fwarn-incomplete-patterns`.

## Architecture

### Entry point

`app/Main.hs` — reads `data/networks.json`, initializes SQLite (`genDb`), spawns one thread per IRC network via `forkWithKill`, and coordinates graceful shutdown through STM sets of connections.

### Module layout (all under `src/Bot/`)

**Type modules** (no logic, define data structures):
- `MessageType` — IRC message ADTs (PrivMsg, Join, Ping, etc.)
- `StateType` — `GlobalState`, `HashStorage` (per-channel modes), `Response`/`Quit` sum types, lens definitions
- `NetworkType` — `IRCNetwork`, `ChanOptions` (parsed from JSON config)
- `EffType` — type aliases for the command effect system (`Cmd`, `CmdImp`, `Func`, `Effect`, `ContFunc`)

**Core modules**:
- `MessageParser` — Attoparsec parser for raw IRC protocol messages
- `Commands` — command dispatch (`runCmd`) and all command implementations; uses `MonadReader InfoPriv` for context
- `Modifier` — text styling system with `TextEffects` (Bold, Italics, Colors, Strikethrough) applied via `Scope`/`Unit` abstractions that emit IRC control codes

**Infrastructure**:
- `Network` — reads network config JSON, establishes connections (`startNetwork`, `reconnectNetwork`, `joinNetwork`)
- `Socket` — low-level socket I/O (`write`, `listen`, `quitNetwork`)
- `Servers` — per-server connection tracking and state
- `State` — STM-based channel state queries/updates (`getChanState`, `modifyChanState`)
- `Database` — SQLite operations (tables: `channels`, `user`, `vomits`; link metadata, vomit counts, roulette)

**Utilities**:
- `Message` — message handling/routing
- `Misc` — small helpers
- `FileOps` — file operations
- `Examples` — example values used by tests

### Key design patterns

- **STM everywhere**: `GlobalState` wraps `StmContainers.Map Text HashStorage` for thread-safe per-channel state. Connections are tracked in `StmContainers.Set`.
- **Reader monad for commands**: command functions use `MonadReader InfoPriv` (via `Cmd m` constraint) to access message context without explicit argument passing.
- **`Response` as a result type**: commands return `Response a | NoResponse | Quit !Quit` — not `Maybe`. This separates "nothing to say" from "shut down".
- **Lens-based state**: `HashStorage` fields (`_dream`, `_mute`, `_fleecy`, `_yuki`) have TH-generated lenses.
- **JSON config**: `data/networks.json` defines all server connections, channels, admin hosts, ignore/ban lists, and per-channel mode defaults. Parsed via Aeson with custom `fieldLabelModifier = drop 1` for underscore-prefixed fields.

## Testing

Tests are in `test/Spec.hs` using hspec. Categories: parser correctness, modifier/text effects, channel state round-trips, command dispatch, and SQLite operations.

```bash
stack test                   # run all tests
```

## Configuration

The bot requires `data/networks.json` (not checked in). See `README.md` for the JSON schema. Key fields per network: `netServer`, `netPort`, `netSSL`, `netNick`, `netPass`, `netAdmins`, `netChans`, `netIgnore`, `netBans`, `netState`.

## Conventions

@.claude/skills/general-conventions/SKILL.md
@.claude/skills/git-conventions/SKILL.md
