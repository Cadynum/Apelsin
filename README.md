# Apelsin
Apelsin is a two-paned, open source, server- and clan browser.
It shows servers and players from **Tremulous 1.1**, **Tremulous GPP**
and **Unvanquished** at the same time.
Refreshing all servers should take about 1-2 seconds depending on your connection.
The only dependency is gtk2, and gmp if you're on linux.
Apelsin typically uses about 14MiB RAM.
License: GPL3
Written in: Haskell

## Features
* Fuzzy search matching everything (`-` excludes)
* Server browser
* Find players
* Online clan members
* Synchronized clan list
* Optional auto-refreshing of servers

You can change the ratio between the panes by dragging the handle in the middle.

### Clan list
The clan list is by default synced against the DDOS server on startup.
As the list has a historical value, no clans will be removed.

**Add or update clan entries:**

* Mail: clans@ddos-tremulous.eu
* Irc: ##ddos @ irc.freenode.net


## Keyboard shotcuts
* Ctrl+R or F5:     Refresh all servers
* Ctrl+S or F6:     Sync clan list
* F7:               About
* Ctrl+L or Ctrl+F: Focus Input
* Alt+[1-5]:        Switch tab

## Build and install
    cd Apelsin
    cabal install

