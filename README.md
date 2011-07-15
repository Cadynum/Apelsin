# Apelsin
Apelsin is a two-paned, open source, server- and clan browser. It shows servers and players from both 1.1 and GPP at the same time.
The primary design goal was to make a fast, elegant and responsive program. Refreshing all servers should take about 1-2 seconds depending on your Internet connection.
The only dependency is gtk2. (and gmp if you're on linux)
License: GPL3
Written in: Haskell

## Features
#### Right pane
Shows information about your currently selected server.

Note: You can change the ratio between the panes by dragging the handle in the middle.

#### Server Browser
The server browser is pretty self explanatory. You can sort and filter by mod, name, map, ping and players.
The filtering algorithm is fuzzy. "gpp niveus" would show all gpp servers with niveus on.
The - operator excludes: "-atcs 1.1" would show all 1.1 servers not playing atcs.

#### Find players
The filtering here matches only the player name.

#### Online clans
Shows a list of online clan members, much like the discontinued clans.tremulous.net.

#### Clan list
A dynamic list of all known Tremulous clans.
If your clan is not on the list, or the information is outdated you should tell us at ##ddos @ irc.freenode.net.
If you don't like irc you can send an email to clans@ddos-tremulous.eu
Due to the spam problem the clanlist is not open to the public for modification.
The list has a historical value, so no clans will be deleted.
Apelsin updates the database on each startup (unless you disable it), and on "Sync clan list"

#### Preferences
Set your tremulous path, change the color theme and modify various other settings.


## Keyboard shotcuts
* Ctrl+R or F5:		Refresh all servers
* Ctrl+S or F6:		Sync clan list
* Ctrl+A or F7:		About
* Ctrl+L or Ctrl+F:	Focus Input
* Alt+1-5:		Switch tab
