module Types (
	module Control.Concurrent.STM
	, Bundle(..), ClanHook, PolledHook, ClanPolledHook, SetCurrent, BrowserStore, PlayerStore
) where
import Graphics.UI.Gtk

import Control.Concurrent.STM
import Control.Concurrent
import Network.Tremulous.Protocol

import Config
import ClanFetcher
import IndividualServerSettings

data Bundle = Bundle {
	  mpolled	:: !(TMVar PollResult)
	, mconfig	:: !(TMVar Config)
	, mclans	:: !(TMVar [Clan])
	, mrefresh	:: !(MVar ())
	, parent	:: !Window
	, browserStore	:: !BrowserStore
	, playerStore	:: !PlayerStore
	, msettings	:: !(TMVar ServerSettings)
	}

type ClanHook		= [Clan] -> IO ()
type PolledHook		= PollResult -> IO ()
type ClanPolledHook	= [Clan] -> PollResult -> IO ()
type SetCurrent		= Bool -> GameServer -> IO ()
type PlayerStore	= ListStore (TI, GameServer)
type BrowserStore	= ListStore GameServer
