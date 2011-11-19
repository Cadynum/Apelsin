module Types (
	module Control.Concurrent, module ConcurrentUtil
	, Bundle(..), ClanHook, PolledHook, ClanPolledHook, SetCurrent, BrowserStore, PlayerStore
) where
import Graphics.UI.Gtk

import Control.Concurrent
import ConcurrentUtil
import Network.Tremulous.Protocol

import Config
import ClanFetcher
import IndividualServerSettings

data Bundle = Bundle {
	  mpolled	:: !(MVar PollResult)
	, mconfig	:: !(MVar Config)
	, mclans	:: !(MVar [Clan])
	, mrefresh	:: !(MVar ())
	, parent	:: !Window
	, browserStore	:: !BrowserStore
	, playerStore	:: !PlayerStore
	, msettings	:: !(MVar ServerSettings)
	}

type ClanHook		= [Clan] -> IO ()
type PolledHook		= PollResult -> IO ()
type ClanPolledHook	= [Clan] -> PollResult -> IO ()
type SetCurrent		= Bool -> GameServer -> IO ()
type PlayerStore	= ListStore (TI, GameServer)
type BrowserStore	= ListStore GameServer
