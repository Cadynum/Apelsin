module Types (
	module Control.Concurrent.STM
	, Bundle(..), ClanHook, PolledHook, ClanPolledHook, SetCurrent
) where
import Graphics.UI.Gtk
import Config
import ClanFetcher
import Control.Concurrent.STM
import Network.Tremulous.Protocol

data Bundle = Bundle {
	  mpolled	:: !(TMVar PollResult)
	, mconfig	:: !(TMVar Config)
	, mclans	:: !(TMVar [Clan])
	, parent	:: !Window
	, browserStore	:: !(ListStore GameServer)
	}

type ClanHook		= [Clan] -> IO ()
type PolledHook		= PollResult -> IO ()
type ClanPolledHook	= [Clan] -> PollResult -> IO ()
type SetCurrent		= Bool -> GameServer -> IO ()
