module Stats where

import StartApp
import Task exposing ( Task )
import Signal exposing ( Signal, Address )
import Effects exposing ( Effects, Never )
import Html exposing ( Html )
import Time

import StatsPanel exposing ( Model, Action )
import StatsUpdater
import Team


-- StartApp Boilerplate
------------------------------------------------------------
app = StartApp.start { init = StatsPanel.init
                     , view = StatsPanel.view
                     , update = StatsPanel.update
                     , inputs = [ modelUpdates ]
                     }

main : Signal Html
main = app.html

port tasks : Signal ( Task Never () )
port tasks = app.tasks


-- Ports
------------------------------------------------------------
port teamIn : Signal String
port teamScoreIn : Signal ( String, Int )

port playerNameIn   : Signal String
port playerScoreIn  : Signal Int
port playerKillsIn  : Signal Int
port playerDeathsIn : Signal Int
port statsUrlIn : Signal String


-- Functions
------------------------------------------------------------
strToTeamID : String -> Team.ID
strToTeamID team_id =
  case team_id of
    "Red"  -> Team.Red
    "Blue" -> Team.Blue
    _      -> Team.Undefined


updateTeamIn : Signal StatsPanel.Action
updateTeamIn =
  let
    toAction = \id -> if id == Team.Undefined then StatsPanel.NoAction
                                              else StatsPanel.UpdateTeam id
    fMap = \str -> toAction ( strToTeamID str )
  in
    Signal.map fMap teamIn



updateTeamScoreIn : Signal StatsPanel.Action
updateTeamScoreIn =
  let
    toAction = \v -> if fst v == Team.Undefined
                       then StatsPanel.NoAction
                       else StatsPanel.UpdateTeamScore (fst v) (snd v)
    fMap = \v -> toAction ((strToTeamID ( fst v)), ( snd v ))
  in
    Signal.map fMap teamScoreIn


-- Mapping to Action and merging the signals
------------------------------------------------------------
modelUpdates : Signal StatsPanel.Action
modelUpdates =
  let
    updatePlayerName = Signal.map StatsPanel.UpdatePlayerName playerNameIn
    updatePlayerScore = Signal.map StatsPanel.UpdateScore playerScoreIn
    updatePlayerKills = Signal.map StatsPanel.UpdateKills playerKillsIn
    updatePlayerDeaths = Signal.map StatsPanel.UpdateDeaths playerDeathsIn
    updateStatsUrl = Signal.map StatsPanel.UpdateFetchUrl statsUrlIn
    fetchStats = Signal.map ( \t -> StatsPanel.UpdateStats StatsUpdater.FetchStats )
                            ( Time.every ( 5 * Time.second ) )
  in
    Signal.mergeMany [ updateTeamIn
                     , updateTeamScoreIn
                     , updatePlayerName
                     , updatePlayerKills
                     , updatePlayerDeaths
                     , updateStatsUrl
                     , fetchStats
                     ]
