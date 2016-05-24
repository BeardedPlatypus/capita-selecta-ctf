module Port ( modelUpdates ) where


import Signal exposing ( Signal )

import StatsPanel exposing ( Model, Action )
import Team

-- Ports
------------------------------------------------------------
port teamIn : Signal String
port teamScoreIn : Signal ( String, Int )

port playerNameIn   : Signal String
port playerScoreIn  : Signal Int
port playerKillsIn  : Signal Int
port playerDeathsIn : Signal Int


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
    fMap = \str -> toAction ( strToTeamId str )
  in
    Signal.map fMap teamIn



updateTeamScoreIn : Signal StatsPanel.Action
updateTeamScoreIn =
  let
    toAction = \v -> if fst v == Team.Undefined
                       then StatsPanel.Noaction
                       else StatsPanel.UpdateTeamScore (fst v) (snd v)
    fMap = \v -> toAction ((strToTeamId ( fst v)), ( snd v ))
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
  in
    Signal.mergeMany [ updateTeamIn
                     , updateTeamScoreIn
                     , updatePlayerName
                     , updatePlayerKills
                     , updatePlayerDeaths
                     ]
