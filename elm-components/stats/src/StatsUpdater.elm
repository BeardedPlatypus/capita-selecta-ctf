module StatsUpdater ( Action ( FetchStats
                             , NewStats
                             )
                    , getStats ) where


import Effects exposing (Effects, Never)
import Http
import Json.Decode exposing (..)
import Task

import Team

-- Update Object
------------------------------------------------------------
type alias Object = { score : Int
                    , kills : Int
                    , deaths : Int
                    , team_scores : List ( Team.ID, Int )
                    }

-- Update actions
------------------------------------------------------------
type Action = FetchStats
            | NewStats ( Maybe Object )


getStats : String -> Effects Action
getStats url =
  Http.get decodeStats url
      |> Task.toMaybe
      |> Task.map NewStats
      |> Effects.task


decodeStats : Decoder Object
decodeStats = object4 Object ("score" := int)
                             ("kills" := int)
                             ("deaths" := int)
                             ("team_scores" := decodeTeamScore)


type alias TeamScore = { tc : String
                       , score : Int
                       }

teamScoreToId : TeamScore -> ( Team.ID, Int )
teamScoreToId team_score =
  let
    toTeamID str_id =
      case str_id of
        "r" -> Team.Red
        "b" -> Team.Blue
        _   -> Team.Undefined
  in
    ( toTeamID team_score.tc
    , team_score.score )


decodeTeamScore : Decoder ( List ( Team.ID, Int ) )
decodeTeamScore =
  list ( map teamScoreToId ( object2 TeamScore ( "tc" := string )
                                               ( "score" := int )
                           )
       )
