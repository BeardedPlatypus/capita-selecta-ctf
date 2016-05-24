module RespawnConnection where


-- is_alive checker
------------------------------------------------------------
import Effects exposing (Effects, Never)
import Http
import Json.Decode exposing (..)
import Task

import PlayerClass

-- Get is alive
------------------------------------------------------------
type IsAliveAction = FetchIsAlive
                   | NewIsAlive ( Maybe Bool )

getIsAlive : String -> Effects Action
getIsALive url =
  Http.get ( "is_alive" := bool ) url
      |> Task.toMaybe
      |> Task.map NewIsAlive
      |> Effects.task



-- Send respawn information
------------------------------------------------------------
type RespawnAsAction = SendRespawnAs PlayerClass.ID
                     | IsSuccess ( Maybe Bool )

respawnAs : PlayerClass.ID -> String -> Effects Action
respawnAs player_class base_url =
  Http.get ( "success" := bool )
           (classIdToRespawnUrl player_class base_url )
    |> Task.toMaybe
    |> Task.map IsSuccess
    |> Effects.task


classIdToRespawnUrl : String -> PlayerClass.Id -> String
classIdToRespawnUrl base_url class_id =
  case class_id of
    PlayerClass.Light  -> base_url ++ "respawn_as_li"
    PlayerClass.Medium -> base_url ++ "respawn_as_me"
    PlayerClass.Heavy  -> base_url ++ "respawn_as_he"
