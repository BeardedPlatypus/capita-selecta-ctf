module ClassDisplayConnection ( Action ( Request
                                       , Response)
                              , RequestAction ( FetchIsAlive
                                              , SendNewClass )
                              , ResponseAction ( ResponseIsAlive
                                               , ResponseClass )
                              , processRequest
                              ) where


-- Import Libraries
--------------------------------------------------------------------------------
import Effects exposing ( Effects )
import Http
import Json.Decode exposing ( .. )
import Task

-- Import ClassDisplay modules
--------------------------------------------------------------------------------
import PlayerClass

-- Update
--------------------------------------------------------------------------------
type Action = Request  RequestAction
            | Response ResponseAction


type RequestAction = FetchIsAlive
                   | SendNewClass PlayerClass.ID


type ResponseAction = ResponseIsAlive ( Maybe Bool )
                    | ResponseClass PlayerClass.ID ( Maybe PlayerClass.ID )


processRequest : String -> RequestAction -> Effects Action
processRequest base_url request =
  case request of
    FetchIsAlive -> fetchIsAlive base_url
    SendNewClass class_id -> sendNewClass base_url class_id


fetchIsAlive : String -> Effects Action
fetchIsAlive base_url =
  Http.get ( "is_alive" := bool ) ( base_url ++ "is_alive/" )
      |> Task.toMaybe
      |> Task.map ResponseIsAlive
      |> Effects.task
      |> Effects.map Response


sendNewClass : String -> PlayerClass.ID -> Effects Action
sendNewClass url id =
  let
    class_url = url ++ "respawn_as_" ++ PlayerClass.toDjango id ++ "/"
    decodeClassID = map PlayerClass.fromDjango ( "class_id" := string )
  in
    Http.get decodeClassID class_url
        |> Task.toMaybe
        |> Task.map ( ResponseClass id )
        |> Effects.task
        |> Effects.map Response
