module GalacticWar.Interface exposing ( .. )


-- Import Libraries
--------------------------------------------------------------------------------
import Html exposing ( Html )
import Html.App as App
import Html.Attributes
import Html.Events
import Platform.Cmd as Cmd exposing ( Cmd )
import Platform.Sub as Sub exposing ( Sub )
import List
import Http
import Json.Decode as Json exposing ( (:=))
import Time
import Task


-- Import modules
--------------------------------------------------------------------------------
import GalacticWar.Stats as Stats
import GalacticWar.ClassDisplay as ClassDisplay
import GalacticWar.Class as Class

import GalacticWar.Util as Util


-- Model
--------------------------------------------------------------------------------
type alias Model = { stats : Stats.Model
                   , class_display : ClassDisplay.Model
                   , server_url : String
                   }


init : Class.ID -> ( Model, Cmd Msg )
init class = ( { stats = fst Stats.init
               , class_display = fst ( ClassDisplay.init class )
               , server_url = "http://ctf-capita.rhcloud.com/players/1/"
               }
             , Cmd.none )


-- Update
--------------------------------------------------------------------------------
type Msg = Interaction InteractionMsg
         | Request     RequestMsg
         | Response    ResponseMsg
--         | UpdateModel UpdateModelMsg
         | UpdateChild UpdateChildMsg


type InteractionMsg = ClickClassDisplay Class.ID

type RequestMsg = PollData
                | RequestNewClass Class.ID

type ResponseMsg = PollSucceed PollDataSet
                 | PollFail Http.Error
                 | NewClassSucceed ( Maybe Class.ID )
                 | NewClassFail Class.ID Http.Error

-- FIXME
type UpdateModelMsg = PlayerDied
                    | PlayerRespawned

type UpdateChildMsg = UpdateClassDisplay ClassDisplay.Msg
                    | UpdateStats        Stats.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Interaction interaction_msg  -> updateInteraction interaction_msg  model
    Request     request_msg      -> updateRequest     request_msg      model
    Response    response_msg     -> updateResponse    response_msg     model
--    UpdateModel update_model_msg -> updateModel       update_model_msg model
    UpdateChild update_child_msg -> updateChild       update_child_msg model


--------------------------------------------------------------------------------
toUpdateStatsCmd : Stats.Msg -> Cmd Msg
toUpdateStatsCmd msg = Util.toCmd ( UpdateChild ( UpdateStats msg ))

toUpdateClassDisplayCmd : ClassDisplay.Msg -> Cmd Msg
toUpdateClassDisplayCmd msg = Util.toCmd ( UpdateChild ( UpdateClassDisplay msg ))


--------------------------------------------------------------------------------
-- FIXME: add actual request to this
updateInteraction : InteractionMsg -> Model -> ( Model, Cmd Msg )
updateInteraction msg model =
  case msg of
    ClickClassDisplay id ->
      ( model
      , Cmd.batch [ Util.toCmd ( Request ( RequestNewClass id ))
                  , toUpdateClassDisplayCmd ( ClassDisplay.Interaction ( ClassDisplay.ClickButton id ))
                  ]
      )


--------------------------------------------------------------------------------
updateRequest : RequestMsg -> Model -> ( Model, Cmd Msg )
updateRequest msg model =
  case msg of
    PollData -> ( model, pollData model.server_url )
    RequestNewClass id -> ( model, requestNewClass model.server_url id )


--------------------------------------------------------------------------------
updateResponse : ResponseMsg -> Model -> ( Model, Cmd Msg )
updateResponse msg model =
  case msg of
    PollSucceed data  ->
      ( model
      , Cmd.batch [ toUpdateStatsCmd ( Stats.UpdateModel ( Stats.UpdateScore data.score ))
                  , toUpdateStatsCmd ( Stats.UpdateModel ( Stats.UpdateKills data.kills ))
                  , toUpdateStatsCmd ( Stats.UpdateModel ( Stats.UpdateDeaths data.deaths ))
                  , toUpdateClassDisplayCmd ( ClassDisplay.UpdateModel
                                                ( ClassDisplay.UpdateIsAlive data.is_alive ))
                  ])
    PollFail    error ->
      ( model, toUpdateStatsCmd ( Stats.UpdateModel ( Stats.UpdateName (toString error ))) )
    NewClassSucceed maybe_new_class ->
      case maybe_new_class of
        Just new_class ->
          ( model
          , toUpdateClassDisplayCmd ( ClassDisplay.UpdateModel ( ClassDisplay.UpdateClass new_class))
          )
        Nothing ->
          ( model
          , Cmd.none
          )
    NewClassFail id error ->
      ( model, Util.toCmd ( Request ( RequestNewClass id )))



--------------------------------------------------------------------------------
--updateModel : UpdateModelMsg -> Model -> ( Model, Cmd Msg )


--------------------------------------------------------------------------------
updateChild : UpdateChildMsg -> Model -> ( Model, Cmd Msg )
updateChild msg model =
  case msg of
    UpdateClassDisplay class_display_msg ->
      let
        ( updated_display, display_cmds ) =
          ClassDisplay.update class_display_msg model.class_display
        mapMsg d_msg = UpdateChild ( UpdateClassDisplay d_msg )
      in
        ( { model | class_display = updated_display }
        , Cmd.map mapMsg display_cmds )
    UpdateStats stats_msg ->
      let
        ( updated_stats, stats_cmds ) =
          Stats.update stats_msg model.stats
        mapMsg s_msg = UpdateChild ( UpdateStats s_msg )
      in
        ( { model | stats = updated_stats }
        , Cmd.map mapMsg stats_cmds )


-- Connection
--------------------------------------------------------------------------------
type alias PollDataSet = { score : Int
                         , deaths : Int
                         , kills : Int
                         , is_alive : Bool
                         }


pollData : String -> Cmd Msg
pollData server_url =
  let
    poll_url = server_url ++ "poll_data/"
    pollSucceed data = Response ( PollSucceed data )
    pollFail error = Response ( PollFail error )
  in
    Task.perform pollFail pollSucceed ( Http.get decodePollData poll_url )


decodePollData : Json.Decoder PollDataSet
decodePollData = Json.object4 PollDataSet ( "score" := Json.int )
                                          ( "deaths" := Json.int )
                                          ( "kills" := Json.int )
                                          ( "is_alive" := Json.bool )

--------------------------------------------------------------------------------
requestNewClass : String -> Class.ID -> Cmd Msg
requestNewClass server_url id =
  let
    class_change_url = server_url ++ "request_class_" ++ (Class.toDjango id) ++ "/"
    requestSucceed data = Response ( NewClassSucceed data )
    requestFail error = Response ( NewClassFail id error )
  in
    Task.perform requestFail requestSucceed ( Http.get decodeRequestClass
                                                       class_change_url)

decodeRequestClass : Json.Decoder ( Maybe Class.ID )
decodeRequestClass = Json.map Class.fromDjango (( "class_id" := Json.string ))


-- Subscriptions
--------------------------------------------------------------------------------
subscriptions : Model -> Sub Msg
subscriptions model =
  let
    requestPoll x = Request PollData
  in
    Time.every Time.second requestPoll


-- View
--------------------------------------------------------------------------------
view : Model -> Html Msg
view model =
  let
    container = Html.div [ Html.Attributes.class "col-lg-12 ctf_component_side" ]
    component_row = Html.div [ Html.Attributes.class "row ctf_component_section" ]

    view_stats = viewStats model.stats
    view_display = viewClassDisplay model.class_display
  in
    container [ component_row [ Html.div [ Html.Attributes.id "stats" ]
                                         [ view_stats ]
                              , Html.div [ Html.Attributes.id "class_display" ]
                                         [ view_display ]
                              ]
              ]


--------------------------------------------------------------------------------
viewStats : Stats.Model -> Html Msg
viewStats model =
  let
    convertMsg msg = UpdateChild ( UpdateStats msg )
  in
    App.map convertMsg ( Stats.view model )


viewClassDisplay : ClassDisplay.Model -> Html Msg
viewClassDisplay model =
  let
    convertMsg msg =
      case msg of
        ClassDisplay.Interaction interaction_msg ->
          case interaction_msg of
            ClassDisplay.ClickButton id -> Interaction ( ClickClassDisplay id )
        _ -> UpdateChild ( UpdateClassDisplay msg )
  in
    App.map convertMsg ( ClassDisplay.view model )


