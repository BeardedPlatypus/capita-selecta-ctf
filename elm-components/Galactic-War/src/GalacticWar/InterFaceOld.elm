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
import GalacticWar.Team as Team
import GalacticWar.NodeDisplay as NodeDisplay
import GalacticWar.NodeDisplay.Node as Node

import GalacticWar.Util as Util


-- Model
--------------------------------------------------------------------------------
type alias Model = { stats : Stats.Model
                   , class_display : ClassDisplay.Model
                   , node_display : NodeDisplay.Model
                   , server_url : String
                   }


init : String -> Class.ID -> Int -> Int -> List Node.Model -> List ( Node.ID, Node.ID ) -> ( Model, Cmd Msg )
init url class n_cols n_rows nodes paths =
  ( { stats = fst Stats.init
    , class_display = fst ( ClassDisplay.init class )
    , node_display = fst ( NodeDisplay.init class
                                            n_cols n_rows
                                            nodes paths )
    , server_url = url
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
                    | ClickNodeDisplayNode Node.ID Node.Status Class.ID

type RequestMsg = PollData
                | RequestNewClass Class.ID
                | RequestNewNodeClass Node.ID Class.ID

type ResponseMsg = PollSucceed PollDataSet
                 | PollFail Http.Error
                 | NewClassSucceed ( Maybe Class.ID )
                 | NewClassFail Class.ID Http.Error
                 | NewNodeClassSucceed ( Maybe ( Node.ID, Class.ID ))
                 | NewNodeClassFail Node.ID Class.ID Http.Error

-- FIXME
type UpdateModelMsg = PlayerDied
                    | PlayerRespawned

type UpdateChildMsg = UpdateClassDisplay ClassDisplay.Msg
                    | UpdateStats        Stats.Msg
                    | UpdateNodeDisplay  NodeDisplay.Msg


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

toUpdateNodeDisplayCmd : NodeDisplay.Msg -> Cmd Msg
toUpdateNodeDisplayCmd msg = Util.toCmd ( UpdateChild ( UpdateNodeDisplay msg ))

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
    ClickNodeDisplayNode node_id node_status class_id ->
      ( model
      , Cmd.batch [ Util.toCmd ( Request ( RequestNewNodeClass node_id class_id ))
                  , toUpdateNodeDisplayCmd ( NodeDisplay.Interaction ( NodeDisplay.NodeClick node_id
                                                                                             node_status
                                                                                             class_id))
                  ]
      )


--------------------------------------------------------------------------------
updateRequest : RequestMsg -> Model -> ( Model, Cmd Msg )
updateRequest msg model =
  case msg of
    PollData -> ( model, pollData model.server_url )
    RequestNewClass id -> ( model
                          , requestNewClass model.server_url id
                          )
    RequestNewNodeClass node_id class_id -> ( model
                                            , requestNewNodeClass model.server_url
                                                                  node_id
                                                                  class_id
                                            )


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
                  , toUpdateNodeDisplayCmd ( NodeDisplay.UpdateModel
                                                ( NodeDisplay.UpdateNodes data.status_nodes ))
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
    NewNodeClassSucceed maybe_new_node_class -> ( model, Cmd.none )
    NewNodeClassFail node_id class_id error ->
      ( model, Util.toCmd ( Request ( RequestNewNodeClass node_id class_id )))


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
    UpdateNodeDisplay node_display_msg ->
      let
        ( updated_node_display, node_display_cmds ) =
          NodeDisplay.update node_display_msg model.node_display
        mapMsg n_msg = UpdateChild ( UpdateNodeDisplay n_msg )
      in
        ( { model | node_display = updated_node_display  }
        , Cmd.map mapMsg node_display_cmds )


-- Connection
--------------------------------------------------------------------------------
type alias PollDataSet = { score : Int
                         , deaths : Int
                         , kills : Int
                         , is_alive : Bool
                         , status_nodes : List ( Node.ID, Node.Status )
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
decodePollData = Json.object5 PollDataSet ( "score" := Json.int )
                                          ( "deaths" := Json.int )
                                          ( "kills" := Json.int )
                                          ( "is_alive" := Json.bool )
                                          ( "status_nodes" := decodeStatusNodes )


type alias StatusEntry = { id : Int
                         , status : String
                         , class : String
                         }


decodeStatusNodes : Json.Decoder ( List ( Node.ID, Node.Status ))
decodeStatusNodes =
  let
    decode_single_entry = Json.object3 StatusEntry ( "id" := Json.int )
                                                   ( "status" := Json.string )
                                                   ( "class" := Json.string )

    entryToStatus s =
      case ( Team.fromDjango s.status ) of
        Just team_id ->
          case Class.fromDjango s.class of
            Just class_id -> Node.Claimed team_id class_id
            Nothing       -> Node.Unclaimed
        Nothing      ->
          Node.Unclaimed
    mapToTuple entry = ( entry.id, entryToStatus entry )
  in
    Json.list ( Json.map mapToTuple decode_single_entry )


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


--------------------------------------------------------------------------------
requestNewNodeClass : String -> Node.ID -> Class.ID -> Cmd Msg
requestNewNodeClass server_url node_id class_id =
  let
    node_change_url = server_url ++ "request_node_" ++ ( toString node_id )
                                 ++ "_to_" ++ ( Class.toDjango class_id ) ++ "/"
    requestSucceed data = Response ( NewNodeClassSucceed data )
    requestFail error = Response ( NewNodeClassFail node_id class_id error )
  in
    Task.perform requestFail requestSucceed ( Http.get decodeRequestNodeClass
                                                       node_change_url )


type alias DecodeRequestNodeClassResponse = { node_id : Int
                                            , class_id : String }

decodeRequestNodeClass : Json.Decoder ( Maybe ( Node.ID, Class.ID ) )
decodeRequestNodeClass =
  let
    decode_raw = Json.object2 DecodeRequestNodeClassResponse ( "node_id" := Json.int )
                                                             ( "class_id" := Json.string )
    toResult entry =
      case Class.fromDjango entry.class_id of
        Just class_id -> Just ( entry.node_id, class_id )
        Nothing       -> Nothing
  in
    Json.map toResult decode_raw


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
    view_node_display = viewNodeDisplay model
  in
    container [ component_row [ Html.div [ Html.Attributes.id "stats" ]
                                         [ view_stats ]
                              , Html.div [ Html.Attributes.id "class_display" ]
                                         [ view_display ]
                              ]
              , component_row [ Html.div [ Html.Attributes.id "node_display" ]
                                         [ view_node_display ]
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


viewNodeDisplay : Model -> Html Msg
viewNodeDisplay model =
  let
    convertMsg msg =
      case msg of
        NodeDisplay.Interaction interaction_msg ->
          case interaction_msg of
            NodeDisplay.NodeClick node_id node_status new_class_id ->
              case node_status of
                Node.Claimed team_id current_class_id ->
                  if team_id == model.stats.team then Interaction ( ClickNodeDisplayNode node_id
                                                                                         node_status
                                                                                         new_class_id )
                                                 else UpdateChild ( UpdateNodeDisplay msg )
                Node.Unclaimed                        ->
                  UpdateChild ( UpdateNodeDisplay msg )
            _ ->
              UpdateChild ( UpdateNodeDisplay msg )
        _ ->
         UpdateChild ( UpdateNodeDisplay msg )
  in
    App.map convertMsg ( NodeDisplay.view model.node_display )
