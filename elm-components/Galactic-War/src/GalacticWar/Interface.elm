port module GalacticWar.Interface exposing ( .. )


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

import GalacticWar.QRModule as QRModule
import GalacticWar.QRModule.QRResult as QRResult

import GalacticWar.Energy as Energy
import GalacticWar.PlayerInteraction as PlayerInteraction

import GalacticWar.Util as Util


-- Model
--------------------------------------------------------------------------------
type alias Model = { stats : Stats.Model
                   , qr : QRModule.Model
                   , energy : Energy.Model
                   , server_url : String
                   , poll_data_interval : Float
                   , is_alive : Bool
                   }


type alias Flags = { server_url : String
                   , poll_data_interval : Float
                   , width  : Int
                   , height : Int
                   , qr_interval : Float
                   , reset_n : Int
                   }


init : Flags -> ( Model, Cmd Msg )
init flags =
  let
    ( qr_model, qr_cmd ) = QRModule.init { width   = flags.width
                                         , height  = flags.height
                                         , reset_n = flags.reset_n
                                         , qr_interval = flags.qr_interval
                                         }
    ( stats_model, stats_cmd ) = Stats.init
    ( energy_model, energy_cmd ) = Energy.init
  in
    ( { qr = qr_model
      , stats = stats_model
      , energy = energy_model
      , server_url = flags.server_url
      , poll_data_interval = flags.poll_data_interval
      , is_alive = True
      }
    , Cmd.batch [ Cmd.map toGenQR qr_cmd
                , Cmd.map toGenStats stats_cmd
                , Cmd.map toGenEnergy energy_cmd
                ]
    )



-- Update
--------------------------------------------------------------------------------
type Msg = Interaction InteractionMsg
         | Request     RequestMsg
         | Response    ResponseMsg
         | UpdateModel UpdateModelMsg
         | ChildGenMsg ChildMsg
         | UpdateChild ChildMsg

type InteractionMsg = QRButtonClick PlayerInteraction.Action

type RequestMsg = QRModuleInit
                | QRModuleDecode
                | PollData
--              | RequestNewClass Class.ID
--              | RequestNewNodeClass Class.ID

type ResponseMsg = QRInitSuccess   String
                 | QRInitFailure   String
                 | QRDecodeSuccess String
                 | QRDecodeFailure String
                 | QRStartDecoding
                 | PollSuccess PollDataSet
                 | PollFailure Http.Error
                 | RespawnSuccess RespawnSuccessObject
                 | RespawnFailure Http.Error
                 | ChallengePlayerSuccess ChallengeSuccessObject
                 | ChallengePlayerFailure Int Http.Error
                 | ChallengeNodeSuccess ChallengeSuccessObject
                 | ChallengeNodeFailure Int Http.Error
--               | NewClassSucceed ( Maybe Class.ID )
--               | NewClassFail Class.ID Http.Error
--               | NewNodeClassSuccess ( Maybe ( Node.ID, Class.ID ))
--               | NewNodeClassFailure Node.ID Class.ID Http.Error

type UpdateModelMsg = Discharge
                    | Recharge
                    | Activate
                    | Deactivate
--                  | PlayerDied
--                  | PlayerRespawned

type ChildMsg = QRMsg           QRModule.Msg
              | StatsMsg        Stats.Msg
              | EnergyMsg       Energy.Msg
--            | NodeDisplayMsg  NodeDisplay.Msg
--            | ClassDisplayMsg ClassDisplay.Msg


--------------------------------------------------------------------------------
toUpdateQR : QRModule.Msg -> Msg
toUpdateQR msg = UpdateChild ( QRMsg msg )

toGenQR : QRModule.Msg -> Msg
toGenQR msg = ChildGenMsg ( QRMsg msg )


toUpdateStats : Stats.Msg -> Msg
toUpdateStats msg = UpdateChild ( StatsMsg msg )

toGenStats : Stats.Msg -> Msg
toGenStats msg = ChildGenMsg ( StatsMsg msg )


toUpdateEnergy : Energy.Msg -> Msg
toUpdateEnergy msg = UpdateChild ( EnergyMsg msg )

toGenEnergy : Energy.Msg -> Msg
toGenEnergy msg = ChildGenMsg ( EnergyMsg msg )

--------------------------------------------------------------------------------


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Interaction interaction_msg  -> updateInteraction interaction_msg  model
    Request     request_msg      -> updateRequest     request_msg      model
    Response    response_msg     -> updateResponse    response_msg     model
    UpdateModel update_model_msg -> updateModel       update_model_msg model
    ChildGenMsg child_msg        -> updateChildGenMsg child_msg        model
    UpdateChild child_msg        -> updateChild       child_msg        model


--------------------------------------------------------------------------------
updateInteraction : InteractionMsg -> Model -> ( Model, Cmd Msg )
updateInteraction msg model =
  let
    discharge_cmd = Util.toCmd ( UpdateModel Discharge )
  in
    case msg of
      QRButtonClick action ->
        case action of
          PlayerInteraction.Respawn                   ->
            ( model, requestRespawn model.server_url )
          PlayerInteraction.ChallengePlayer player_id ->
            ( model, Cmd.batch [ requestChallengePlayer model.server_url player_id
                               , discharge_cmd ])
          PlayerInteraction.ChallengeNode node_id     ->
            ( model, Cmd.batch [ requestChallengeNode model.server_url node_id
                               , discharge_cmd ])
          _                         ->
            ( model, Cmd.none )


--------------------------------------------------------------------------------
updateRequest : RequestMsg -> Model -> ( Model, Cmd Msg )
updateRequest msg model =
  case msg of
    QRModuleInit    -> ( model
                       , ( initVideoSource { width  = model.qr.camera_feed.width
                                         , height = model.qr.camera_feed.height
                                         } )
                       )
    QRModuleDecode  -> ( model
                       , if model.qr.qr_decoding then ( requestQR { } )
                                                 else Cmd.none )
    PollData        -> ( model, pollData model.server_url )


--------------------------------------------------------------------------------
updateResponse : ResponseMsg -> Model -> ( Model, Cmd Msg )
updateResponse msg model =
  case msg of
    QRInitSuccess   video_url ->
      ( model
      , Cmd.batch [ Util.toCmd ( toUpdateQR ( QRModule.InitSuccess video_url
                                           |> QRModule.Response ))
                  , playVideoSource { width  = model.qr.camera_feed.width
                                    , height = model.qr.camera_feed.height
                                    } ]
      )
    QRInitFailure   error_msg ->
      ( model
      , Util.toCmd ( toUpdateQR ( QRModule.InitFailure error_msg
                               |> QRModule.Response ))
      )
    QRDecodeSuccess result    ->
      ( model
      , Util.toCmd ( toUpdateQR ( QRModule.QRSuccess result
                               |> QRModule.Response ))
      )
    QRDecodeFailure error_msg ->
      ( model
      , Util.toCmd ( toUpdateQR ( QRModule.QRFailure error_msg
                               |> QRModule.Response ))
      )
    QRStartDecoding           ->
      ( model
      , Util.toCmd ( toUpdateQR ( QRModule.StartDecoding
                               |> QRModule.Response ))
      )
    PollSuccess data          ->
      let
        change_state = if model.is_alive && ( not data.is_alive )      then [ Util.toCmd ( UpdateModel Deactivate ) ]
                       else if ( not model.is_alive ) && data.is_alive then [ Util.toCmd ( UpdateModel Activate ) ]
                       else [ ]
      in
        ( { model | is_alive = data.is_alive }
        , Cmd.batch ([ Util.toCmd ( toUpdateStats ( Stats.UpdateScore data.score
                                                 |> Stats.UpdateModel ))
                     , Util.toCmd ( toUpdateStats ( Stats.UpdateKills data.kills
                                                 |> Stats.UpdateModel ))
                     , Util.toCmd ( toUpdateStats ( Stats.UpdateDeaths data.deaths
                                                 |> Stats.UpdateModel ))
                    ] ++ change_state ))
    PollFailure error         ->
      ( model
      , Cmd.none
      )  -- FIXME
    RespawnSuccess data      ->
      ( model
      , Cmd.none )
    RespawnFailure error    ->
      ( model
      , requestRespawn model.server_url )
    ChallengePlayerSuccess result ->
      ( model
      , Util.toCmd ( toUpdateEnergy ( Energy.UpdateModel ( Energy.UpdateResetTime result.cool_down ))))
    ChallengePlayerFailure player_id error ->
      ( model
      , requestChallengePlayer model.server_url player_id )
    ChallengeNodeSuccess result ->
      ( model
      , Util.toCmd ( toUpdateEnergy ( Energy.UpdateModel ( Energy.UpdateResetTime result.cool_down ))))
    ChallengeNodeFailure node_id error ->
      ( model
      , requestChallengeNode model.server_url node_id )

--------------------------------------------------------------------------------
updateModel : UpdateModelMsg -> Model -> ( Model, Cmd Msg )
updateModel msg model =
  case msg of
    Discharge  ->
      ( model
      , Cmd.batch [ Util.toCmd ( toUpdateEnergy ( Energy.UpdateModel Energy.Discharge ))
                  , Util.toCmd ( toUpdateQR ( QRModule.UpdateModel
                                            ( QRModule.UpdateIsDisabled True )))
                  ]
      )
    Recharge   ->
      ( model
      , Util.toCmd ( toUpdateQR ( QRModule.UpdateModel
                                ( QRModule.UpdateIsDisabled False )))
      )
    Deactivate ->
      ( model
      , Util.toCmd ( toUpdateQR ( QRModule.UpdateModel
                                ( QRModule.UpdateIsActive False ))))
    Activate   ->
      ( model
      , Util.toCmd ( toUpdateQR ( QRModule.UpdateModel
                                ( QRModule.UpdateIsActive True ))))

--------------------------------------------------------------------------------
updateChildGenMsg : ChildMsg -> Model -> ( Model, Cmd Msg )
updateChildGenMsg msg model =
  case msg of
    QRMsg qr_msg ->
      let
        child_cmd = Util.toCmd ( toUpdateQR qr_msg )
        additional_cmds =
          case qr_msg of
            QRModule.Interaction interaction_msg ->
              case interaction_msg of
                QRModule.ButtonInteraction action -> [ Util.toCmd ( Interaction ( QRButtonClick action )) ]
            QRModule.Request     request_msg     ->
              case request_msg of
                QRModule.RequestInit -> [ Util.toCmd ( Request QRModuleInit ) ]
                QRModule.RequestQR   -> [ Util.toCmd ( Request QRModuleDecode ) ]
            _ -> [ ]
      in
        ( model, Cmd.batch ( [ child_cmd ] ++ additional_cmds ))
    StatsMsg stats_msg ->
      let
        child_cmd = Util.toCmd ( toUpdateStats stats_msg )
      in
        ( model, child_cmd )
    EnergyMsg energy_msg ->
      let
        child_cmd = Util.toCmd ( toUpdateEnergy energy_msg )
        additional_cmds =
          case energy_msg of
            Energy.UpdateModel update_model_msg ->
              case update_model_msg of
                Energy.Recharge -> [ Util.toCmd ( UpdateModel Recharge ) ]
                _               -> [ ]
            _ -> [ ]
      in
        ( model, Cmd.batch ([ child_cmd ] ++ additional_cmds ))


--------------------------------------------------------------------------------
updateChild : ChildMsg -> Model -> ( Model, Cmd Msg )
updateChild msg model =
  case msg of
    QRMsg qr_msg ->
      let
        ( updated_qr, qr_cmd ) = QRModule.update qr_msg model.qr
      in
        ( { model | qr = updated_qr }
        , Cmd.map toGenQR qr_cmd )
    StatsMsg stats_msg ->
      let
        ( updated_stats, stats_cmd ) = Stats.update stats_msg model.stats
      in
        ( { model | stats = updated_stats }
        , Cmd.map toGenStats stats_cmd )
    EnergyMsg energy_msg ->
      let
        ( updated_energy, energy_cmd ) = Energy.update energy_msg model.energy
      in
        ( { model | energy = updated_energy }
        , Cmd.map toGenEnergy energy_cmd )


-- Communication
--------------------------------------------------------------------------------
-- Out
type alias RequestInitObject = { width  : Int
                               , height : Int }
port initVideoSource : RequestInitObject -> Cmd msg
port playVideoSource : RequestInitObject -> Cmd msg

type alias RequestQRObject = { }
port requestQR : RequestQRObject -> Cmd msg


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
    pollSuccess data  = Response ( PollSuccess data )
    pollFailure error = Response ( PollFailure error )
  in
    Task.perform pollFailure pollSuccess ( Http.get decodePollData poll_url )


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
requestRespawn : String -> Cmd Msg
requestRespawn server_url =
  let
    respawn_url = server_url ++ "respawn/"
    requestSuccess data  = Response ( RespawnSuccess data )
    requestFailure error = Response ( RespawnFailure error )
  in
    Task.perform requestFailure requestSuccess ( Http.get decodeRequestRespawn
                                                          respawn_url
                                               )

type alias RespawnSuccessObject = { player_id : Int
                                  , is_alive  : Bool
                                  }

decodeRequestRespawn : Json.Decoder RespawnSuccessObject
decodeRequestRespawn = Json.object2 RespawnSuccessObject ( "player_id" := Json.int  )
                                                         ( "is_alive"  := Json.bool )

--------------------------------------------------------------------------------
requestChallengePlayer : String -> Int -> Cmd Msg
requestChallengePlayer server_url player_id =
  let
    challenge_player_url = server_url ++ "challenge_player_" ++ ( toString player_id ) ++ "/"
    requestSuccess data  = Response ( ChallengePlayerSuccess data )
    requestFailure error = Response ( ChallengePlayerFailure player_id error )
  in
    Task.perform requestFailure requestSuccess ( Http.get decodeChallengeResult
                                                          challenge_player_url
                                               )


requestChallengeNode : String -> Int -> Cmd Msg
requestChallengeNode server_url node_id =
  let
    challenge_node_url = server_url ++ "challenge_node_" ++ ( toString node_id ) ++ "/"
    requestSuccess data  = Response ( ChallengeNodeSuccess data )
    requestFailure error = Response ( ChallengeNodeFailure node_id error )
  in
    Task.perform requestFailure requestSuccess ( Http.get decodeChallengeResult
                                                          challenge_node_url
                                               )


type ChallengeStatus = Win
                     | Draw
                     | Lose
                     | Ignore

toChallengeStatus : String -> ChallengeStatus
toChallengeStatus status_str =
  case status_str of
    "win"  -> Win
    "draw" -> Draw
    "lose" -> Lose
    _      -> Ignore

type alias ChallengeSuccessObject = { cool_down : Int
                                    , status : ChallengeStatus
                                    }

decodeChallengeResult : Json.Decoder ChallengeSuccessObject
decodeChallengeResult =
  let
    statusDecoder = Json.map toChallengeStatus Json.string
  in
    Json.object2 ChallengeSuccessObject ( "cool_down" := Json.int )
                                        ( "status" := statusDecoder )


--------------------------------------------------------------------------------
-- In
type alias ResponseInitSuccessObject = { url : String }
port receiveVideoSourceSuccess : ( ResponseInitSuccessObject -> msg ) -> Sub msg

type alias ResponseInitFailureObject = { error_msg : String }
port receiveVideoSourceFailure : ( ResponseInitFailureObject -> msg ) -> Sub msg


-------------------------------------------------------------------------------
type alias ResponseStartDecodingObject = { is_playing : Bool }
port receiveStartDecoding : ( ResponseStartDecodingObject -> msg ) -> Sub msg


-------------------------------------------------------------------------------
type alias ResponseQRDecodeSuccessObject = { result: String }
port receiveQRDecodeSuccess : ( ResponseQRDecodeSuccessObject -> msg ) -> Sub msg

type alias ResponseQRDecodeFailureObject = { error_msg : String }
port receiveQRDecodeFailure : ( ResponseQRDecodeFailureObject -> msg ) -> Sub msg


-------------------------------------------------------------------------------
requestPollData : Float -> Sub Msg
requestPollData interval = Time.every ( interval * Time.second ) (\_ -> ( Request PollData ))

-- Subscriptions
--------------------------------------------------------------------------------
subscriptions : Model -> Sub Msg
subscriptions model =
  let
    responseInitSuccessToMsg response = Response ( QRInitSuccess response.url )
    responseInitFailureToMsg response = Response ( QRInitFailure response.error_msg )

    responseStartDecodingToMsg response = Response QRStartDecoding

    responseQRDecodeSuccessToMsg response = Response ( QRDecodeSuccess response.result )
    responseQRDecodeFailureToMsg response = Response ( QRDecodeFailure response.error_msg )
  in
    Sub.batch [ -- Port Subscriptions
                receiveVideoSourceSuccess responseInitSuccessToMsg
              , receiveVideoSourceFailure responseInitFailureToMsg
              , receiveQRDecodeSuccess    responseQRDecodeSuccessToMsg
              , receiveQRDecodeFailure    responseQRDecodeFailureToMsg
              , receiveStartDecoding      responseStartDecodingToMsg
                -- Poll Data
              , requestPollData           model.poll_data_interval
                -- Module Subscriptions
              , Sub.map toGenQR ( QRModule.subscriptions model.qr )
              , Sub.map toGenEnergy (Energy.subscriptions model.energy )
              ]


-- View
--------------------------------------------------------------------------------
view : Model -> Html Msg
view model =
  let
    container = Html.div [ Html.Attributes.class "col-lg-12 ctf_component_side" ]
    component_row = Html.div [ Html.Attributes.class "row ctf_component_section" ]

    view_stats  = viewStats  model.stats
    view_qr     = viewQR     model.qr
    view_energy = viewEnergy model.energy
  in
    container [ component_row [ Html.div [ Html.Attributes.id "stats" ]
                                         [ view_stats ]
                              ]
              , component_row [ Html.div [ Html.Attributes.id "qr" ]
                                         [ view_qr ]
                              ]
              , component_row [ Html.div [ Html.Attributes.id "energy" ]
                                         [ view_energy ]
                              ]
              ]


viewQR : QRModule.Model -> Html Msg
viewQR model =
  App.map toGenQR ( QRModule.view model )

viewStats : Stats.Model -> Html Msg
viewStats model =
  App.map toGenStats ( Stats.view model )

viewEnergy : Energy.Model -> Html Msg
viewEnergy model =
  App.map toGenEnergy ( Energy.view model )
