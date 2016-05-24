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

import GalacticWar.Util as Util


-- Model
--------------------------------------------------------------------------------
type alias Model = { qr : QRModule.Model
                   }


type alias Flags = { width  : Int
                   , height : Int
                   , qr_interval : Float
                   , reset_n : Int
                   }


init : Flags -> ( Model, Cmd Msg )
init flags =
  let
    ( qr_model, qr_cmd ) = QRModule.init flags
  in
    ( { qr = qr_model
      }
    , Cmd.map toGenQR qr_cmd )



-- Update
--------------------------------------------------------------------------------
type Msg = Interaction InteractionMsg
         | Request     RequestMsg
         | Response    ResponseMsg
         | ChildGenMsg ChildMsg
         | UpdateChild ChildMsg

type InteractionMsg = QRButtonClick

type RequestMsg = QRModuleInit
                | QRModuleDecode
--              | PollData
--              | RequestNewClass Class.ID
--              | RequestNewNodeClass Class.ID

type ResponseMsg = QRInitSuccess   String
                 | QRInitFailure   String
                 | QRDecodeSuccess String
                 | QRDecodeFailure String
                 | QRStartDecoding
--               | PollSuccess PollDataSet
--               | PollFailure Http.Error
--               | NewClassSucceed ( Maybe Class.ID )
--               | NewClassFail Class.ID Http.Error
--               | NewNodeClassSuccess ( Maybe ( Node.ID, Class.ID ))
--               | NewNodeClassFailure Node.ID Class.ID Http.Error

--type UpdateModelMsg = PlayerDied
--                  | PlayerRespawned

type ChildMsg = QRMsg           QRModule.Msg
--            | StatsMsg        Stats.Msg
--            | NodeDisplayMsg  NodeDisplay.Msg
--            | ClassDisplayMsg ClassDisplay.Msg


--------------------------------------------------------------------------------
toUpdateQR : QRModule.Msg -> Msg
toUpdateQR msg = UpdateChild ( QRMsg msg )

toGenQR : QRModule.Msg -> Msg
toGenQR msg = ChildGenMsg ( QRMsg msg )

--------------------------------------------------------------------------------


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Interaction interaction_msg  -> updateInteraction interaction_msg  model
    Request     request_msg      -> updateRequest     request_msg      model
    Response    response_msg     -> updateResponse    response_msg     model
    ChildGenMsg child_msg        -> updateChildGenMsg child_msg        model
--  UpdateModel update_model_msg -> updateModel       update_model_msg model
    UpdateChild child_msg        -> updateChild       child_msg        model


--------------------------------------------------------------------------------
updateInteraction : InteractionMsg -> Model -> ( Model, Cmd Msg )
updateInteraction msg model =
  case msg of
    QRButtonClick -> ( model, Cmd.none )


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
                QRModule.ButtonInteraction -> [ ] --FIXME
            QRModule.Request     request_msg     ->
              case request_msg of
                QRModule.RequestInit -> [ Util.toCmd ( Request QRModuleInit ) ]
                QRModule.RequestQR   -> [ Util.toCmd ( Request QRModuleDecode ) ]
            _ -> [ ]
      in
        ( model, Cmd.batch ( [ child_cmd ] ++ additional_cmds ))


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
               -- Module Subscriptions
              , Sub.map toGenQR ( QRModule.subscriptions model.qr )
              ]


-- View
--------------------------------------------------------------------------------
view : Model -> Html Msg
view model =
  let
    container = Html.div [ Html.Attributes.class "col-lg-12 ctf_component_side" ]
    component_row = Html.div [ Html.Attributes.class "row ctf_component_section" ]

    view_qr = viewQR model.qr
  in
    container [ component_row [ Html.div [ Html.Attributes.id "qr" ]
                              [ view_qr ]
                              ]
              ]


viewQR : QRModule.Model -> Html Msg
viewQR model =
  App.map toGenQR ( QRModule.view model )
