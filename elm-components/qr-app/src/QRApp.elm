port module QRApp exposing ( .. )


-- Import Libraries
--------------------------------------------------------------------------------
import Html.App
import Html exposing ( Html )
import Html.Attributes
import Time


-- Import Modules
--------------------------------------------------------------------------------
import CameraFeed
import QRResult
import Util


-- Start App boiler plate
--------------------------------------------------------------------------------
main = Html.App.programWithFlags { init = init
                                 , update = update
                                 , subscriptions = subscriptions
                                 , view = view
                                 }

-- Model
--------------------------------------------------------------------------------
type alias Model = { camera_feed : CameraFeed.Model
                   , qr_result   : QRResult.Model
                   , qr_decode_interval : Float
                   , qr_decoding : Bool
                   }


type alias Flags = { width  : Int
                   , height : Int
                   , qr_interval : Float
                   , reset_n : Int
                   }

init : Flags -> ( Model, Cmd Msg )
init flags =
  let
    ( qr_model, qr_cmd ) = QRResult.init flags.reset_n
    ( camera_feed_model, camera_feed_cmd ) = CameraFeed.init flags.width
                                                             flags.height
  in
    ( { camera_feed = camera_feed_model
      , qr_result = qr_model
      , qr_decode_interval = flags.qr_interval
      , qr_decoding = False
      }
    , Cmd.batch [ Cmd.map toGenCF camera_feed_cmd
                , Cmd.map toGenQR qr_cmd
                ]
    )


-- Update
--------------------------------------------------------------------------------
type Msg = Interaction InteractionMsg
         | Request     RequestMsg
         | Response    ResponseMsg
         | ChildGenMsg ChildMsg
--       | UpdateModel UpdateModelMsg
         | UpdateChild ChildMsg

type InteractionMsg = ButtonInteraction

type RequestMsg = RequestInit
                | RequestQR

type ResponseMsg = InitSuccess String
                 | InitFailure String
                 | QRSuccess   String
                 | QRFailure   String
                 | StartDecoding

-- type UpdateModelMsg =

type ChildMsg = CFMsg CameraFeed.Msg
              | QRMsg QRResult.Msg


--------------------------------------------------------------------------------
toUpdateCF : CameraFeed.Msg -> Msg
toUpdateCF msg = UpdateChild ( CFMsg msg )

toUpdateQR : QRResult.Msg -> Msg
toUpdateQR msg = UpdateChild ( QRMsg msg )

toGenCF : CameraFeed.Msg -> Msg
toGenCF msg = ChildGenMsg ( CFMsg msg )

toGenQR : QRResult.Msg -> Msg
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
    ButtonInteraction -> ( model, Cmd.none )


--------------------------------------------------------------------------------
updateRequest : RequestMsg -> Model -> ( Model, Cmd Msg )
updateRequest msg model =
  case msg of
    RequestInit -> ( model, initVideoSource { width  = model.camera_feed.width
                                            , height = model.camera_feed.height
                                            })
    RequestQR   ->
      if ( model.camera_feed.is_streaming && model.qr_decoding ) then
        ( model
        , Cmd.batch [ ( requestQR { } )
                    , Util.toCmd ( toUpdateQR ( QRResult.Scanning
                                             |> QRResult.UpdateCurrentStatus
                                             |> QRResult.UpdateModel ))
                    ]
        )
      else
        ( model
        , Cmd.none )


type alias RequestInitObject = { width  : Int
                               , height : Int
                               }

port initVideoSource : RequestInitObject -> Cmd msg


type alias RequestQRObject = { }

port requestQR : RequestQRObject -> Cmd msg


--------------------------------------------------------------------------------
updateResponse : ResponseMsg -> Model -> ( Model, Cmd Msg )
updateResponse msg model =
  case msg of
    InitSuccess video_stream_url ->
      ( model
      , Cmd.batch [ Util.toCmd ( toUpdateCF ( CameraFeed.UpdateModel ( CameraFeed.UpdateVideoSource video_stream_url )))
                  , Util.toCmd ( toUpdateCF ( CameraFeed.UpdateModel ( CameraFeed.UpdateIsStreaming True )))
                  , playVideoSource { width  = model.camera_feed.width
                                    , height = model.camera_feed.height }
                  ] )
    InitFailure error_msg ->
      ( model
      , Cmd.none )  -- FIXME don't silently ignore failure

    QRSuccess result    ->
      ( model
      , Util.toCmd ( toUpdateQR ( QRResult.QRCodeFound result
                               |> QRResult.UpdateCurrentStatus
                               |> QRResult.UpdateModel )))
    QRFailure error_msg ->
      ( model
      , Util.toCmd ( toUpdateQR ( QRResult.QRError error_msg
                               |> QRResult.UpdateCurrentStatus
                               |> QRResult.UpdateModel )))
    StartDecoding -> ( { model | qr_decoding = True }, Cmd.none )


port playVideoSource : RequestInitObject -> Cmd msg


--------------------------------------------------------------------------------
updateChildGenMsg : ChildMsg -> Model -> ( Model, Cmd Msg )
updateChildGenMsg msg model =
  case msg of
    CFMsg camera_feed_msg ->
      let
        cmd = Util.toCmd ( toUpdateCF camera_feed_msg )
        additional_cmds =
          case camera_feed_msg of
            CameraFeed.Request request_msg ->
              case request_msg of CameraFeed.Init -> [ ( Util.toCmd ( Request RequestInit )) ]
            _ -> [ ]
      in
        ( model, Cmd.batch ( [ cmd ] ++ additional_cmds ))
    QRMsg qr_result_msg   ->
      let
        cmd = Util.toCmd ( toUpdateQR qr_result_msg )
      in
        ( model, cmd )


--------------------------------------------------------------------------------
updateChild : ChildMsg -> Model -> ( Model, Cmd Msg )
updateChild msg model =
  case msg of
    CFMsg camera_feed_msg ->
      let
        ( updated_camera_feed, camera_feed_cmd ) =
          CameraFeed.update camera_feed_msg model.camera_feed
      in
        ( { model | camera_feed = updated_camera_feed }
        , Cmd.map toGenCF camera_feed_cmd )
    QRMsg qr_result_msg ->
      let
        ( updated_qr_result, qr_result_cmd ) =
          QRResult.update qr_result_msg model.qr_result
      in
        ( { model | qr_result = updated_qr_result }
        , Cmd.map toGenQR qr_result_cmd )


-- Subscriptions
-------------------------------------------------------------------------------
type alias ResponseInitSuccessObject = { url : String }
port receiveVideoSourceSuccess : ( ResponseInitSuccessObject -> msg ) -> Sub msg

responseInitSuccessToMsg : ResponseInitSuccessObject -> Msg
responseInitSuccessToMsg response = Response ( InitSuccess response.url )

type alias ResponseInitFailureObject = { error_msg : String }
port receiveVideoSourceFailure : ( ResponseInitFailureObject -> msg ) -> Sub msg

responseInitFailureToMsg : ResponseInitFailureObject -> Msg
responseInitFailureToMsg response = Response ( InitFailure response.error_msg )


-------------------------------------------------------------------------------
type alias ResponseStartDecodingObject = { is_playing : Bool }
port receiveStartDecoding : ( ResponseStartDecodingObject -> msg ) -> Sub msg

responseStartDecodingToMsg : ResponseStartDecodingObject -> Msg
responseStartDecodingToMsg response = Response StartDecoding

-------------------------------------------------------------------------------
type alias ResponseQRDecodeSuccessObject = { result: String }
port receiveQRDecodeSuccess : ( ResponseQRDecodeSuccessObject -> msg ) -> Sub msg

responseQRDecodeSuccessToMsg : ResponseQRDecodeSuccessObject -> Msg
responseQRDecodeSuccessToMsg response = Response ( QRSuccess response.result )

type alias ResponseQRDecodeFailureObject = { error_msg : String }
port receiveQRDecodeFailure : ( ResponseQRDecodeFailureObject -> msg ) -> Sub msg

responseQRDecodeFailureToMsg : ResponseQRDecodeFailureObject -> Msg
responseQRDecodeFailureToMsg response = Response ( QRFailure response.error_msg )


--------------------------------------------------------------------------------
requestQRDecode : Float -> Sub Msg
requestQRDecode interval = Time.every ( interval * Time.second ) (\_ -> ( Request RequestQR ))


--------------------------------------------------------------------------------
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch [ receiveVideoSourceSuccess responseInitSuccessToMsg
            , receiveVideoSourceFailure responseInitFailureToMsg
            , receiveQRDecodeSuccess    responseQRDecodeSuccessToMsg
            , receiveQRDecodeFailure    responseQRDecodeFailureToMsg
            , receiveStartDecoding      responseStartDecodingToMsg
            , requestQRDecode model.qr_decode_interval
            ]


-- View
--------------------------------------------------------------------------------
view : Model -> Html Msg
view model =
  let
    outer_container = Html.div [ Html.Attributes.class "col-xs-12 col-sm-12 col-md-12 col-lg-12" ]
    row = Html.div [ Html.Attributes.class "row" ]

    camera_feed = row [ viewCameraFeed model.camera_feed ]
    qr_result   = row [ viewQRResult   model.qr_result ]
  in
    outer_container [ camera_feed
                    , qr_result
                    ]


viewCameraFeed : CameraFeed.Model -> Html Msg
viewCameraFeed model =
  Html.App.map toGenCF ( CameraFeed.view model )


viewQRResult : QRResult.Model -> Html Msg
viewQRResult model =
  Html.App.map toGenQR ( QRResult.view model )
