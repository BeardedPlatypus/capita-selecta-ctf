module GalacticWar.QRModule exposing ( .. )


-- Import Libraries
--------------------------------------------------------------------------------
import Html.App
import Html exposing ( Html )
import Html.Attributes
import Time


-- Import Modules
--------------------------------------------------------------------------------
import GalacticWar.QRModule.CameraFeed as CameraFeed
import GalacticWar.QRModule.QRResult as QRResult
import GalacticWar.Util as Util
import GalacticWar.PlayerInteraction as PlayerInteraction


-- Model
--------------------------------------------------------------------------------
type alias Model = { camera_feed : CameraFeed.Model
                   , qr_result   : QRResult.Model
                   , qr_decode_interval : Float
                   , qr_decoding : Bool
                   , click : Int -- FIXME
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
      , click = 0
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
         | UpdateModel UpdateModelMsg
         | UpdateChild ChildMsg

type InteractionMsg = ButtonInteraction PlayerInteraction.Action

type RequestMsg = RequestInit
                | RequestQR

type ResponseMsg = InitSuccess String
                 | InitFailure String
                 | QRSuccess   String
                 | QRFailure   String
                 | StartDecoding

type UpdateModelMsg = UpdateIsActive Bool
                    | UpdateIsDisabled Bool

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
    UpdateModel update_model_msg -> updateModel       update_model_msg model
    UpdateChild child_msg        -> updateChild       child_msg        model


--------------------------------------------------------------------------------
updateInteraction : InteractionMsg -> Model -> ( Model, Cmd Msg )
updateInteraction msg model =
  case msg of
    ButtonInteraction action -> ( { model | click = model.click + 1 }, Cmd.none )


--------------------------------------------------------------------------------
updateRequest : RequestMsg -> Model -> ( Model, Cmd Msg )
updateRequest msg model =
  case msg of
    RequestInit -> ( model, Cmd.none )
    RequestQR   ->
      if ( model.camera_feed.is_streaming && model.qr_decoding ) then
        ( model
        , Util.toCmd ( toUpdateQR ( QRResult.Scanning
                                 |> QRResult.UpdateCurrentStatus
                                 |> QRResult.UpdateModel )
                     )
        )
      else
        ( model
        , Cmd.none )


--------------------------------------------------------------------------------
updateResponse : ResponseMsg -> Model -> ( Model, Cmd Msg )
updateResponse msg model =
  case msg of
    InitSuccess video_stream_url ->
      ( model
      , Cmd.batch [ Util.toCmd ( toUpdateCF ( CameraFeed.UpdateModel ( CameraFeed.UpdateVideoSource video_stream_url )))
                  , Util.toCmd ( toUpdateCF ( CameraFeed.UpdateModel ( CameraFeed.UpdateIsStreaming True )))
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


--------------------------------------------------------------------------------
updateModel : UpdateModelMsg -> Model -> ( Model, Cmd Msg )
updateModel msg model =
  case msg of
    UpdateIsActive   new_is_active ->
      ( model
      , Util.toCmd ( toUpdateQR ( QRResult.UpdateModel
                                ( QRResult.UpdateIsActive new_is_active ))))
    UpdateIsDisabled new_is_disabled ->
      ( model
      , Util.toCmd ( toUpdateQR ( QRResult.UpdateModel
                                ( QRResult.UpdateIsDisabled new_is_disabled ))))

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
        additional_cmds =
          case qr_result_msg of
            QRResult.Interaction interaction_msg ->
              case interaction_msg of
                QRResult.Click action -> [ Util.toCmd ( Interaction ( ButtonInteraction action )) ]
            _                                    -> [ ]
      in
        ( model, Cmd.batch ( [ cmd ] ++ additional_cmds ) )


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


--------------------------------------------------------------------------------
requestQRDecode : Float -> Sub Msg
requestQRDecode interval = Time.every ( interval * Time.second ) (\_ -> ( Request RequestQR ))


--------------------------------------------------------------------------------
subscriptions : Model -> Sub Msg
subscriptions model = requestQRDecode model.qr_decode_interval


-- View
--------------------------------------------------------------------------------
view : Model -> Html Msg
view model =
  let
    outer_container = Html.div [ Html.Attributes.class "col-xs-12 col-sm-12 col-md-12 col-lg-12" ]
    row = Html.div [ Html.Attributes.class "row" ]

    camera_feed = row [ viewCameraFeed model.camera_feed ]
    qr_result   = row [ viewQRResult   model.qr_result ]
    click_test = row [ Html.text ( toString model.click )]
  in
    outer_container [ camera_feed
                    , qr_result
                    , click_test
                    ]


viewCameraFeed : CameraFeed.Model -> Html Msg
viewCameraFeed model =
  Html.App.map toGenCF ( CameraFeed.view model )


viewQRResult : QRResult.Model -> Html Msg
viewQRResult model =
  Html.App.map toGenQR ( QRResult.view model )
