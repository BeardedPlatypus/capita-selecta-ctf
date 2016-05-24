module CameraFeed exposing ( .. )


-- Import
--==============================================================================
-- Import Libraries
-------------------
import Html exposing ( Html )
import Html.Attributes

-- Import Modules
-----------------
import Util


-- Model
--==============================================================================
type alias Model = { width  : Int
                   , height : Int
                   , is_streaming  : Bool
                   , stream_source : String
                   }

init : Int -> Int -> ( Model, Cmd Msg )
init width height = ( { width  = width
                      , height = height
                      , is_streaming = False
                      , stream_source = ""
                      }
                    , Util.toCmd ( Request Init )
                    )


-- Update
--==============================================================================
type Msg = UpdateModel UpdateModelMsg
         | Request RequestMsg

type UpdateModelMsg = UpdateWidth       Int
                    | UpdateHeight      Int
                    | UpdateIsStreaming Bool
                    | UpdateVideoSource String

type RequestMsg = Init


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    UpdateModel update_model_msg -> updateModel   update_model_msg model
    Request     request_msg      -> updateRequest request_msg      model


--------------------------------------------------------------------------------
updateModel : UpdateModelMsg -> Model -> ( Model, Cmd Msg )
updateModel msg model =
  case msg of
    UpdateWidth       new_width        -> ( { model | width = new_width }
                                          , Cmd.none )
    UpdateHeight      new_height       -> ( { model | height = new_height }
                                          , Cmd.none )
    UpdateIsStreaming new_is_streaming -> ( { model | is_streaming = new_is_streaming }
                                          , Cmd.none )
    UpdateVideoSource new_video_src    -> ( { model | stream_source = new_video_src }
                                          , Cmd.none )


updateRequest : RequestMsg -> Model -> ( Model, Cmd Msg )
updateRequest msg model =
  case msg of
    Init -> ( model, Cmd.none )


-- View
--==============================================================================
view : Model -> Html Msg
view model =
  let
    outer_container  = Html.div [ Html.Attributes.class "camera" ]
    video_attributes = [ Html.Attributes.id "video"
                       , Html.Attributes.width  model.width
                       , Html.Attributes.height model.height
                       ]
    streaming_url    = if model.is_streaming then
                         [ Html.Attributes.src model.stream_source ]
                       else
                         [ ]
    canvas = Html.canvas [ Html.Attributes.id "qr-canvas"
                         , Html.Attributes.width  model.width
                         , Html.Attributes.height model.height
                         ] [ ]
  in
    outer_container [ Html.video ( video_attributes ++ streaming_url ) [ ]
                    , canvas
                    ]
