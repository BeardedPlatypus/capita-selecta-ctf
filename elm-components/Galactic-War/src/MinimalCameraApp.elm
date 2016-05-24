port module MinimalCameraApp exposing ( .. )


-- Import Libraries
--------------------------------------------------------------------------------
import Html exposing ( Html )
import Html.Attributes
import Html.App as App
import Task


-- StartApp BoilerPlate
--------------------------------------------------------------------------------
main = App.programWithFlags { init = initWithFlags
                            , update = update
                            , subscriptions = subscriptions
                            , view = view
                            }


type alias Flags = { width  : Int
                   , height : Int
                   }


initWithFlags : Flags -> ( Model, Cmd Msg )
initWithFlags flags = init flags.width
                           flags.height


-- Util Functions
--------------------------------------------------------------------------------
toCmd : msg -> Cmd msg
toCmd msg =
  let
    onFail    val = val
    onSucceed val = val
  in
    Task.perform onFail onSucceed ( Task.succeed msg )


-- Model
--------------------------------------------------------------------------------
type alias Model = { width  : Int
                   , height : Int
                   , is_streaming  : Bool
                   , stream_source : String
                   }

init : Int -> Int -> ( Model, Cmd Msg )
init width height = ( { width  = width
                      , height = height
                      , is_streaming  = False
                      , stream_source = ""
                      }
                    , initVideoSource { width  = width
                                      , height = height
                                      }
                    )


-- Update
--------------------------------------------------------------------------------
type Msg = UpdateModel UpdateModelMsg
         | Init String
         | Ignore


type UpdateModelMsg = UpdateWidth       Int
                    | UpdateHeight      Int
                    | UpdateIsStreaming Bool
                    | UpdateVideoSource String


--------------------------------------------------------------------------------
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    UpdateModel update_model_msg -> updateModel update_model_msg model
    Init        video_src        -> ( model
                                    , Cmd.batch [ toCmd ( UpdateModel ( UpdateVideoSource video_src ))
                                                , toCmd ( UpdateModel ( UpdateIsStreaming True ))
                                                , startVideoStream { width = model.width
                                                                   , height = model.height
                                                                   }
                                                ]
                                    )
    Ignore                       -> ( model, Cmd.none )


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


-- Subscriptions
--------------------------------------------------------------------------------
type alias RequestObject = { width  : Int
                           , height : Int
                           }

port initVideoSource : RequestObject -> Cmd msg

port startVideoStream : RequestObject -> Cmd msg

-- Success Response
-------------------
type alias ResponseSuccessObject = { url : String }

port receiveVideoSource : ( ResponseSuccessObject -> msg ) -> Sub msg

responseSuccessToMsg : ResponseSuccessObject -> Msg
responseSuccessToMsg response = Init response.url

-- Failure Response
-------------------
type alias ResponseFailureObject = { error_msg : String }

port receiveError : ( ResponseFailureObject -> msg ) -> Sub msg

responseFailureToMsg : ResponseFailureObject -> Msg
responseFailureToMsg response = Ignore


--------------------------------------------------------------------------------
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch [ receiveVideoSource responseSuccessToMsg
            , receiveError       responseFailureToMsg
            ]


-- Video
--------------------------------------------------------------------------------
view : Model -> Html Msg
view model =
  let
    outer_container = Html.div [ Html.Attributes.class "camera" ]
    video_attributes = [ Html.Attributes.id "video"
                       , Html.Attributes.width model.width
                       , Html.Attributes.height model.height
                       ] ++ ( if model.is_streaming then
                                [ Html.Attributes.src model.stream_source ]
                              else
                                [ ] )
  in
    outer_container [ Html.video video_attributes [ ] ]
