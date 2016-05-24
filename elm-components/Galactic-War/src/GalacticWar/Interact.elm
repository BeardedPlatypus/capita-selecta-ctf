module GalacticWar.Interact exposing ( .. )


-- Import Libraries
--------------------------------------------------------------------------------
import Html exposing ( Html )
import Html.Attributes
import Html.Events
import Platform.Cmd as Cmd exposing ( Cmd )
import Platform.Sub as Sub exposing ( Sub )


-- Model
--------------------------------------------------------------------------------
type alias Model = { is_active : Bool }

init : ( Model, Cmd Msg )
init = ( { is_active = True }, Cmd.none )

-- Update
--------------------------------------------------------------------------------
type Msg = Interaction InteractionMsg
         | UpdateModel UpdateModelMsg

type InteractionMsg = Click

type UpdateModelMsg = UpdateIsActive Bool

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Interaction interaction_msg  -> updateInteraction interaction_msg model
    UpdateModel update_model_msg -> updateModel update_model_msg model

updateInteraction : InteractionMsg -> Model -> ( Model, Cmd Msg )
updateInteraction msg model =
  case msg of
    Click -> ( model, Cmd.none )

updateModel : UpdateModelMsg -> Model -> ( Model, Cmd Msg )
updateModel msg model =
  case msg of
    UpdateIsActive new_is_active -> ( { model | is_active = new_is_active }
                                    , Cmd.none )


-- Subscriptions
--------------------------------------------------------------------------------
subscriptions : Model -> Sub Msg
subscriptions model = Sub.none


-- View
--------------------------------------------------------------------------------
view : Model -> Html Msg
view model =
  let
    camera_feed = Html.div [ Html.Attributes.class "center"
                           , Html.Attributes.id "reader"
                           , Html.Attributes.style [ ("width",  "300px")
                                                   , ("height", "250px")]
                           ] []
    result = Html.span [ Html.Attributes.class "center"
                       , Html.Attributes.id "read"
                       ] []
    read_error = Html.span [ Html.Attributes.class "center"
                           , Html.Attributes.id "read_error"
                           ] []
    vid_error = Html.span [ Html.Attributes.class "center"
                          , Html.Attributes.id "vid_error"
                          ] []
    outer_container = Html.div [ Html.Attributes.class "container" ]
    button = Html.a [ Html.Attributes.class "btn btn-block btn-default btn-embossed"
                    , Html.Events.onClick ( Interaction Click )] [ ]
  in
    outer_container [ camera_feed
                    , button
                    , result
                    , read_error
                    , vid_error
                    ]
