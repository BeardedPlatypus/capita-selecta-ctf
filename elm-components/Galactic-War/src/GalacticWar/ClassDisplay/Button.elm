module GalacticWar.ClassDisplay.Button
  exposing ( Model, init
           , Msg ( Interaction, UpdateModel )
           , InteractionMsg ( Click )
           , UpdateModelMsg ( UpdateClass, UpdateIsDisabled, UpdateIsDefault )
           , update, view, subscriptions )


-- Import Libraries
--------------------------------------------------------------------------------
import Html exposing ( Html )
import Html.Attributes
import Html.Events
import Platform.Cmd as Cmd exposing ( Cmd )
import Platform.Sub as Sub exposing ( Sub )


-- Import modules
--------------------------------------------------------------------------------
import GalacticWar.Class as Class


-- Model
--------------------------------------------------------------------------------
type alias Model = { class : Class.ID
                   , is_disabled : Bool
                   , is_default : Bool
                   }


init : Class.ID -> ( Model, Cmd Msg )
init class_id = ( { class = class_id
                  , is_disabled = False
                  , is_default = False
                  }
                , Cmd.none )


-- Update
--------------------------------------------------------------------------------
type Msg = Interaction InteractionMsg
         | UpdateModel UpdateModelMsg


type InteractionMsg = Click


type UpdateModelMsg = UpdateClass Class.ID
                    | UpdateIsDisabled Bool
                    | UpdateIsDefault Bool


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
    UpdateClass class_id ->
      ( { model | class = class_id }
      , Cmd.none )
    UpdateIsDisabled new_is_disabled ->
      ( { model | is_disabled = new_is_disabled }
      , Cmd.none )
    UpdateIsDefault new_is_default ->
      ( { model | is_default = new_is_default }
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
    attributes =
      ( if model.is_default then
          [ Html.Attributes.class "btn btn-block btn-orange active" ]
        else
          [ Html.Attributes.class "btn btn-block btn-default btn-embossed"
          , Html.Events.onClick ( Interaction Click )]
      ) ++ [ Html.Attributes.disabled model.is_disabled ]
    img = Html.img [ Html.Attributes.src ( Class.toImgUrl model.class )
                   , Html.Attributes.alt ( Class.toStr model.class )
                   , Html.Attributes.width 100 ]
                   [ ]
  in
    Html.a attributes [ img ]
