module GalacticWar.QRModule.InteractButton exposing ( .. )


-- Import Libraries
--------------------------------------------------------------------------------
import Html exposing ( Html )
import Html.App as App
import Html.Attributes
import Html.Events

-- Import Modules
--------------------------------------------------------------------------------
import GalacticWar.PlayerInteraction as PlayerInteraction

-- Model
--------------------------------------------------------------------------------
type alias Model = { action : PlayerInteraction.Action
                   , is_active : Bool
                   , is_disabled : Bool
                   , click_n : Int
                   }


init : ( Model, Cmd Msg )
init = ( { action = PlayerInteraction.NoAction
         , is_active = True
         , is_disabled = False
         , click_n = 0
         }
       , Cmd.none )


-- Update
--------------------------------------------------------------------------------
type Msg = Interaction InteractionMsg
         | UpdateModel UpdateModelMsg

type InteractionMsg = Click PlayerInteraction.Action

type UpdateModelMsg = UpdateIsActive   Bool
                    | UpdateAction     PlayerInteraction.Action
                    | UpdateIsDisabled Bool


--------------------------------------------------------------------------------
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Interaction interaction_msg  -> updateInteraction interaction_msg  model
    UpdateModel update_model_msg -> updateModel       update_model_msg model


updateInteraction : InteractionMsg -> Model -> ( Model, Cmd Msg )
updateInteraction msg model =
  case msg of
    Click _ -> ( { model | click_n = model.click_n + 1 }, Cmd.none )


updateModel : UpdateModelMsg -> Model -> ( Model, Cmd Msg )
updateModel msg model =
  case msg of
    UpdateAction     action      -> ( { model | action = action }
                                    , Cmd.none )
    UpdateIsDisabled is_disabled -> ( { model | is_disabled = is_disabled }
                                    , Cmd.none )
    UpdateIsActive   is_active   -> ( { model | is_active = is_active }
                                    , Cmd.none )


-- View
--------------------------------------------------------------------------------
view : Model -> Html Msg
view model =
  let
    attributes = [ Html.Attributes.class "btn btn-block btn-default" ]
    text = PlayerInteraction.actionToString model.action
    btn =
      if model.is_active then
        case model.action of
          PlayerInteraction.NoAction ->
            Html.a (attributes ++ [ Html.Attributes.disabled True ] ) [ Html.text text ]
          PlayerInteraction.Respawn ->
            Html.a (attributes ++ [ Html.Attributes.disabled True ] ) [ Html.text "You are still alive" ]
          _                          ->
            if ( not model.is_disabled ) then
              Html.a (attributes ++ [ Html.Events.onClick ( Interaction ( Click model.action )) ])
                     [ Html.text text ]
            else
              Html.a (attributes ++ [ Html.Attributes.disabled True ] ) [ Html.text "Recharging" ]
      else
        case model.action of
          PlayerInteraction.Respawn ->
            Html.a (attributes ++ [ Html.Events.onClick ( Interaction ( Click model.action )) ])
                   [ Html.text text ]
          _ ->
            Html.a ( attributes ++ [ Html.Attributes.disabled True ] ) [ Html.text "You are dead" ]
    in
      btn
