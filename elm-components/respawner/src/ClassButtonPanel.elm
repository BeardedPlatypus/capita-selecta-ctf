module ClassButtonPanel ( Model, init
                        , Action ( NoAction, ActivateButton )
                        , update
                        , view
                        ) where

-- Packages
------------------------------------------------------------
-- Elm Packages --
import List
import Effects exposing ( Effects, Never )
import Html exposing ( Html )
import Html.Attributes exposing ( class )
import Html.Events exposing ( onClick )
import Signal exposing ( Signal, Address )

-- Respawner Modules --
import ClassButton
import PlayerClass

-- Model
------------------------------------------------------------
type alias Model = List ClassButton.Model

init : ( Model, Effects Action )
init = ( [ { class = PlayerClass.Light
           , is_active = True
           }
         , { class = PlayerClass.Medium
           , is_active = False
           }
         , { class = PlayerClass.Heavy
           , is_active = False
           }
         ]
       , Effects.none
       )

-- Update
------------------------------------------------------------
type Action = NoAction
            | ActivateButton PlayerClass.ID

update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    NoAction -> ( model, Effects.none )
    ActivateButton id ->
      let
        updateButton classButtonModel =
          if classButtonModel.class == id
            then ClassButton.update ClassButton.Activate classButtonModel
            else ClassButton.update ClassButton.Deactivate classButtonModel
        updated_buttons = List.unzip ( List.map updateButton model )
      in
        ( fst updated_buttons
        , Effects.none
        )

-- View
------------------------------------------------------------
view : Address Action -> Model -> Html
view address model =
  let
    button_container = Html.div [ Html.Attributes.class "col-xs-4 col-sm-4 col-md-4 col-lg-4"]
    buttons = List.map ( \b -> ( button_container [ viewButton address b ] ) ) model
    attributes_container = [ Html.Attributes.class "row" ]
  in
    Html.div attributes_container buttons

-- View support buttons
toAction : ClassButton.Model ->ClassButton.Action -> Action
toAction model action =
  case action of
    ClassButton.Activate -> ActivateButton model.class
    _                    -> NoAction


viewButton : Address Action -> ClassButton.Model -> Html
viewButton address model =
  ClassButton.view ( Signal.forwardTo address ( toAction model )) model
