module PowerUpPanel ( Model, init
                    , Action, update
                    , view ) where

-- Packages
------------------------------------------------------------
import Effects exposing ( Effects )
import Html exposing ( Html )
import Html.Attributes
import Html.Events
import Signal exposing ( Signal, Address )

import PowerUpButton


-- Model
------------------------------------------------------------
type alias Model = List PowerUpButton.Model

init : ( Model, Effects Action )
init = ( [ fst PowerUpButton.speedButton
         , fst PowerUpButton.invincibilityButton
         , fst PowerUpButton.invisibilityButton
         , fst PowerUpButton.bombButton ]
       , Effects.none )

-- Update
------------------------------------------------------------
type Action = NoAction
            | ActivateButton PowerUpButton.Model

update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    NoAction                    -> ( model, Effects.none )
    ActivateButton button_model -> ( model, Effects.none )

-- View
------------------------------------------------------------
view : Address Action -> Model -> Html
view address model =
  let
    button_container = Html.div [ Html.Attributes.class "col-xs-3 col-sm-3 col-md-3 col-lg-3" ]
    buttons = List.map ( \b -> ( button_container [ viewButton address b ] )) model
  in
    Html.div [ Html.Attributes.class "row" ] buttons

toAction : PowerUpButton.Model -> PowerUpButton.Action -> Action
toAction model action =
  case action of
    PowerUpButton.Click -> ActivateButton model
    _                   -> NoAction

viewButton : Address Action -> PowerUpButton.Model -> Html
viewButton address model =
  PowerUpButton.view ( Signal.forwardTo address ( toAction model )) model
