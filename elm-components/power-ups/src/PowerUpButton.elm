module PowerUpButton ( Model
                     , speedButton, invincibilityButton
                     , invisibilityButton, bombButton
                     , Action ( NoAction, Click)
                     , update, view ) where

-- Imports
------------------------------------------------------------
import Effects exposing ( Effects )
import Html exposing ( Html )
import Html.Attributes exposing ( class )
import Html.Events
import Signal exposing ( Signal, Address )

import PowerUpDefinitions

-- Model
------------------------------------------------------------
type alias Model = { id : PowerUpDefinitions.ID
                   , is_active : Bool }


speedButton : ( Model, Effects Action )
speedButton = ( { id = PowerUpDefinitions.Speed
                , is_active = True }
              , Effects.none )

invincibilityButton : ( Model, Effects Action )
invincibilityButton = ( { id = PowerUpDefinitions.Invincibility
                        , is_active = True }
                      , Effects.none )

invisibilityButton : ( Model, Effects Action )
invisibilityButton = ( { id = PowerUpDefinitions.Invisibility
                       , is_active = True }
                     , Effects.none )

bombButton : ( Model, Effects Action )
bombButton = ( { id = PowerUpDefinitions.Bomb
               , is_active = True }
             , Effects.none )


-- Update
------------------------------------------------------------
type Action = NoAction
            | Click

update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    NoAction -> ( model, Effects.none )
    Click    -> ( model, Effects.none )


-- View
------------------------------------------------------------
view : Address Action -> Model -> Html
view address model =
  let
    attributes = [ Html.Attributes.class "btn btn-block btn-warning btn-embossed"
                 , Html.Events.onClick address Click
                 ]
    img = Html.img [ Html.Attributes.src ( PowerUpDefinitions.toImgUrl model.id )
                   , Html.Attributes.alt ( PowerUpDefinitions.toStr model.id )
                   , Html.Attributes.width 100 ]
                   [ ]
  in
    Html.a attributes [ img ]
