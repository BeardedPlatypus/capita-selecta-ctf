module RespawnPanel ( Model, init
                    , update, Action ( NoAction, ActivateButton )
                    , view ) where

-- Packages
------------------------------------------------------------
-- Elm Packages
import Effects exposing (Effects, Never)
import Html exposing (Html)
import Html.Attributes exposing ( class )
import Signal exposing (Signal, Address)

-- Respawner Modules
import PlayerClass
import CurrentClass
import ClassButtonPanel

import RespawnConnection

-- Model
------------------------------------------------------------
type alias Model = { current_class : CurrentClass.Model
                   , buttons : ClassButtonPanel.Model
                   }

init : ( Model, Effects Action )
init = ( { current_class = ( fst CurrentClass.init )
         , buttons = ( fst ClassButtonPanel.init )
         }
       , Effects.none
       )


-- Update
------------------------------------------------------------
type Action = NoAction
            | ActivateButton PlayerClass.ID
            | CommunicationIsAliveAction RespawnConnection.IsAliveAction
            | CommunicationRespawnAsAction RespawnConnection.RespawnAsAction 

update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    NoAction -> ( model, Effects.none )
    ActivateButton id ->
      let
        new_current_class = CurrentClass.update ( CurrentClass.RespawnAs id ) model.current_class
        new_buttons = ClassButtonPanel.update ( ClassButtonPanel.ActivateButton id ) model.buttons
      in
        ( { model | current_class = fst new_current_class
                  , buttons = fst new_buttons }
        , Effects.none )
    CommunicationIsAliveAction is_alive_action ->
      case is_alive_action of
        RespawnConnection.FetchIsAlive -> ( model
                                          , Effects.map CommunicationIsAliveAction
                                                        ( RespawnAction))

        RespawnConnection.NewIsAlive   ->



-- View
------------------------------------------------------------
view : Address Action -> Model -> Html
view address model =
  let
    outer_container = Html.div [ Html.Attributes.class "col-xs-6 col-sm-6 col-md-6 col-lg-6" ]
    row = Html.div [ Html.Attributes.class "row" ]
    current_class = row [ viewCurrentClass address model.current_class ]
    button_panel = row [ viewButtonPanel address model.buttons ]
  in
    outer_container [ current_class
                    , button_panel
                    ]


buttonPanelToAction : ClassButtonPanel.Action -> Action
buttonPanelToAction action =
  case action of
    ClassButtonPanel.ActivateButton id -> ActivateButton id
    _                                  -> NoAction

viewButtonPanel : Address Action -> ClassButtonPanel.Model -> Html
viewButtonPanel address model =
  ClassButtonPanel.view ( Signal.forwardTo address buttonPanelToAction ) model


currentClassToAction : CurrentClass.Action -> Action
currentClassToAction action =
  case action of
    _ -> NoAction

viewCurrentClass : Address Action -> CurrentClass.Model -> Html
viewCurrentClass address model =
  CurrentClass.view ( Signal.forwardTo address currentClassToAction ) model
