module ClassButtonPanel ( Model, init
                        , Action ( NoAction
                                 , ClickButton
                                 , UpdateIsTimedOut
                                 , UpdateCurrentActive
                                 )
                        , update, view) where

-- Import Libraries
------------------------------------------------------------
import Effects exposing ( Effects, Never )
import Html exposing ( Html )
import Html.Attributes exposing ( class )
import Html.Events
import Signal exposing ( Signal, Address )
import Task


-- Import ClassDisplay modules
------------------------------------------------------------
import ClassChangeButton
import PlayerClass


-- Model
------------------------------------------------------------
type alias Model = { buttons : List ( PlayerClass.ID, ClassChangeButton.Model )
                   , current_active : PlayerClass.ID
                   }

init : ( Model, Effects Action )
init = ( { buttons = [ ( PlayerClass.Light,  fst ( ClassChangeButton.init PlayerClass.Light ))
                     , ( PlayerClass.Medium, fst ( ClassChangeButton.init PlayerClass.Medium ))
                     , ( PlayerClass.Heavy,  fst ( ClassChangeButton.init PlayerClass.Heavy ))
                     ]
         , current_active = PlayerClass.Undefined
         }
       , Effects.none
       )


-- Update
------------------------------------------------------------
type Action = NoAction
            | ClickButton PlayerClass.ID
            | UpdateIsTimedOut Bool
            | UpdateCurrentActive PlayerClass.ID
            | UpdateButton PlayerClass.ID ClassChangeButton.Action

toEffect : Action -> Effects Action
toEffect action = Effects.task ( Task.succeed action )

update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    NoAction       -> ( model, Effects.none )
    ClickButton id -> ( model, toEffect ( UpdateButton id ClassChangeButton.Click ))
    UpdateIsTimedOut new_timed_out ->
      let
        mapButtonToEffect ( button_id, _ ) =
          toEffect ( UpdateButton button_id ( ClassChangeButton.UpdateIsTimedOut new_timed_out ))
      in
        ( model
        , Effects.batch ( List.map mapButtonToEffect model.buttons ))
    UpdateCurrentActive id ->
      if id == model.current_active then
        ( model, Effects.none )
      else
        ( model
        , Effects.batch [ toEffect ( UpdateButton model.current_active
                                                  ( ClassChangeButton.UpdateIsActive False ))
                        , toEffect ( UpdateButton id
                                                  ( ClassChangeButton.UpdateIsActive True ))
                        ]
        )
    UpdateButton id button_action ->
      let
        updateButton ( button_id, button_model ) =
          if button_id == id then
            let
              ( new_model, effects ) = ClassChangeButton.update button_action button_model
            in
              ( ( button_id, new_model )
              , ( button_id, effects )
              )
          else ( ( button_id, button_model )
               , ( button_id, Effects.none )
               )
        mapEffect ( button_id, effects ) = Effects.map ( UpdateButton button_id ) effects

        updated_buttons = List.unzip ( List.map updateButton model.buttons )
      in
        ( { model | buttons = fst updated_buttons }
        , Effects.batch ( List.map mapEffect ( snd updated_buttons ) )
        )


-- View
------------------------------------------------------------
view : Address Action -> Model -> Html
view address model =
  let
    button_div = Html.div [ Html.Attributes.class "col-xs-4 col-sm-4 col-md-4 col-lg-4" ]
    mapButtonToHtml ( button_id, button_model ) = button_div [ viewButton address button_model ]
                                                  buttons = List.map  model.buttons
  in
    Html.div [ Html.Attributes.class "row" ] buttons


viewButton : Address Action -> ClassChangeButton.Model -> Html
viewButton address model =
  let
    toAction action =
      case action of
        ClassChangeButton.Click -> ClickButton model.class
        _                       -> NoAction
  in
    ClassChangeButton.view ( Signal.forwardTo address toAction ) model




