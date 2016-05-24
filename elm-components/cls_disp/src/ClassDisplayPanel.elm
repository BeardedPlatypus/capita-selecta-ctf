module ClassDisplayPanel ( Model, init
                         , Action ( NoAction
                                  , ClickButton
                                  , UpdateIsAlive
                                  , UpdateCurrentClass
                                  , CommunicationAction
                                  , UpdateButtonPanel
                                  , UpdateClassDisplay
                                  , UpdateBaseUrl
                                  )
                         , update, view ) where


-- Import Libraries
--------------------------------------------------------------------------------
import Effects exposing ( Effects )
import Html exposing ( Html )
import Html.Attributes
import Signal exposing ( Signal, Address )
import Task


-- Import ClassDisplay modules
--------------------------------------------------------------------------------
import ClassButtonPanel
import CurrentClassDisplay
import PlayerClass
import ClassDisplayConnection

-- Model
--------------------------------------------------------------------------------
type alias Model = { class_display : CurrentClassDisplay.Model
                   , button_panel : ClassButtonPanel.Model
                   , base_url : String
                   }


init : ( Model, Effects Action )
init = ( { class_display = fst CurrentClassDisplay.init
         , button_panel = fst ClassButtonPanel.init
         , base_url = ""
         }
       , Effects.none )


-- Update
--------------------------------------------------------------------------------
type Action = NoAction
            ---- Direct Panel Interaction ----
            | ClickButton PlayerClass.ID
            ---- Update Panel ----
            | UpdateIsAlive Bool
            | UpdateCurrentClass PlayerClass.ID
            ---- Panel communication ----
            | CommunicationAction ClassDisplayConnection.Action
            ---- Update Elements ----
            | UpdateButtonPanel ClassButtonPanel.Action
            | UpdateClassDisplay CurrentClassDisplay.Action
            | UpdateBaseUrl String


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    NoAction -> ( model, Effects.none )

    ---- Direct Panel Interaction ----
    ClickButton class_id -> ( model
                            , Effects.batch
                              [ toEffect ( UpdateButtonPanel ( ClassButtonPanel.ClickButton class_id ))
                              , toEffect ( CommunicationAction
                                             ( ClassDisplayConnection.Request
                                                 ( ClassDisplayConnection.SendNewClass class_id )))
                              ])

    ---- Panel Communication ----
    CommunicationAction com_action -> updateCommunication com_action model

    ---- Update Panel ----
    UpdateIsAlive val -> ( model
                         , Effects.batch
                             [ toEffect (UpdateButtonPanel ( ClassButtonPanel.UpdateIsTimedOut val))
                             , toEffect (UpdateClassDisplay ( CurrentClassDisplay.UpdateIsAlive val))
                             ]
                         )
    UpdateCurrentClass val -> ( model
                              , Effects.batch
                              [ toEffect ( UpdateButtonPanel ( ClassButtonPanel.UpdateCurrentActive val ))
                              , toEffect ( UpdateClassDisplay ( CurrentClassDisplay.UpdateClass val ))
                              ])

    ---- Update ClassDisplayPanel Elements ----
    UpdateButtonPanel action ->
      let
        ( updated_panel, panel_effects ) =
          ClassButtonPanel.update action model.button_panel
      in
        ( { model | button_panel = updated_panel }
        , Effects.map UpdateButtonPanel panel_effects )
    UpdateClassDisplay action ->
      let
        ( updated_class_display, class_display_effects ) =
          CurrentClassDisplay.update action model.class_display
      in
        ( { model | class_display = updated_class_display }
        , Effects.map UpdateClassDisplay class_display_effects )
    UpdateBaseUrl url -> ( { model | base_url = url }, Effects.none )


toEffect : Action -> Effects Action
toEffect action = Effects.task ( Task.succeed action )


updateCommunication : ClassDisplayConnection.Action -> Model -> ( Model, Effects Action )
updateCommunication action model =
  case action of
    ClassDisplayConnection.Request request_action ->
      ( model
      , Effects.map CommunicationAction
                    ( ClassDisplayConnection.processRequest model.base_url
                                                            request_action ))
    ClassDisplayConnection.Response response_action ->
      case response_action of
        ClassDisplayConnection.ResponseIsAlive response ->
          case response of
            Nothing -> ( model, toEffect ( CommunicationAction
                                             ( ClassDisplayConnection.Request
                                                 ClassDisplayConnection.FetchIsAlive )))
            Just value -> ( model, toEffect ( UpdateIsAlive value ))
        ClassDisplayConnection.ResponseClass class_id response ->
          case response of
            Nothing -> ( model, toEffect ( CommunicationAction
                                             ( ClassDisplayConnection.Request
                                               ( ClassDisplayConnection.SendNewClass class_id ))))
            Just value -> ( model, toEffect ( UpdateCurrentClass value ))


-- View
--------------------------------------------------------------------------------
view : Address Action -> Model -> Html
view address model =
  let
    row = Html.div [ Html.Attributes.class "row" ]
    class_display = row [ viewClassDisplay address model.class_display ]
    button_panel = row [ viewButtonPanel address model.button_panel ]
  in
    Html.div [ Html.Attributes.class "col-xs-6 col-sm-6 col-md-6 col-lg-6" ]
             [ class_display, button_panel ]


viewClassDisplay : Address Action -> CurrentClassDisplay.Model -> Html
viewClassDisplay address =
  CurrentClassDisplay.view ( Signal.forwardTo address UpdateClassDisplay )


viewButtonPanel : Address Action -> ClassButtonPanel.Model -> Html
viewButtonPanel address =
  let
    toAction action =
      case action of
        ClassButtonPanel.ClickButton button_id -> ClickButton button_id
        action                                 -> UpdateButtonPanel action
  in
    ClassButtonPanel.view ( Signal.forwardTo address toAction )


