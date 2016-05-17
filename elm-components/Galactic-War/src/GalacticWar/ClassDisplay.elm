module GalacticWar.ClassDisplay exposing ( .. )


-- Import Libraries
--------------------------------------------------------------------------------
import Html exposing ( Html )
import Html.App as App
import Html.Attributes
import Html.Events
import Platform.Cmd as Cmd exposing ( Cmd )
import Platform.Sub as Sub exposing ( Sub )
import List

-- Import modules
--------------------------------------------------------------------------------
import GalacticWar.Class as Class
import GalacticWar.ClassDisplay.ButtonPanel as ButtonPanel
import GalacticWar.ClassDisplay.Display as Display

import GalacticWar.Util as Util


-- Model
--------------------------------------------------------------------------------
type alias Model = { button_panel : ButtonPanel.Model
                   , display : Display.Model
                   }

init : Class.ID -> ( Model, Cmd Msg )
init id = ( { button_panel = fst ( ButtonPanel.init id )
            , display =      fst ( Display.init id )
            }
          , Cmd.none )


-- Update
--------------------------------------------------------------------------------
type Msg = Interaction InteractionMsg
         | UpdateModel UpdateModelMsg
         | UpdateChild UpdateChildMsg

type InteractionMsg = ClickButton Class.ID

type UpdateModelMsg = UpdateIsAlive Bool
                    | UpdateIsInteractive Bool
                    | UpdateClass Class.ID

type UpdateChildMsg = UpdateButtonPanel ButtonPanel.Msg
                    | UpdateDisplay     Display.Msg

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Interaction interaction_msg  -> updateInteraction interaction_msg model
    UpdateModel update_model_msg -> updateModel update_model_msg model
    UpdateChild update_child_msg -> updateChild update_child_msg model

--------------------------------------------------------------------------------
toUpdatePanelCmd : ButtonPanel.Msg -> Cmd Msg
toUpdatePanelCmd msg = Util.toCmd ( UpdateChild ( UpdateButtonPanel msg ))

toUpdateDisplayCmd : Display.Msg -> Cmd Msg
toUpdateDisplayCmd msg = Util.toCmd ( UpdateChild ( UpdateDisplay msg ))


--------------------------------------------------------------------------------
updateInteraction : InteractionMsg -> Model -> ( Model, Cmd Msg )
updateInteraction msg model =
  case msg of
    ClickButton id ->
      ( model
      , toUpdatePanelCmd ( ButtonPanel.Interaction ( ButtonPanel.ClickButton id ))
      )


--------------------------------------------------------------------------------
updateModel : UpdateModelMsg -> Model -> ( Model, Cmd Msg )
updateModel msg model =
  case msg of
    UpdateIsAlive new_is_alive ->
      let
        display_cmd = Display.UpdateIsAlive new_is_alive
      in
        ( model
        , Cmd.batch [ Util.toCmd ( UpdateModel ( UpdateIsInteractive new_is_alive ))
                    , toUpdateDisplayCmd ( Display.UpdateModel display_cmd )
                    ]
        )
    UpdateIsInteractive new_is_interactive ->
      let
        panel_cmd = if new_is_interactive then ButtonPanel.Activate
                                          else ButtonPanel.Deactivate
      in
        ( model
        , toUpdatePanelCmd (ButtonPanel.UpdateModel panel_cmd )
        )
    UpdateClass class_id ->
      let
        panel_cmd = ButtonPanel.UpdateModel ( ButtonPanel.ChangeActive class_id )
        display_cmd = Display.UpdateModel ( Display.UpdateClass class_id)
      in
        ( model
        , Cmd.batch [ toUpdatePanelCmd panel_cmd
                    , toUpdateDisplayCmd display_cmd
                    ]
        )



--------------------------------------------------------------------------------
updateChild : UpdateChildMsg -> Model -> ( Model, Cmd Msg )
updateChild msg model =
  case msg of
    UpdateButtonPanel panel_msg ->
      let
        ( updated_panel, panel_cmds ) = ButtonPanel.update panel_msg model.button_panel
        mapMsg p_msg = UpdateChild ( UpdateButtonPanel p_msg )
      in
        ( { model | button_panel = updated_panel }
        , Cmd.map mapMsg panel_cmds
        )
    UpdateDisplay display_msg ->
      let
        ( updated_display, display_cmds ) = Display.update display_msg model.display
        mapMsg d_msg = UpdateChild ( UpdateDisplay d_msg )
      in
        ( { model | display = updated_display }
        , Cmd.map mapMsg display_cmds
        )


-- Subscriptions
--------------------------------------------------------------------------------
subscriptions : Model -> Sub Msg
subscriptions model = Sub.none


-- View
--------------------------------------------------------------------------------
view : Model -> Html Msg
view model =
  let
    row = Html.div [ Html.Attributes.class "row" ]
    display = row [ viewDisplay model.display ]
    panel = row [ viewButtonPanel model.button_panel ]
  in
    Html.div [ Html.Attributes.class "col-xs-6 col-sm-6 col-md-6 col-lg-6" ]
             [ display, panel ]


viewDisplay : Display.Model -> Html Msg
viewDisplay model =
  let
    convertMsg msg = UpdateChild ( UpdateDisplay msg )
  in
    App.map convertMsg ( Display.view model )


viewButtonPanel : ButtonPanel.Model -> Html Msg
viewButtonPanel model =
  let
    convertMsg msg =
      case msg of
        ButtonPanel.Interaction interaction_msg ->
          case interaction_msg of
            ButtonPanel.ClickButton class_id -> Interaction ( ClickButton class_id )
        _ -> UpdateChild ( UpdateButtonPanel msg )
  in
    App.map convertMsg ( ButtonPanel.view model )

