module GalacticWar.NodeDisplay exposing ( .. )


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
import GalacticWar.NodeDisplay.Node as Node
import GalacticWar.NodeDisplay.NodeGrid as NodeGrid
import GalacticWar.NodeDisplay.ButtonPanel as ButtonPanel

import GalacticWar.Util as Util

-- Model
--------------------------------------------------------------------------------
type alias Model = { button_panel : ButtonPanel.Model
                   , node_grid : NodeGrid.Model
                   }


init : Class.ID -> Int -> Int -> List Node.Model -> List ( Node.ID, Node.ID ) -> ( Model, Cmd Msg )
init id n_cols n_rows nodes paths = ( { button_panel = fst ( ButtonPanel.init id )
                                      , node_grid = fst ( NodeGrid.init n_cols n_rows nodes paths )
                                      }
                                    , Cmd.none
                                    )


-- Update
--------------------------------------------------------------------------------
type Msg = Interaction InteractionMsg
         | UpdateModel UpdateModelMsg
         | UpdateChild UpdateChildMsg

type InteractionMsg = NodeClick Node.ID Node.Status Class.ID
                    | ClickButton Class.ID

type UpdateModelMsg = UpdateNodes ( List ( Node.ID, Node.Status ))

type UpdateChildMsg = UpdateNodeGrid    NodeGrid.Msg
                    | UpdateButtonPanel ButtonPanel.Msg

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Interaction interaction_msg  -> updateInteraction interaction_msg  model
    UpdateModel update_model_msg -> updateModel       update_model_msg model
    UpdateChild update_child_msg -> updateChild       update_child_msg model


--------------------------------------------------------------------------------
toUpdateNodeGridCmd : NodeGrid.Msg -> Cmd Msg
toUpdateNodeGridCmd msg = Util.toCmd ( UpdateChild ( UpdateNodeGrid msg ))

toUpdateButtonPanelCmd : ButtonPanel.Msg -> Cmd Msg
toUpdateButtonPanelCmd msg = Util.toCmd ( UpdateChild ( UpdateButtonPanel msg ))


--------------------------------------------------------------------------------
updateInteraction : InteractionMsg -> Model -> ( Model, Cmd Msg )
updateInteraction interaction_msg model =
  case interaction_msg of
    NodeClick node_id status class_id ->
      ( model
      , Cmd.none )
    ClickButton class_id ->
      ( model
      , toUpdateButtonPanelCmd ( ButtonPanel.UpdateModel ( ButtonPanel.ChangeActive class_id )))


--------------------------------------------------------------------------------
updateModel : UpdateModelMsg -> Model -> ( Model, Cmd Msg )
updateModel msg model =
  case msg of
    UpdateNodes list -> ( model, toUpdateNodeGridCmd ( NodeGrid.UpdateModel (NodeGrid.UpdateStatusNodes list )))

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
    UpdateNodeGrid node_grid_msg ->
      let
        (updated_node_grid, node_grid_cmds ) = NodeGrid.update node_grid_msg model.node_grid
        mapMsg n_msg = UpdateChild ( UpdateNodeGrid n_msg )
      in
        ( { model | node_grid = updated_node_grid }
        , Cmd.map mapMsg node_grid_cmds
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
    panel = Html.div [ Html.Attributes.class "col-xs-2 col-sm-2 col-md-2 col-lg-2" ]
                     [ viewButtonPanel model.button_panel ]
    grid  = Html.div [ Html.Attributes.class "col-xs-10 col-sm-10 col-md-10 col-lg-10" ]
                     [ viewNodeGrid model ]
  in
    Html.div [ Html.Attributes.class "row" ]
             [ panel, grid ]


--------------------------------------------------------------------------------
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


viewNodeGrid : Model -> Html Msg
viewNodeGrid model =
  let
    convertMsg msg =
      case msg of
        NodeGrid.Interaction interaction_msg ->
          case interaction_msg of
            NodeGrid.NodeClick id status ->
              case model.button_panel.current_active of
                Just class_id -> Interaction ( NodeClick id status class_id )
                Nothing -> UpdateChild ( UpdateNodeGrid msg )
        _ -> UpdateChild ( UpdateNodeGrid msg )
  in
    App.map convertMsg ( NodeGrid.view model.node_grid )
