module GalacticWar.NodeDisplay.NodeGrid exposing ( .. )

-- Import Libraries
--------------------------------------------------------------------------------
import Html exposing ( Html )
import Html.App
import Platform.Cmd as Cmd exposing ( Cmd )
import Platform.Sub as Sub exposing ( Sub )

import Svg exposing ( Svg )
import Svg.Attributes


-- Import Modules
--------------------------------------------------------------------------------
import GalacticWar.Team as Team
import GalacticWar.Class as Class
import GalacticWar.NodeDisplay.Node as Node
import GalacticWar.Util as Util
import List


-- Model
--------------------------------------------------------------------------------
type alias Model = { n_cols : Int
                   , n_rows : Int
                   , nodes : List Node.Model
                   , paths : List ( Node.ID, Node.ID )
                   }

init : Int -> Int -> List Node.Model -> List ( Node.ID, Node.ID ) -> ( Model, Cmd Msg )
init n_cols n_rows nodes paths = ( { n_cols = n_cols
                                   , n_rows = n_rows
                                   , nodes = nodes
                                   , paths = paths
                                   }
                                 , Cmd.none
                                 )


-- Update
--------------------------------------------------------------------------------
type Msg = Interaction InteractionMsg
         | UpdateModel UpdateModelMsg
         | UpdateChild UpdateChildMsg

type InteractionMsg = NodeClick Node.ID Node.Status
type UpdateModelMsg = UpdateStatusNodes ( List ( Node.ID, Node.Status ))
type UpdateChildMsg = UpdateNode Node.ID Node.Msg

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Interaction interaction_msg  -> updateInteraction interaction_msg  model
    UpdateModel update_model_msg -> updateModel       update_model_msg model
    UpdateChild update_child_msg -> updateChild       update_child_msg model


--------------------------------------------------------------------------------
toUpdateNodeCmd : Node.ID -> Node.Msg -> Cmd Msg
toUpdateNodeCmd id msg = Util.toCmd ( UpdateChild ( UpdateNode id msg ))

updateInteraction : InteractionMsg -> Model -> ( Model, Cmd Msg )
updateInteraction msg model =
  case msg of
    NodeClick id status ->
      ( model
      , toUpdateNodeCmd id ( Node.Interaction ( Node.NodeClick status ))
      )

updateModel : UpdateModelMsg -> Model -> ( Model, Cmd Msg )
updateModel msg model =
  let
    mapToCmd ( id, status ) = toUpdateNodeCmd id ( Node.UpdateModel ( Node.UpdateStatus status ))
  in
    case msg of
      UpdateStatusNodes list ->
        ( model
        , Cmd.batch ( List.map mapToCmd list )
        )


updateChild : UpdateChildMsg -> Model -> ( Model, Cmd Msg )
updateChild msg model =
  case msg of
    UpdateNode id node_msg ->
      let
        condUpdateNode node = if node.id == id then updateNode node node_msg
                                               else ( node, Cmd.none )
        ( updated_nodes, cmds ) = ( List.map condUpdateNode model.nodes )
                                  |> List.unzip
      in
        ( { model | nodes = updated_nodes }
        , Cmd.batch cmds )


updateNode : Node.Model -> Node.Msg -> ( Node.Model, Cmd Msg )
updateNode model msg =
  let
    ( updated_model, node_cmd ) = Node.update msg model
    mapMsg node_msg = UpdateChild ( UpdateNode model.id node_msg )
  in
    ( updated_model
    , Cmd.map mapMsg node_cmd
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
    svg_paths = viewLineSvg model
    svg_nodes = List.concat ( List.map Node.viewSvg model.nodes )
    svg_start_nodes = viewStartNodes model
    svg_start_nodes_paths = viewStartNodePaths model
    dimensions = ( "0 0 " ++ ( viewBoxWidth model )) ++ ( viewBoxHeight model )

    toMsg (id, msg ) =
      case msg of
        Node.Interaction interaction_msg ->
          case interaction_msg of
            Node.NodeClick status -> Interaction ( NodeClick id status )
        _ -> UpdateChild ( UpdateNode id msg )
  in
    Html.App.map toMsg ( Svg.svg [ Svg.Attributes.viewBox dimensions
                                 , Svg.Attributes.width "83%"
                                 ] ((( svg_start_nodes_paths
                                         ++ svg_paths )
                                       ++ svg_start_nodes )
                                      ++ svg_nodes )
                       )



--------------------------------------------------------------------------------
viewBoxWidth : Model -> String
viewBoxWidth model = " " ++ toString (( model.n_cols - 1 ) * Node.x_step + 8 * Node.unit )

viewBoxHeight : Model -> String
viewBoxHeight model = " " ++ toString (( model.n_rows ) * Node.y_step + 2 * Node.unit )


--------------------------------------------------------------------------------
viewLineSvg : Model -> List ( Svg msg )
viewLineSvg model =
  let
    filterMapFunc path_couple = pathMap model.nodes path_couple
    filtered_paths = List.filterMap filterMapFunc model.paths

    toSvgPath ( node_1, node_2 ) = Svg.line [ Svg.Attributes.x1 ( toString ( Node.calcXSvgPos node_1 ))
                                            , Svg.Attributes.y1 ( toString ( Node.calcYSvgPos node_1 ))
                                            , Svg.Attributes.x2 ( toString ( Node.calcXSvgPos node_2 ))
                                            , Svg.Attributes.y2 ( toString ( Node.calcYSvgPos node_2 ))
                                            , Svg.Attributes.stroke "#d3d3d3"
                                            , Svg.Attributes.strokeWidth "3"
                                            ] [ ]
  in
    List.map toSvgPath filtered_paths

idToNode : List Node.Model -> Node.ID -> Maybe Node.Model
idToNode nodes id =
  let
    filterFunc node = node.id == id
    filtered_nodes = List.filter filterFunc nodes
  in
    if List.isEmpty filtered_nodes then Nothing
                                   else List.head filtered_nodes

pathMap : List Node.Model -> ( Node.ID, Node.ID ) -> Maybe ( Node.Model, Node.Model )
pathMap nodes ( id_1, id_2) =
  let
    maybe_node_1 = idToNode nodes id_1
    maybe_node_2 = idToNode nodes id_2
  in
    case maybe_node_1 of
      Nothing     -> Nothing
      Just node_1 ->
        case maybe_node_2 of
          Nothing     -> Nothing
          Just node_2 -> Just ( node_1, node_2 )


--------------------------------------------------------------------------------
-- FIXME: make this more generic in the future currently assume only two teams exist

-- assumption red  = right
--            blue = left
viewStartNodes : Model -> List ( Svg msg )
viewStartNodes model =
  let
    end_x  = ( model.n_cols - 1 ) * Node.x_step + 7 * Node.unit
    height = round ((( toFloat ( model.n_rows - 1 )) / 2 ) * Node.y_step + Node.unit + 50 )
    blue_team_node = viewStartNode Team.Blue Node.unit height
    red_team_node  = viewStartNode Team.Red  end_x     height
  in
    blue_team_node ++ red_team_node


viewStartNodePaths : Model -> List ( Svg msg )
viewStartNodePaths model =
  let
    end_x  = ( model.n_cols - 1 ) * Node.x_step + 7 * Node.unit
    height = round ((( toFloat ( model.n_rows - 1 )) / 2 ) * Node.y_step + Node.unit + 50 )

    filterByColumn column node = column == ( fst node.pos )
    blue_connected = List.filter ( filterByColumn 0 ) model.nodes
    red_connected = List.filter ( filterByColumn (model.n_cols - 1 )) model.nodes

    toSvgPath ( nx, ny ) node = Svg.line [ Svg.Attributes.x1 ( toString nx )
                                         , Svg.Attributes.y1 ( toString ny )
                                         , Svg.Attributes.x2 ( toString ( Node.calcXSvgPos node ))
                                         , Svg.Attributes.y2 ( toString ( Node.calcYSvgPos node ))
                                         , Svg.Attributes.stroke "#d3d3d3"
                                         , Svg.Attributes.strokeWidth "3"
                                         ] [ ]
    blue_paths = List.map ( toSvgPath ( Node.unit, height )) blue_connected
    red_paths = List.map ( toSvgPath ( end_x, height )) red_connected
  in
    blue_paths ++ red_paths

viewStartNode : Team.ID -> Int -> Int -> List ( Svg msg )
viewStartNode id x y =
  let
    colour = Team.toHexColour id
  in
    [ Svg.circle [ Svg.Attributes.cx ( toString x )
                 , Svg.Attributes.cy ( toString y )
                 , Svg.Attributes.r "100"
                 , Svg.Attributes.fill "#000000" ] [ ]
    , Svg.circle [ Svg.Attributes.cx ( toString x )
                 , Svg.Attributes.cy ( toString y )
                 , Svg.Attributes.r "99"
                 , Svg.Attributes.fill colour ] [ ]
    ]

