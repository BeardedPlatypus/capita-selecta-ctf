module GalacticWar.NodeDisplay.NodeGrid exposing ( .. )

-- Import Libraries
--------------------------------------------------------------------------------
import Html exposing ( Html )
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
--         | UpdateModel UpdateModelMsg
         | UpdateChild UpdateChildMsg

type InteractionMsg = NodeClick Node.ID

-- type UpdateModelMsg = PlaceHolder

type UpdateChildMsg = UpdateNode Node.ID Node.Msg

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Interaction interaction_msg  -> updateInteraction interaction_msg  model
    UpdateChild update_child_msg -> updateChild       update_child_msg model


--------------------------------------------------------------------------------
toUpdateNodeCmd : Node.ID -> Node.Msg -> Cmd Msg
toUpdateNodeCmd id msg = Util.toCmd ( UpdateChild ( UpdateNode id msg ))

updateInteraction : InteractionMsg -> Model -> ( Model, Cmd Msg )
updateInteraction msg model =
  case msg of
    NodeClick id ->
      ( model
      , toUpdateNodeCmd id ( Node.Interaction ( Node.NodeClick ))
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
    dimensions = ( "0 0 " ++ ( viewBoxWidth model )) ++ ( viewBoxHeight model )
  in
    Svg.svg [ Svg.Attributes.viewBox dimensions
            , Svg.Attributes.width "100%" ]
            ( svg_paths ++ svg_nodes )



viewBoxWidth : Model -> String
viewBoxWidth model = " " ++ toString (( model.n_cols - 1 ) * Node.x_step + 8 * Node.unit )

viewBoxHeight : Model -> String
viewBoxHeight model = " " ++ toString (( model.n_rows ) * Node.y_step + 2 * Node.unit )

viewLineSvg : Model -> List ( Svg msg )
viewLineSvg model =
  let
    filterMapFunc path_couple = pathMap model.nodes path_couple
    filtered_paths = List.filterMap filterMapFunc model.paths

    toSvgPath ( node_1, node_2 ) = Svg.line [ Svg.Attributes.x1 ( Node.calcXSvgPos node_1 )
                                            , Svg.Attributes.y1 ( Node.calcYSvgPos node_1 )
                                            , Svg.Attributes.x2 ( Node.calcXSvgPos node_2 )
                                            , Svg.Attributes.y2 ( Node.calcYSvgPos node_2 )
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
