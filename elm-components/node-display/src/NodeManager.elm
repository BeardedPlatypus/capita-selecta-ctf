module NodeManager exposing ( .. )


-- Import Libraries
--------------------------------------------------------------------------------
import Svg
import Color exposing ( Color )


-- Import Modules
--------------------------------------------------------------------------------
import GalacticWar.Team as Team


-- Model
--------------------------------------------------------------------------------
type alias Model = { nodes = List Node
                   , paths = List ( NodeID, NodeID )
                   , team = Team.ID
                   }

init : List Node -> List ( NodeID, NodeID ) -> Team.ID -> ( Model, Cmd Msg )
init nodes paths team = { nodes = nodes
                        , paths = paths
                        , team = team
                        }


--------------------------------------------------------------------------------
type alias NodeID = Int


type alias Node = { id : NodeID
                  , size : Int
                  , pos : ( Int, Int )
                  , node_status : NodeStatus
                  }


type NodeStatus = Unclaimed
                | Claimed Team.ID


-- Update
--------------------------------------------------------------------------------
type Msg = Interaction InteractionMsg
         | UpdateModel UpdateModelMsg

type InteractionMsg = NodeClick NodeID

type UpdateModelMsg = UpdateNode NodeID 


-- View
--------------------------------------------------------------------------------
