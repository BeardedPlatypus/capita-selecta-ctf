module GalacticWar exposing ( .. )


-- Library imports
--------------------------------------------------------------------------------
import Html.App exposing ( program, programWithFlags )


-- Module imports
--------------------------------------------------------------------------------
--import GalacticWar.ClassDisplay.Button as Button
--import GalacticWar.ClassDisplay.ButtonPanel as ButtonPanel
--import GalacticWar.ClassDisplay.Display as Display
--import GalacticWar.ClassDisplay as ClassDisplay

--import GalacticWar.Stats as Stats

import GalacticWar.Class as Class
import GalacticWar.Interface as Interface
import GalacticWar.NodeDisplay.Node as Node
import GalacticWar.NodeDisplay.NodeGrid as NodeGrid
import GalacticWar.NodeDisplay.ButtonPanel as ButtonPanel
import GalacticWar.NodeDisplay as NodeDisplay
import GalacticWar.Team as Team


-- Application boilerplate
--------------------------------------------------------------------------------
-- Note program with flags for the final code
--main = program { init = Button.init Class.S
--               , update = Button.update
--               , subscriptions = Button.subscriptions
--               , view = Button.view
--               }

--main = program { init = ButtonPanel.init
--               , update = ButtonPanel.update
--               , subscriptions = ButtonPanel.subscriptions
--               , view = ButtonPanel.view
--               }

--main = program { init = Display.init Class.S
--               , update = Display.update
--               , subscriptions = Display.subscriptions
--               , view = Display.view
--               }

--main = program { init = ClassDisplay.init Class.S
--               , update = ClassDisplay.update
--               , subscriptions = ClassDisplay.subscriptions
--               , view = ClassDisplay.view
--               }

--main = program { init = Interface.init Class.S
--               , update = Interface.update
--               , subscriptions = Interface.subscriptions
--               , view = Interface.view
--               }
--main = program { init = Node.init 0 ( 0, 0 )
--               , update = Node.update
--               , subscriptions = Node.subscriptions
--               , view = Node.view
--               }

{-
getNode : Node.ID -> ( Int, Int ) -> Node.Model
getNode id pos = { id = id
                 , pos = pos
                 , radius = 50
                 , status = Node.Unclaimed
                 }
getNodeClaimed : Node.ID -> ( Int, Int ) -> Team.ID -> Class.ID -> Node.Model
getNodeClaimed id pos team class = { id = id
                                   , pos = pos
                                   , radius = 50
                                   , status = Node.Claimed team class }

main = program { init = NodeGrid.init 7 7 [ getNode  0 ( 0, 1 )
                                          , getNodeClaimed  1 ( 0, 3 ) Team.Blue Class.S
                                          , getNode  2 ( 0, 5 )
                                          --------------------
                                          , getNode  3 ( 1, 0 )
                                          , getNode  4 ( 1, 2 )
                                          , getNode  5 ( 1, 4 )
                                          , getNode  6 ( 1, 6 )
                                          --------------------
                                          , getNode  7 ( 2, 0 )
                                          , getNode  8 ( 2, 2 )
                                          , getNode  9 ( 2, 4 )
                                          , getNode 10 ( 2, 6 )
                                          --------------------
                                          , getNode 11 ( 3, 1 )
                                          , getNode 12 ( 3, 3 )
                                          , getNode 13 ( 3, 5 )
                                          --------------------
                                          , getNode 14 ( 4, 0 )
                                          , getNode 15 ( 4, 2 )
                                          , getNode 16 ( 4, 4 )
                                          , getNode 17 ( 4, 6 )
                                          --------------------
                                          , getNode 18 ( 5, 0 )
                                          , getNode 19 ( 5, 2 )
                                          , getNode 20 ( 5, 4 )
                                          , getNode 21 ( 5, 6 )
                                          --------------------
                                          , getNode 22 ( 6, 1 )
                                          , getNode 23 ( 6, 3 )
                                          , getNode 24 ( 6, 5 )
                                          ] [ ( 0, 3 )
                                            , ( 0, 4 )
                                            , ( 1, 4 )
                                            , ( 1, 5 )
                                            , ( 2, 5 )
                                            , ( 2, 6 )
                                            , ( 3, 7 )
                                            , ( 3, 8 )
                                            , ( 4, 7 )
                                            , ( 4, 8 )
                                            , ( 4, 9 )
                                            , ( 5, 8 )
                                            , ( 5, 9 )
                                            , ( 5, 10 )
                                            , ( 6, 9 )
                                            , ( 6, 10 )
                                            , ( 7, 11 )
                                            , ( 8, 11 )
                                            , ( 8, 12 )
                                            , ( 9, 12 )
                                            , ( 9, 13 )
                                            , ( 10, 13 )
                                            ]
               , update = NodeGrid.update
               , subscriptions = NodeGrid.subscriptions
               , view = NodeGrid.view
               }
-}
{-
main = program { init = ButtonPanel.init Class.S
               , update = ButtonPanel.update
               , subscriptions = ButtonPanel.subscriptions
               , view = ButtonPanel.view
               }
-}

{- 
getNode : Node.ID -> ( Int, Int ) -> Node.Model
getNode id pos = { id = id
                 , pos = pos
                 , radius = 50
                 , status = Node.Unclaimed
                 }
getNodeClaimed : Node.ID -> ( Int, Int ) -> Team.ID -> Class.ID -> Node.Model
getNodeClaimed id pos team class = { id = id
                                   , pos = pos
                                   , radius = 50
                                   , status = Node.Claimed team class }

main = program { init = NodeDisplay.init Class.S 7 7 [ getNode  0 ( 0, 1 )
                                                     , getNodeClaimed  1 ( 0, 3 ) Team.Blue Class.S
                                                     , getNode  2 ( 0, 5 )
                                                     --------------------
                                                     , getNode  3 ( 1, 0 )
                                                     , getNode  4 ( 1, 2 )
                                                     , getNode  5 ( 1, 4 )
                                                     , getNode  6 ( 1, 6 )
                                                     --------------------
                                                     , getNode  7 ( 2, 0 )
                                                     , getNode  8 ( 2, 2 )
                                                     , getNode  9 ( 2, 4 )
                                                     , getNode 10 ( 2, 6 )
                                                     --------------------
                                                     , getNode 11 ( 3, 1 )
                                                     , getNode 12 ( 3, 3 )
                                                     , getNode 13 ( 3, 5 )
                                                     --------------------
                                                     , getNode 14 ( 4, 0 )
                                                     , getNode 15 ( 4, 2 )
                                                     , getNode 16 ( 4, 4 )
                                                     , getNode 17 ( 4, 6 )
                                                     --------------------
                                                     , getNode 18 ( 5, 0 )
                                                     , getNode 19 ( 5, 2 )
                                                     , getNode 20 ( 5, 4 )
                                                     , getNode 21 ( 5, 6 )
                                                     --------------------
                                                     , getNode 22 ( 6, 1 )
                                                     , getNode 23 ( 6, 3 )
                                                     , getNode 24 ( 6, 5 )
                                                     ] [ ( 0, 3 )
                                                       , ( 0, 4 )
                                                       , ( 1, 4 )
                                                       , ( 1, 5 )
                                                       , ( 2, 5 )
                                                       , ( 2, 6 )
                                                       , ( 3, 7 )
                                                       , ( 3, 8 )
                                                       , ( 4, 7 )
                                                       , ( 4, 8 )
                                                       , ( 4, 9 )
                                                       , ( 5, 8 )
                                                       , ( 5, 9 )
                                                       , ( 5, 10 )
                                                       , ( 6, 9 )
                                                       , ( 6, 10 )
                                                       , ( 7, 11 )
                                                       , ( 8, 11 )
                                                       , ( 8, 12 )
                                                       , ( 9, 12 )
                                                       , ( 9, 13 )
                                                       , ( 10, 13 )
                                                       ]
               , update = NodeDisplay.update
               , subscriptions = NodeDisplay.subscriptions
               , view = NodeDisplay.view
               }
-}
getNode : Node.ID -> ( Int, Int ) -> Node.Model
getNode id pos = { id = id
                 , pos = pos
                 , radius = 50
                 , status = Node.Unclaimed
                 }
getNodeClaimed : Node.ID -> ( Int, Int ) -> Team.ID -> Class.ID -> Node.Model
getNodeClaimed id pos team class = { id = id
                                   , pos = pos
                                   , radius = 50
                                   , status = Node.Claimed team class }

type alias Flags = { class : String
                   , n_cols : Int
                   , n_rows : Int
                   , server_url : String
                   , nodes : List NodeFlag
                   , paths : List PathFlag
                   }

type alias NodeFlag = { id : Int
                      , x : Int
                      , y : Int
                      }

type alias PathFlag = { node_1 : Int
                      , node_2 : Int
                      }

toNodes : List NodeFlag -> List Node.Model
toNodes node_flags =
  let
    flagToNode flag = fst ( Node.init flag.id ( flag.x, flag.y ))
  in
    List.map flagToNode node_flags

toPaths : List PathFlag -> List ( Node.ID, Node.ID )
toPaths path_flags =
  let
    flagToPath flag = ( flag.node_1, flag.node_2 )
  in
    List.map flagToPath path_flags

{-
initWithFlags : Flags -> ( Interface.Model, Cmd Interface.Msg )
initWithFlags flags =
  let
    class_id =
      case Class.fromDjango flags.class of
        Just cid -> cid
        Nothing  -> Class.S
  in
    Interface.init flags.server_url
                   class_id
                   flags.n_cols
                   flags.n_rows
                   ( toNodes flags.nodes )
                   ( toPaths flags.paths )
-}

main = programWithFlags { init = Interface.init
                        , update = Interface.update
                        , subscriptions = Interface.subscriptions
                        , view = Interface.view
                        }
