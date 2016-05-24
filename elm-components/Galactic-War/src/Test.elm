module Test exposing ( .. )

-- Library imports
--------------------------------------------------------------------------------
import Html.App exposing ( program, programWithFlags )

import GalacticWar.Interact as Interact

main = program { init = Interact.init
               , update = Interact.update
               , subscriptions = Interact.subscriptions
               , view = Interact.view
               }
