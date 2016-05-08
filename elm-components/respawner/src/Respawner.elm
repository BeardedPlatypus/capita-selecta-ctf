module Respawner where


import StartApp
import Task exposing (Task)
import Signal exposing (Signal, Address)
import Effects exposing (Effects, Never)
import Html exposing (Html)

import RespawnPanel

-- StartApp boilerplate
------------------------------------------------------------
app = StartApp.start { init = RespawnPanel.init
                     , view = RespawnPanel.view
                     , update = RespawnPanel.update
                     , inputs = []
                     }

main : Signal Html
main = app.html

port tasks : Signal (Task Never ())
port tasks = app.tasks
