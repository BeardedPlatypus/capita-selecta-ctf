module Respawner where


import StartApp
import Task exposing (Task)
import Signal exposing (Signal, Address)
import Effects exposing (Effects, Never)
import Html exposing (Html)

import ClassButtonPanel
import CurrentClass

-- StartApp boilerplate
------------------------------------------------------------
app = StartApp.start { init = ClassButtonPanel.init
                     , view = ClassButtonPanel.view
                     , update = ClassButtonPanel.update
                     , inputs = []
                     }

main : Signal Html
main = app.html

port tasks : Signal (Task Never ())
port tasks = app.tasks
