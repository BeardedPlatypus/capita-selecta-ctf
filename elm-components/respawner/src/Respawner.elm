module Respawner where


import StartApp
import Task exposing (Task)
import Signal exposing (Signal, Address)
import Effects exposing (Effects, Never)
import Html exposing (Html)

import ClassButton

-- StartApp boilerplate
------------------------------------------------------------
app = StartApp.start { init = ClassButton.lightButton
                     , view = ClassButton.view
                     , update = ClassButton.update
                     , inputs = []
                     }

main : Signal Html
main = app.html

port tasks : Signal (Task Never ())
port tasks = app.tasks
