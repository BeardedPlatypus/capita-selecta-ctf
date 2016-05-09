module Stats where

import StartApp
import Task exposing ( Task )
import Signal exposing ( Signal, Address )
import Effects exposing ( Effects, Never )
import Html exposing ( Html )

import StatsPanel


-- StartApp Boilerplate
------------------------------------------------------------
app = StartApp.start { init = StatsPanel.init
                     , view = StatsPanel.view
                     , update = StatsPanel.update
                     , inputs = []
                     }

main : Signal Html
main = app.html

port tasks : Signal ( Task Never () )
port tasks = app.tasks
