module Stats where

import StartApp
import Task exposing ( Task )
import Signal exposing ( Signal, Address )
import Effects exposing ( Effects, Never )
import Html exposing ( Html )

import PowerUpPanel


-- StartApp Boilerplate
------------------------------------------------------------
app = StartApp.start { init = PowerUpPanel.init
                     , view = PowerUpPanel.view
                     , update = PowerUpPanel.update
                     , inputs = []
                     }

main : Signal Html
main = app.html

port tasks : Signal ( Task Never () )
port tasks = app.tasks
