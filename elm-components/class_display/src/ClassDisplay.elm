module ClassDisplay where


-- Import Libraries
------------------------------------------------------------
import StartApp
import Task exposing ( Task )
import Signal exposing ( Signal, Address )
import Effects exposing ( Effects, Never )
import Html exposing ( Html )


-- Import ClassDisplay modules
------------------------------------------------------------
import CurrentClassDisplay
import ClassButtonPanel
import ClassChangeButton
import PlayerClass

import ClassDisplayPanel

-- StartApp boiler plate
------------------------------------------------------------
app = StartApp.start { init = ClassDisplayPanel.init
                     , view = ClassDisplayPanel.view
                     , update = ClassDisplayPanel.update
                     , inputs = [ modelUpdates ]
                     }

main : Signal Html
main = app.html


-- Additional Communication ports
------------------------------------------------------------
port activeClassIn : Signal String
port isAliveIn : Signal Bool
port baseUrlIn : Signal String


modelUpdates =
  let
    updateBaseUrl = Signal.map ClassDisplayPanel.UpdateBaseUrl baseUrlIn
    updateIsAlive = Signal.map ClassDisplayPanel.UpdateIsAlive isAliveIn
    updateActiveClass = Signal.map ClassDisplayPanel.UpdateCurrentClass
                                   ( Signal.map PlayerClass.fromDjango activeClassIn )
  in
    Signal.mergeMany [ updateBaseUrl
                     , updateIsAlive
                     , updateActiveClass
                     ]
