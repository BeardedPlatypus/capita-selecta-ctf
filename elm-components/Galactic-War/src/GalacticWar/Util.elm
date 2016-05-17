module GalacticWar.Util exposing ( .. )

-- Import Libraries
--------------------------------------------------------------------------------
import Platform.Cmd as Cmd exposing ( Cmd )
import Platform.Sub as Sub exposing ( Sub )
import Task


-- Utility Functions
--------------------------------------------------------------------------------
toCmd : a -> Cmd a
toCmd msg =
  let
    onFail val = val
    onSucceed val = val
  in
    Task.perform onFail onSucceed ( Task.succeed msg )
