module Util exposing ( .. )


-- Import Libraries
--------------------------------------------------------------------------------
import Task


-- Functions
--------------------------------------------------------------------------------
toCmd : msg -> Cmd msg
toCmd msg =
  let
    onFail    val = val
    onSucceed val = val
  in
    Task.perform onFail onSucceed ( Task.succeed msg )
