module Team ( ID ( Red, Blue, Undefined )
            , toStr, toHexColour
            , iconUrl
            ) where

-- Teams
------------------------------------------------------------
type ID = Red
        | Blue
        | Undefined

-- Functions
------------------------------------------------------------
toStr : ID -> String
toStr team =
  case team of
    Red       -> "Red"
    Blue      -> "Blue"
    Undefined -> "Undefined"

toHexColour : ID -> String
toHexColour team =
  case team of
    Red       -> "#f84545"
    Blue      -> "#02baf2"
    Undefined -> "#FFFFFF"

iconUrl : String
iconUrl = "http://i.imgur.com/vqtnJqI.png"
