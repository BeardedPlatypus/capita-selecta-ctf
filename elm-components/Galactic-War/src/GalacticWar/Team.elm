module GalacticWar.Team exposing ( .. )


-- Type
--------------------------------------------------------------------------------
type ID = Red
        | Blue


-- Functions
--------------------------------------------------------------------------------
toStr : ID -> String
toStr team =
  case team of
    Red  -> "Red"
    Blue -> "Blue"


toHexColour : ID -> String
toHexColour team =
  case team of
    Red       -> "#f84545"
    Blue      -> "#02baf2"

fromDjango : String -> Maybe ID
fromDjango s =
  case s of
    "r" -> Just Red
    "b" -> Just Blue
    _   -> Nothing

iconUrl : String
iconUrl = "http://i.imgur.com/vqtnJqI.png"

