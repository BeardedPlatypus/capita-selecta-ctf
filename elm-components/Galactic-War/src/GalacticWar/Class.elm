module GalacticWar.Class exposing ( ID ( R, P, S)
                                  , toStr, toCharacter
                                  , toImgUrl, toDjango, fromDjango
                                  , deathToImgUrl, deathToStr )


-- Types
--------------------------------------------------------------------------------
type ID = R -- Rock
        | P -- Paper
        | S -- Scissors

-- Conversion Functions
--------------------------------------------------------------------------------
toStr : ID -> String
toStr id =
  case id of
    R -> "R-class"
    P -> "P-class"
    S -> "S-class"

toCharacter : ID -> String
toCharacter id =
  case id of
    R -> "R"
    P -> "P"
    S -> "S"


-- FIXME: update these with the appropriate images
toImgUrl : ID -> String
toImgUrl id =
  case id of
    R -> "http://i.imgur.com/yY5f0hm.png"
    P -> "http://i.imgur.com/epotstg.png"
    S -> "http://i.imgur.com/rDVcgPk.png"

toDjango : ID -> String
toDjango id =
  case id of
    R -> "r"
    P -> "p"
    S -> "s"


fromDjango : String -> Maybe ID
fromDjango id =
  case id of
    "r" -> Just R
    "p" -> Just P
    "s" -> Just S
    _   -> Nothing


-- Death functions
--------------------------------------------------------------------------------
deathToImgUrl : String
deathToImgUrl = ""

deathToStr = "Dead"
