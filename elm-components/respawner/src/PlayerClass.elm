module PlayerClass ( ID ( Light, Medium, Heavy )
                   , toStr, toImgUrl
                   ) where

-- PlayerClass Types
------------------------------------------------------------
type ID = Light
        | Medium
        | Heavy


-- Functions
------------------------------------------------------------
toStr : ID -> String
toStr player_class =
  case player_class of
    Light  -> "Light Class"
    Medium -> "Medium Class"
    Heavy  -> "Heavy Class"

toImgUrl : ID -> String
toImgUrl player_class =
  case player_class of
    Light  -> "http://i.imgur.com/yY5f0hm.png"
    Medium -> "http://i.imgur.com/epotstg.png"
    Heavy  -> "http://i.imgur.com/rDVcgPk.png"
