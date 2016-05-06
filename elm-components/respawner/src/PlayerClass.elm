module PlayerClass ( ID ( Light, Medium, Heavy )
                   , toStr, toImgUrl
                   , deathToUrl, deathToStr
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

deathToUrl : String
deathToUrl = "http://www.mysoti.com/img/user/altoonativ/product/web/1209107/1209107_show_default.png"

deathToStr : String
deathToStr = "Death"
