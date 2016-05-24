module PlayerClass ( ID ( Light, Medium, Heavy, Undefined )
                   , toStr, toImgUrl, toDjango, fromDjango
                   , deathToUrl, deathToStr ) where

-- Types
------------------------------------------------------------
type ID = Light
        | Medium
        | Heavy
        | Undefined


-- Conversion Functions
------------------------------------------------------------
toStr : ID -> String
toStr id =
  case id of
    Light     -> "Light Class"
    Medium    -> "Medium Class"
    Heavy     -> "Heavy Class"
    Undefined -> "Undefined Class"

toImgUrl : ID -> String
toImgUrl id =
  case id of
    Light     -> "http://i.imgur.com/yY5f0hm.png"
    Medium    -> "http://i.imgur.com/epotstg.png"
    Heavy     -> "http://i.imgur.com/rDVcgPk.png"
    Undefined -> ""

toDjango : ID -> String
toDjango id =
  case id of
    Light     -> "li"
    Medium    -> "me"
    Heavy     -> "he"
    Undefined -> "NULL"

fromDjango : String -> ID
fromDjango id =
  case id of
    "li" -> Light
    "me" -> Medium
    "he" -> Heavy
    _    -> Undefined

-- Death
------------------------------------------------------------
deathToUrl : String
deathToUrl = "http://www.mysoti.com/img/user/altoonativ/product/web/1209107/1209107_show_default.png"

deathToStr : String
deathToStr = "Death"

