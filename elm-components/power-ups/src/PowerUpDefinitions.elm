module PowerUpDefinitions ( ID ( Speed
                               , Invincibility
                               , Invisibility
                               , Bomb )
                          , toStr, toImgUrl ) where

-- Model
---------------------------------------------------
type ID = Speed
        | Invincibility
        | Invisibility
        | Bomb

toStr : ID -> String
toStr id =
  case id of
    Speed         -> "Speed"
    Invincibility -> "Invincibility"
    Invisibility  -> "Invisibility"
    Bomb          -> "Bomb"

toImgUrl : ID -> String
toImgUrl id =
  case id of
    Speed         -> "http://i.imgur.com/HNA2sJw.png"
    Invincibility -> "http://i.imgur.com/vIumg1n.png"
    Invisibility  -> "http://i.imgur.com/SvTRr2J.png"
    Bomb          -> "http://i.imgur.com/h0Mkcyd.png"
