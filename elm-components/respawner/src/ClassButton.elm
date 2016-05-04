module ClassButton ( ClassButton
                   , lightButton, mediumButton, heavyButton
                   , Action( NoAction )
                   , update
                   , view
                   ) where

-- Packages
------------------------------------------------------------
-- Elm packages --
------------------
import Effects exposing (Effects, Never)
import Html exposing (Html)
import Html.Attributes exposing ( class )
import Signal exposing (Signal, Address)

-- Respawner imports --
-----------------------
import PlayerClass


-- Model
------------------------------------------------------------
type alias ClassButton = { class : PlayerClass.ID
                         , is_active : Bool
                         }
type alias Model = ClassButton

-- Functions
------------------------------------------------------------
-- Init --
----------
lightButton : (Model, Effects Action)
lightButton = ( { class = PlayerClass.Light
                , is_active = False
                }
              , Effects.none
              )

mediumButton : (Model, Effects Action)
mediumButton = ( { class = PlayerClass.Medium
                , is_active = False
                }
              , Effects.none
              )

heavyButton : (Model, Effects Action)
heavyButton = ( { class = PlayerClass.Heavy
                , is_active = False
                }
              , Effects.none
              )

-- Update --
------------
type Action = NoAction

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoAction -> (model, Effects.none)

-- View --
----------
view : Address Action -> Model -> Html
view address model =
  let
    attributes = [ Html.Attributes.class "btn btn-block btn-danger btn-embossed" ]
    img = Html.img [ Html.Attributes.src ( PlayerClass.toImgUrl model.class )
                   , Html.Attributes.alt ( PlayerClass.toStr model.class )
                   , Html.Attributes.width 100
                   ]
                   [ ]
    elements = [ img ]
  in
    Html.a attributes elements
