module CurrentClass ( Model, init
                    , update, Action ( NoAction, RespawnAs, Die )
                    , view) where

-- Packages
------------------------------------------------------------
-- Elm Packages --
import Effects exposing (Effects, Never)
import Html exposing (Html)
import Html.Attributes exposing ( class )
import Signal exposing (Signal, Address)

-- Respawner Modules --
import PlayerClass

-- Model
------------------------------------------------------------
type alias Model = { class : PlayerClass.ID
                   , is_alive : Bool
                   }

init : ( Model, Effects Action )
init = ( { class = PlayerClass.Medium
         , is_alive = True
         }
       , Effects.none
       )


-- Update
------------------------------------------------------------
type Action = NoAction
            | RespawnAs PlayerClass.ID
            | Die

update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    NoAction -> ( model, Effects.none )
    RespawnAs class_id -> ( { class = class_id
                            , is_alive = True
                            }
                          , Effects.none
                          )
    Die -> ( { model | is_alive = False }
           , Effects.none
           )

-- View
------------------------------------------------------------
view : Address Action -> Model -> Html
view address model =
  let
    attributes = [ ]
    src = Html.Attributes.src ( if model.is_alive
                                  then ( PlayerClass.toImgUrl model.class )
                                  else ( PlayerClass.deathToUrl )
                              )
    alt = Html.Attributes.alt ( if model.is_alive
                                  then ( PlayerClass.toStr model.class )
                                  else ( PlayerClass.deathToStr )
                              )
    img = Html.img [ src
                   , alt
                   , Html.Attributes.class "ctf_class_symbol"
                   ]
                   [ ]
    elements = [ img ]
  in
    Html.div attributes elements

