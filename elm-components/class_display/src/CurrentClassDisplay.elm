module CurrentClassDisplay ( Model, init
                           , Action ( NoAction
                                    , UpdateClass
                                    , UpdateIsAlive)
                           , update, view )where


-- Import Libraries
------------------------------------------------------------
import Effects exposing ( Effects )
import Html exposing ( Html )
import Html.Attributes
import Signal exposing ( Signal, Address )

-- Import ClassDisplay modules
------------------------------------------------------------
import PlayerClass


-- Model
------------------------------------------------------------
type alias Model = { class : PlayerClass.ID
                   , is_alive : Bool
                   }

init : ( Model, Effects Action )
init = ( { class = PlayerClass.Undefined
         , is_alive = True
         }
       , Effects.none
       )


-- Update
------------------------------------------------------------
type Action = NoAction
            | UpdateClass PlayerClass.ID
            | UpdateIsAlive Bool

update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    NoAction                -> ( model, Effects.none )
    UpdateClass class_id    -> ( { model | class = class_id }
                               , Effects.none )
    UpdateIsAlive new_alive -> ( { model | is_alive = new_alive }
                               , Effects.none )


-- View
------------------------------------------------------------
view : Address Action -> Model -> Html
view address model =
  let
    src = Html.Attributes.src ( if model.is_alive
                                  then PlayerClass.toImgUrl model.class
                                  else PlayerClass.deathToUrl
                              )
    alt = Html.Attributes.alt ( if model.is_alive
                                  then PlayerClass.toStr model.class
                                  else PlayerClass.deathToStr
                              )
    img = Html.img [ src, alt, Html.Attributes.class "ctf_class_symbol" ] [ ]
  in
    Html.div [ ] [ img ]

