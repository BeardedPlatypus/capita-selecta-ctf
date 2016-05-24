module ClassChangeButton ( Model, init
                         , Action ( NoAction
                                  , Click
                                  , UpdateIsActive
                                  , UpdateIsTimedOut
                                  )
                         , update, view ) where


-- Import Libraries
------------------------------------------------------------
import Effects exposing ( Effects, Never )
import Html exposing ( Html )
import Html.Attributes exposing ( class )
import Html.Events
import Signal exposing ( Signal, Address )


-- Import ClassDisplay modules
------------------------------------------------------------
import PlayerClass


-- Model
------------------------------------------------------------
type alias Model = { class : PlayerClass.ID
                   , is_active : Bool
                   , is_timed_out : Bool
                   }


init : PlayerClass.ID -> ( Model, Effects Action )
init id = ( { class = id
            , is_active = False
            , is_timed_out = False
            }
          , Effects.none
          )


-- Update
------------------------------------------------------------
type Action = NoAction
            | Click
            | UpdateIsActive Bool
            | UpdateIsTimedOut Bool


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    NoAction -> ( model, Effects.none )
    Click    -> ( model, Effects.none )
    UpdateIsActive new_active ->
      ( { model | is_active = new_active }
      , Effects.none
      )
    UpdateIsTimedOut new_timed_out ->
      ( {model | is_timed_out = new_timed_out }
      , Effects.none
      )


-- View
------------------------------------------------------------
view : Address Action -> Model -> Html
view address model =
  let
    attributes = ( if model.is_active
                   then [ Html.Attributes.class "btn btn-block btn-orange active"
                        ]
                   else [ Html.Attributes.class "btn btn-block btn-default btn-embossed"
                        , Html.Events.onClick address Click
                        ]
                 ) ++ [ Html.Attributes.disabled model.is_timed_out ]
    img = Html.img [ Html.Attributes.src ( PlayerClass.toImgUrl model.class )
                   , Html.Attributes.alt ( PlayerClass.toStr model.class )
                   , Html.Attributes.width 100
                   ]
                   [ ]
  in
    Html.a attributes [ img ]
