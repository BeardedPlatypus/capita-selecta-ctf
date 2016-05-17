module GalacticWar.ClassDisplay.Display exposing ( Model, init
                                                 , Msg ( UpdateModel )
                                                 , UpdateModelMsg ( UpdateClass
                                                                  , UpdateIsAlive )
                                                 , update, subscriptions, view )


-- Import Libraries
--------------------------------------------------------------------------------
import Html exposing ( Html )
import Html.Attributes
import Platform.Cmd as Cmd exposing ( Cmd )
import Platform.Sub as Sub exposing ( Sub )


-- Import Modules
--------------------------------------------------------------------------------
import GalacticWar.Class as Class


-- Model
--------------------------------------------------------------------------------
type alias Model = { class : Class.ID
                   , is_alive : Bool
                   }

init : Class.ID -> ( Model, Cmd Msg )
init class_id = ( { class = class_id
                  , is_alive = True
                  }
                , Cmd.none )


-- Update
--------------------------------------------------------------------------------
type Msg = UpdateModel UpdateModelMsg

type UpdateModelMsg = UpdateClass Class.ID
                    | UpdateIsAlive Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    UpdateModel update_model_msg -> updateModel update_model_msg model


updateModel : UpdateModelMsg -> Model -> ( Model, Cmd Msg )
updateModel msg model =
  case msg of
    UpdateClass class_id ->
      ( { model | class = class_id }
      , Cmd.none )
    UpdateIsAlive new_is_alive ->
      ( { model | is_alive = new_is_alive }
      , Cmd.none )


-- Subscriptions
--------------------------------------------------------------------------------
subscriptions : Model -> Sub Msg
subscriptions model = Sub.none


-- View
--------------------------------------------------------------------------------
view : Model -> Html Msg
view model =
  let
    src = Html.Attributes.src ( if model.is_alive
                                  then Class.toImgUrl model.class
                                  else Class.deathToImgUrl
                              )
    alt = Html.Attributes.alt ( if model.is_alive
                                  then Class.toStr model.class
                                  else Class.deathToStr
                              )
    img = Html.img [ src, alt, Html.Attributes.class "ctf_class_symbol" ] [ ]
  in
    Html.div [ ] [ img ]

