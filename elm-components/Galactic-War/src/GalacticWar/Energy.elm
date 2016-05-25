module GalacticWar.Energy exposing ( .. )


-- Import Libraries
--------------------------------------------------------------------------------
import Html.App
import Html exposing ( Html )
import Html.Attributes
import Time


-- Import Modules
--------------------------------------------------------------------------------
import GalacticWar.Util as Util


-- Model
--------------------------------------------------------------------------------
type alias Model = { is_charged  : Bool
                   , reset_time  : Int
                   , is_counting : Bool
                   , current_n   : Int
                   }


init : ( Model, Cmd Msg )
init = ( { is_charged = True
         , reset_time = 0
         , is_counting = False
         , current_n = 0
         }
       , Cmd.none )


-- Update
--------------------------------------------------------------------------------
type Msg = Request     RequestMsg
         | UpdateModel UpdateModelMsg

type RequestMsg = RequestTick

type UpdateModelMsg = Discharge
                    | Recharge
                    | Tick
                    | UpdateResetTime Int


--------------------------------------------------------------------------------
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Request     request_msg      -> updateRequest request_msg      model
    UpdateModel update_model_msg -> updateModel   update_model_msg model


updateRequest : RequestMsg -> Model -> ( Model, Cmd Msg )
updateRequest msg model =
  case msg of
    RequestTick ->
      ( model
      , if model.is_counting then ( Util.toCmd ( UpdateModel Tick ))
                             else Cmd.none
      )

updateModel : UpdateModelMsg -> Model -> ( Model, Cmd Msg )
updateModel msg model =
  case msg of
    Discharge -> ( { model | is_charged = False
                           , current_n  = 0
                   }
                 , Cmd.none )
    Recharge  -> ( { model | is_charged = True
                   , is_counting = False
                   }
                 , Cmd.none )
    Tick      ->
      if ( model.current_n == model.reset_time ) then
        ( model, Util.toCmd ( UpdateModel Recharge ))
      else
        ( { model | current_n = model.current_n + 1 }
        , Cmd.none )
    UpdateResetTime n ->
      ( { model | reset_time = n
                , is_counting = True
        }
      , Cmd.none )


-- Subscriptions
--------------------------------------------------------------------------------
requestTick : Sub Msg
requestTick = Time.every Time.second (\_ -> ( Request RequestTick ))

subscriptions model = requestTick


-- View
--------------------------------------------------------------------------------
view : Model -> Html Msg
view model =
  let
    container = Html.div [ Html.Attributes.class "row ctf_component_section" ]
    inner_class_8 = Html.div [ Html.Attributes.class "col-md-8" ]
    inner_class_4 = Html.div [ Html.Attributes.class "col-md-4" ]

    description = inner_class_8 [ Html.text "Battery:" ]
    state =       inner_class_4 ( if model.is_charged then
                                    [ Html.text "Charged" ]
                                  else if not model.is_counting then
                                    [ Html.text "Discharged" ]
                                  else
                                    [ Html.text (( toString ( model.reset_time -
                                                              model.current_n )) ++
                                                " seconds" )
                                    ]
                                )
  in
    container [ description
              , state
              ]

