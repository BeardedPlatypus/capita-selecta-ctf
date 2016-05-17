module GalacticWar.NodeDisplay.Node exposing ( .. )


-- Import Libraries
--------------------------------------------------------------------------------
import Html exposing ( Html )
import Platform.Cmd as Cmd exposing ( Cmd )
import Platform.Sub as Sub exposing ( Sub )

import Svg exposing ( Svg )
import Svg.Attributes


-- Import Modules
--------------------------------------------------------------------------------
import GalacticWar.Team as Team
import GalacticWar.Class as Class


-- Model
--------------------------------------------------------------------------------
type alias Model = { id : ID
                   , pos : ( Int, Int )
                   , radius : Int
                   , status : Status
                   }


init : ID -> ( Int, Int ) -> ( Model, Cmd Msg )
init id pos = ( { id = id
                , pos = pos
                , radius = 50
                , status = Unclaimed
                }
              , Cmd.none )

--------------------------------------------------------------------------------
type alias ID = Int

type Status = Claimed Team.ID Class.ID
            | Unclaimed


-- Update
--------------------------------------------------------------------------------
type Msg = Interaction InteractionMsg
         | UpdateModel UpdateModelMsg

type InteractionMsg = NodeClick

type UpdateModelMsg = UpdateStatus Status
                    | UpdatePos  ( Int, Int )
                    | UpdateSize Int

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Interaction interaction_msg  -> updateInteraction interaction_msg  model
    UpdateModel update_model_msg -> updateModel       update_model_msg model


updateInteraction : InteractionMsg -> Model -> ( Model, Cmd Msg )
updateInteraction msg model =
  case msg of
    NodeClick -> ( model, Cmd.none )

updateModel : UpdateModelMsg -> Model -> ( Model, Cmd Msg )
updateModel msg model =
  case msg of
    UpdateStatus new_status -> ( { model | status = new_status }
                               , Cmd.none )
    UpdatePos    new_pos    -> ( { model | pos = new_pos }
                               , Cmd.none )
    UpdateSize   new_radius -> ( { model | radius = new_radius }
                               , Cmd.none )


-- Subscriptions
--------------------------------------------------------------------------------
subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- View
--------------------------------------------------------------------------------
viewSvg : Model -> List ( Svg msg )
viewSvg model =
  let
    fill_colour =
      case model.status of
        Claimed team_id _ -> Team.toHexColour team_id
        Unclaimed         -> "#d3d3d3"
  in
    [ Svg.circle [ Svg.Attributes.cx ( calcXSvgPos model )
                 , Svg.Attributes.cy ( calcYSvgPos model )
                 , Svg.Attributes.r ( toString model.radius)
                 , Svg.Attributes.fill "#000000" ] [ ]  -- Black Outline
    , Svg.circle [ Svg.Attributes.cx ( calcXSvgPos model )
                 , Svg.Attributes.cy ( calcYSvgPos model )
                 , Svg.Attributes.r ( toString ( model.radius - 1 ))
                 , Svg.Attributes.fill fill_colour ] [ ] -- White fill
    ]


view : Model -> Html Msg
view model =
  Svg.svg [ Svg.Attributes.viewBox "0 0 800 300"
          , Svg.Attributes.width "400px" ]
          ( viewSvg model )


--------------------------------------------------------------------------------
unit = 100
x_step = 2 * unit
y_step = 1 * unit

calcXSvgPos : Model -> String
calcXSvgPos model = toString (( fst model.pos ) * x_step + 4 * unit )

calcYSvgPos : Model -> String
calcYSvgPos model = toString (( snd model.pos ) * y_step + unit + 50 )
