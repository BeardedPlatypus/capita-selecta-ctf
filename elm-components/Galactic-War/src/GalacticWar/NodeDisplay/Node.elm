module GalacticWar.NodeDisplay.Node exposing ( .. )


-- Import Libraries
--------------------------------------------------------------------------------
import Html exposing ( Html )
import Html.App
import Platform.Cmd as Cmd exposing ( Cmd )
import Platform.Sub as Sub exposing ( Sub )

import Svg exposing ( Svg )
import Svg.Attributes
import Svg.Events


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
type alias NodeMsg = ( ID,  Msg )

type Msg = Interaction InteractionMsg
         | UpdateModel UpdateModelMsg

type InteractionMsg = NodeClick Status

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
    NodeClick status -> ( model, Cmd.none )

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
viewSvg : Model -> List ( Svg NodeMsg )
viewSvg model =
  let
    fill_colour =
      case model.status of
        Claimed team_id _ -> Team.toHexColour team_id
        Unclaimed         -> "#d3d3d3"
    pos_x = calcXSvgPos model
    pos_y = calcYSvgPos model
    half_radius = round (( toFloat model.radius ) / 2 )

    text =
      case model.status of
        Claimed _ class -> [ placeText model.id model.status
                                       ( pos_x - half_radius )
                                       ( pos_y + half_radius )
                                       "75" "#000000"
                                       ( Class.toCharacter class ) ]
        Unclaimed       -> [ ]

  in
    [ Svg.circle [ Svg.Attributes.cx ( toString pos_x )
                 , Svg.Attributes.cy ( toString pos_y )
                 , Svg.Attributes.r ( toString model.radius)
                 , Svg.Attributes.fill "#000000"
                 ] [ ]  -- Black Outline
    , Svg.circle [ Svg.Attributes.cx ( toString pos_x )
                 , Svg.Attributes.cy ( toString pos_y )
                 , Svg.Attributes.r ( toString ( model.radius - 1 ))
                 , Svg.Attributes.fill fill_colour
                 --, Svg.Events.onClick ( model.id, ( Interaction ( NodeClick model.status )))
                 ] [ ] -- White fill
    ] --++ text


view : Model -> Html Msg
view model =
  Html.App.map snd ( Svg.svg [ Svg.Attributes.viewBox "0 0 800 300"
                             , Svg.Attributes.width "400px"
                             ] ( viewSvg model ))


--------------------------------------------------------------------------------
unit = 100
x_step = 2 * unit
y_step = 1 * unit

calcXSvgPos : Model -> Int
calcXSvgPos model = ( fst model.pos ) * x_step + 4 * unit

calcYSvgPos : Model -> Int
calcYSvgPos model = ( snd model.pos ) * y_step + unit + 50


--------------------------------------------------------------------------------
placeText: ID -> Status -> Int -> Int -> String -> String -> String -> Svg NodeMsg
placeText id status x y size colour txt =
  Svg.text' [ Svg.Attributes.x (toString x)
            , Svg.Attributes.y (toString y)
            , Svg.Attributes.fill colour
            , Svg.Attributes.fontSize size
            , Svg.Events.onClick ( id, ( Interaction ( NodeClick status )))
            ] [ Svg.text txt ]
