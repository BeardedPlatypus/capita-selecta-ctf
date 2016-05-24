module StatsPanel ( Model, init
                  , Action ( NoAction
                           , UpdateTeam
                           , UpdateTeamScore
                           , UpdatePlayerName
                           , UpdateScore
                           , UpdateKills
                           , UpdateDeaths
                           , UpdateFetchUrl
                           , UpdateStats
                           )
                  , update, view) where

-- Imports
------------------------------------------------------------
import Html exposing ( Html )
import Html.Attributes
import Effects exposing ( Effects )
import Signal exposing (Signal, Address)


import Team
import StatsUpdater


-- Model
------------------------------------------------------------
type alias Model = { team : Team.ID
                   , team_score : List ( Team.ID, Int )
                   , player_name : String
                   , score : Int
                   , kills : Int
                   , deaths : Int
                   , update_url : String
                   }

init : ( Model, Effects Action )
init = ( { team = Team.Red
         , team_score = [ ( Team.Red,  0 )
                        , ( Team.Blue, 0 ) ]
         , player_name = "Placeholder"
         , score = 0
         , kills = 0
         , deaths = 0
         , update_url = ""
         }
       , Effects.none
       )


-- Update
------------------------------------------------------------
type Action = NoAction
            | UpdateTeam Team.ID
            | UpdateTeamScore Team.ID Int
            | UpdatePlayerName String
            | UpdateScore Int
            | UpdateKills Int
            | UpdateDeaths Int
            | UpdateFetchUrl String
            | UpdateStats StatsUpdater.Action

update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    NoAction           -> ( model, Effects.none )
    UpdateTeam team_id -> ( { model | team = team_id }
                          , Effects.none )
    UpdateTeamScore team_id new_score ->
      let
        update_score = \ (cur_team_id, old_score) -> if team_id == cur_team_id
                                                    then ( cur_team_id, new_score )
                                                    else ( cur_team_id, old_score )
      in ( { model | team_score = List.map update_score model.team_score }
         , Effects.none )
    UpdatePlayerName new_name      -> ( { model | player_name = new_name }
                                      , Effects.none )
    UpdateScore new_score          -> ( { model | score = new_score }
                                      , Effects.none )
    UpdateKills number             -> ( { model | kills = number }
                                      , Effects.none )
    UpdateDeaths number            -> ( { model | deaths = number }
                                      , Effects.none )
    UpdateFetchUrl url -> ( { model | update_url = url }
                          , Effects.none )
    UpdateStats update_action ->
      case update_action of
        StatsUpdater.FetchStats -> ( model
                                   , Effects.map UpdateStats ( StatsUpdater.getStats model.update_url )
                                   )
        StatsUpdater.NewStats maybeStats ->
          case maybeStats of
            Nothing    -> ( model, Effects.none )
            Just stats -> ( { model | score  = stats.score
                                    , kills  = stats.kills
                                    , deaths = stats.deaths
                                    , team_score = stats.team_scores
                                    }
                          , Effects.none )


-- View
------------------------------------------------------------
view : Address Action -> Model -> Html
view address model =
  let
    outer_container = Html.div [ Html.Attributes.class "col-xs-6 col-sm-6 col-md-6 col-lg-6 ctf_companent_side" ]
  in
    outer_container [ Html.div [ Html.Attributes.class "row ctf_stats-teams" ]
                               [ viewTeamScores model.team model.team_score ]
                    , Html.div [ Html.Attributes.class "row ctf_stats-values" ]
                               [ viewPlayerStats model ]
                    ]


viewTeamScores : Team.ID -> List ( Team.ID, Int ) -> Html
viewTeamScores player_team scores =
  let
    class_name t =
      case t of
        Team.Red  -> "row ctf_team_red_row"
        Team.Blue -> "row ctf_team_blue_row"
        _         -> "row ctf_team_red_row"  -- FIXME
    outer_row t = Html.div [ Html.Attributes.class ( class_name t ) ]
    inner_class_1 = Html.div [ Html.Attributes.class "col-md-2" ]
    inner_class_3 = Html.div [ Html.Attributes.class "col-md-6" ]
    inner_class_2 = Html.div [ Html.Attributes.class "col-md-4" ]

    icon_player_team = Html.img [ Html.Attributes.src ( Team.iconUrl )
                                , Html.Attributes.class "ctf_team_icon"] [ ]
    team_player_icon = \t -> inner_class_1 (if ( player_team == t ) 
                                            then [ icon_player_team ]
                                            else [ ] )
    team_name = \t -> inner_class_3 [ Html.text ( Team.toStr t )]
    team_score = \s -> inner_class_2 [ Html.text ( toString s ) ]
  in
    Html.div [ Html.Attributes.class "col-md-12" ]
             ( List.map (\t -> ( outer_row ( fst t)) [ team_player_icon ( fst t )
                                                     , team_name ( fst t )
                                                     , team_score ( snd t )
                                                     ])
                        scores )

viewPlayerStats : Model -> Html
viewPlayerStats model =
  let
    outer_row = Html.div [ Html.Attributes.class "row" ]
    inner_class_4 = Html.div [ Html.Attributes.class "col-md-8" ]
    inner_class_2 = Html.div [ Html.Attributes.class "col-md-4" ]
    int_value = \i -> inner_class_2 [ Html.text (toString i ) ]

    player_name =
      outer_row [ inner_class_4 [ Html.text "Player name:" ]
                , inner_class_2 [ Html.text model.player_name]]
    player_score =
      outer_row [ inner_class_4 [ Html.text "Score:" ]
                , int_value model.score ]
    player_kills =
      outer_row [ inner_class_4 [ Html.text "Kills:" ]
                , int_value model.kills ]
    player_deaths =
      outer_row [ inner_class_4 [ Html.text "Deaths:" ]
                , int_value model.deaths ]
  in
    Html.div [ Html.Attributes.class "col-md-12" ]
             [ player_name
             , player_score
             , player_kills
             , player_deaths
             ]
