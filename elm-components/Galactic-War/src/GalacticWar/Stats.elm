module GalacticWar.Stats exposing ( Model, init
                                  , Msg ( UpdateModel )
                                  , UpdateModelMsg ( UpdateTeam
                                                   , UpdateTeamScore
                                                   , UpdateName
                                                   , UpdateScore
                                                   , UpdateKills
                                                   , UpdateDeaths
                                                   )
                                  , update, subscriptions, view )


-- Import Libraries
--------------------------------------------------------------------------------
import Html exposing ( Html )
import Html.App as App
import Html.Attributes
import Html.Events
import Platform.Cmd as Cmd exposing ( Cmd )
import Platform.Sub as Sub exposing ( Sub )
import List

-- Import modules
--------------------------------------------------------------------------------
import GalacticWar.Team as Team


-- Model
--------------------------------------------------------------------------------
-- TODO fix the team_score display
type alias Model = { team : Team.ID
                   , team_scores : List ( Team.ID, Int )
                   , name : String
                   , score : Int
                   , kills : Int
                   , deaths : Int
                   }


init : ( Model, Cmd Msg )
init = ( { team = Team.Red
         , team_scores = [ ( Team.Red, 0 )
                         , ( Team.Blue, 0 )
                         ]
         , name = "Placeholder"
         , score = 0
         , kills = 0
         , deaths = 0
         }
       , Cmd.none
       )


-- Update
--------------------------------------------------------------------------------
type Msg = UpdateModel UpdateModelMsg

type UpdateModelMsg = UpdateTeam Team.ID
                    | UpdateTeamScore ( List ( Team.ID, Int ))
                    | UpdateName String
                    | UpdateScore Int
                    | UpdateKills Int
                    | UpdateDeaths Int

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    UpdateModel update_model_msg -> updateModel update_model_msg model


--------------------------------------------------------------------------------
-- FIXME implement update team score in a safer way
updateModel : UpdateModelMsg -> Model -> ( Model, Cmd Msg )
updateModel msg model =
  case msg of
    UpdateTeam team_id ->
      ( { model | team = team_id }, Cmd.none )
    UpdateTeamScore new_team_scores ->
      ( { model | team_scores = new_team_scores }, Cmd.none )
    UpdateName new_name ->
      ( { model | name = new_name }, Cmd.none )
    UpdateScore new_score ->
      ( { model | score = new_score }, Cmd.none )
    UpdateKills new_kills ->
      ( { model | kills = new_kills }, Cmd.none )
    UpdateDeaths new_deaths ->
      ( { model | deaths = new_deaths }, Cmd.none )


-- Subscriptions
--------------------------------------------------------------------------------
subscriptions : Model -> Sub Msg
subscriptions model = Sub.none


-- View
--------------------------------------------------------------------------------
view : Model -> Html Msg
view model =
  let
    outer_container =
      Html.div [ Html.Attributes.class "col-xs-6 col-sm-6 col-md-6 col-lg-6 ctf_companent_side" ]
  in
    outer_container [ Html.div [ Html.Attributes.class "row ctf_stats-teams" ]
                               [ viewTeamScores model.team model.team_scores ]
                    , Html.div [ Html.Attributes.class "row ctf_stats-values" ]
                               [ viewPlayerStats model ]
                    ]


viewTeamScores : Team.ID -> List ( Team.ID, Int ) -> Html Msg
viewTeamScores player_team team_scores =
  let
    cls t = case t of
              Team.Red  -> "row ctf_team_red row"
              Team.Blue -> "row ctf_team blue_row"
    outer_row t = Html.div [ Html.Attributes.class ( cls t ) ]
    inner_class_2 = Html.div [ Html.Attributes.class "col-md-2" ]
    inner_class_6 = Html.div [ Html.Attributes.class "col-md-6" ]
    inner_class_4 = Html.div [ Html.Attributes.class "col-md-4" ]

    icon_player_team = Html.img [ Html.Attributes.src ( Team.iconUrl )
                                , Html.Attributes.class "ctf_team_icon"
                                ] [ ]
    team_player_icon t = inner_class_2 ( if player_team == t then [ icon_player_team ]
                                                             else [ ]
                                       )
    team_name t = inner_class_6 [ Html.text ( Team.toStr t ) ]
    team_score s = inner_class_4 [ Html.text ( toString s ) ]

    toHtml ( id, score ) = outer_row id [ team_player_icon id
                                        , team_name id
                                        , team_score score
                                        ]
  in
    Html.div [ Html.Attributes.class "col-md-12" ]
             ( List.map toHtml team_scores )


viewPlayerStats : Model -> Html Msg
viewPlayerStats model =
  let
    outer_row = Html.div [ Html.Attributes.class "row" ]
    inner_class_8 = Html.div [ Html.Attributes.class "col-md-8" ]
    inner_class_4 = Html.div [ Html.Attributes.class "col-md-4" ]

    intToHtml i = inner_class_4 [ Html.text ( toString i )]

    player_name = outer_row [ inner_class_8 [ Html.text "Player name:" ]
                            , inner_class_4 [ Html.text model.name ]]
    player_score = outer_row [ inner_class_8 [ Html.text "Score:" ]
                             , intToHtml model.score ]
    player_kills = outer_row [ inner_class_8 [ Html.text "Kills:"]
                             , intToHtml model.kills ]
    player_deaths = outer_row [ inner_class_8 [ Html.text "Deaths:"]
                              , intToHtml model.deaths ]
  in
    Html.div [ Html.Attributes.class "col-md-12" ]
             [ player_name
             , player_score
             , player_kills
             , player_deaths
             ]
