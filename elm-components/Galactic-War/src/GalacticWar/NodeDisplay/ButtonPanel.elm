module GalacticWar.NodeDisplay.ButtonPanel exposing ( Model, init
                                                    , Msg ( Interaction, UpdateModel, UpdateChild )
                                                    , InteractionMsg ( ClickButton )
                                                    , UpdateModelMsg ( Activate, Deactivate, ChangeActive )
                                                    , UpdateChildMsg ( UpdateButton )
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
import GalacticWar.Class as Class
import GalacticWar.NodeDisplay.Button as ClassButton

import GalacticWar.Util as Util


-- Model
--------------------------------------------------------------------------------
type alias Model = { buttons : List ClassButton.Model
                   , current_active : Maybe Class.ID
                   }


-- FIXME : add support for selecting an initial class
init : Class.ID -> ( Model, Cmd Msg )
init id =
  ( { buttons = [ fst ( ClassButton.init Class.R )
                , fst ( ClassButton.init Class.P )
                , fst ( ClassButton.init Class.S )
                ]
    , current_active = Just id
    }
  , Cmd.none )


-- Update
--------------------------------------------------------------------------------
type Msg = Interaction InteractionMsg
         | UpdateModel UpdateModelMsg
         | UpdateChild UpdateChildMsg


type InteractionMsg = ClickButton Class.ID

type UpdateModelMsg = Activate
                    | Deactivate
                    | ChangeActive Class.ID

type UpdateChildMsg = UpdateButton Class.ID ClassButton.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Interaction interaction_msg  -> updateInteraction interaction_msg model
    UpdateModel update_model_msg -> updateModel update_model_msg model
    UpdateChild update_child_msg -> updateChild update_child_msg model


--------------------------------------------------------------------------------
toUpdateButtonCmd : Class.ID -> ClassButton.Msg -> Cmd Msg
toUpdateButtonCmd id msg = Util.toCmd ( UpdateChild ( UpdateButton id msg ))


--------------------------------------------------------------------------------
updateInteraction : InteractionMsg -> Model -> ( Model, Cmd Msg )
updateInteraction msg model =
  case msg of
    ClickButton id ->
      ( model
      , toUpdateButtonCmd id ( ClassButton.Interaction ClassButton.Click )
      )


--------------------------------------------------------------------------------
updateModel : UpdateModelMsg -> Model -> ( Model, Cmd Msg )
updateModel msg model =
  case msg of
    Activate ->
      let
        toCmd button_model =
          toUpdateButtonCmd button_model.class
                            ( ClassButton.UpdateModel ( ClassButton.UpdateIsDisabled False ))
      in
        ( model, Cmd.batch ( List.map toCmd model.buttons ))
    Deactivate ->
      let
        toCmd button_model =
          toUpdateButtonCmd button_model.class
                            ( ClassButton.UpdateModel ( ClassButton.UpdateIsDisabled True ))
      in
        ( model, Cmd.batch ( List.map toCmd model.buttons ))
    ChangeActive new_active_id ->
      case model.current_active of
        Just current_id ->
          ( { model | current_active = Just new_active_id }
          , Cmd.batch [ toUpdateButtonCmd new_active_id
                          ( ClassButton.UpdateModel ( ClassButton.UpdateIsDefault True ))
                      , toUpdateButtonCmd current_id
                          ( ClassButton.UpdateModel ( ClassButton.UpdateIsDefault False))
                      ]
          )
        Nothing ->
          ( { model | current_active = Just new_active_id }
          , toUpdateButtonCmd new_active_id
              ( ClassButton.UpdateModel ( ClassButton.UpdateIsDefault True ))
          )


--------------------------------------------------------------------------------
updateChild : UpdateChildMsg -> Model -> ( Model, Cmd Msg )
updateChild msg model =
  case msg of
    UpdateButton class_id button_msg ->
      let
        condUpdateButton button =
          if button.class == class_id then updateButton button button_msg
                                      else ( button, Cmd.none )
        ( updated_buttons, cmds ) = ( List.map condUpdateButton model.buttons )
                                    |> List.unzip
      in
        ( { model | buttons = updated_buttons }
        , Cmd.batch cmds )


updateButton : ClassButton.Model -> ClassButton.Msg -> ( ClassButton.Model, Cmd Msg )
updateButton model msg =
  let
    ( updated_model, button_cmd ) = ClassButton.update msg model
    mapMsg button_msg = UpdateChild ( UpdateButton model.class button_msg )
  in
    ( updated_model
    , Cmd.map mapMsg button_cmd
    )


-- Subscriptions
--------------------------------------------------------------------------------
subscriptions : Model -> Sub Msg
subscriptions model = Sub.none


-- View
--------------------------------------------------------------------------------
view : Model -> Html Msg
view model =
  let
    viewButtonAsRow button = Html.div [ Html.Attributes.class "row" ] [ viewButton button ]
  in
    Html.div [ Html.Attributes.class "container" ]
             ( List.map viewButtonAsRow model.buttons )


-- FIXME rewrite this to something more readable
viewButton : ClassButton.Model -> Html Msg
viewButton model =
  let
    button_div = Html.div [ Html.Attributes.class "col-xs-2 col-sm-2 col-md-2 col-lg-2" ]
    convertMsg msg =
      case msg of
        ClassButton.Interaction interaction_msg ->
          case interaction_msg of
            ClassButton.Click -> Interaction ( ClickButton model.class )
        _                                       ->
          UpdateChild ( UpdateButton model.class msg )
  in
    button_div [ ( App.map convertMsg ( ClassButton.view model )) ]
