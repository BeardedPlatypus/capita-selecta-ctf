module GalacticWar.QRModule.QRResult exposing ( .. )


-- Import
--------------------------------------------------------------------------------
-- Import Libraries
-------------------
import Html exposing ( Html )
import Html.Attributes
import Html.App as App

-- Import Modules
-----------------
import GalacticWar.Util as Util
import GalacticWar.QRModule.InteractButton as InteractButton

import GalacticWar.PlayerInteraction as PlayerInteraction


-- Model
--------------------------------------------------------------------------------
type alias Model = { last_result : String
                   , current_status : QRStatus
                   , current_n_polls : Int
                   , reset_n_polls : Int
                   , interact_button : InteractButton.Model
                   }


init : Int -> ( Model, Cmd Msg )
init reset_n =
  let
    ( button_model, button_cmd ) = InteractButton.init
  in
    ( { last_result = ""
      , current_status = NoData
      , current_n_polls = 0
      , reset_n_polls = reset_n
      , interact_button = button_model
      }
    , Cmd.map toGenButton button_cmd )


type QRStatus = NoData
              | Scanning
              | QRCodeFound String
              | QRError     String

statusToStr : QRStatus -> String
statusToStr status =
  case status of
    NoData        -> "No Data"
    Scanning      -> "Scanning"
    QRCodeFound _ -> "QR-code found"
    QRError msg   -> "QR error"


-- Update
--------------------------------------------------------------------------------
type Msg = Interaction InteractionMsg
         | UpdateModel UpdateModelMsg
         | ChildGenMsg ChildMsg
         | UpdateChild ChildMsg

type InteractionMsg = Click PlayerInteraction.Action

type UpdateModelMsg = UpdateCurrentStatus QRStatus

type ChildMsg = ButtonMsg InteractButton.Msg


toUpdateButton : InteractButton.Msg -> Msg
toUpdateButton msg = UpdateChild ( ButtonMsg msg )

toGenButton : InteractButton.Msg -> Msg
toGenButton msg = ChildGenMsg ( ButtonMsg msg )


--------------------------------------------------------------------------------
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Interaction interaction_msg  -> updateInteraction interaction_msg  model
    UpdateModel update_model_msg -> updateModel       update_model_msg model
    ChildGenMsg child_msg        -> updateChildGenMsg child_msg        model
    UpdateChild child_msg        -> updateChild       child_msg        model


--------------------------------------------------------------------------------
updateInteraction : InteractionMsg -> Model -> ( Model, Cmd Msg )
updateInteraction msg model =
  case msg of
    Click action -> ( model, Cmd.none )


updateModel : UpdateModelMsg -> Model -> ( Model, Cmd Msg )
updateModel msg model =
  case msg of
    UpdateCurrentStatus new_status ->
      case new_status of
        Scanning ->
          let
            new_n_polls = model.current_n_polls + 1
            last_result = if new_n_polls == model.reset_n_polls then
                            ""
                          else
                            model.last_result

            update_cmd = if new_n_polls == model.reset_n_polls then
                           (( InteractButton.UpdateModel ( InteractButton.UpdateAction PlayerInteraction.NoAction ))
                           |> toUpdateButton )
                           |> Util.toCmd
                         else
                           Cmd.none 
          in
            ( { model | last_result = last_result
                      , current_status = new_status
                      , current_n_polls = new_n_polls
              }
            , update_cmd )
        QRCodeFound new_result ->
          let
            action = PlayerInteraction.resultToAction new_result
            update_cmd = if action == PlayerInteraction.NoAction then
                           Cmd.none
                         else
                           (( InteractButton.UpdateModel ( InteractButton.UpdateAction action ))
                           |> toUpdateButton )
                           |> Util.toCmd
            new_current_n_polls = if action == PlayerInteraction.NoAction then
                                    model.current_n_polls
                                  else
                                    0
          in
            ( { model | last_result = new_result
              , current_status = new_status
              , current_n_polls = new_current_n_polls
              }
            , update_cmd )
        _ -> ( { model | current_status = new_status }, Cmd.none )



--------------------------------------------------------------------------------
updateChildGenMsg : ChildMsg -> Model -> ( Model, Cmd Msg )
updateChildGenMsg msg model =
  case msg of
    ButtonMsg button_msg ->
      let
        child_cmd = Util.toCmd ( toUpdateButton button_msg )
        additional_cmds =
          case button_msg of
            InteractButton.Interaction interaction_msg ->
              case interaction_msg of
                InteractButton.Click action ->
                  [ Util.toCmd ( Interaction ( Click action )) ]
            _ -> [ ]
      in
        ( model, Cmd.batch ( [ child_cmd ] ++ additional_cmds ))


--------------------------------------------------------------------------------
updateChild : ChildMsg -> Model -> ( Model, Cmd Msg )
updateChild msg model =
  case msg of
    ButtonMsg button_msg ->
      let
        ( updated_button, button_cmd ) = InteractButton.update button_msg model.interact_button
      in
        ( { model | interact_button = updated_button }
        , Cmd.map toGenButton button_cmd )


-- View
--------------------------------------------------------------------------------
view : Model -> Html Msg
view model =
  let
    outer_container = Html.div [ Html.Attributes.class "col-xs-12 col-sm-12 col-md-12 col-lg-12" ]
    row = Html.div [ Html.Attributes.class "row" ]

    container_4 = Html.div [ Html.Attributes.class "col-xs-4 col-sm-4 col-md-4 col-lg-4" ]
    container_8 = Html.div [ Html.Attributes.class "col-xs-8 col-sm-8 col-md-8 col-lg-8" ]

    last_result = row [ container_4 [ Html.text "Last Result:" ]
                      , container_8 [ Html.text model.last_result ]
                      ]
    current_status = row [ container_4 [ Html.text "Current Status" ]
                         , container_8 [ Html.text ( statusToStr model.current_status )]
                         ]

    error_msg_text =
      case model.current_status of
        QRError error -> Html.text error
        _             -> Html.text "None"

    error_msg = row [ container_4 [ Html.text "Error: Msg" ]
                    , container_8 [ error_msg_text ]
                    ]
    n_scans = row [ container_4 [ Html.text "n polls:"]
                  , container_8 [ Html.text ( toString model.current_n_polls )]]

    view_button = row [ viewButton model.interact_button ]
  in
    outer_container [ last_result
                    , current_status
                    , error_msg
                    , n_scans
                    , view_button
                    ]


viewButton : InteractButton.Model -> Html Msg
viewButton model =
  App.map toGenButton ( InteractButton.view model )
