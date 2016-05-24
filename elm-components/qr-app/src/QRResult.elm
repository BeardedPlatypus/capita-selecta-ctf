module QRResult exposing ( .. )


-- Import
--------------------------------------------------------------------------------
-- Import Libraries
-------------------
import Html exposing ( Html )
import Html.Attributes

-- Import Modules
-----------------
import Util


-- Model
--------------------------------------------------------------------------------
type alias Model = { last_result : String
                   , current_status : QRStatus
                   , current_n_polls : Int
                   , reset_n_polls : Int
                   }


init : Int -> ( Model, Cmd Msg )
init reset_n = ( { last_result = ""
                 , current_status = NoData
                 , current_n_polls = 0
                 , reset_n_polls = reset_n
                 }
               , Cmd.none )


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
type Msg = UpdateModel UpdateModelMsg

type UpdateModelMsg = UpdateCurrentStatus QRStatus


--------------------------------------------------------------------------------
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    UpdateModel update_model_msg -> updateModel update_model_msg model


--------------------------------------------------------------------------------
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
          in
            ( { model | last_result = last_result
                      , current_status = new_status
                      , current_n_polls = new_n_polls
              }
            , Cmd.none )
        QRCodeFound new_result ->
          ( { model | last_result = new_result
                    , current_status = new_status
                    , current_n_polls = 0
            }
          , Cmd.none )
        _ -> ( { model | current_status = new_status }, Cmd.none )


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
  in
    outer_container [ last_result
                    , current_status
                    , error_msg
                    , n_scans
                    ]
