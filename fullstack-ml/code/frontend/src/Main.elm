module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (srcdoc, style, height, width)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Time as Time

-- MODEL
type alias Model =
    { fmt : Maybe String
    , inputData : String
    , tickCount : Int
    , outputData : Maybe String
    , knownFormats : Maybe (List String)
    , hasChanges : Bool
    }

mkModel : Model
mkModel =
    { fmt = Just "markdown"
    , inputData = ""
    , tickCount = 0
    , outputData = Nothing
    , knownFormats = Nothing
    , hasChanges = False
    }

 -- UPDATE
type Msg
    = SelectFormat String
    | UpdateContent String
    | Tick Time.Posix
    | FormatUpdateMsg (Result Http.Error (List String))
    | PreviewUpdateMsg (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectFormat f ->
            ( { model | fmt = Just f, hasChanges = True }
            , updatePreview model.inputData f
            )

        UpdateContent newContent ->
            ( { model | tickCount = 0, inputData = newContent, hasChanges = True }
            , Cmd.none
            )

        Tick t ->
            let newCnt = model.tickCount + 1
                f = if newCnt > 1 && (String.isEmpty model.inputData == False) && model.hasChanges
                    then updatePreview model.inputData (Maybe.withDefault "markdown" model.fmt)
                    else Cmd.none
            in ({model | tickCount = newCnt }, f)
        FormatUpdateMsg (Err error) ->
            ({model | knownFormats = Nothing }, fetchKnownFormats)
        FormatUpdateMsg (Ok lst) ->
            ({model | knownFormats = Just lst }, Cmd.none)
        PreviewUpdateMsg (Err error) ->
            let msg1 = Just (stringifyErr error)
            in
            ({model | tickCount = 0, hasChanges = False, outputData = msg1}
            , Cmd.none
            )
        PreviewUpdateMsg (Ok innerHTML) ->
            ({model | tickCount = 0, outputData = Just innerHTML, hasChanges = False}
            , Cmd.none
            )

stringifyErr : Http.Error -> String
stringifyErr e =
    case e of
        Http.BadUrl s       -> "Bad URL" ++ s
        Http.Timeout        -> "Request Timeout"
        Http.NetworkError   -> "Network Error"
        Http.BadStatus _    -> "Bad Status"
        Http.BadPayload _ _ -> "Bad Payload"

updatePreview : String -> String -> Cmd Msg
updatePreview s fmt =
    newRenderRequest s fmt |> Http.send PreviewUpdateMsg

newRenderRequest : String -> String -> Http.Request String
newRenderRequest s fmt =
    Http.request
        { method = "POST"
        , headers = []
        , url = previewURL fmt
        , body = Http.stringBody "" s
        , expect = Http.expectString
        , timeout = Nothing
        , withCredentials = False
        }

fetchKnownFormats : Cmd Msg
fetchKnownFormats =
    Http.get formatListURL (Decode.list Decode.string)
        |> Http.send FormatUpdateMsg

-- VIEW
view : Model -> Html Msg
view m = div [ style "width" "100%"
             , style "height" "100%"]
             [formatDiv m
             , inputBox
             , outputBox m
             ]

formatButton : String -> Html Msg
formatButton s =
    label [ Html.Attributes.style "padding" "20px" ]
          [ input
              [ Html.Attributes.type_ "radio"
              , Html.Attributes.name "format"
              , onClick (SelectFormat s)
              ]
              []
          , text s
          ]

formatDiv : Model -> Html Msg
formatDiv m =
    let
        fmts = m.knownFormats
        btns = List.map formatButton (Maybe.withDefault [ "markdown" ] fmts)
        errText = text "Cannot fetch list of supported formats."
        warning =
            if fmts == Nothing
            then div [] [errText]
            else div [] []
        s = [ warning ] ++ btns
    in div [style "width" "100%", style "height" "100%"] [h2 [] [text "Select Format to Preview" ], div [] s]


inputBox : Html Msg
inputBox =
    div []
        [ h2 [] [ text "Input" ]
        , textarea [ onInput UpdateContent ] []
        ]


outputBox : Model -> Html Msg
outputBox m =
    let innerHTML = Maybe.withDefault "" m.outputData
    in div [style "width" "100%", style "height" "100%"]
        [ div [] [ h2 [] [ text "Preview" ] ]
        , iframe [style "height" "800px", style "width" "80%",  srcdoc innerHTML ] []
        ]

subscriptions : Model -> Sub Msg
subscriptions model = Time.every 1.0 Tick

init : () -> ( Model, Cmd Msg )
init = const (mkModel, fetchKnownFormats)

main =
  Browser.element { init = init
                  , view = view
                  , update = update
                  , subscriptions = subscriptions
                  }

-- UTIL
formatListURL : String
formatListURL = "/supportedformats"

previewURL : String -> String
previewURL fmt = "/html?format=" ++fmt

defaultFormatList : List String
defaultFormatList = [ "markdown" ]

const : a -> b -> a
const val = \_ -> val
