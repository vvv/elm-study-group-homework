-- Borrowed from https://gitlab.com/k-bx/elm-study-group-homework/


module Timer exposing (main)

import Browser
import Html exposing (Html, button, div, input, text)
import Html.Attributes as A
    exposing
        ( attribute
        , class
        , max
        , min
        , style
        , type_
        , value
        )
import Html.Events exposing (onClick, onInput)
import Task
import Time


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



----------------------------------------------------------------------
-- MODEL


type alias Model =
    { started : Maybe Time.Posix
    , now : Maybe Time.Posix
    , duration : Int
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { started = Nothing
      , now = Nothing
      , duration = 10
      }
    , Task.perform StartTimer Time.now
    )



----------------------------------------------------------------------
-- UPDATE


type Msg
    = StartTimer Time.Posix
    | TimeUpdate Time.Posix
    | ResetTimer
    | SetDuration String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartTimer t ->
            ( { model | started = Just t }, Cmd.none )

        TimeUpdate t ->
            ( { model | now = Just t }, Cmd.none )

        ResetTimer ->
            ( { model | started = Nothing, now = Nothing }
            , Task.perform StartTimer Time.now
            )

        SetDuration s ->
            case String.toInt s of
                Just n ->
                    ( { model | duration = n }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )



----------------------------------------------------------------------
-- VIEW


percentPassed : Model -> Int
percentPassed model =
    case ( model.started, model.now ) of
        ( Just started, Just now ) ->
            let
                elapsed =
                    Time.posixToMillis now - Time.posixToMillis started
            in
            100 * elapsed // (model.duration * 1000)

        _ ->
            0


progressBar : Model -> Html Msg
progressBar model =
    let
        percent =
            String.fromInt (percentPassed model)
    in
    div [ class "progress" ]
        [ div
            [ class "progress-bar"
            , attribute "role" "progressbar"
            , style "width" (percent ++ "%")
            , attribute "aria-valuenow" percent
            , attribute "aria-valuemin" "0"
            , attribute "aria-valuemax" "100"
            ]
            []
        ]


view : Model -> Html Msg
view model =
    div [ class "container mt-4" ]
        [ div [ style "max-width" "20em" ]
            [ text "Elapsed time:"
            , progressBar model
            ]
        , div [] [ text (String.fromInt model.duration ++ "s") ]
        , div []
            [ text "Duration:"
            , input
                [ type_ "range"
                , A.min "1"
                , A.max "60"
                , value (String.fromInt model.duration)
                , onInput SetDuration
                ]
                []
            ]
        , div [] [ button [ onClick ResetTimer ] [ text "Reset" ] ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 200 TimeUpdate
