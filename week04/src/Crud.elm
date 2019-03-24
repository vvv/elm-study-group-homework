module Crud exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes as A
import Html.Events exposing (onClick, onInput)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



----------------------------------------------------------------------
-- MODEL


type alias Person =
    { name : String
    , surname : String
    }


type alias Model =
    { db : List Person
    , selected : Maybe Person
    , filter : String
    , entry : Person
    }


init : Model
init =
    let
        roma =
            Person "Roman" "Tisch"
    in
    { db =
        [ Person "Hans" "Emil"
        , Person "Max" "Mustermann"
        , roma
        ]
    , selected = Just roma
    , filter = ""
    , entry = Person "John" "Romba"
    }



----------------------------------------------------------------------
-- UPDATE


type Msg
    = Create
    | Update
    | Delete
    | Select String
    | SetFilter String
    | SetName String
    | SetSurname String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Create ->
            let
                db_ =
                    model.db ++ [ model.entry ]
            in
            if personIsEmpty model.entry then
                model
            else
                { model | db = db_, entry = Person "" "" }

        Update ->
            Debug.todo "XXX IMPLEMENTME"

        Delete ->
            case model.selected of
                Nothing ->
                    model

                Just selected ->
                    let
                        db_ =
                            List.filter (\x -> x /= selected) model.db
                    in
                    { model | db = db_, selected = Nothing }

        Select str ->
            { model | selected = Just (personFromString str) }

        SetFilter str ->
            { model | filter = str }

        SetName str ->
            let
                entry =
                    model.entry
            in
            { model | entry = { entry | name = str } }

        SetSurname str ->
            let
                entry =
                    model.entry
            in
            { model | entry = { entry | surname = str } }



----------------------------------------------------------------------
-- VIEW


personToString : Person -> String
personToString person =
    person.surname ++ ", " ++ person.name


personFromString : String -> Person
personFromString str =
    case String.split ", " str of
        surname :: rest ->
            { name = String.concat rest
            , surname = surname
            }

        [] ->
            Person "" ""

personIsEmpty : Person -> Bool
personIsEmpty person =
    String.isEmpty person.name && String.isEmpty person.surname


isNothing : Maybe a -> Bool
isNothing m =
    case m of
        Nothing ->
            True

        Just _ ->
            False


view : Model -> Html Msg
view model =
    let
        moption : Person -> Maybe (Html Msg)
        moption person =
            let
                str =
                    personToString person
            in
            if String.startsWith model.filter str then
                Just <|
                    option
                        [ A.value str
                        , A.selected (Just person == model.selected)
                        ]
                        [ text str ]

            else
                Nothing

        nothingSelected =
            isNothing model.selected
    in
    div []
        [ div []
            [ text "Filter prefix: "
            , input
                [ A.value model.filter
                , onInput SetFilter
                ]
                []
            ]
        , div []
            [ select
                [ A.size 6, onInput Select ]
                (List.filterMap moption model.db)
            ]
        , div []
            [ text "Name: "
            , input
                [ A.value model.entry.name
                , onInput SetName
                ]
                []
            ]
        , div []
            [ text "Surname: "
            , input
                [ A.value model.entry.surname
                , onInput SetSurname
                ]
                []
            ]
        , div []
            [ button
                [ onClick Create
                , A.disabled (personIsEmpty model.entry)
                ]
                [ text "Create" ]
            , button
                [ onClick Update
                , A.disabled nothingSelected
                ]
                [ text "Update" ]
            , button
                [ onClick Delete
                , A.disabled nothingSelected
                ]
                [ text "Delete" ]
            ]
        ]
