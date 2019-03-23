module Crud exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes as A
import Html.Events exposing (onClick, onInput)
import List.Zipper as Z


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
    { db : Z.Zipper Person
    , filter : String
    , entry : Person
    }


init : Model
init =
    { db =
        Z.Zipper
            (List.reverse
                [ Person "Hans" "Emil"
                , Person "Max" "Mustermann"
                ]
            )
            (Person "Roman" "Tisch")
            []
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
            Debug.todo "XXX IMPLEMENTME"

        Update ->
            Debug.todo "XXX IMPLEMENTME"

        Delete ->
            let
                db =
                    model.db
            in
            { model | db = Z.withDefault (Person "XXX" "XXX") <| Z.fromList (Z.before db ++ Z.after db) }

        Select str ->
            case Z.findFirst (\x -> x == personFromString str) model.db of
                Just z ->
                    { model | db = z }

                Nothing ->
                    let
                        _ =
                            Debug.log
                                ("Impossible happened! " ++ "(str, model.db)")
                                ( str, model.db )
                    in
                    model

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


personToOption : Bool -> Person -> Html Msg
personToOption selected person =
    let
        str =
            personToString person
    in
    option [ A.value str, A.selected selected ] [ text str ]


view : Model -> Html Msg
view model =
    let
        selectOptions =
            List.concat
                [ List.map (personToOption False) (Z.before model.db)
                , [ personToOption True <| Z.current model.db ]
                , List.map (personToOption False) (Z.after model.db)
                ]
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
        , div [] [ select [ A.size 6, onInput Select ] selectOptions ]
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
            [ button [ onClick Create ] [ text "Create" ]
            , button [ onClick Update ] [ text "Update" ]
            , button [ onClick Delete ] [ text "Delete" ]
            ]
        ]
