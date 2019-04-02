module CrudHttp exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes as A
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as J
import List.Zipper as Z


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
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


init : () -> ( Model, Cmd Msg )
init _ =
    ( { db = []
      , selected = Nothing
      , filter = ""
      , entry = Person "" ""
      }
    , Http.get
        { url = "http://localhost:8000/api/users/list.json"
        , expect = Http.expectJson GotList decodeUsers
        }
    )


decodeUser : J.Decoder Person
decodeUser =
    J.map2 Person
        (J.field "name" J.string)
        (J.field "surname" J.string)


decodeUsers : J.Decoder (List Person)
decodeUsers =
    J.list decodeUser


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
    | GotList (Result Http.Error (List Person))


-- XXX REVISEME: Issue HTTP requests in accordance with the desired
-- business logic.
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Create ->
            if personIsEmpty model.entry then
                ( model, Cmd.none )
            else
                let
                    db_ =
                        model.db ++ [ model.entry ]
                in
                    ( { model | db = db_, entry = Person "" "" }, Cmd.none )

        Update ->
            let
                mdb_ =
                    model.db
                        |> Z.fromList
                        |> Maybe.andThen
                            (Z.find <|
                                \x -> Just x == model.selected
                            )
                        |> Maybe.map (Z.mapCurrent <| \_ -> model.entry)
                        |> Maybe.map Z.toList
            in
            case mdb_ of
                Just db_ ->
                    ( { model | db = db_ }, Cmd.none )

                Nothing ->
                    Debug.todo "Impossible happened"

        Delete ->
            case model.selected of
                Nothing ->
                    ( model, Cmd.none )

                Just selected ->
                    let
                        db_ =
                            List.filter (\x -> x /= selected) model.db
                    in
                    ( { model | db = db_, selected = Nothing }, Cmd.none )

        Select str ->
            ( { model | selected = Just (personFromString str) }, Cmd.none )

        SetFilter str ->
            ( { model | filter = str }, Cmd.none )

        SetName str ->
            let
                entry =
                    model.entry
            in
            ( { model | entry = { entry | name = str } }, Cmd.none )

        SetSurname str ->
            let
                entry =
                    model.entry
            in
            ( { model | entry = { entry | surname = str } }, Cmd.none )

        GotList result ->
            case result of
                Ok persons ->
                    ( { model | db = persons }, Cmd.none )

                Err err ->
                    Debug.todo ("XXX-IMPLEMENTME " ++ Debug.toString err)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


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
                , A.disabled (nothingSelected || personIsEmpty model.entry)
                ]
                [ text "Update" ]
            , button
                [ onClick Delete
                , A.disabled nothingSelected
                ]
                [ text "Delete" ]
            ]
        ]
