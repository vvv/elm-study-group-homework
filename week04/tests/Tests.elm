module Tests exposing (tests)

import Expect exposing (Expectation)
import Json.Decode as J
import Test exposing (Test, describe, skip, test)
import Time
import Week04
    exposing
        ( Country
        , Mottos
        , decodeDate
        , decodeMottos
        , decodeUser
        , mottos
        )


tests : Test
tests =
    describe "Week 4 homework"
        [ test "decodeUser" <|
            \_ ->
                let
                    samples =
                        [ ( "{\"name\": \"Kostia\", \"cats\": 2}"
                          , Ok { cats = Just 2, name = "Kostia" }
                          )
                        , ( "{\"name\": \"Droopy The Dog\", \"cats\": null}"
                          , Ok { cats = Nothing, name = "Droopy The Dog" }
                          )
                        ]

                    check ( json, result ) =
                        Expect.equal (J.decodeString decodeUser json) result
                in
                Expect.all (List.map (always << check) samples) ()

        , skip <|
            -- XXX
            test "decodeMottos" <|
                \_ ->
                    Expect.equal
                        (J.decodeString decodeMottos mottos)
                        (Ok <|
                            Mottos
                                [ Country
                                    "Germany"
                                    "Einigkeit und Recht und Freiheit"
                                    "EUR"
                                , Country
                                    "England"
                                    "God Save the Queen"
                                    "GBP"
                                , Country
                                    "France"
                                    "Liberté, Égalité, Fraternité"
                                    "EUR"
                                ]
                        )

        , test "decodeDate" <|
            \_ ->
                let
                    input =
                        "\"2018-10-01T12:48:00.000Z\""

                    expected =
                        Ok (Time.millisToPosix 1538398080000)
                in
                Expect.equal (J.decodeString decodeDate input) expected
        ]
