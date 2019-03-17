module Tests exposing (suite)

import Expect exposing (Expectation)
import Json.Decode as J
import Test exposing (Test, describe, skip, test)
import Week04 exposing (Country, Mottos, decodeMottos, decodeUser, mottos)


suite : Test
suite =
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

        , skip <|  -- XXX
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
        ]
