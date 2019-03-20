module Tests exposing (tests)

import Expect exposing (Expectation)
import Json.Decode as J
import Test exposing (Test, describe, skip, test)
import Time
import Week04
    exposing
        ( AccountInfo
        , Country
        , Mottos
        , decodeAccountInfo
        , decodeDate
        , decodeMottos
        , decodeUser
        , jsonPair
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

        , skip <| -- XXX
            test "decodeMottos"
            <|
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

        , test "decodeAccountInfo" <|
            \_ ->
                let
                    input =
                        """
                        {
                            "id": 14,
                            "email": "bilbo@shire.co.uk",
                            "full_name": "Bilbo Baggins",
                            "phone_number": null,
                            "info_complete": false
                        }
                        """

                    expected =
                        Ok <|
                            AccountInfo 14
                                "bilbo@shire.co.uk"
                                (Just "Bilbo Baggins")
                                Nothing
                                False
                in
                Expect.equal (J.decodeString decodeAccountInfo input) expected

        , test "jsonPair" <|
            \_ ->
                let
                    input =
                        "[\"abc\", 123]"

                    expected =
                        Ok ( "abc", 123 )

                    decode =
                        jsonPair J.string J.int
                in
                Expect.equal (J.decodeString decode input) expected
        ]
