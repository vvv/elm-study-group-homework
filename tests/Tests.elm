module Tests exposing (tests)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, string)
import Test exposing (..)
import Week02
    exposing
        ( buildStatsUrl
        , convert
        , convert02
        , convert03
        , mkUser
        , setPhone
        )


tests : Test
tests =
    describe "Week 2 homework"
        [ test "convert" <|
            \_ ->
                let
                    actual =
                        convert
                            [ { name = "John"
                              , email = "john@gmail.com"
                              , phone = "+3801234567"
                              }
                            ]

                    expected =
                        [ { name = "John"
                          , email = "john@gmail.com"
                          }
                        ]
                in
                Expect.equal actual expected

        , test "convert02" <|
            \_ ->
                let
                    actual =
                        convert02
                            [ { name = Just "John"
                              , email = Just "john@gmail.com"
                              }
                            , { name = Nothing
                              , email = Just "email"
                              }
                            , { name = Just "name"
                              , email = Nothing
                              }
                            , { name = Nothing
                              , email = Nothing
                              }
                            ]

                    expected =
                        [ { name = "John"
                          , email = "john@gmail.com"
                          }
                        ]
                in
                Expect.equal actual expected

        , test "convert03" <|
            \_ ->
                let
                    actual =
                        convert03
                            [ { name = Just "John"
                              , email = Just "john@gmail.com"
                              }
                            , { name = Nothing
                              , email = Just "email"
                              }
                            , { name = Just "name"
                              , email = Nothing
                              }
                            , { name = Nothing
                              , email = Nothing
                              }
                            ]

                    expected =
                        [ { name = "John"
                          , email = "john@gmail.com"
                          }
                        , { name = "<unspecified>"
                          , email = "email"
                          }
                        , { name = "name"
                          , email = "<unspecified>"
                          }
                        , { name = "<unspecified>"
                          , email = "<unspecified>"
                          }
                        ]
                in
                Expect.equal actual expected

        , fuzz2 string string "setPhone" <|
            \s t ->
                Expect.equal (setPhone s (mkUser t)) (mkUser s)

        , test "buildStatsUrl" <|
            \_ ->
                let
                    mkExpectation : (Maybe String, Maybe Int, String)
                                  -> subject -> Expectation
                    mkExpectation (mdate, mnum, suffix) =
                        \_ ->
                            Expect.equal
                                (buildStatsUrl 12 { startDate = mdate
                                                  , numElems = mnum
                                                  })
                                ("https://myapi.com/api/item/12/stats.json" ++
                                     suffix)

                    samples =
                        [ (Nothing, Nothing, "")
                        , ( Just "2019-01-01", Nothing
                          , "?start_date=2019-01-01" )
                        , ( Nothing, Just 10, "?num_items=10" )
                        , ( Just "2019-01-01", Just 10
                          , "?start_date=2019-01-01&num_items=10" )
                        ]
                in
                    Expect.all (List.map mkExpectation samples) ()
        ]

{--
suite : Test
suite =
    describe "The String module"
        [ describe "String.reverse"
            -- Nest as many descriptions as you like.
            [ test "has no effect on a palindrome" <|
                \_ ->
                    let
                        palindrome =
                            "hannah"
                    in
                    Expect.equal palindrome (String.reverse palindrome)

            -- Expect.equal is designed to be used in pipeline style, like this.
            , test "reverses a known string" <|
                \_ ->
                    "ABCDEFG"
                        |> String.reverse
                        |> Expect.equal "GFEDCBA"

            -- fuzz runs the test 100 times with randomly-generated inputs!
            , fuzz string "restores the original string if you run it again" <|
                \randomlyGeneratedString ->
                    randomlyGeneratedString
                        |> String.reverse
                        |> String.reverse
                        |> Expect.equal randomlyGeneratedString
            ]
        ]
--}
