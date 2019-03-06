module Week02 exposing
    ( buildStatsUrl
    , catMaybes
    , convert
    , convert02
    , convert03
    , mapMaybes
    , mkUser
    , setPhone
    )

import Url.Builder as Url

-- | Map one structure to another.
convert
    :  List { name : String, email : String, phone : String }
    -> List { name : String, email : String }
convert =
    List.map <|
        \r -> { name = r.name, email = r.email }


-- | Filter elements with non-empty name and email.
convert02
    :  List { name : Maybe String, email : Maybe String }
    -> List { name : String, email : String }
convert02 =
    List.filterMap <|
        \r ->
            case (r.name, r.email) of
                (Just name, Just email) ->
                    Just { name = name, email = email }
                _ ->
                    Nothing


-- | Fill in missing fields with `<unspecified>`, while removing elements.
convert03
    :  List { name : Maybe String, email : Maybe String }
    -> List { name : String, email : String }
convert03 =
    List.map <|
        let
            f =
                Maybe.withDefault "<unspecified>"
        in
        \r -> { name = f r.name, email = f r.email }


-- Rewrite bird using `<|`, then using `|>` instead of parens
-- (where applicable).

bird : Int
bird =
    let
        notThree x =
            x /= 3

        incr x =
            x + 1
    in
    List.sum (List.filter notThree (List.map incr [ 1, 2, 3 ]))


-- using <|
bird2 =
    let
        notThree x =
            x /= 3

        incr x =
            x + 1
    in
    List.sum <| List.filter notThree <| List.map incr [ 1, 2, 3 ]


-- using |>
bird3 =
    let
        notThree x =
            x /= 3

        incr x =
            x + 1
    in
    [ 1, 2, 3 ] |> List.map incr |> List.filter notThree |> List.sum


type alias User = { profile : Profile }
type alias Profile = { address : Address }
type alias Address = { phone : String }


mkUser : String -> User
mkUser s = { profile = { address = { phone = s } } }


setPhone : String -> User -> User
setPhone phone _ =
    mkUser phone


mapMaybes : (a -> Maybe b) -> List a -> List b
mapMaybes f lst =
    case lst of
        [] ->
            []

        x :: xs ->
            let
                rest =
                    mapMaybes f xs
            in
            case f x of
                Just y ->
                    y :: rest

                Nothing ->
                    rest


catMaybes : List (Maybe a) -> List a
catMaybes lst =
    case lst of
        [] ->
            []

        Just a :: rest ->
            a :: catMaybes rest

        Nothing :: rest ->
            catMaybes rest


buildStatsUrl
    : Int
    -> { startDate : Maybe String, numElems : Maybe Int }
    -> String
buildStatsUrl n rec =
    let
        mdate =
            Maybe.map (Url.string "start_date") rec.startDate

        mnum =
            Maybe.map (Url.int "num_items") rec.numElems
    in
    "https://myapi.com/api/item"
        ++ Url.absolute [ String.fromInt n, "stats.json" ]
            (catMaybes [ mdate, mnum ])


{--
**Temperature converter**

Implement "Temperature Converter" from 7GYUs as described in https://eugenkiss.github.io/7guis/tasks

**(optional) Eight Queens**

Very famous Eight Queens Problem. Please see
https://johncrane.gitbooks.io/ninety-nine-elm-problems/content/p/p90.html
for details.


andMap : Maybe a -> Maybe (a -> b) -> Maybe b
andMap =
    Maybe.map2 (|>)
--}
