module Week01 exposing
  ( elementAt
  , myButLast
  , myLast
  )

myLast : List a -> Maybe a
myLast xs =
    -- List.head (List.reverse xs)
    case xs of
        [] ->
            Nothing
        y :: [] ->
            Just y
        _ :: ys ->
            myLast ys


myButLast : List a -> Maybe a
myButLast xs =
    -- List.head (List.drop 1 (List.reverse xs))
    case xs of
        [] ->
            Nothing
        y :: [] ->
            Nothing
        y :: _ :: [] ->
            Just y
        _ :: ys ->
            myButLast ys


elementAt : List a -> Int -> Maybe a
elementAt xs i =
    if i < 0
    then Nothing
    else List.head (List.drop i xs)
    -- if i < 2 then
    --     case xs of
    --         [] ->
    --             Nothing
    --         y :: _ ->
    --             Just y
    -- else
    --     case xs of
    --         [] ->
    --             Nothing
    --         y :: ys ->
    --             elementAt ys (n-1)


{--}
myLength : List a -> number
myLength lst =
    let go n lst1 =
            case lst1 of
                []      -> n
                _ :: xs -> go (n+1) xs
    in go 0 lst


myReverse : List a -> List a
myReverse lst =
    let go acc lst1 =
            case lst1 of
                []      -> acc
                x :: xs -> go (x :: acc) xs
    in go [] lst


isPalindrome : List a -> Bool
isPalindrome lst =
    List.reverse lst == lst


compress : String -> String
compress =
    applyToStr compressL


dropEvery : String -> number -> String
dropEvery s n =
    applyToStr (dropEveryL n) s


clap : String -> String
clap =
    -- String.concat << List.intersperse " 👏 " << String.words
    String.join " 👏 " << String.words
    -- let go acc words =
    --         case words of
    --             []        -> String.concat (List.reverse acc)
    --             w :: rest -> go (w :: " 👏 " :: acc) rest
    -- in case (String.words s) of
    --        []        -> ""
    --        w :: rest -> w ++ go [] rest


----------------------------------------------------------------------
-- Helper functions

applyToStr : (List Char -> List Char) -> String -> String
applyToStr f =
    -- String.fromList (f (String.toList s))
    String.fromList << f << String.toList


compressL : List a -> List a
compressL lst =
    case lst of
        []        -> []
        (x :: xs) ->
            let go acc lst1 =
                    case lst1 of
                        []      -> List.reverse acc
                        y :: ys -> if Just y == List.head acc
                                   then go acc ys
                                   else go (y :: acc) ys
            in go [x] xs


dropEveryL : number -> List a -> List a
dropEveryL n lst =
    let go i acc lst1 =
            case lst1 of
                []      -> List.reverse acc
                x :: xs -> if i == n
                           then go 1 acc xs
                           else go (i+1) (x :: acc) xs
    in go 1 [] lst
