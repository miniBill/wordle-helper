module Constraints exposing (..)

import Dict exposing (Dict)
import Html exposing (Html)
import List.Extra


gray : List Char
gray =
    [ 'q'
    , 'j'
    , 'k'
    , 'l'
    , 'z'
    , 'x'
    , 'v'
    , 'b'
    , 'm'
    ]


green : Dict Int Char
green =
    [ ( 0, 'p' )
    ]
        |> Dict.fromList


yellow : List ( Char, List Int )
yellow =
    [ ( 'r', [ 1 ] )
    , ( 'i', [ 1, 2 ] )
    , ( 'e', [ 4 ] )
    ]


possible : List String
possible =
    let
        greens : List Char
        greens =
            Dict.values green

        yellows : List Char
        yellows =
            List.map Tuple.first yellow

        popable : List Char
        popable =
            (greens ++ yellows ++ gray)
                |> List.Extra.unique
    in
    List.repeat 5 popable
        |> List.Extra.cartesianProduct
        |> List.filterMap
            (\cardinal ->
                if respectsGreen cardinal && respectsYellow cardinal then
                    Just (String.fromList cardinal)

                else
                    Nothing
            )
        |> List.sortBy
            (\q ->
                if List.any (\w -> String.contains w q) veryUnlikely then
                    999

                else if List.any (\w -> String.contains w q) unlikely then
                    100

                else
                    0
            )


unlikely : List String
unlikely =
    [ "aa"
    , "ae"
    , "ao"
    , "ea"
    , "ee"
    , "fd"
    , "oa"
    , "pp"
    , "rj"
    , "rq"
    , "rr"
    , "rz"
    , "tt"
    , "uu"
    , "yq"
    ]


veryUnlikely : List String
veryUnlikely =
    [ "aeo"
    , "cch"
    , "cdh"
    , "chh"
    , "crh"
    , "dm"
    , "dx"
    , "dz"
    , "eee"
    , "fdy"
    , "fgy"
    , "fj"
    , "fky"
    , "fmy"
    , "fv"
    , "fx"
    , "hch"
    , "hll"
    , "jy"
    , "kd"
    , "ldr"
    , "lll"
    , "lqr"
    , "lrq"
    , "lrv"
    , "lvr"
    , "lxr"
    , "lyq"
    , "mhl"
    , "ouo"
    , "ppr"
    , "qrq"
    , "rfd"
    , "rmv"
    , "rmx"
    , "uuu"
    , "xhl"
    , "ydy"
    , "yfy"
    , "ygy"
    , "yj"
    , "yky"
    , "yxy"
    , "yy"
    , "zvr"
    ]


respectsGreen : List Char -> Bool
respectsGreen cardinal =
    let
        go : Int -> List Char -> Bool
        go i queue =
            case queue of
                [] ->
                    True

                h :: t ->
                    case Dict.get i green of
                        Nothing ->
                            go (i + 1) t

                        Just e ->
                            if e == h then
                                go (i + 1) t

                            else
                                False
    in
    go 0 cardinal


respectsYellow : List Char -> Bool
respectsYellow cardinal =
    let
        go : Int -> List Char -> Bool
        go i queue =
            case queue of
                [] ->
                    True

                h :: t ->
                    if List.any (\( y, yis ) -> y == h && List.member i yis) yellow then
                        False

                    else
                        go (i + 1) t
    in
    hasYellows cardinal && go 0 cardinal


hasYellows : List Char -> Bool
hasYellows cardinal =
    List.all (\( y, _ ) -> List.member y cardinal) yellow


main : Html msg
main =
    (if List.length possible > 100 then
        List.take 100 possible ++ [ "..." ]

     else
        possible
    )
        |> List.map (\p -> Html.li [] [ Html.text p ])
        |> Html.ul []
