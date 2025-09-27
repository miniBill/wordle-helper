module Constraints exposing (..)

import Dict exposing (Dict)
import Html exposing (Html)
import List.Extra


gray : List Char
gray =
    [ 'q'
    , 'd'
    , 'f'
    , 'j'
    , 'k'
    , 'z'
    , 'x'
    , 'v'
    ]


green : Dict Int Char
green =
    [ ( 1, 'r' )
    , ( 2, 'i' )
    , ( 3, 't' )
    ]
        |> Dict.fromList


yellow : List ( Char, List Int )
yellow =
    []


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
