module InverseRegex exposing (main)

import FastDict
import FastSet as Set exposing (Set)
import Html exposing (Html)


type Regex
    = Concat (List Regex)
    | Class (List Char)
    | Alternative (List Regex)
    | Char Char


type alias Complete =
    Bool


all : List Char
all =
    [ 'e', 'a', 'n', 'h', 'q', 'j', 'z', 'x', 'b' ]


last : List Char
last =
    [ 'a', 'h', 'q', 'j', 'z', 'x', 'b' ]


today : Regex
today =
    Alternative
        [ Concat [ Char 'a', Class all, Char 'n', Char 'e', Class last ]
        , Concat [ Class all, Char 'a', Char 'n', Char 'e', Class last ]
        , Concat [ Class all, Class all, Char 'n', Char 'e', Char 'a' ]
        ]


generate : Int -> Regex -> List String
generate budget regex =
    let
        ( set, complete ) =
            generateHelp budget regex
    in
    if complete then
        Set.toList set

    else
        Set.toList set ++ [ "..." ]


generateHelp : Int -> Regex -> ( Set String, Complete )
generateHelp budget regex =
    if budget <= 0 then
        ( Set.empty, False )

    else
        case regex of
            Char c ->
                ( Set.singleton (String.fromChar c), True )

            Class c ->
                let
                    set : Set String
                    set =
                        c
                            |> List.map String.fromChar
                            |> Set.fromList
                in
                setWithBudget budget set

            Concat c ->
                concat budget c

            Alternative c ->
                alternative budget c


setWithBudget : Int -> Set comparable -> ( Set comparable, Complete )
setWithBudget budget set =
    if budget < Set.size set then
        ( Set.stoppableFoldl
            (\v ( a, b ) ->
                if b <= 0 then
                    FastDict.Stop ( a, b )

                else
                    FastDict.Continue ( Set.insert v a, b - 1 )
            )
            ( Set.empty, budget )
            set
            |> Tuple.first
        , False
        )

    else
        ( set, True )


alternative : Int -> List Regex -> ( Set String, Complete )
alternative b c =
    let
        go budget queue acc complete =
            case queue of
                [] ->
                    ( acc, complete )

                head :: tail ->
                    if budget <= 0 then
                        ( acc, False )

                    else
                        let
                            ( headSet, headComplete ) =
                                generateHelp budget head
                        in
                        go (budget - Set.size headSet)
                            tail
                            (Set.union acc headSet)
                            (complete && headComplete)
    in
    go b c Set.empty True


concat : Int -> List Regex -> ( Set String, Complete )
concat budget c =
    let
        go queue acc complete =
            case queue of
                [] ->
                    ( acc, complete )

                head :: tail ->
                    let
                        childBudget =
                            (budget + Set.size acc - 1) // Set.size acc

                        ( childSet, childComplete ) =
                            generateHelp childBudget head
                    in
                    if Set.isEmpty childSet && childComplete then
                        ( Set.empty, True )

                    else
                        let
                            ( nextSet, nextComplete ) =
                                Set.stoppableFoldl
                                    (\accElement ( midAcc, midComplete ) ->
                                        if Set.size midAcc >= budget then
                                            FastDict.Stop ( midAcc, False )

                                        else
                                            FastDict.Continue
                                                (Set.stoppableFoldl
                                                    (\childElement ( innerAcc, innerComplete ) ->
                                                        if Set.size innerAcc >= budget then
                                                            FastDict.Stop ( innerAcc, False )

                                                        else
                                                            FastDict.Continue
                                                                ( Set.insert (accElement ++ childElement) innerAcc
                                                                , innerComplete
                                                                )
                                                    )
                                                    ( midAcc, midComplete )
                                                    childSet
                                                )
                                    )
                                    ( Set.empty, complete )
                                    acc
                        in
                        go tail nextSet nextComplete
    in
    go c (Set.singleton "") True


main : Html msg
main =
    today
        |> generate 1000
        |> List.map (\line -> Html.li [] [ Html.text line ])
        |> Html.ul []
