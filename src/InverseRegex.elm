module InverseRegex exposing (main)

import FastDict
import FastSet as Set exposing (Set)
import Html exposing (Html)
import List.Extra


type Regex
    = Concat (List Regex)
    | Class (List Char)
    | Alternative (List Regex)
    | Char Char


type alias Complete =
    Bool


gray : List Char
gray =
    [ 'l', 'b', 'q', 'w', 'j', 'z', 'x' ]


yellow : List Char
yellow =
    [ 'e', 'd', 'n' ]


all : List Char
all =
    yellow ++ gray


nonE : List Char
nonE =
    List.Extra.remove 'e' all


today : Regex
today =
    Alternative
        [ Concat [ Char 'd', Char 'n', Char 'e', Class nonE, Class nonE ]
        , Concat [ Char 'd', Class nonE, Char 'e', Char 'n', Class nonE ]
        , Concat [ Char 'd', Class nonE, Char 'e', Class nonE, Char 'n' ]
        , Concat [ Char 'e', Char 'd', Class ('e' :: gray), Char 'n', Class nonE ]
        , Concat [ Char 'e', Char 'd', Class ('e' :: gray), Class nonE, Char 'n' ]
        , Concat [ Char 'e', Char 'n', Class ('e' :: gray), Char 'd', Class nonE ]
        , Concat [ Char 'e', Char 'n', Class ('e' :: gray), Class nonE, Char 'd' ]
        , Concat [ Char 'e', Class nonE, Class ('e' :: gray), Char 'd', Char 'n' ]
        , Concat [ Char 'e', Class nonE, Class ('e' :: gray), Char 'n', Char 'd' ]
        , Concat [ Char 'n', Char 'd', Char 'e', Class nonE, Class nonE ]
        , Concat [ Char 'n', Class nonE, Char 'e', Char 'd', Class nonE ]
        , Concat [ Char 'n', Class nonE, Char 'e', Class nonE, Char 'd' ]
        , Concat [ Class all, Char 'd', Char 'e', Char 'n', Class nonE ]
        , Concat [ Class all, Char 'd', Char 'e', Class nonE, Char 'n' ]
        , Concat [ Class all, Char 'n', Char 'e', Char 'd', Class nonE ]
        , Concat [ Class all, Char 'n', Char 'e', Class nonE, Char 'd' ]
        , Concat [ Class all, Class nonE, Char 'e', Char 'd', Char 'n' ]
        , Concat [ Class all, Class nonE, Char 'e', Char 'n', Char 'd' ]
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
        |> generate 2000
        |> List.map (\line -> Html.li [] [ Html.text line ])
        |> Html.ul []
