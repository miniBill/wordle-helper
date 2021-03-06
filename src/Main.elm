module Main exposing (main)

import Browser
import Dict
import Dictionary
import Element.WithContext as Element exposing (alignTop, fill, height, rgb255, text, width)
import Element.WithContext.Background as Background
import Element.WithContext.Font as Font
import Element.WithContext.Input as Input
import List.Extra
import Maybe.Extra
import Set
import Theme exposing (Context, Element)


type alias Flags =
    ()


type alias Model =
    { context : Context
    , games : List Game
    , selectedGame : Int
    }


type alias Game =
    { groups : List Group
    }


type alias Group =
    List String


type Msg
    = Edit { group : Int, line : Int, value : String }
    | SelectGame Int
    | Compact
    | FullCompact
    | ResetThis
    | FilterImpossibleSubstrings


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view =
            \model ->
                Element.layout model.context
                    [ Theme.fontSizes.normal
                    , width fill
                    , height fill
                    , Font.family
                        [ Font.typeface "Fira Code"
                        , Font.monospace
                        ]
                    ]
                    (view model)
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { context = {}
      , games = []
      , selectedGame = 0
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newModel =
            case msg of
                Edit data ->
                    let
                        updateGroup group =
                            if data.line == List.length group then
                                group ++ [ data.value ]

                            else
                                List.Extra.setAt data.line data.value group

                        updateGame { groups } =
                            if data.group == List.length groups then
                                { groups = groups ++ [ [ data.value ] ] }

                            else
                                { groups = List.Extra.updateAt data.group updateGroup groups }

                        newGames =
                            if model.selectedGame == List.length model.games then
                                model.games ++ [ { groups = [ [ data.value ] ] } ]

                            else
                                List.Extra.updateAt model.selectedGame updateGame model.games
                    in
                    { model | games = newGames }

                Compact ->
                    { model | games = List.map compact model.games }

                FilterImpossibleSubstrings ->
                    { model | games = List.map filterImpossibleSubstrings model.games }

                FullCompact ->
                    { model | games = List.Extra.updateAt model.selectedGame fullCompact model.games }

                ResetThis ->
                    { model | games = List.Extra.setAt model.selectedGame { groups = [] } model.games }

                SelectGame index ->
                    { model | selectedGame = index }
    in
    ( let
        newGames =
            newModel.games
                |> List.map clean
                |> List.Extra.dropWhileRight (\{ groups } -> List.isEmpty groups)
      in
      { newModel
        | games = newGames
        , selectedGame =
            min
                (List.length newGames)
                newModel.selectedGame
      }
    , Cmd.none
    )


filterImpossibleSubstrings : Game -> Game
filterImpossibleSubstrings { groups } =
    { groups =
        groups
            |> List.Extra.select
            |> List.map
                (\( group, others ) ->
                    group
                        |> List.Extra.filterNot
                            (\option ->
                                List.any
                                    (\impossible ->
                                        String.contains impossible option
                                    )
                                    Dictionary.impossibleSubstrings
                                    || (getResults ([ option ] :: others)
                                            |> List.all
                                                (\result ->
                                                    List.any
                                                        (\impossible ->
                                                            String.contains impossible result
                                                        )
                                                        Dictionary.impossibleSubstrings
                                                )
                                       )
                            )
                )
    }


fullCompact : Game -> Game
fullCompact { groups } =
    { groups = [ getResults groups ] }


compact : Game -> Game
compact { groups } =
    { groups =
        groups
            |> dedup
            |> compactLetters
            |> removeImpossibles
            |> compactSingles
            |> removeUseless
    }


removeUseless : List Group -> List Group
removeUseless groups =
    groups
        |> List.sortBy List.length
        |> List.foldl
            (\e acc ->
                if getResults (e :: acc) == getResults acc then
                    acc

                else
                    e :: acc
            )
            []


dedup : List Group -> List Group
dedup =
    List.map List.Extra.unique


compactLetters : List Group -> List Group
compactLetters groups =
    groups
        |> List.foldl
            (\e ( dacc, lacc ) ->
                case singleLetter e of
                    Just s ->
                        ( Dict.update s
                            (\f ->
                                case f of
                                    Nothing ->
                                        Just e

                                    Just i ->
                                        Just <| intersectGroups e i
                            )
                            dacc
                        , lacc
                        )

                    Nothing ->
                        ( dacc, e :: lacc )
            )
            ( Dict.empty, [] )
        |> (\( dacc, lacc ) -> lacc ++ Dict.values dacc)


intersectGroups : Group -> Group -> Group
intersectGroups g1 g2 =
    Set.toList <| Set.intersect (Set.fromList g1) (Set.fromList g2)


singleLetter : Group -> Maybe Char
singleLetter group =
    let
        isSingleLetter g =
            case g |> String.toList |> List.Extra.unique |> List.Extra.filterNot ((==) '_') of
                [ s ] ->
                    Just s

                _ ->
                    Nothing

        try l ts =
            List.all (\t -> isSingleLetter t == Just l) ts
    in
    case group of
        [] ->
            Nothing

        h :: ts ->
            isSingleLetter h
                |> Maybe.andThen
                    (\l ->
                        if try l ts then
                            Just l

                        else
                            Nothing
                    )


removeImpossibles : List Group -> List Group
removeImpossibles groups =
    groups
        |> List.Extra.select
        |> List.map
            (\( group, others ) ->
                List.Extra.filterNot
                    (\option ->
                        List.isEmpty <| getResults ([ option ] :: others)
                    )
                    group
            )


compactSingles : List Group -> List Group
compactSingles groups =
    groups
        |> List.sortBy List.length
        |> List.foldl
            (\e acc ->
                case e of
                    [ _ ] ->
                        [ getResults <| e :: acc ]

                    _ ->
                        e :: acc
            )
            []


clean : Game -> Game
clean game =
    let
        cleanGroup g =
            let
                cleaned =
                    List.Extra.filterNot String.isEmpty g
            in
            case cleaned of
                [] ->
                    Nothing

                _ ->
                    cleaned
                        |> List.concatMap expandShortcuts
                        |> Just

        mask c i =
            String.repeat i "_" ++ String.fromChar c ++ String.repeat (4 - i) "_"

        expandShortcuts s =
            case String.toList s of
                [ c, '*' ] ->
                    List.map (mask c) (List.range 0 4)

                [ c, '+', d ] ->
                    case String.toInt (String.fromChar d) of
                        Nothing ->
                            [ s ]

                        Just i ->
                            [ mask c (i - 1) ]

                [ c, '-', '-', d ] ->
                    case String.toInt (String.fromChar d) of
                        Nothing ->
                            [ s ]

                        Just di ->
                            List.range 0 4
                                |> List.filterMap
                                    (\i ->
                                        if i == di - 1 then
                                            Nothing

                                        else
                                            Just <| mask c i
                                    )

                [ c, '?' ] ->
                    allPositions c

                [ c, '-', d ] ->
                    case String.toInt (String.fromChar d) of
                        Nothing ->
                            [ s ]

                        Just di ->
                            allPositions c
                                |> List.filter (\p -> String.slice (di - 1) di p == "_")

                [ c, d ] ->
                    case String.toInt (String.fromChar d) of
                        Nothing ->
                            [ s ]

                        Just di ->
                            allPositions c
                                |> List.filter (\p -> String.slice (di - 1) di p == String.fromChar c)

                _ ->
                    [ s ]
    in
    { game | groups = List.map (List.map String.toUpper) <| List.filterMap cleanGroup game.groups }


allPositions : Char -> List String
allPositions c =
    let
        mask i =
            String.repeat i "_" ++ String.fromChar c ++ String.repeat (4 - i) "_"

        range =
            List.range 0 4

        one =
            List.map mask range

        two =
            one
                |> List.concatMap
                    (\l ->
                        let
                            ls =
                                String.toList l
                        in
                        one
                            |> List.filterMap
                                (\r ->
                                    let
                                        rs =
                                            String.toList r
                                    in
                                    combine ls rs
                                )
                            |> List.map String.fromList
                    )

        three =
            two
                |> List.concatMap
                    (\l ->
                        let
                            ls =
                                String.toList l
                        in
                        one
                            |> List.filterMap
                                (\r ->
                                    let
                                        rs =
                                            String.toList r
                                    in
                                    combine ls rs
                                )
                            |> List.map String.fromList
                    )

        four =
            three
                |> List.concatMap
                    (\l ->
                        let
                            ls =
                                String.toList l
                        in
                        one
                            |> List.filterMap
                                (\r ->
                                    let
                                        rs =
                                            String.toList r
                                    in
                                    combine ls rs
                                )
                            |> List.map String.fromList
                    )
    in
    List.Extra.unique <| one ++ two ++ three ++ four ++ [ String.fromList <| List.repeat 5 c ]


view : Model -> Element Msg
view { games, selectedGame } =
    let
        gamesPlus =
            games ++ [ { groups = [] } ]
    in
    Theme.column [ Theme.padding ]
        [ gamePicker selectedGame gamesPlus
        , List.Extra.getAt selectedGame gamesPlus
            |> Maybe.map viewGame
            |> Maybe.withDefault (text "Pick a valid game")
        ]


gamePicker : Int -> List Game -> Element Msg
gamePicker selectedIndex games =
    let
        notWonColor =
            rgb255 0xC0 0xC0 0xC0

        wonColor =
            rgb255 0x00 0xFF 0x00
    in
    games
        |> List.indexedMap
            (\i game ->
                let
                    hint =
                        getResults game.groups
                            |> List.Extra.foldl1 intersect
                            |> Maybe.withDefault ""
                            |> (\s ->
                                    if s == "" || s == "_____" then
                                        ""

                                    else
                                        " " ++ s
                               )

                    baseColor =
                        case game.groups of
                            [ [ solution ] ] ->
                                if String.contains "_" solution || String.length solution < 5 then
                                    notWonColor

                                else
                                    wonColor

                            _ ->
                                notWonColor
                in
                Theme.button
                    [ Background.color <|
                        if i == selectedIndex then
                            baseColor

                        else
                            Theme.lighten baseColor
                    ]
                    { onPress = Just <| SelectGame i
                    , label = text <| String.fromInt (i + 1) ++ hint
                    }
            )
        |> Theme.wrappedRow []


intersect : String -> String -> String
intersect l r =
    let
        ls =
            String.toList l

        rs =
            String.toList r
    in
    String.fromList <|
        List.map2
            (\x y ->
                if x == y then
                    x

                else
                    '_'
            )
            ls
            rs


viewGame : Game -> Element Msg
viewGame { groups } =
    let
        groupViews =
            (groups ++ [ [] ])
                |> List.indexedMap viewGroup
    in
    Theme.column [] <|
        Theme.wrappedRow [ width fill ]
            [ Theme.button []
                { onPress = Just Compact
                , label = text "Compact"
                }
            , Theme.button []
                { onPress = Just FilterImpossibleSubstrings
                , label = text "Filter impossible substrings"
                }
            , Theme.button []
                { onPress = Just FullCompact
                , label = text "Full compact this game"
                }
            , Theme.button []
                { onPress = Just ResetThis
                , label = text "Reset this game"
                }
            ]
            :: Theme.wrappedRow [ width fill ] groupViews
            :: viewResults groups


viewResults : List Group -> List (Element Msg)
viewResults groups =
    getResults groups
        |> List.map text


getResults : List Group -> List String
getResults groups =
    groups
        |> List.foldl resultGroupStep [ List.repeat 5 '_' ]
        |> List.map String.fromList
        |> List.sort
        |> List.Extra.unique


resultGroupStep : Group -> List (List Char) -> List (List Char)
resultGroupStep group acc =
    List.concatMap (resultOptionStep acc) group


resultOptionStep : List (List Char) -> String -> List (List Char)
resultOptionStep acc line =
    let
        listed =
            String.toList line
    in
    List.filterMap (combine listed) acc


combine : List Char -> List Char -> Maybe (List Char)
combine l r =
    List.map2 Tuple.pair l r
        |> Maybe.Extra.traverse
            (\pair ->
                case pair of
                    ( '_', rc ) ->
                        Just rc

                    ( lc, '_' ) ->
                        Just lc

                    ( lc, rc ) ->
                        if lc == rc then
                            Just lc

                        else
                            Nothing
            )


viewGroup : Int -> Group -> Element Msg
viewGroup groupIndex group =
    (group ++ [ "" ])
        |> List.indexedMap (viewLine groupIndex)
        |> Theme.column [ Theme.border, Theme.padding, width fill, alignTop ]


viewLine : Int -> Int -> String -> Element Msg
viewLine groupIndex lineIndex line =
    Input.text [ width fill ]
        { text = line
        , onChange = \newValue -> Edit { group = groupIndex, line = lineIndex, value = newValue }
        , label = Input.labelHidden ""
        , placeholder = Nothing
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
