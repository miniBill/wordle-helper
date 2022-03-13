module Main exposing (main)

import Browser
import Element.WithContext as Element exposing (alignTop, fill, height, text, width)
import Element.WithContext.Font as Font
import Element.WithContext.Input as Input
import List.Extra
import Maybe.Extra
import Theme exposing (Context, Element)


type alias Flags =
    ()


type alias Model =
    { context : Context
    , groups : List Group
    }


type alias Group =
    List String


type Msg
    = Edit Int Int String


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
      , groups = []
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Edit groupIndex lineIndex line ->
            let
                updateGroup g =
                    if lineIndex == List.length g then
                        g ++ [ line ]

                    else
                        List.Extra.setAt lineIndex line g

                newGroups =
                    if groupIndex == List.length model.groups then
                        model.groups ++ [ [ line ] ]

                    else
                        List.Extra.updateAt groupIndex updateGroup model.groups
            in
            ( clean { model | groups = newGroups }, Cmd.none )


clean : Model -> Model
clean model =
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
                        |> List.concatMap
                            (\s ->
                                case String.toList s of
                                    [ c, '?' ] ->
                                        List.range 0 4
                                            |> List.map
                                                (\i ->
                                                    String.repeat i "_" ++ String.fromChar c ++ String.repeat (4 - i) "_"
                                                )

                                    [ c, '-', d ] ->
                                        case String.toInt (String.fromChar d) of
                                            Nothing ->
                                                [ s ]

                                            Just di ->
                                                List.range 0 4
                                                    |> List.filterMap
                                                        (\i ->
                                                            if i == di then
                                                                Nothing

                                                            else
                                                                Just <| String.repeat i "_" ++ String.fromChar c ++ String.repeat (4 - i) "_"
                                                        )

                                    [ c, d ] ->
                                        case String.toInt (String.fromChar d) of
                                            Nothing ->
                                                [ s ]

                                            Just i ->
                                                [ String.repeat i "_" ++ String.fromChar c ++ String.repeat (4 - i) "_" ]

                                    _ ->
                                        [ s ]
                            )
                        |> Just
    in
    { model | groups = List.filterMap cleanGroup model.groups }


view : Model -> Element Msg
view { groups } =
    let
        groupViews =
            (groups ++ [ [] ])
                |> List.indexedMap viewGroup
    in
    Theme.column [ Theme.padding ] <|
        Theme.wrappedRow [ Theme.padding, width fill ] groupViews
            :: viewResults groups


viewResults : List Group -> List (Element Msg)
viewResults groups =
    groups
        |> List.foldl resultGroupStep [ List.repeat 5 '_' ]
        |> List.sort
        |> List.Extra.unique
        |> List.map viewResult


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

                    _ ->
                        Nothing
            )


viewResult : List Char -> Element msg
viewResult result =
    text <| String.fromList result


viewGroup : Int -> Group -> Element Msg
viewGroup groupIndex group =
    (group ++ [ "" ])
        |> List.indexedMap (viewLine groupIndex)
        |> Theme.column [ Theme.border, Theme.padding, width fill, alignTop ]


viewLine : Int -> Int -> String -> Element Msg
viewLine groupIndex lineIndex line =
    Input.text [ width fill ]
        { text = line
        , onChange = Edit groupIndex lineIndex
        , label = Input.labelHidden ""
        , placeholder = Nothing
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
