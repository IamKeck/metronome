port module Main exposing (..)

import Html exposing (program)
import Html.Events
import Html.Attributes
import Time
import Result
import Task
import String
import Process


type alias Model =
    { isRunning : Bool, bpm : Int, tapList : List Float, holdDirection : Maybe BpmDirection, holdBeginTime : Maybe Float }


type BpmDirection
    = Up
    | Down


type Msg
    = Toggle
    | Tap
    | TapTime Float
    | BpmChange BpmDirection
    | BpmHold BpmDirection
    | BpmRelease
    | GotHoldBeginTime Float
    | GotHoldJudgeTime BpmDirection Float


maxBpm : Int
maxBpm =
    500


defaultBpm : Int
defaultBpm =
    120


holdInterval : Float
holdInterval =
    500


initialModel : Model
initialModel =
    { isRunning = False, bpm = 120, tapList = [], holdDirection = Nothing, holdBeginTime = Nothing }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Toggle ->
            { model | isRunning = not model.isRunning } ! [ bpmToInterval model.bpm |> toggle ]

        Tap ->
            model ! [ Time.now |> Task.perform TapTime ]

        TapTime t ->
            let
                tapList =
                    t :: model.tapList

                ( newBpm, newList ) =
                    newBpmAndTapList tapList

                bpm =
                    Maybe.withDefault model.bpm newBpm |> min maxBpm
            in
                { model | bpm = bpm, tapList = newList } ! [ bpmToInterval bpm |> newInterval ]

        BpmChange d ->
            let
                newBpm =
                    case d of
                        Up ->
                            model.bpm + 1 |> min maxBpm

                        Down ->
                            model.bpm - 1 |> max 0
            in
                { model | bpm = newBpm } ! [ bpmToInterval newBpm |> newInterval ]

        BpmHold d ->
            model
                ! [ Process.sleep (holdInterval * Time.millisecond) |> Task.andThen (always Time.now) |> Task.perform (GotHoldJudgeTime d)
                  , Time.now |> Task.perform GotHoldBeginTime
                  ]

        BpmRelease ->
            { model | holdDirection = Nothing, holdBeginTime = Nothing } ! []

        GotHoldBeginTime f ->
            { model | holdBeginTime = Just f } ! []

        GotHoldJudgeTime d f ->
            { model
                | holdDirection =
                    if (Maybe.map ((-) f >> flip (>) (holdInterval * 0.7)) model.holdBeginTime |> Maybe.withDefault False) then
                        Just d
                    else
                        Nothing
            }
                ! []


main : Program Never Model Msg
main =
    program { init = initialModel ! [], update = update, subscriptions = subscriptions, view = view }


subscriptions : Model -> Sub Msg
subscriptions m =
    case m.holdDirection of
        Just d ->
            Time.every (20 * Time.millisecond) (always (BpmChange d))

        Nothing ->
            Sub.none


view : Model -> Html.Html Msg
view model =
    Html.ul []
        [ Html.li []
            [ Html.text <| "BPM: " ++ (toString model.bpm) ]
        , Html.li []
            [ Html.button [ Html.Events.onClick Toggle ]
                [ Html.text <|
                    if model.isRunning then
                        "stop"
                    else
                        "start"
                ]
            ]
        , Html.li []
            [ Html.button [ Html.Events.onClick Tap ] [ Html.text "tap" ]
            ]
        , Html.li []
            [ Html.button
                [ Html.Events.onClick <| BpmChange Up
                , Html.Events.onMouseDown (BpmHold Up)
                , Html.Events.onMouseUp BpmRelease
                ]
                [ Html.text "up" ]
            ]
        , Html.li []
            [ Html.button
                [ Html.Events.onClick <| BpmChange Down
                , Html.Events.onMouseDown (BpmHold Down)
                , Html.Events.onMouseUp BpmRelease
                ]
                [ Html.text "down" ]
            ]
        ]


createDiffList : (number -> number -> number) -> List number -> List number
createDiffList f lst =
    let
        folder item accAndList =
            case accAndList.list of
                [] ->
                    accAndList

                x :: xs ->
                    { acc = (f item x :: accAndList.acc), list = xs }
    in
        case lst of
            [] ->
                []

            x :: xs ->
                List.foldl folder { acc = [], list = xs } lst |> .acc |> List.reverse


intervalToBpm : Float -> Int
intervalToBpm f =
    60 / f |> round


bpmToInterval : Int -> Float
bpmToInterval b =
    60 / (toFloat b)


newBpmAndTapList : List Float -> ( Maybe Int, List Float )
newBpmAndTapList tapList =
    if List.length tapList < 3 then
        ( Nothing, tapList )
    else
        let
            diffList =
                createDiffList (-) tapList |> List.take 3
        in
            if List.all (\x -> x < 6000) diffList then
                ( List.sum diffList |> Time.inSeconds |> (flip (/) 3) |> intervalToBpm |> Just, List.take 4 tapList )
            else
                ( Nothing, tapList )


port toggle : Float -> Cmd msg


port newInterval : Float -> Cmd msg
