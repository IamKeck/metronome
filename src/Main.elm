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
    { isRunning : Bool
    , bpm : Int
    , tapList : List Float
    , holdDirection : Maybe BpmDirection
    , holdBeginTime : Maybe Float
    , gains : Gains
    }


type alias Gains =
    { headGain : Int
    , eighthGain : Int
    , sixteenthGain : Int
    , tripletsGain : Int
    }


type NoteType
    = Head
    | Eighth
    | Sixteenth
    | Triplets


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
    | GainChanged NoteType String


maxBpm : Int
maxBpm =
    250


defaultBpm : Int
defaultBpm =
    120


holdInterval : Float
holdInterval =
    500


initialModel : Model
initialModel =
    { isRunning = False
    , bpm = 120
    , tapList = []
    , holdDirection = Nothing
    , holdBeginTime = Nothing
    , gains =
        { headGain = 100
        , eighthGain = 0
        , sixteenthGain = 0
        , tripletsGain = 0
        }
    }


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

        GainChanged t v ->
            let
                changedGains =
                    String.toInt v
                        |> Result.map (setVolume t model.gains)
                        |> Result.withDefault model.gains
            in
                { model | gains = changedGains } ! [ newGains changedGains ]


main : Program Never Model Msg
main =
    program { init = initialModel ! [ newGains initialModel.gains ], update = update, subscriptions = subscriptions, view = view }


subscriptions : Model -> Sub Msg
subscriptions m =
    case m.holdDirection of
        Just d ->
            Time.every (20 * Time.millisecond) (always (BpmChange d))

        Nothing ->
            Sub.none


view : Model -> Html.Html Msg
view model =
    Html.div []  
        [
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
            ], 
        Html.div [ Html.Attributes.class "volume_container"]  <|
            List.map
                (\t ->
                    Html.div
                        [ Html.Attributes.class "volume_button"
                        ]
                        [ Html.text <| noteTypeToString t
                        , Html.input
                            [ Html.Events.onInput <| GainChanged t
                            , Html.Attributes.type_ "range"
                            , Html.Attributes.attribute "orient" "vertical"
                            , getVolume t model.gains |> toString |> Html.Attributes.value
                            ]
                            []
                        ]
                )
                [ Head, Eighth, Sixteenth, Triplets ]



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


noteTypeToString : NoteType -> String
noteTypeToString n =
    case n of
        Head ->
            "♩"

        Eighth ->
            "r ♪"

        Sixteenth ->
            "16"

        Triplets ->
            "3"


getVolume : NoteType -> Gains -> Int
getVolume n g =
    case n of
        Head ->
            g.headGain

        Eighth ->
            g.eighthGain

        Sixteenth ->
            g.sixteenthGain

        Triplets ->
            g.tripletsGain


setVolume : NoteType -> Gains -> Int -> Gains
setVolume n g v =
    case n of
        Head ->
            { g | headGain = v }

        Eighth ->
            { g | eighthGain = v }

        Sixteenth ->
            { g | sixteenthGain = v }

        Triplets ->
            { g | tripletsGain = v }


port toggle : Float -> Cmd msg


port newInterval : Float -> Cmd msg


port newGains : Gains -> Cmd msg
