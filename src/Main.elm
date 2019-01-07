port module Main exposing (BpmDirection(..), Gains, Model, Msg(..), NoteType(..), bpmToInterval, createDiffList, defaultBpm, getClickEvent, getMouseDownEvent, getMouseUpEvent, getVolume, holdInterval, init, intervalToBpm, main, maxBpm, newBpmAndTapList, newGains, newInterval, noteTypeToClass, setVolume, subscriptions, toggle, update, view)

import Html
import Html.Attributes
import Html.Events
import Json.Decode
import Process
import Result
import String
import Task
import Time
import Maybe
import Browser


type alias Model =
    { isRunning : Bool
    , bpm : Int
    , tapList : List Float
    , holdDirection : Maybe BpmDirection
    , holdBeginTime : Maybe Float
    , gains : Gains
    , isTouchable : Bool
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


init : Bool -> ( Model, Cmd Msg )
init isTouchable =
    let
        initialGains =
            { headGain = 100
            , eighthGain = 0
            , sixteenthGain = 0
            , tripletsGain = 0
            }
    in
    ( { isRunning = False
      , bpm = 120
      , tapList = []
      , holdDirection = Nothing
      , holdBeginTime = Nothing
      , gains = initialGains
      , isTouchable = isTouchable
      }
    , newGains initialGains
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Toggle ->
            ( { model | isRunning = not model.isRunning }
            , bpmToInterval model.bpm |> toggle
            )

        Tap ->
            ( model
            , Time.now |> Task.map (Time.posixToMillis >> toFloat >> \x -> x / 1000) |> Task.perform TapTime
            )

        TapTime t ->
            let
                tapList =
                    t :: model.tapList

                ( newBpm, newList ) =
                    newBpmAndTapList tapList

                bpm =
                    Maybe.withDefault model.bpm newBpm |> min maxBpm
            in
            ( { model | bpm = bpm, tapList = newList }
            , bpmToInterval bpm |> newInterval
            )

        BpmChange d ->
            let
                newBpm =
                    case d of
                        Up ->
                            model.bpm + 1 |> min maxBpm

                        Down ->
                            model.bpm - 1 |> max 0
            in
            ( { model | bpm = newBpm }
            , bpmToInterval newBpm |> newInterval
            )

        BpmHold d ->
            let
                newBpm =
                    case d of
                        Up ->
                            model.bpm + 1 |> min maxBpm

                        Down ->
                            model.bpm - 1 |> max 0
            in
            ( { model | bpm = newBpm }
            , Cmd.batch
                [ Process.sleep holdInterval
                    |> Task.andThen (always Time.now)
                    |> Task.map (Time.posixToMillis >> toFloat)
                    |> Task.perform (GotHoldJudgeTime d)
                , Time.now |> Task.map (Time.posixToMillis >> toFloat) |>Task.perform GotHoldBeginTime
                , bpmToInterval newBpm |> newInterval
                ]
            )

        BpmRelease ->
            ( { model | holdDirection = Nothing, holdBeginTime = Nothing }
            , Cmd.none
            )

        GotHoldBeginTime f ->
            ( { model | holdBeginTime = Just f }
            , Cmd.none
            )

        GotHoldJudgeTime d f ->
            ( { model
                | holdDirection =
                    if Maybe.map ((-) f >> (\a -> (>) a (holdInterval * 0.7))) model.holdBeginTime |> Maybe.withDefault False then
                        Just d

                    else
                        Nothing
              }
            , Cmd.none
            )

        GainChanged t v ->
            let
                changedGains =
                    String.toInt v
                        |> Maybe.map (setVolume t model.gains)
                        |> Maybe.withDefault model.gains
            in
            ( { model | gains = changedGains }
            , newGains changedGains
            )


main =
    Browser.document { init = init, update = update, subscriptions = subscriptions, view = view }


subscriptions : Model -> Sub Msg
subscriptions m =
    case m.holdDirection of
        Just d ->
            Time.every 20 (always (BpmChange d))

        Nothing ->
            Sub.none


getMouseDownEvent : Bool -> Msg -> Html.Attribute Msg
getMouseDownEvent isTouchable msg =
    case isTouchable of
        True ->
            Html.Events.on "touchstart" (Json.Decode.succeed msg)

        False ->
            Html.Events.onMouseDown msg


getMouseUpEvent : Bool -> Msg -> Html.Attribute Msg
getMouseUpEvent isTouchable msg =
    case isTouchable of
        True ->
            Html.Events.on "touchend" (Json.Decode.succeed msg)

        False ->
            Html.Events.onMouseUp msg


getClickEvent : Bool -> Msg -> Html.Attribute Msg
getClickEvent isTouchable msg =
    case isTouchable of
        True ->
            Html.Events.on "touchend" (Json.Decode.succeed msg)

        False ->
            Html.Events.onClick msg


view : Model -> Browser.Document Msg
view model =
    { title = "Metronome"
    , body = [
        Html.div [ Html.Attributes.id "container" ]
            [ Html.div [ Html.Attributes.id "bpm", Html.Attributes.class "box" ]
                [ "BPM: " ++ String.fromInt model.bpm |> Html.text |> List.singleton |> Html.p []
                ]
            , Html.div [ Html.Attributes.id "controls" ]
                [ Html.div [ Html.Attributes.class "volume_container" ] <|
                    List.map
                        (\t ->
                            Html.div
                                [ Html.Attributes.class "volume_button"
                                ]
                                [ Html.input
                                    [ Html.Events.onInput <| GainChanged t
                                    , Html.Attributes.type_ "range"
                                    , Html.Attributes.attribute "orient" "vertical"
                                    , Html.Attributes.class "volume_slider"
                                    , getVolume t model.gains |> String.fromInt |> (\a -> (++) a "%") |> Html.Attributes.title
                                    , getVolume t model.gains |> String.fromInt |> Html.Attributes.value
                                    ]
                                    []
                                , Html.div
                                    [ Html.Attributes.class "note_glyph"
                                    , Html.Attributes.class <| noteTypeToClass t
                                    ]
                                    []
                                ]
                        )
                        [ Head, Eighth, Sixteenth, Triplets ]
                , Html.ul [ Html.Attributes.id "control_buttons" ]
                    [ Html.li []
                        [ Html.button
                            [ getClickEvent model.isTouchable Toggle
                            , Html.Attributes.class "button"
                            , Html.Attributes.class "is-primary"
                            , Html.Attributes.class "is-large"
                            ]
                            [ Html.text <|
                                if model.isRunning then
                                    "stop"

                                else
                                    "start"
                            ]
                        ]
                    , Html.li []
                        [ Html.button
                            [ getMouseDownEvent model.isTouchable Tap
                            , Html.Attributes.class "button"
                            , Html.Attributes.class "is-success"
                            , Html.Attributes.class "is-large"
                            ]
                            [ Html.text "tap" ]
                        ]
                    , Html.li []
                        [ Html.button
                            [ getMouseDownEvent model.isTouchable <| BpmHold Up
                            , getMouseUpEvent model.isTouchable BpmRelease
                            , Html.Attributes.class "button"
                            , Html.Attributes.class "is-info"
                            , Html.Attributes.class "is-large"
                            ]
                            [ Html.text "tempo ▲" ]
                        ]
                    , Html.li []
                        [ Html.button
                            [ getMouseDownEvent model.isTouchable <| BpmHold Down
                            , getMouseUpEvent model.isTouchable BpmRelease
                            , Html.Attributes.class "button"
                            , Html.Attributes.class "is-link"
                            , Html.Attributes.class "is-large"
                            ]
                            [ Html.text "tempo ▼" ]
                        ]
                    ]
                ]
            ]

        ]

    }


createDiffList : (number -> number -> number) -> List number -> List number
createDiffList f lst =
    let
        folder item accAndList =
            case accAndList.list of
                [] ->
                    accAndList

                x :: xs ->
                    { acc = f item x :: accAndList.acc, list = xs }
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
    60 / toFloat b


newBpmAndTapList : List Float -> ( Maybe Int, List Float )
newBpmAndTapList tapList =
    if List.length tapList < 3 then
        ( Nothing, tapList )

    else
        let
            diffList =
                createDiffList (-) tapList |> List.take 3
        in
        if List.all (\x -> x < 6) diffList then
            ( List.sum diffList |> (\a -> (/) a 3) |> intervalToBpm |> Just, List.take 4 tapList )

        else
            ( Nothing, tapList )


noteTypeToClass : NoteType -> String
noteTypeToClass n =
    case n of
        Head ->
            "note_fourth"

        Eighth ->
            "note_eighth"

        Sixteenth ->
            "note_sixteenth"

        Triplets ->
            "note_triplets"


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
