port module Main exposing (..)

import Html exposing (program)
import Html.Events
import Html.Attributes
import Time
import Result
import Task
import String
import Debug


type alias Model =
    { isRunning : Bool, bpm : Int, tapList : List Float }


type Msg
    = InputBpm String
    | Toggle
    | Tap
    | TapTime Float


initialModel : Model
initialModel =
    { isRunning = False, bpm = 120, tapList = [] }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputBpm s ->
            String.toInt s |> Result.withDefault 120 |> \bpm -> { model | bpm = bpm } ! []

        Toggle ->
            {model| isRunning = not model.isRunning} ! [ bpmToInterval model.bpm |> toggle ]

        Tap ->
            model ! [ Time.now |> Task.perform TapTime ]

        TapTime t ->
            let
                tapList =
                    t :: model.tapList |> Debug.log "taplist"

                ( newBpm, newList ) =
                    newBpmAndTapList tapList
            in
                { model | bpm = Maybe.withDefault model.bpm newBpm, tapList = newList } ! []


main : Program Never Model Msg
main =
    program { init = initialModel ! [], update = update, subscriptions = subscriptions, view = view }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html.Html Msg
view model =
    Html.ul []
        [ Html.li []
            [ Html.input [ Html.Events.onInput InputBpm, Html.Attributes.value <| toString model.bpm ] []
            ]
        , Html.li []
            [ Html.button [ Html.Events.onClick Toggle ] [ Html.text <| if model.isRunning then "stop" else "start" ]
             
            ]
        , Html.li []
            [ Html.button [ Html.Events.onClick Tap ] [ Html.text "tap" ]
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
                createDiffList (-) tapList |> List.take 3 |> Debug.log "diff"
        in
            if List.all (\x -> x < 6000) diffList then
                ( List.sum diffList |> Time.inSeconds |> (flip (/) 3) |> intervalToBpm |> Just, List.take 4 tapList )
            else
                ( Nothing, tapList )


port toggle : Float -> Cmd msg
