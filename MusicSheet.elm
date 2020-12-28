port module MusicSheet exposing (..)

import Array
import Browser
import Browser.Navigation exposing (Key(..))
import Dict as Dict
import GraphicSVG exposing (..)
import GraphicSVG.App exposing (..)
import Json.Encode as Encode
import Process
import Task
import TwinkleAnimation
import Url as Url


--myShapes model =
--    case model.startTime of
--        Nothing ->
--            scanner
--                :: drawMusic myMusic
--                ++ [text "Play!"
--                    |> centered
--                    |> filled green
--                    |> move (0, -50)
--                    |> notifyTap PlaySound]
--        -- move the music shape to the left
--        Just st ->
--            let
--                now =
--                    model.time - st
--                idx =
--                    getCurNote now 0
--                movedDist =
--                    -noteSpacing * idx
--                elapsedTime =
--                    List.sum <| List.take idx durationList
--                curTempoDur =
--                    case
--                        Array.fromList durationList
--                            |> Array.get idx
--                    of
--                        Nothing ->
--                            0
--                        Just n ->
--                            n
--                curNote =
--                    case
--                        Array.fromList notesList
--                            |> Array.get idx
--                    of
--                        Nothing ->
--                            Note 0 Do 0 ""
--                        Just note ->
--                            note
--                moving =
--                    case compare now (elapsedTime + 8) of
--                        LT ->
--                            toFloat movedDist - (now - elapsedTime) * (noteSpacing / curTempoDur)
--                        _ ->
--                            0
--            in
--            scanner
--                :: [ group (drawMusic myMusic)
--                        |> move ( moving, 0 )
--                   ]
--                ++ [ text
--                        ((String.fromInt <| round moving)
--                            ++ " | "
--                            ++ String.fromFloat curTempoDur
--                            ++ " | "
--                            ++ String.fromInt idx
--                            ++ " | "
--                            ++ String.fromFloat model.time
--                            ++ " | "
--                            ++ String.fromFloat st
--                        )
--                        |> fixedwidth
--                        |> size 8
--                        |> filled black
--                        |> move ( -70, 50 )
--                   ]
--                ++ [ moveBouncingBall curNote (now - elapsedTime) curTempoDur ]
--                ++ [text "Play!"
--                    |> centered
--                    |> filled green
--                    |> move (0, -50)
--                    --|> notifyTap PlaySound]


myShapes : Model -> List (Shape Msg)
myShapes model =
    if model.playMusic == False then
        -- view music score
        [ group (drawMusicSheet myMusic)
            |> scale 0.4
            |> move ( 10, model.y )
        ]
            ++ [--text (String.fromFloat model.y) |> filled red |> move (80,-50)
               ]
            ++ [ playButton
                    |> move ( -80, 16 )
                    |> notifyTap PlaySound
               ]
            ++ [indicator model.y]

    else
        -- play music
        case model.startTime of
            Nothing ->
                let
                    music =
                        group
                            (scanner
                                :: drawMusic myMusic
                            )
                in
                [ music
                    |> scale 0.5
                    |> move ( -64, -40 )
                , playButton
                    |> move ( -80, 55 )
                    |> notifyTap PlaySound
                ]

            Just st ->
                let
                    now =
                        model.time - st

                    idx =
                        if model.curNoteIdx < 0 then
                            0

                        else
                            model.curNoteIdx

                    movedDist =
                        -noteSpacing * idx

                    elapsedTime =
                        List.sum <| List.take idx durationList

                    curTempoDur =
                        case
                            Array.fromList durationList
                                |> Array.get idx
                        of
                            Nothing ->
                                0

                            Just n ->
                                n

                    curNote =
                        case
                            Array.get idx noteArray
                        of
                            Nothing ->
                                Note 0 Rest 0 "" BlankScreen

                            Just note ->
                                note

                    nxtNote =
                        case
                            Array.get (idx + 1) noteArray
                        of
                            Nothing ->
                                Note 0 Rest 0 "" BlankScreen

                            Just note ->
                                note

                    numOfRest =
                        getNumOfRest (idx + 1) noteArray

                    pastNumOfRest =
                        getPastNumOfRest (idx + 1) noteArray

                    -- 8 : after the last note, continue moving 8 seconds
                    moving =
                        case compare now (elapsedTime + 8) of
                            LT ->
                                toFloat movedDist - (now - elapsedTime) * (noteSpacing / curTempoDur)

                            _ ->
                                0

                    music =
                        group
                            (scanner
                                :: [ group (drawMusic myMusic)
                                        |> move ( moving, 0 )
                                   ]
                                ++ [ if (numOfRest + pastNumOfRest) <= 1 then
                                        moveBouncingBall curNote nxtNote (now - elapsedTime) curTempoDur

                                     else
                                        let
                                            startIndex =
                                                idx - pastNumOfRest

                                            sts =
                                                List.sum (List.take startIndex durationList)

                                            t =
                                                now - sts

                                            et =
                                                List.sum (List.take (startIndex + totalNumOfRest + 1) durationList)

                                            totalDur =
                                                et - sts

                                            totalNumOfRest =
                                                numOfRest + pastNumOfRest
                                        in
                                        moveBouncingBallRest t totalDur totalNumOfRest
                                   ]
                                ++ [ if now < durationOfMusic then
                                        emLibretto curNote (now - elapsedTime) curTempoDur (noteSpacing / curTempoDur)

                                     else
                                        text "" |> ghost
                                   ]
                            )
                in
                [ music
                    |> scale 0.5
                    |> move ( -64, -40 )
                , playButton
                    |> move ( -80, 16 )
                    |> notifyTap PlaySound
                , rect 24 96
                    |> filled black
                    |> move ( 84, 32 )
                , group
                    ((rect 192 128 |> filled yellow |> scale 1.05)
                        :: model.curAnimation (model.time - model.animationStartTime)
                    )
                    |> clip (rect 192 128 |> ghost)
                    |> scale 0.7
                    |> move ( 0, 26 )
                , batonModel model
                    |> scale 0.25
                    |> move ( 76, 16 )
                ]


playButton =
    group
        [ triangle 5
            |> filled black
        , circle 7
            |> outlined (solid 1) black
        ]


durationOfMusic =
    List.sum durationList


--get current note index in the list
getCurNote : Float -> Int -> Int
getCurNote time idx =
    if idx < List.length durationList then
        if time <= List.sum (List.take (idx + 1) durationList) then
            idx

        else
            getCurNote time (idx + 1)

    else
        List.length durationList - 1



-- list of every note's duration in the custom music


multi =
    2


durationList =
    List.map ((*) multi) (getDurationList myMusic)



--use map double the duration of each note
-- get the list of duration of music


getDurationList : Music -> List Float
getDurationList (Music _ _ measures) =
    List.concat measures
        |> List.map (\(Measure note) -> note)
        |> List.concat
        |> List.map (\(Note dua _ _ _ _) -> dua)


notesList =
    getNoteList myMusic



-- get the note list of music


getNoteList : Music -> List Note
getNoteList (Music _ _ measures) =
    List.concat measures
        |> List.map (\(Measure note) -> note)
        |> List.concat


drawMusic : Music -> List (Shape Msg)
drawMusic (Music _ _ voices) =
    [ treble |> move ( -25, 5 )
    , ts44 |> move ( -15, 0.25 )
    , drawMeasurescore 50 |> move ( -25, 0 )
    ]
        ++ (List.concat <| List.indexedMap drawVoice voices ++ List.indexedMap drawStaff voices)


drawVoice : Int -> List Measure -> List (Shape Msg)
drawVoice voiceNumber measures =
    case measures of
        measure1 :: moreMeasures ->
            ((bar |> move ( measureWidth measure1 - 0.5 * noteSpacing, 0 ))
                :: drawMeasure measure1
            )
                ++ (drawVoice voiceNumber moreMeasures
                        |> List.map (move ( measureWidth measure1, 0 ))
                   )

        [] ->
            []


drawStaff : Int -> List Measure -> List (Shape Msg)
drawStaff voiceNumber measures =
    case measures of
        measure1 :: moreMeasures ->
            (drawMeasurescore (measureWidth measure1) |> move ( 0.5 * measureWidth measure1, 0 ))
                :: (drawStaff voiceNumber moreMeasures
                        |> List.map (move ( measureWidth measure1, 0 ))
                   )

        [] ->
            []


-- List Measure | Rest of Meaure Numbers | Line Number | Measure Number Annotation
drawLine : List Measure -> Int -> Int -> Int -> (Int, List (Shape Msg))
drawLine measures n l mark=
    let
        drawTS =
            group
                [ bar |> move ( -50, 0 )
                , treble |> move ( -40, 5 )

                --, ts44 |> move ( -15, 0.25 )
                , drawMeasurescore 20 |> move ( -40, 0 )
                , drawSpacing |> move ( -20, 0 )
                ]

        drawSpacing =
            drawMeasurescore 40
        
        setMInL i m =
            let
                w = List.sum <| List.map measureWidth <| List.take i m
            in
            if  w < 320 && List.length (List.drop i m) >0 then
                setMInL (i+1) m
            else if w > 320 then
                i-1
            else
                i

        measuresInLine =                  
            setMInL 1 measures

    in
    -- more than 1 lines
    if n > measuresInLine then
        if l == 1 then
            Tuple.mapSecond
            ((::)
            (group
                [ bar |> move ( -50, 0 )
                , treble |> move ( -40, 5 )
                , tsOfMusic |> move ( -25, 0.25 )
                , drawMeasurescore 20 |> move ( -40, 0 )
                , drawSpacing |> move ( -20, 0 )
                ]
                :: [text (String.fromInt mark) |> filled black |> scale 0.5 |> move (-50,20)]
                    ++
                    (List.concat <|
                        List.indexedMap drawVoice [ List.take measuresInLine measures ]
                            ++ List.indexedMap drawStaff [ List.take measuresInLine measures ]
                   )
                |> group
                |> subtract (rect 20 30 |> ghost |> move ( List.map measureWidth (List.take measuresInLine measures) |> List.sum |> (+) 0.1, 0 ))
                |> move ( -140, toFloat ((l - 1) * -50 + 85) )
            ))
                ( drawLine (List.drop measuresInLine measures) (n - measuresInLine) (l + 1) (mark+measuresInLine) )

        else
            Tuple.mapSecond
            ( (::)
            (drawTS
                :: [text (String.fromInt mark) |> filled black |> scale 0.5 |> move (-50,20)]
                    ++
                    (List.concat <|
                        List.indexedMap drawVoice [ List.take measuresInLine measures ]
                            ++ List.indexedMap drawStaff [ List.take measuresInLine measures ]
                   )
                |> group
                |> subtract (rect 20 30 |> ghost |> move ( List.map measureWidth (List.take measuresInLine measures) |> List.sum |> (+) 0.1, 0 ))
                |> move ( -140, toFloat ((l - 1) * -50 + 85) )
            ))
                (drawLine (List.drop measuresInLine measures) (n - measuresInLine) (l + 1) (mark+measuresInLine))
    
    else
        -- the last line or only 1 line
        if l == 1 then
            [ [ treble |> move ( -25, 5 )
              , tsOfMusic |> move ( -25, 0.25 )
              , drawMeasurescore 20 |> move ( -20, 0 )
              , drawSpacing |> move ( -20, 0 )
              ]
                ++ [text (String.fromInt mark) |> filled black |> scale 0.5 |> move (-50,20)]
                ++ (List.concat <|
                        List.indexedMap drawVoice [ List.take measuresInLine measures ]
                            ++ List.indexedMap drawStaff [ List.take measuresInLine measures ]
                   )
                |> group
                |> subtract (rect 20 30 |> ghost |> move ( List.map measureWidth (List.take measuresInLine measures) |> List.sum |> (+) 0.1, 0 ))
                |> move ( -140, toFloat ((l - 1) * -50 + 85) )
            ]
            |> Tuple.pair l

        else
            [ (drawTS
                :: [text (String.fromInt mark) |> filled black |> scale 0.5 |> move (-50,20)]
                    ++
                    (List.concat <| List.indexedMap drawVoice [ measures ] ++ List.indexedMap drawStaff [ measures ])
              )
                |> group
                |> subtract (rect 20 30 |> ghost |> move ( List.map measureWidth (List.take measuresInLine measures) |> List.sum |> (+) 0.1, 0 ))
                |> move ( -140, toFloat ((l - 1) * -50 + 85) )
            ]
            |> Tuple.pair l


musicSheet : (Int, List (Shape Msg))
musicSheet =
    let
        numOfMeasures =
            getNumofMeasures myMusic

        -- List Measure
        measuresList =
            List.concat ((\(Music _ _ voices) -> voices ) myMusic)
    in
    drawLine measuresList numOfMeasures 1 1


sheetHeight : Float
sheetHeight = 50 * 0.4 * toFloat (Tuple.first musicSheet)


{-onWheel : msg -> Attribut msg
onWheel message =
    on "scroll" (Json.succeed message)-}


indicator : Float -> Shape Msg
indicator scrolly = 
    let
        a =  ( scrolly / (sheetHeight - 100) ) * 128
    in
    group
    [--text ((String.fromFloat a ) ++ " %") |> alignRight |> filled black |> scale 0.3 |> move (91,55)
    --,
    rect 1 a |> filled lightBlue |> move (91,64-a/2)
    ]


drawMusicSheet : Music -> List (Shape Msg)
drawMusicSheet (Music _ _ voices) =
    Tuple.second musicSheet


getNumofMeasures : Music -> Int
getNumofMeasures music =
    case music of
        Music _ _ v ->
            List.concat v |> List.length


drawMeasurescore : Float -> Shape Msg
drawMeasurescore measureScorewidth =
    group
        [ rect measureScorewidth 0.5 |> filled black |> move ( 0, 10 )
        , rect measureScorewidth 0.5 |> filled black |> move ( 0, 5 )
        , rect measureScorewidth 0.5 |> filled black
        , rect measureScorewidth 0.5 |> filled black |> move ( 0, -5 )
        , rect measureScorewidth 0.5 |> filled black |> move ( 0, -10 )
        ]


myMusic : Music
myMusic =
    Music Piano
        FourFour
        [ 
          [ Measure
                [ Note 0.25 Do 1 "" ContinueVideo |> dot 1
                , Note 0.125 Re 1 "" ContinueVideo
                , Note 0.125 Mi 1 "" ContinueVideo
                , Note 0.125 Mi 1 "" ContinueVideo
                , Note 0.125 Re 1 "" ContinueVideo
                , Note 0.125 Do 1 "" ContinueVideo
                ]
          , Measure
                [
                  Note 0.25 Do 1 "" ContinueVideo |> dot 1
                , Note 0.125 Re 1 "" ContinueVideo 
                , Note 0.125 Mi 1 "" ContinueVideo |>raiseOctave
                , Note 0.125 Mi 1 "" ContinueVideo |>raiseOctave
                , Note 0.125 Re 1 "" ContinueVideo
                , Note 0.125 Do 1 "" ContinueVideo
                ]
          , Measure
                [ half Mi |> sing "mi" |> addVideo TwinkleAnimation.myShapes 
                , Note 0.25 Sol 1 "sol" ContinueVideo |> dot 1
                , Note 0.125 Sol 1 "sol," ContinueVideo
                
                ]
          , Measure
                [ Note 0.5 Do 1 "do" ContinueVideo |> dot 1
                , Note 0.25 Re 1 "re" ContinueVideo
                ]
          , Measure
                [
                  Note 0.25 Mi 1 "mi" ContinueVideo
                , Note 0.25 Fa 1 "fa" ContinueVideo
                , Note 0.25 Sol 1 "sol" ContinueVideo
                , Note 0.25 La 1 "la" ContinueVideo 
                ]
          , Measure
                [ Note 0.5 Re 1 "re" ContinueVideo
                , Note 0.5 Rest 1 "" ContinueVideo
                ]
          , Measure
                [ Note 0.5 Mi 1 "mi" ContinueVideo
                , Note 0.25 Fa 1 "fi" ContinueVideo |> dot 1
                , Note 0.125 Fa 1 "fi" ContinueVideo
                ]
          , Measure
                [ Note 0.5 Sol 1 "sol" ContinueVideo |> dot 1
                , Note 0.25 La 1 "la" ContinueVideo
                ]
          , Measure
                [
                  Note 0.25 Ti 1 "ti" ContinueVideo
                , Note 0.25 Ti 1 "to" ContinueVideo
                , Note 0.25 La 1 "la" ContinueVideo
                , Note 0.25 La 1 "la" ContinueVideo 
                ]
          , Measure
                [ Note 0.5 Sol 1 "sol" ContinueVideo
                , Note 0.25 Rest 1 "" ContinueVideo
                , Note 0.125 Re 1 "Re" ContinueVideo |> dot 1
                , Note 0.0625 Mi 1 "mi" ContinueVideo
                ]
          , Measure
                [ Note 0.25 Fa 1 "fa" ContinueVideo |> dot 1
                , Note 0.125 Mi 1 "mi" ContinueVideo
                , Note 0.25 Re 1 "re" ContinueVideo
                , Note 0.125 Mi -1 "mi" ContinueVideo |> dot 1
                , Note 0.0625 Fa -1 "fa" ContinueVideo
                ]
          , Measure
                [ Note 0.25 Sol 1 "sol" ContinueVideo |> dot 1
                , Note 0.125 Fa 1 "fa" ContinueVideo
                , Note 0.25 Mi 1 "mi" ContinueVideo
                , Note 0.125 Fa -1 "fa" ContinueVideo |> dot 1
                , Note 0.0625 Sol -1 "sol" ContinueVideo
                ]
          , Measure
                [ Note 0.25 La 1 "la" ContinueVideo
                , Note 0.25 Sol 1 "sol" ContinueVideo
                , Note 0.25 Fa 1 "fa" ContinueVideo
                , Note 0.25 Mi 1 "mi" ContinueVideo
                ]
          , Measure
                [ Note 0.5 Re 1 "re" ContinueVideo
                , Note 0.25 Rest 1 "" ContinueVideo
                , Note 0.125 Re 1 "re" ContinueVideo |> dot 1
                , Note 0.0625 Mi 1 "mi" ContinueVideo
                ]
          , Measure
                [ Note 0.25 Fa 1 "fa" ContinueVideo |> dot 1
                , Note 0.125 Mi 1 "mi" ContinueVideo 
                , Note 0.25 Re 1 "re" ContinueVideo
                , Note 0.125 Mi -1 "mi" ContinueVideo |> dot 1
                , Note 0.0625 Fa 1 "fa" ContinueVideo
                ]
          , Measure
                [ Note 0.25 Sol 1 "sol" ContinueVideo |> dot 1
                , Note 0.125 Fa 1 "fa" ContinueVideo
                , Note 0.25 Mi 1 "mi" ContinueVideo
                , Note 0.25 Mi 1 "mi" ContinueVideo
                ]
          , Measure
                [ Note 0.25 Re 1 "re" ContinueVideo
                , Note 0.25 Sol 1 "sol" ContinueVideo
                , Note 0.125 Sol 1 "sol" ContinueVideo
                , Note 0.125 Fa -1 "fi" ContinueVideo
                , Note 0.125 Mi -1 "mi" ContinueVideo
                , Note 0.125 Fa -1 "fi" ContinueVideo
                ]
          , Measure
                [ Note 0.5 Sol 1 "sol" ContinueVideo
                , Note 0.5 Rest 1 "" ContinueVideo
                ]
          , Measure
                [ Note 0.5 Mi 1 "mi" ContinueVideo
                , Note 0.25 Sol 1 "sol" ContinueVideo |> dot 1
                , Note 0.125 Sol 1 "sol" ContinueVideo
                ]
          , Measure
                [ Note 0.5 Do 1 "do" ContinueVideo
                , Note 0.5 Rest 1 "" ContinueVideo
                ]
          , Measure
                [ Note 0.5 Fa 1 "fa" ContinueVideo
                , Note 0.25 La 1 "la" ContinueVideo |> dot 1
                , Note 0.15 La 1 "la" ContinueVideo
                ]
          , Measure
                [ Note 0.5 Re 1 "re" ContinueVideo |> dot 1
                , Note 0.25 Rest 1 "" ContinueVideo
                ]
          , Measure
                [ Note 0.5 Sol 1 "sol" ContinueVideo
                , Note 0.25 Sol 1 "si" ContinueVideo |> dot 1
                , Note 0.125 Sol 1 "si" ContinueVideo
                ]
          , Measure
                [ Note 0.25 La 1 "la" ContinueVideo
                , Note 0.25 Fa 1 "fa" ContinueVideo
                , Note 0.25 Mi 1 "mi" ContinueVideo
                , Note 0.25 Re -1 "re" ContinueVideo
                ]
          , Measure
                [ Note 0.5 Do 1 "do" ContinueVideo
                , Note 0.5 Re 1 "re" ContinueVideo
                ]
          , Measure
                [ Note 0.5 Mi 1 "mi" ContinueVideo
                , Note 0.5 Rest 1 "" ContinueVideo
                ]
          , Measure
                [ Note 0.5 Sol 1 "sol" ContinueVideo
                , Note 0.25 Do 1 "do" ContinueVideo |> dot 1 
                , Note 0.125 Do 1 "do" ContinueVideo
                ] 
          , Measure
                [ Note 0.5 Sol 1 "sol" ContinueVideo
                , Note 0.25 Do 1 "do" ContinueVideo |> dot 1 |> raiseOctave
                , Note 0.125 Do 1 "do" ContinueVideo |> raiseOctave
                ] 
          , Measure
                [ Note 0.25 La 1 "la" ContinueVideo
                , Note 0.25 Fa 1 "fa" ContinueVideo
                , Note 0.25 Mi 1 "mi" ContinueVideo
                , Note 0.25 Re -1 "re" ContinueVideo
                ]
          , Measure
                [ Note 0.5 Sol 1 "sol" ContinueVideo
                , Note 0.5 Do 1 "to" ContinueVideo --|> lowerOctave
                ]
          , Measure
                [ Note 0.5 Do 1 "do" ContinueVideo |> dot 1
                , Note 0.25 Rest 1 "" ContinueVideo
                ]
          ]
        ]


noteSpacing : number
noteSpacing =
    20


{- }
   measureWidth : Float
   measureWidth =
     noteSpacing * toFloat beatsPerMeasure
-}


measureWidth : Measure -> Float
measureWidth (Measure notes) =
    noteSpacing * toFloat (List.length notes)


beatsPerMeasure =
    (\(Music _ ts _) ->
        case ts of
            FourFour ->
                4

            ThreeFour ->
                3

            ThreeEighths ->
                3

            TwoFour ->
                2

            SixEight ->
                6

            FiveEight ->
                5

            TwoTwo ->
                2
    )
        myMusic


noteType = 
    (\(Music _ ts _) ->
        case ts of
            FourFour ->
                4

            ThreeFour ->
                4

            ThreeEighths ->
                8

            TwoFour ->
                4

            SixEight ->
                8

            FiveEight ->
                8

            TwoTwo ->
                2
    )
        myMusic


drawMeasure : Measure -> List (Shape Msg)
drawMeasure (Measure notes) =
    List.indexedMap (drawNote (Measure notes)) notes


extractDots : Float -> Int
extractDots d =
    if d >= 1 then
        1 + extractDots (d - 1)

    else if d >= 0.5 then
        1 + extractDots (d - 0.5)

    else if d >= 0.25 then
        1 + extractDots (d - 0.25)

    else if d >= 0.125 then
        1 + extractDots (d - 0.125)

    else if d >= 0.0625 then
        1 + extractDots (d - 0.0625)

    else
        -1

{-beat =
    1 / noteType-}


beaming : Int -> Note -> Shape Msg -> Measure -> Shape Msg
beaming idx note shape measure =
    let
        notes =
            (\(Measure notelist) -> notelist) measure

        dur =
            (\(Note d _ _ _ _) -> d) note

        pit =
            (\(Note _ p _ _ _) -> p) note

        nxt_dur =
            (\(Note d _ _ _ _) -> d) (Maybe.withDefault (Note -1 Rest 0 "" ContinueVideo) (List.head <| List.drop (idx + 1) notes))

        nxt_pit =
            (\(Note _ p _ _ _) -> p) (Maybe.withDefault (Note -1 Rest 0 "" ContinueVideo) (List.head <| List.drop (idx + 1) notes))

        pre_dur =
            (\(Note d _ _ _ _) -> d) (Maybe.withDefault (Note -1 Rest 0 "" ContinueVideo) (List.head <| List.drop (idx - 1) notes))

        pre_pit =
            (\(Note _ p _ _ _) -> p) (Maybe.withDefault (Note -1 Rest 0 "" ContinueVideo) (List.head <| List.drop (idx - 1) notes))

        height =
            pitchHeight pit

        nxt_height =
            pitchHeight nxt_pit

        tip_quarterN =
            ( 9.9 * 0.28, 60 * 0.28 + height )

        nxt_tip_quarterN =
            ( 9.9 * 0.28 + noteSpacing, 60 * 0.28 + nxt_height )

        middlePoint p1 p2 =
            Tuple.pair ((Tuple.first p1 + Tuple.first p2) / 2) ((Tuple.second p1 + Tuple.second p2) / 2)

        -- get the beat of every note
        groupBeats ns i pre =
            case ns of
                n :: rest ->
                    let
                        d = (\(Note du _ _ _ _) -> du) n
                        dotN = extractDots d
                        calBeat a b =
                            case a of
                                0 -> b
                                _ -> b / toFloat (2 ^ a) + calBeat (a-1) b
                        
                        acc_cur_beats = List.sum <| List.take (i+1) acc_beatList

                        groupOrNot =  pre >= 1 && (acc_cur_beats == toFloat (truncate acc_cur_beats))

                        cur = d/beat

                    in
                    if groupOrNot || d >= beat then
                        1 :: groupBeats rest (i+1) cur
                    else if d >= beat * 0.5 && d < beat then
                        (calBeat dotN 0.5) :: groupBeats rest (i+1) cur
                    else if d >= beat * 0.25 && d < beat * 0.5 then
                        (calBeat dotN 0.25) :: groupBeats rest (i+1) cur
                    else if d >= beat * 0.125 && d < beat * 0.25  then
                        (calBeat dotN 0.125):: groupBeats rest (i+1) cur
                    else if d >= beat * 0.0625 && d < beat * 0.125 then
                        (calBeat dotN 0.0625) :: groupBeats rest (i+1) cur
                    else
                        0 :: groupBeats rest (i+1) cur
                [] ->
                    []
 
        beatsList = groupBeats notes 0 -1

        -- accurate beats
        acc_beatList = getBeats notes
        
        -- use 1 instead of all beat larger than or equal to 1
        preBeats = List.sum <| List.take idx beatsList
        curBeats = List.sum <| List.take (idx+1) beatsList

    in
    -- not grouping with previous
    if preBeats == toFloat (truncate preBeats) then

        case compare curBeats (toFloat (truncate curBeats)) of
            EQ -> shape
            _ ->
                if dur >= 0.125 && dur < 0.25 then
                    if nxt_dur >= 0.125 && nxt_dur < 0.25 then
                        group
                            [ quarterN |> move ( 0, height )
                            , line tip_quarterN nxt_tip_quarterN |> outlined (solid 2) black
                            ]

                    else if nxt_dur >= 0.0625 && nxt_dur < 0.125 then
                        group
                            [ quarterN |> move ( 0, height )
                            , line tip_quarterN nxt_tip_quarterN |> outlined (solid 2) black
                            , line (middlePoint tip_quarterN nxt_tip_quarterN) nxt_tip_quarterN |> outlined (solid 2) black |> move ( 0, -4 )
                            ]
                    else
                        shape

                else if dur >= 0.0625  && dur < 0.125 then
                    if nxt_dur >= 0.125 && nxt_dur < 0.25 then
                        group
                            [ quarterN |> move ( 0, height )
                            , line tip_quarterN nxt_tip_quarterN |> outlined (solid 2) black
                            , line tip_quarterN (middlePoint tip_quarterN nxt_tip_quarterN) |> outlined (solid 2) black |> move ( 0, -4 )
                            ]

                    else if nxt_dur >= 0.0625 && nxt_dur < 0.125 then
                        group
                            [ quarterN |> move ( 0, height )
                            , line tip_quarterN nxt_tip_quarterN |> outlined (solid 2) black
                            , line tip_quarterN nxt_tip_quarterN |> outlined (solid 2) black |> move (0, -4)
                            ]
                    else
                        shape
                
                -- no need to group current note
                else
                    shape

    -- grouping with previous
    else
        {-let
            curBeats = List.sum <| List.take (idx+1) beatsList
        in-}

        -- not grouping with next
        if curBeats == toFloat (truncate curBeats) then
            quarterN |> move ( 0, height )
           
        -- grouping with next
        else 
            if dur >= 0.125 && dur < 0.25 then
                if nxt_dur >= 0.125 && nxt_dur < 0.25 then
                    group
                        [ quarterN |> move ( 0, height )
                        , line tip_quarterN nxt_tip_quarterN |> outlined (solid 2) black
                        ]

                else if nxt_dur >= 0.0625 && nxt_dur < 0.125 then
                    group
                        [ quarterN |> move ( 0, height )
                        , line tip_quarterN nxt_tip_quarterN |> outlined (solid 2) black
                        , line (middlePoint tip_quarterN nxt_tip_quarterN) nxt_tip_quarterN |> outlined (solid 2) black |> move ( 0, -4 )
                        ]
                else
                    quarterN |> move ( 0, height)

            else if dur >= 0.0625  && dur < 0.125 then
                if nxt_dur >= 0.125 && nxt_dur < 0.25 then
                    group
                        [ quarterN |> move ( 0, height )
                        , line tip_quarterN nxt_tip_quarterN |> outlined (solid 2) black
                        , line tip_quarterN (middlePoint tip_quarterN nxt_tip_quarterN) |> outlined (solid 2) black |> move ( 0, -4 )
                        ]

                else if nxt_dur >= 0.0625 && nxt_dur < 0.125 then
                    group
                        [ quarterN |> move ( 0, height )
                        , line tip_quarterN nxt_tip_quarterN |> outlined (solid 2) black
                        , line tip_quarterN nxt_tip_quarterN |> outlined (solid 2) black |> move (0, -4)
                        ]
                else
                    quarterN |> move ( 0, height)
            else
                quarterN |> move ( 0, height)  


drawNote : Measure -> Int -> Note -> Shape Msg
drawNote measure idx (Note d pitch vol libretto a) =
    (if pitch == Rest then
        if d == 0.0625 then
            sixteenthR
        
        else if d == 0.125 then
            eighthR

        else if d == 0.25 then
            quarterR

        else if d == 0.5 then
            halfR

        else if d == 1 then
            wholeR

        else if d > 1 then
            group (wholeR :: addDot (extractDots d))

        else if d > 0.5 then
            group (halfR :: addDot (extractDots d))

        else if d > 0.25 then
            group (quarterR :: addDot (extractDots d))

        else if d > 0.125 then
            group (eighthR :: addDot (extractDots d))

        else if d > 0.0625 then
            group (sixteenthR :: addDot (extractDots d))

        else
            (text <| "unsupported rest duration " ++ String.fromFloat d) |> filled red

     else
        let
            height =
                pitchHeight pitch
        in
        group
            ((
              if d >= 0.0625 && d < 0.125 then
                group
                    [ beaming
                        idx
                        (Note d pitch vol libretto a)
                        (sixteenthN
                            |> move (0, height)
                            )
                        measure
                            |> invertNote height "treble"
                    , text libretto
                        |> size 5
                        |> centered
                        |> filled black
                        |> move ( 0, -25 )
                    , group (addDot (extractDots d))
                        |> move ( 0, height )
                    ]

              else if d >= 0.125 && d < 0.25 then
                group
                    [ beaming
                        idx
                        (Note d pitch vol libretto a)
                        (eighthN
                            |> move ( 0, height )
                        )
                        measure
                            |> invertNote height "treble"
                    , text libretto
                        |> size 5
                        |> centered
                        |> filled black
                        |> move ( 0, -25 )
                    , group (addDot (extractDots d))
                        |> move ( 0, height )
                    ]

              else if d >= 0.25 && d < 0.5 then
                group
                    [ quarterN
                        |> invertNote height "treble"
                        |> move ( 0, height )
                    , text libretto
                        |> size 5
                        |> centered
                        |> filled black
                        |> move ( 0, -25 )
                    , group (addDot (extractDots d))
                        |> move ( 0, height )
                    ]

              else if d >= 0.5 && d < 1 then
                group
                    [ halfN
                        |> invertNote height "treble"
                        |> move ( 0, height )
                    , text libretto
                        |> size 5
                        |> centered
                        |> filled black
                        |> move ( 0, -25 )
                    , group (addDot (extractDots d))
                        |> move ( 0, height )
                    ]

              else if d >= 1 && d < 2 then
                group
                    [ wholeN
                        |> invertNote height "treble"
                        |> move ( 0, height )
                    , text libretto
                        |> size 5
                        |> centered
                        |> filled black
                        |> move ( 0, -25 )
                    , group (addDot (extractDots d))
                        |> move ( 0, height )
                    ]

              else
                (text <| "unsupported duration " ++ String.fromFloat d) |> filled red
             )
                :: extraLines height
            )
    )
        |> move ( toFloat (noteSpacing * idx), 0 )


invertNote : Float -> String -> Shape Msg -> Shape Msg
invertNote height clef shape =
    case clef of
        "treble" ->
            if height > 0 then
                group[
                -- oval
                    shape |> rotate (degrees 180)
                -- flag
                  , shape |> rotate (degrees 180) |> mirrorX |> move (-5.5,0)
                ] |> subtract (rect 20 60 |> ghost |> move (-13,0))
            else
                shape

        _ ->
            shape


extraLines : number -> List (Shape Msg)
extraLines height =
    if height >= 15 then
        (rect 8 0.5 |> filled black |> move ( 0, 15 ))
            :: (extraLines (height - 5) |> List.map (move ( 0, 5 )))

    else if height <= -15 then
        (rect 8 0.5 |> filled black |> move ( 0, -15 ))
            :: (extraLines (height + 5) |> List.map (move ( 0, -5 )))

    else
        []


pitchHeight : Pitch -> Float
pitchHeight pitch =
    case pitch of
        Do ->
            -15

        Re ->
            -12.5

        Mi ->
            -10

        Fa ->
            -7.5

        Sol ->
            -5

        La ->
            -2.5

        Ti ->
            0

        OctaveUp p ->
            7 * 2.5 + pitchHeight p

        _ ->
            60


moveRight : Shape Msg -> Shape Msg
moveRight shape =
    shape |> move ( 10, 0 )


moveByAmount : Int -> Shape Msg -> Shape Msg
moveByAmount idx shape =
    shape |> move ( 3 ^ toFloat idx, -40 )


type VideoFrame
    = BlankScreen
    | StartVideo (Float -> List (Shape Msg))
    | ContinueVideo


type Note
    = Note
        Duration
        -- duration
        Pitch
        -- pitch
        Volume
        -- volume (0.0 <= vol <= 1.0)
        String
        -- liberto/lyrics to be sung at the note
        VideoFrame


type Pitch
    = Do
    | Re
    | Mi
    | Fa
    | Sol
    | La
    | Ti
    | Rest
    | OctaveUp Pitch
    | OctaveDown Pitch


type TimeSignature
    = FourFour
    | ThreeFour
    | ThreeEighths
    | TwoFour
    | SixEight
    | FiveEight
    | TwoTwo


type
    Measure
    -- Seq Music
    = Measure (List Note)


type Instrument
    = Piano
    | AcousticNylonGuitar
    | AcousticSteelGuitar
    | Bass
    | Violin
    | Trumpet
    | Sitar


type Music
    = Music Instrument TimeSignature (List (List Measure))


type alias Time =
    Float


type alias MusicEvent =
    { time : Time
    , pitch : Pitch
    , duration : Time
    , volume : Volume
    , lyric : String
    }


type alias Tempo =
    { duration : Duration, bpm : Int }


type alias Duration =
    Float


type alias Volume =
    Float


type Msg
    = Tick Float GetKeyState
    | MakeRequest Browser.UrlRequest
    | UrlChange Url.Url
    | PlayMusic
    | PlaySound


type alias Model =
    { time : Float
    , startTime : Maybe Float
    , playMusic : Bool
    , curAnimation : Float -> List (Shape Msg)
    , curNoteIdx : Int
    , animationStartTime : Float
    , y : Float
    }


init : () -> Url.Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    let
        model =
            { time = 0
            , startTime = Nothing
            , playMusic = False
            , curAnimation = \_ -> []
            , curNoteIdx = -1
            , animationStartTime = 0
            , y = 0.0
            }
    in
    ( model, Cmd.none )


update msg model =
    case msg of
        Tick t ( keys, _, _ ) ->
            case model.startTime of
                Just st ->
                    if getCurNote (model.time - st) 0 == model.curNoteIdx then
                        ( { model | time = t }, Cmd.none )

                    else
                        let
                            idx =
                                getCurNote (model.time - st) 0

                            curNote =
                                case
                                    Array.get idx noteArray
                                of
                                    Nothing ->
                                        Note 0 Rest 0 "" BlankScreen

                                    Just note ->
                                        note

                            noteRecord =
                                getNoteRecord curNote
                        in
                        ( { model
                            | time = t
                            , animationStartTime =
                                case noteRecord.video of
                                    BlankScreen ->
                                        t

                                    StartVideo animation ->
                                        t

                                    ContinueVideo ->
                                        model.animationStartTime
                            , curNoteIdx = getCurNote (model.time - st) 0
                            , curAnimation =
                                case noteRecord.video of
                                    BlankScreen ->
                                        \_ -> []

                                    StartVideo animation ->
                                        animation

                                    ContinueVideo ->
                                        model.curAnimation
                            , y =
                                if keys UpArrow == JustDown && model.y < sheetHeight - 100 then
                                    model.y + 10

                                else if keys DownArrow == JustDown && model.y > 0.0 then
                                    model.y - 10
                                else
                                    model.y
                          }
                        , Cmd.none
                        )

                Nothing ->
                    ( { model
                        | time = t
                        , startTime =
                            if model.playMusic then
                                case model.startTime of
                                    Nothing ->
                                        Just t

                                    justSomething ->
                                        justSomething

                            else
                                model.startTime
                        , y =
                            if keys UpArrow == JustDown && model.y < sheetHeight - 100 then
                                model.y + 10
                            else if keys DownArrow == JustDown && model.y > 0.0 then
                                model.y - 10
                            else
                                model.y
                      }
                    , Cmd.none
                    )

        MakeRequest _ ->
            ( model, Cmd.none )

        UrlChange _ ->
            ( model, Cmd.none )

        PlayMusic ->
            ( { model | playMusic = True }, Cmd.none )

        PlaySound ->
            let
                delay : Float -> Msg -> Cmd Msg
                delay time messg =
                    Process.sleep time
                        |> Task.andThen (always <| Task.succeed messg)
                        |> Task.perform identity

                tempo =
                    { duration = 0.25, bpm = 120 }
            in
            ( model, Cmd.batch [ play <| perform tempo myMusic, delay 500 PlayMusic ] )


main : AppWithTick () Model Msg
main =
    appWithTick Tick
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChange
        , onUrlRequest = MakeRequest
        }


view model =
    { body = collage 192 128 (myShapes model), title = "OurMusic" }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


eighth : Pitch -> Note
eighth pitch =
    Note 0.125 pitch 1 "" ContinueVideo


quarter : Pitch -> Note
quarter pitch =
    Note 0.25 pitch 1 "" ContinueVideo


quarterL : Pitch -> String -> Note
quarterL pitch lyric =
    Note 0.25 pitch 1 lyric ContinueVideo


half : Pitch -> Note
half pitch =
    Note 0.5 pitch 1 "" ContinueVideo


whole : Pitch -> Note
whole pitch =
    Note 1 pitch 1 "" ContinueVideo


dot : Int -> Note -> Note
dot num (Note d p v w a) =
    let
        dottedD n =
            case n of
                0 ->
                    d

                _ ->
                    d / toFloat (2 ^ n) + dottedD (n - 1)
    in
    Note (dottedD num) p v w a



-- TODO how should these grow?


forte : Note -> Note
forte (Note d pitch volume word video) =
    Note d pitch 0.75 word video


piano : Note -> Note
piano (Note d pitch volume word video) =
    Note d pitch 0.5 word video


pianissimo : Note -> Note
pianissimo (Note d pitch volume word video) =
    Note d pitch 0.25 word video


fortissimo : Note -> Note
fortissimo (Note d pitch volume word video) =
    Note d pitch 1 word video


silence : Note -> Note
silence (Note d pitch volume word video) =
    Note d pitch -1 word video


do : Note -> Note
do (Note d pitch volume word video) =
    Note d Do volume word video


re : Note -> Note
re (Note d pitch volume word video) =
    Note d Re volume word video



--maryHadALittleLambWithLibreto : Result String Music
--maryHadALittleLambWithLibreto =
--    maryHadALittleLamb
--        |> addLibreto
--            ["Mar-", "y", "had", "a", "lit", "tle", "lamb", "lit", "tle", "lamb", "lit", "tle", "lamb", "Mar-", "y", "had", "a"]
--maryHadALittleLamb : Music
--maryHadALittleLamb =
--    Music Piano FourFour
--        [
--            [
--                Measure (
--                    [ quarter Mi |> sing "Mar-" , quarter Re |> sing "y", quarter Do |> sing "had" , quarter Re |> sing "a"]
--                        |> List.map (\note -> pianissimo note)
--                ),
--                Measure (
--                    [ quarter Mi |> sing "lit" , quarter Mi |> sing "tle", half Mi |> sing "lamb"]
--                        |> List.map (\note -> piano note)
--                ),
--                Measure (
--                    [ quarter Re, quarter Re, half Re]
--                        |> List.map (\note -> piano note)
--                ),
--                Measure (
--                    [ quarter Mi, quarter Sol, half Sol]
--                        |> List.map (\note -> forte note)
--                ),
--                Measure (
--                    [ quarter Mi, quarter Re, quarter Do, quarter Re]
--                        |> List.map (\note -> fortissimo note)
--                )
--            ]
--        ]


sing : String -> Note -> Note
sing newWord (Note duration pitch volume oldWord video) =
    Note duration pitch volume newWord video


addVideo : (Float -> List (Shape Msg)) -> Note -> Note
addVideo animation (Note duration pitch volume word _) =
    Note duration pitch volume word (StartVideo animation)



--addLibreto : List String -> Music -> Result String Music
--addLibreto lyrics (Music instrument timeSignature measures) =
--    let
--        measureLength =
--            measures
--                |> List.map (\(Measure notes) -> List.length notes)
--                |> List.sum
--        lyricsLength =
--            List.length lyrics
--    in
--        if measureLength == lyricsLength then
--            let
--                lenghtsOfNotes =
--                    measures
--                        |> List.map (\(Measure notes) -> List.length notes)
--                flattenedNotes =
--                    measures
--                        |> List.map (\(Measure notes) -> notes)
--                        |> List.concat
--                flattenedLibertoNotes =
--                    List.map2 (\note lyric -> sing lyric note) flattenedNotes lyrics
--                foldWords : Int -> List (List Note) -> List (List Note)
--                foldWords len listOfNotes =
--                    let
--                        lenListNotes =
--                            listOfNotes
--                                |> List.map (\notes -> List.length notes)
--                                |> List.sum
--                    in
--                        List.append listOfNotes [(List.take len (List.drop lenListNotes flattenedLibertoNotes))]
--                result =
--                    lenghtsOfNotes
--                        |> List.foldl foldWords []
--                        |> List.map (\notes -> Measure notes)
--            in
--                Ok (Music instrument timeSignature [result]) -- TODO: fix addlibreto for parallel music
--        else
--            Err "Unable to add lyrics: number of words do not match number of notes"
-- next step:  draw notes, conventionally or with rectangles
--  probably want List.indexedMap
--  (Morton Srobotnik - Making Music)


raise : Note -> Note
raise (Note d pitch volume word video) =
    Note d (raisePitch pitch) volume word video


raiseOctave : Note -> Note
raiseOctave (Note d pitch volume word video) =
    Note d
        (case pitch of
            Rest ->
                Rest

            OctaveUp p ->
                raiseOctave (Note d p volume word video)
                    |> getPitch
                    |> OctaveUp

            OctaveDown p ->
                p

            _ ->
                OctaveUp pitch
        )
        volume
        word
        video


lowerOctave : Note -> Note
lowerOctave (Note d pitch volume word video) =
    Note d
        (case pitch of
            Rest ->
                Rest

            OctaveUp p ->
                p

            OctaveDown p ->
                lowerOctave (Note d p volume word video)
                    |> getPitch
                    |> OctaveDown

            _ ->
                OctaveDown pitch
        )
        volume
        word
        video


raisePitch : Pitch -> Pitch
raisePitch pitch =
    case pitch of
        Do ->
            Re

        Re ->
            Mi

        Mi ->
            Fa

        Fa ->
            Sol

        Sol ->
            La

        La ->
            Ti

        Ti ->
            OctaveUp Do

        Rest ->
            Rest

        OctaveUp p ->
            OctaveUp (raisePitch p)

        OctaveDown p ->
            OctaveDown (raisePitch p)



--makeMeasure : TimeSignature -> List Note -> Result String Measure
--makeMeasure timeSig notes
--  = case timeSig of
--      FourFour -> if duration notes == 1 then
--                    Ok (Measure notes)
--                  else
--                    Err "Wrong number of notes in measure."
--      _ -> Err "Only 4/4 supported!"


raiseMeasure (Measure notes) =
    Measure <| List.map raise notes


type alias NoteRecord =
    { duration : Duration
    , pitch : Pitch
    , volume : Volume
    , lyric : String
    , video : VideoFrame
    }


getNoteRecord : Note -> NoteRecord
getNoteRecord (Note duration pitch volume lyric video) =
    { duration = duration
    , pitch = pitch
    , volume = volume
    , lyric = lyric
    , video = video
    }


seqNotesToMEvents : Time -> Tempo -> List Note -> List MusicEvent
seqNotesToMEvents currentTime tempo notes =
    let
        metro =
            60 / (toFloat tempo.bpm * 0.25)
    in
    case notes of
        [] ->
            []

        x :: xs ->
            let
                noteRecord =
                    getNoteRecord x

                clampedVolume =
                    clamp -1.0 1.0 noteRecord.volume
            in
            MusicEvent currentTime noteRecord.pitch (noteRecord.duration * metro) clampedVolume noteRecord.lyric
                :: seqNotesToMEvents (currentTime + noteRecord.duration * metro) tempo xs


merge : (a -> comparable) -> List a -> List a -> List a
merge compare list1 list2 =
    case ( list1, list2 ) of
        ( [], ys ) ->
            ys

        ( xs, [] ) ->
            xs

        ( x :: xs, y :: ys ) ->
            if compare x < compare y then
                x :: merge compare xs (y :: ys)

            else
                y :: merge compare ys (x :: xs)


musicToMEvents : Time -> Tempo -> Music -> List MusicEvent
musicToMEvents currentTime tempo (Music instrument timeSig listOfListOfMeasures) =
    let
        measureListToMEvents : List Measure -> List MusicEvent
        measureListToMEvents mList =
            let
                notes =
                    mList
                        |> List.map (\(Measure ntsList) -> ntsList)
                        |> List.foldr (\x y -> x ++ y) []
            in
            seqNotesToMEvents currentTime tempo notes

        mEventsList =
            listOfListOfMeasures
                |> List.map measureListToMEvents
                |> List.foldl (merge .time) []
    in
    mEventsList


getMusicIntstrument : Music -> Instrument
getMusicIntstrument (Music instrument _ _) =
    instrument



-- see: https://github.com/surikov/webaudiofont#catalog-of-instruments


instrumentToMIDINumber : Instrument -> Int
instrumentToMIDINumber instrument =
    case instrument of
        Piano ->
            5

        AcousticNylonGuitar ->
            248

        AcousticSteelGuitar ->
            262

        Bass ->
            369

        Violin ->
            452

        Trumpet ->
            620

        Sitar ->
            1124


perform : Tempo -> Music -> Encode.Value
perform tempo music =
    Encode.object
        [ ( "instrument"
          , music
                |> getMusicIntstrument
                |> instrumentToMIDINumber
                |> Encode.int
          )
        , ( "events", Encode.list encode (musicToMEvents 0 tempo music) )
        ]


port play : Encode.Value -> Cmd msg


pitchToMIDINumber : Pitch -> Int
pitchToMIDINumber pitch =
    case pitch of
        Do ->
            60

        Re ->
            62

        Mi ->
            64

        Fa ->
            65

        Sol ->
            67

        La ->
            69

        Ti ->
            71

        Rest ->
            60

        OctaveUp lowerPitch ->
            pitchToMIDINumber lowerPitch + 12

        OctaveDown higherPitch ->
            pitchToMIDINumber higherPitch - 12


encode : MusicEvent -> Encode.Value
encode event =
    Encode.object
        [ ( "time", Encode.float event.time )
        , ( "pitch", Encode.int (pitchToMIDINumber event.pitch) )
        , ( "duration", Encode.float event.duration )
        , ( "volume", Encode.float event.volume )
        , ( "lyric", Encode.string event.lyric )
        ]


treble =
    group
        [ curve ( -0.728, 3.7325 ) [ Pull ( -3.144, 9.1121 ) ( -3.641, 15.931 ), Pull ( -4.56, 26.939 ) ( 0, 34.867 ), Pull ( 3.9463, 40.287 ) ( 6.3726, 36.506 ), Pull ( 8.3893, 33.411 ) ( 9.2859, 30.315 ), Pull ( 11.545, 19.765 ) ( 9.2859, 12.654 ), Pull ( 7.2181, 4.9374 ) ( 0.9103, -0.819 ), Pull ( -6.496, -5.883 ) ( -10.74, -13.74 ), Pull ( -16.4, -22.52 ) ( -11.1, -29.58 ), Pull ( -4.969, -39.51 ) ( 7.6472, -36.32 ), Pull ( 16.67, -31.04 ) ( 11.652, -20.84 ), Pull ( 8.0968, -15.15 ) ( 1.8207, -17.38 ), Pull ( -7.145, -23.93 ) ( 0.7283, -31.04 ), Pull ( 2.6103, -31.93 ) ( 1.0924, -31.95 ), Pull ( -2.411, -31.72 ) ( -4.916, -27.58 ), Pull ( -9.301, -20.81 ) ( -2.366, -12.83 ), Pull ( 4.4608, -7.69 ) ( 11.288, -11.74 ), Pull ( 20.221, -19.21 ) ( 17.115, -29.4 ), Pull ( 13.768, -36.71 ) ( 5.4623, -38.5 ), Pull ( -4.83, -40.63 ) ( -12.56, -31.04 ), Pull ( -19.93, -22.06 ) ( -16.02, -12.29 ), Pull ( -13.54, -3.205 ) ( -4.551, 4.2788 ), Pull ( 1.3993, 9.6494 ) ( 6.1906, 16.66 ), Pull ( 10.506, 28.81 ) ( 5.4623, 29.041 ), Pull ( 1.5869, 29.399 ) ( -0.728, 21.758 ), Pull ( -2.98, 13.581 ) ( 0.7283, 6.6458 ), Pull ( 0.2731, 5.4623 ) ( -0.728, 3.7325 ) ]
            |> filled black
        , curve ( -1.092, 1.7297 ) [ Pull ( 0.6372, -6.463 ) ( 2.3669, -14.65 ), Pull ( 4.7339, -25.94 ) ( 7.1009, -37.23 ), Pull ( 7.8293, -40.6 ) ( 8.5576, -43.97 ), Pull ( 10.591, -53.93 ) ( 7.4651, -56.53 ), Pull ( 1.1697, -62.53 ) ( -4.005, -57.44 ), Pull ( 3.8192, -55.72 ) ( 0.3641, -48.15 ), Pull ( -3.459, -43.81 ) ( -7.283, -47.43 ), Pull ( -11.19, -51.7 ) ( -8.739, -55.98 ), Pull ( -6.887, -59.99 ) ( -1.274, -61.45 ), Pull ( 6.4788, -62.42 ) ( 9.8321, -55.8 ), Pull ( 11.436, -51.24 ) ( 10.56, -45.61 ), Pull ( 9.65, -41.42 ) ( 8.7396, -37.23 ), Pull ( 6.6458, -26.94 ) ( 4.5519, -16.66 ), Pull ( 2.6401, -6.554 ) ( 0.7283, 3.5504 ), Pull ( 0, 2.9132 ) ( -1.092, 1.7297 ) ]
            |> filled black
        ]
        |> scale 0.4


bass =
    group
        [ curve ( -47.15, -61.17 ) [ Pull ( -57.6, -65.4 ) ( -48.05, -57.55 ), Pull ( -39.34, -50.77 ) ( -30.64, -43.98 ), Pull ( -17.2, -33.34 ) ( -8.254, -19.33 ), Pull ( 2.7277, 0.3976 ) ( 1.4699, 21.371 ), Pull ( -0.576, 33.943 ) ( -10.74, 39.915 ), Pull ( -20.86, 44.503 ) ( -32.9, 38.332 ), Pull ( -41.86, 32.658 ) ( -40.81, 27.024 ), Pull ( -41.19, 25.187 ) ( -36.97, 24.989 ), Pull ( -31.2, 25.87 ) ( -25.44, 23.632 ), Pull ( -15.44, 16.251 ) ( -22.72, 7.3498 ), Pull ( -32.11, 1.5783 ) ( -41.49, 5.7667 ), Pull ( -52.08, 12.175 ) ( -49.18, 21.823 ), Pull ( -48.59, 30.504 ) ( -39.91, 38.106 ), Pull ( -28.87, 47.016 ) ( -14.58, 46.247 ), Pull ( -0.748, 46.518 ) ( 10.289, 36.749 ), Pull ( 20.858, 25.074 ) ( 20.466, 9.159 ), Pull ( 19.711, -4.862 ) ( 9.8374, -18.88 ), Pull ( -2.467, -34.49 ) ( -20.69, -46.02 ), Pull ( -33.01, -53.14 ) ( -47.15, -61.17 ) ]
            |> filled black
        , circle 8
            |> filled black
            |> move ( 35, 27 )
        , circle 8
            |> filled black
            |> move ( 35, -2 )
        ]


wholeN =
    group
        [ oval 50 40
            |> filled black
            |> subtract (oval 23 35 |> ghost |> rotate (degrees 30))
        ]
        |> scale 0.11


halfN =
    group
        [ oval 23 15
            |> filled black
            |> rotate (degrees 25)
            |> subtract (oval 19 7 |> ghost |> rotate (degrees 30))
        , rect 2 60 |> filled black |> move ( 9.9, 33 )
        ]
        |> scale 0.28


quarterN =
    group
        [ oval 23 15 |> filled black |> rotate (degrees 25)
        , rect 2 60 |> filled black |> move ( 9.9, 33 )
        ]
        |> scale 0.28


eighthN =
    group
        [ oval 23 15 |> filled black |> rotate (degrees 25)
        , rect 2 60 |> filled black |> move ( 9.9, 33 )
        , flag |> move ( -12.3, 14.8 )
        ]
        |> scale 0.28

sixteenthN =
    group
         [ oval 23 15 |> filled black |> rotate (degrees 25)
         , rect 2 60 |> filled black |> move (9.9,33)
         , flag2 |> move (9.2,32)
         ]
        |> scale 0.28


flag2 = group 
 [curve (-0.184,55.121) [Pull (1.4797,55.028) (3.1445,54.936),Pull (2.1884,43.685) (16.832,34.034),Pull (28.291,23.611) (30.150,9.9884),Pull (30.225,4.4994) (28.300,-2.589),Pull (34.040,-14.42) (29.780,-26.26),Pull (27.745,-30.70) (25.710,-35.14),Pull (24.786,-34.77) (23.861,-34.40),Pull (32.923,-14.71) (24.786,-4.624),Pull (18.365,2.2520) (3.1445,10.728),Pull (1.9421,10.450) (0.7398,10.173),Pull (0.8323,23.491) (0.9248,36.809),Pull (1.7572,35.606) (2.5895,34.404),Pull (3.6508,22.211) (18.312,9.6184),Pull (23.781,5.0867) (26.450,0.5549),Pull (28.764,12.947) (16.277,25.341),Pull (10.526,30.765) (2.7745,34.589),Pull (1.5722,35.976) (0.3699,37.364),Pull (0.5549,45.502) (-0.184,55.121)]
 |> filled black |> scale 0.55] 


flag =
    group
        [ curve ( 23.088, 48.202 ) [ Pull ( 23.493, 48.202 ) ( 23.898, 48.202 ), Pull ( 26.854, 38.301 ) ( 33.89, 32 ), Pull ( 39.335, 26.123 ) ( 39.021, 18.767 ), Pull ( 38.575, 12.972 ) ( 36.05, 8.7763 ), Pull ( 35.24, 8.5063 ) ( 34.43, 8.2362 ), Pull ( 34.565, 8.9113 ) ( 34.7, 9.5864 ), Pull ( 36.77, 15.057 ) ( 36.32, 22.008 ), Pull ( 34.534, 31.649 ) ( 24.708, 32.81 ), Pull ( 23.898, 32.945 ) ( 23.088, 33.08 ), Pull ( 23.223, 40.236 ) ( 23.088, 48.202 ) ]
            |> filled black
        ]


wholeR =
    group
        [ rect 14 5 |> filled black |> move ( 0, 6 )
        ]
        |> scale 0.58


halfR =
    group
        [ rect 14 5 |> filled black |> move ( 0, 2.5 )
        ]
        |> scale 0.58


quarterR =
    group
        [ curve ( 34.16, 30.919 ) [ Pull ( 32.895, 32.009 ) ( 33.35, 30.379 ), Pull ( 35.49, 26.919 ) ( 35.51, 24.978 ), Pull ( 33.62, 22.388 ) ( 31.729, 20.118 ), Pull ( 34.16, 16.877 ) ( 36.59, 13.637 ), Pull ( 26.455, 13.091 ) ( 36.32, 4.1856 ), Pull ( 30.821, 12.051 ) ( 40.641, 10.396 ), Pull ( 38.751, 12.962 ) ( 36.86, 15.527 ), Pull ( 35.465, 20.288 ) ( 39.831, 23.088 ), Pull ( 37.4, 26.599 ) ( 34.16, 30.919 ) ]
            |> filled black
        ]
        |> move ( -35, -17 )
        |> scale 0.48


eighthR =
    group
        [ curve ( 7.6962, 33.35 ) [ Pull ( 9.4514, 34.745 ) ( 11.206, 32.54 ), Pull ( 1.4852, -0.675 ) ( -8.236, -33.89 ), Pull ( -14.57, -37.5 ) ( -17.14, -34.16 ), Pull ( -7.966, -8.371 ) ( 1.2151, 17.417 ), Pull ( -19.61, 9.0579 ) ( -27.4, 20.658 ), Pull ( -33.47, 31.949 ) ( -20.65, 37.4 ), Pull ( -14.68, 39.81 ) ( -9.586, 33.62 ), Pull ( -6.726, 28.894 ) ( -6.345, 24.168 ), Pull ( -3.745, 20.268 ) ( 0.135, 24.168 ), Pull ( 3.6455, 28.354 ) ( 7.6962, 33.35 ) ]
            |> filled black
        ]
        |> scale 0.13


sixteenthR =
    group
        [
          curve (-14.56,-50.34) [Pull (-12.38,-50.43) (-10.19,-50.52),Pull (2.0028,-7.465) (14.201,35.596),Pull (13.109,35.687) (12.017,35.778),Pull (1.4029,22.219) (-2.731,25.581),Pull (-0.196,33.321) (-5.462,37.780),Pull (-10.42,41.367) (-16.38,37.234),Pull (-21.48,32.268) (-18.38,25.581),Pull (-15.58,20.826) (-8.375,19.391),Pull (-0.621,19.356) (6.3726,23.761),Pull (2.9132,11.379) (-0.546,-1.001),Pull (-9.179,-8.617) (-11.65,-5.553),Pull (-9.034,0.4241) (-13.65,6.2816),Pull (-19.39,11.039) (-25.12,5.9174),Pull (-31.35,-1.139) (-25.85,-7.556),Pull (-20.10,-13.34) (-10.19,-10.65),Pull (-6.181,-8.981) (-2.366,-7.192),Pull (-8.284,-28.40) (-14.56,-50.34)]
          |> filled black
          ,
          circle 9.6 |> filled black |> move (-10.2,30)
          ,
          circle 9.6 |> filled black |> move (-19,-1)
        ]
        |> scale 0.13


addDot : Int -> List (Shape Msg)
addDot num =
    case num of
        0 ->
            []

        _ ->
            (circle 1 |> filled black |> move ( 3.0 + 2.5 * toFloat num, 0 )) :: addDot (num - 1)


score =
    group
        [ rect 192 0.5 |> filled black |> move ( 0, 10 )
        , rect 192 0.5 |> filled black |> move ( 0, 5 )
        , rect 192 0.5 |> filled black
        , rect 192 0.5 |> filled black |> move ( 0, -5 )
        , rect 192 0.5 |> filled black |> move ( 0, -10 )
        ]



tsOfMusic =
    let
        ts = (\(Music _ a _ )-> a ) myMusic
    in
    case ts of
        FourFour -> ts44
        ThreeFour -> ts34
        ThreeEighths -> ts38
        TwoFour -> ts24
        SixEight -> ts68
        TwoTwo -> ts22
        FiveEight -> ts58



ts44 =
    group
        [ text "4" |> size 14 |> filled black, text "4" |> size 14 |> filled black |> move ( 0, -10 ) ]


ts34 =
    group
        [ text "3" |> size 14 |> filled black, text "4" |> size 14 |> filled black |> move ( 0, -10 ) ]


ts38 =
    group
        [ text "3" |> size 14 |> filled black, text "8" |> size 14 |> filled black |> move ( 0, -10 ) ]


ts24 =
    group
        [ text "2" |> size 14 |> filled black, text "4" |> size 14 |> filled black |> move ( 0, -10 ) ]


ts68 =
    group
        [ text "6" |> size 14 |> filled black, text "8" |> size 14 |> filled black |> move ( 0, -10 ) ]


ts58 =
    group
        [ text "5" |> size 14 |> filled black, text "8" |> size 14 |> filled black |> move ( 0, -10 ) ]


ts22 =
    group
        [ text "2" |> size 14 |> filled black, text "2" |> size 14 |> filled black |> move ( 0, -10 ) ]


scanner : Shape Msg
scanner =
    group
        [ rect (noteSpacing / 2) 40 |> filled red |> makeTransparent 0.5 ]


bar : Shape Msg
bar =
    group
        [ rect 0.5 20 |> filled black ]



-----------------------------------------------------
--------------bouncing ball--------------------------
-----------------------------------------------------


bouncingBall : Color -> Shape Msg
bouncingBall color =
    group
        [ circle 2 |> filled color ]


changeColor : Float -> Color
changeColor height =
    if height <= 4 then
        yellow

    else
        black


noteArray =
    Array.fromList notesList



-- * bouncing ball : uniform motion
-- time:time from the beginning of this note
-- dur: note duration


moveBouncingBall note nxtNote time dur =
    let
        startHeight =
            -30 + 7

        -- moving from the height of libretto
        movingHeight =
            pitchHeight (getPitch note) + 20

        curHeight =
            case movingHeight > 60 of
                True ->
                    if time <= dur then
                        30 - 0.5 * (2 * 30 / (dur ^ 2)) * (time ^ 2)

                    else
                        0

                False ->
                    case getPitch nxtNote of
                        Rest ->
                            if (\(Note d _ _ _ _) -> d) nxtNote == 0 then
                                0

                            else if time <= dur then
                                30 - 0.5 * (2 * 30 / (dur ^ 2)) * ((dur - time) ^ 2)

                            else
                                --0
                                30 - 0.5 * (2 * 30 / (dur ^ 2)) * ((dur - time) ^ 2)

                        _ ->
                            if time <= dur / 2 then
                                movingHeight - 0.5 * acc * ((dur / 2 - time) ^ 2)

                            else if time > dur / 2 && time <= dur then
                                movingHeight - 0.5 * acc * ((time - dur / 2) ^ 2)

                            else
                                0

        acc =
            2 * movingHeight / ((dur / 2) ^ 2)
    in
    group
        [ bouncingBall (changeColor curHeight)
            |> move
                ( 0
                , startHeight
                    + curHeight
                )

        {- } ,
           (text (String.fromInt (round curHeight) )
           |> size 14
           |> filled black
           |> move (0,50))
        -}
        ]


getPitch : Note -> Pitch
getPitch note =
    (\(Note _ p _ _ _) -> p) note


getPastNumOfRest i array =
    if i > 1 then
        case Array.get (i - 1) array of
            Nothing ->
                0

            Just a ->
                case getPitch a of
                    Rest ->
                        1 + getPastNumOfRest (i - 1) array

                    _ ->
                        0

    else
        0



-- calculate the rest note number from current note


getNumOfRest idx array =
    case Array.get idx array of
        Just a ->
            case getPitch a of
                Rest ->
                    1 + getNumOfRest (idx + 1) array

                _ ->
                    0

        Nothing ->
            0



-- ball meet multiple rest
-- time is from the begining of jumping up


moveBouncingBallRest time totalDur totalNumOfRest =
    let
        startHeight =
            -30 + 7

        curHeight =
            if time < totalDur / 2 then
                10 * time

            else
                10 * (totalDur - time)
    in
    group
        [ bouncingBall (changeColor curHeight)
            |> move
                ( 0, startHeight + curHeight )

        {- } ,
           (text (String.fromInt (round totalNumOfRest+1) )
           |> size 14
           |> filled black
           |> move (0,50))
        -}
        ]


emLibretto note time dur speed =
    group
        [ (if time < dur then
            text ((\(Note _ _ _ libretto _) -> libretto) note)

           else
            text ""
          )
            |> size 5
            |> bold
            |> centered
            |> filled red
            |> move
                ( if time < dur then
                    -speed * time

                  else
                    0
                , -25
                )
        ]



---------------------------------------------------
------------------ baton --------------------------
---------------------------------------------------


batonColour =
    white


batonModel : Model -> Shape Msg
batonModel model =
    group
        [ batonMotion myFun (model.time - Maybe.withDefault 0 model.startTime) pointList duraList ]


gcd : Int -> Int -> Int
gcd n1 n2 =
    let
        tu =
            if n1 <= n2 then
                ( remainderBy n1 n2, n1 )

            else
                ( remainderBy n2 n1, n2 )

        r =
            Tuple.first tu

        smallNum =
            Tuple.second tu
    in
    if r == 0 then
        smallNum

    else
        gcd smallNum r



-- baton motion track points


pointList =
    let
        ts = (\(Music _ a _ )-> a ) myMusic
    in
    selectBaton <|
        case ts of
            FourFour -> (4,4)
            ThreeFour -> (3,4)
            ThreeEighths -> (3,8)
            TwoFour -> (2,4)
            SixEight -> (6,8)
            TwoTwo -> (2,2)
            FiveEight -> (5,8)


selectBaton : (Int, Int) -> List ( Float, Float )
selectBaton (b1, b2) =
    let
        gcd_b1_b2 =
            gcd b1 b2
    in
    case ( b1 // gcd_b1_b2, b2 // gcd_b1_b2 ) of
        ( 1, 2 ) ->
            cp24or68

        ( 2, 3 ) ->
            cp24or68

        ( 3, 4 ) ->
            cp34

        ( 1, 1 ) ->
            cp44

        _ ->
            []


cp24or68 =
    [ ( -15.59, 53.647 ), ( -15.32, 14.386 ), ( -15.32, 12.504 ), ( -15.32, 10.084 ), ( -15.32, 7.3949 ), ( -15.32, 1.21 ), ( -15.32, 1.21 ), ( -15.05, -2.554 ), ( -15.05, -7.394 ), ( -14.78, -9.008 ), ( -14.78, -12.77 ), ( -14.78, -15.73 ), ( -14.52, -17.88 ), ( -14.25, -20.03 ), ( -13.71, -22.45 ), ( -12.63, -26.48 ), ( -12.1, -29.17 ), ( -11.02, -31.86 ), ( -10.21, -34.28 ), ( -8.873, -36.97 ), ( -7.798, -39.66 ), ( -6.991, -41.27 ), ( -5.647, -42.89 ), ( -3.495, -44.5 ), ( -1.882, -46.11 ), ( 0.8067, -48 ), ( 3.4957, -49.34 ), ( 6.4537, -49.88 ), ( 9.4117, -49.88 ), ( 11.831, -49.88 ), ( 13.983, -49.88 ), ( 16.403, -49.61 ), ( 17.478, -49.34 ), ( 20.436, -48.26 ), ( 22.319, -46.38 ), ( 23.932, -45.31 ), ( 25.546, -43.96 ), ( 27.159, -41.81 ), ( 28.504, -40.2 ), ( 30.386, -37.78 ), ( 31.462, -35.89 ), ( 32.268, -34.28 ), ( 33.344, -32.13 ), ( 34.151, -30.78 ), ( 34.957, -28.9 ), ( 35.764, -26.75 ), ( 36.571, -24.87 ), ( 32.806, -29.44 ), ( 30.924, -31.86 ), ( 28.773, -34.28 ), ( 26.084, -35.09 ), ( 22.857, -36.16 ), ( 19.899, -36.43 ), ( 16.941, -36.43 ), ( 13.445, -35.89 ), ( 10.487, -34.28 ), ( 8.3361, -32.67 ), ( 6.1848, -30.78 ), ( 3.7647, -27.56 ), ( 2.4201, -25.41 ), ( 0.8067, -22.45 ), ( -1.075, -18.68 ), ( -2.42, -15.73 ), ( -4.84, -10.35 ), ( -5.915, -5.781 ), ( -7.26, -0.672 ), ( -8.605, 4.7058 ), ( -9.68, 8.4705 ), ( -10.21, 11.966 ), ( -11.02, 17.882 ), ( -11.56, 22.184 ), ( -12.1, 26.487 ), ( -12.9, 31.596 ), ( -13.17, 35.092 ), ( -13.71, 38.319 ), ( -14.25, 41.815 ), ( -14.52, 44.773 ), ( -14.78, 47.462 ), ( -15.59, 53.647 ) ]


cp34 =
    [ ( -32.59, 49.98 ), ( -30.4, -40.14 ), ( -30.4, -40.87 ), ( -30.58, -41.6 ), ( -30.77, -42.69 ), ( -30.95, -43.24 ), ( -31.13, -44.15 ), ( -31.31, -44.69 ), ( -31.49, -44.88 ), ( -31.49, -44.88 ), ( -31.86, -46.15 ), ( -32.95, -46.52 ), ( -34.59, -47.06 ), ( -36.59, -46.88 ), ( -36.96, -46.52 ), ( -37.68, -45.79 ), ( -38.6, -44.51 ), ( -38.96, -43.6 ), ( -39.51, -42.51 ), ( -39.51, -41.05 ), ( -39.51, -39.96 ), ( -39.51, -38.32 ), ( -38.96, -37.05 ), ( -38.6, -35.77 ), ( -37.68, -34.86 ), ( -36.96, -33.77 ), ( -36.05, -33.22 ), ( -35.5, -33.04 ), ( -35.32, -32.86 ), ( -34.77, -32.86 ), ( -34.77, -32.86 ), ( -34.59, -32.68 ), ( -33.5, -32.31 ), ( -33.5, -32.31 ), ( -32.04, -31.95 ), ( -31.86, -31.95 ), ( -31.68, -31.95 ), ( -31.68, -31.95 ), ( -30.58, -31.59 ), ( -28.95, -31.4 ), ( -28.4, -31.4 ), ( -27.31, -31.22 ), ( -23.85, -31.4 ), ( -18.93, -31.95 ), ( -16.56, -32.68 ), ( -14.93, -32.86 ), ( -13.1, -33.77 ), ( -11.1, -34.5 ), ( -8.557, -35.23 ), ( -7.1, -36.32 ), ( -4.916, -37.23 ), ( -2.366, -38.5 ), ( -0.364, -39.78 ), ( 2.7311, -41.24 ), ( 6.3726, -42.51 ), ( 12.381, -45.06 ), ( 15.658, -45.79 ), ( 19.846, -47.06 ), ( 24.216, -47.06 ), ( 27.493, -46.88 ), ( 29.496, -46.52 ), ( 31.317, -45.24 ), ( 32.227, -44.51 ), ( 33.684, -43.24 ), ( 34.958, -41.6 ), ( 36.233, -40.14 ), ( 37.871, -37.05 ), ( 39.328, -34.13 ), ( 40.967, -29.58 ), ( 42.241, -25.03 ), ( 44.426, -17.2 ), ( 41.695, -19.39 ), ( 40.603, -20.66 ), ( 38.054, -22.48 ), ( 35.869, -24.3 ), ( 33.32, -26.67 ), ( 31.499, -27.94 ), ( 28.403, -29.04 ), ( 26.036, -29.4 ), ( 22.395, -29.4 ), ( 17.843, -28.85 ), ( 12.381, -26.67 ), ( 8.7396, -24.48 ), ( 4.0056, -21.02 ), ( 0.9103, -17.93 ), ( -1.82, -14.11 ), ( -5.28, -9.376 ), ( -18.02, 15.385 ), ( -32.59, 49.98 ) ]


cp44 =
    [ ( 23.487, 35.666 )
    , ( 23.735, 15.782 )
    , ( 23.487, -1.615 )
    , ( 23.487, -20.5 )
    , ( 23.487, -42.62 )
    , ( 20.007, -35.41 )
    , ( 12.8, -34.42 )
    , ( -5.095, -43.12 )
    , ( -10.06, -39.14 )
    , ( -1.366, -31.44 )
    , ( 12.8, -27.46 )
    , ( 32.186, -31.68 )
    , ( 53.561, -41.13 )
    , ( 64.745, -30.94 )
    , ( 52.069, -33.92 )
    , ( 39.642, -21.74 )
    , ( 23.487, 35.666 )
    ]


batonMotionTrack : List ( Float, Float ) -> List (Shape userMsg)
batonMotionTrack list =
    case list of
        first :: (second :: rest) ->
            (line first second |> outlined (solid 0.5) red)
                :: batonMotionTrack (second :: rest)

        _ :: [] ->
            []

        [] ->
            []



-- using time to determine the x position


batonMotion : (Float -> Float) -> Float -> List ( Float, Float ) -> List Float -> Shape userMsg
batonMotion f time points duras =
    let
        getX t l d st =
            case l of
                ( x1, y1 ) :: (( x2, y2 ) :: rest) ->
                    let
                        curDur =
                            case List.head d of
                                Just a ->
                                    a

                                Nothing ->
                                    1
                    in
                    if t - st > curDur then
                        getX t (( x2, y2 ) :: rest) (List.drop 1 d) (curDur + st)

                    else if x1 /= x2 then
                        x1 + (x2 - x1) * ((t - st) / curDur)

                    else
                        x1

                _ ->
                    if List.length d > 0 then
                        getX t points d st

                    else
                        case List.head l of
                            Just ( a, _ ) ->
                                a

                            Nothing ->
                                0

        x =
            getX time points duras 0

        y =
            f time
    in
    group
        [ circle 3
            |> filled batonColour
            |> move ( x, y )

        {- ,
           text (String.fromInt (round time))
           |> filled red
           |> move (-50,20)
           ,
           text (String.fromInt (round x))
           |> filled red
           |> move (-50,0)
           ,
           text (String.fromInt (round y))
           |> filled red
           |> move (-50,-20)
        -}
        ]



-- generate the list of pair of duration and equation of each line
-- (duration , equation)


sequenceAnimations : List ( Float, Float -> anytype ) -> (Float -> anytype) -> Float -> anytype
sequenceAnimations intervals finalAnimation time =
    case intervals of
        ( durationSequence, animation ) :: rest ->
            -- time starting from the beginning, and subtracting the duration every time
            -- pass the time starting from entering current line to the animation function
            if time <= durationSequence then
                animation time

            else
                sequenceAnimations
                    rest
                    finalAnimation
                    (time - durationSequence)

        [] ->
            finalAnimation time



-- input : point1, point2 , x position (if x1/= x2)
-- output: y position


linearEquation : ( Float, Float ) -> ( Float, Float ) -> Float -> Float
linearEquation ( x1, y1 ) ( x2, y2 ) xt =
    if x1 /= x2 then
        (y2 - y1) / (x2 - x1) * xt + (y1 * x2 - y2 * x1) / (x2 - x1)

    else
        xt


noteNumList = 
    let
        measures = List.concat ( (\(Music _ _ v) -> v) myMusic)

        getNoteNum ms =
            case ms of
                m :: r ->
                    (List.length ((\(Measure l)-> l) m) ) :: getNoteNum r
                [] -> 
                    []

        a = getNoteNum measures
    in
        List.concat (List.map2 List.repeat a a)


beat =
    1 / noteType

-- get the accurate beat of every note
getBeats m =
    case m of
        n :: rest ->
            let
                d = (\(Note du _ _ _ _) -> du) n
                dotN = extractDots d
                calBeat a b =
                    case a of
                        0 -> b
                        _ -> b / toFloat (2 ^ a) + calBeat (a-1) b
            in
            if d >= beat * 0.5 && d < beat then
                (calBeat dotN 0.5) :: getBeats rest
            else if d >= beat * 0.25 && d < beat * 0.5 then
                (calBeat dotN 0.25) :: getBeats rest
            else if d >= beat * 0.125 && d < beat * 0.25  then
                (calBeat dotN 0.125):: getBeats rest
            else if d >= beat * 0.0625 && d < beat * 0.125 then
                (calBeat dotN 0.0625) :: getBeats rest
            else if d >= beat && d < 2 * beat then
                (calBeat dotN 1) :: getBeats rest
            else if d >= 2 * beat && d < 4 * beat then
                (calBeat dotN 2) :: getBeats rest
            else if d >= 4 * beat && d < 8 * beat then
                (calBeat dotN 4) :: getBeats rest
            else
                0 :: getBeats rest
        [] ->
            []


totalBeatsList = getBeats notesList


durationBaton: List Float -> List Int -> List Float -> List Float
durationBaton listOfDuration listOfNoteNum listOfBeats=
    case listOfDuration of
        d :: rest ->
            let
                noteNum = Maybe.withDefault 0 (List.head listOfNoteNum)
                b = Maybe.withDefault 0 (List.head listOfBeats)
                -- the parts of a duration divided
                divisor = b * noteType
            in
            List.repeat (round divisor) ( d /divisor )
                ++ durationBaton rest (List.drop 1 listOfNoteNum) (List.drop 1 listOfBeats)

        [] ->
            []


-- get the duration of baton moving among points

duraList = durationBaton durationList noteNumList totalBeatsList



-- produce a list of pair of duration and function of mapping time to y position
-- animation function will get the (t) time from entering current line


durationAnimation : List ( Float, Float ) -> List Float -> List ( Float, Float -> Float )
durationAnimation points duras =
    case points of
        ( x1, y1 ) :: (( x2, y2 ) :: rest) ->
            let
                curDur =
                    case List.head duras of
                        Just a ->
                            a

                        Nothing ->
                            1
            in
            if x2 /= x1 then
                ( curDur
                , \t ->
                    linearEquation
                        ( x1, y1 )
                        ( x2, y2 )
                        (x1 + (x2 - x1) * (t / curDur))
                )
                    :: durationAnimation (( x2, y2 ) :: rest) (List.drop 1 duras)

            else
                ( curDur
                , \t ->
                    y1 + (y2 - y1) * (t / curDur)
                )
                    :: durationAnimation (( x2, y2 ) :: rest) (List.drop 1 duras)

        _ ->
            if List.length duras > 0 then
                durationAnimation pointList duras

            else
                []



-- get the y postion of baton at the current time


myFun : Float -> Float
myFun time =
    sequenceAnimations
        (durationAnimation pointList duraList)
        (\t ->
            (\( _, y ) -> y)
                (case List.head pointList of
                    Just p ->
                        p

                    Nothing ->
                        ( 0, 0 )
                )
        )
        time
