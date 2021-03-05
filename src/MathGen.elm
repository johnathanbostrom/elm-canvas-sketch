module MathGen exposing (Rectangle, randomRectangle, updateNumberScores, NumberScores, getQuestion, initNumberScores, advanceMultiplicand)
import Random
import Random.List as Random


randomRectangleSide : Int -> Random.Generator Int
randomRectangleSide max =
    Random.int 1 max

randomRectangle : Int -> Random.Generator Rectangle
randomRectangle max =
    Random.map2 Rectangle (randomRectangleSide max) (randomRectangleSide max)


type alias Rectangle =
    { h : Int
    , w : Int
    }


type alias NumberScores =
    { ones : Int
    , twos : Int
    , threes : Int
    , fours : Int
    , fives : Int
    , sixes : Int
    , sevens : Int
    , eights : Int
    , nines : Int
    , tens : Int
    }

initNumberScores : NumberScores
initNumberScores =
    { ones = 10
    , twos = 10
    , threes = 10
    , fours = 10
    , fives = 10
    , sixes = 10
    , sevens = 10
    , eights = 10
    , nines = 10
    , tens = 10
    }

boundedScore score = 
    max score minScore
    |> min maxScore

minScore = 1
maxScore = 50

updateNumberScores : Int -> Int -> Int -> Bool -> NumberScores -> NumberScores
updateNumberScores multiplicand multiplier timeLeft correct scores =
    let
        change = 
            if correct then 
                timeLeft
            else
                -9

        update i s =
            case i of
               1 -> { s | ones = boundedScore (s.ones + change) }
               2 -> { s | twos = boundedScore (s.twos + change) }
               3 -> { s | threes = boundedScore (s.threes + change) }
               4 -> { s | fours = boundedScore (s.fours + change) }
               5 -> { s | fives = boundedScore (s.fives + change) }
               6 -> { s | sixes = boundedScore (s.sixes + change) }
               7 -> { s | sevens = boundedScore (s.sevens + change) }
               8 -> { s | eights = boundedScore (s.eights + change) }
               9 -> { s | nines = boundedScore (s.nines + change) }
               10 -> { s | tens = boundedScore (s.tens + change) }
               _ -> s
    in        
        update multiplicand scores
        |> update multiplier


advanceMultiplicand : Int -> NumberScores -> NumberScores
advanceMultiplicand newMax scores =
    case newMax of
       1 -> { scores | ones = minScore }
       2 -> { scores | twos = minScore }
       3 -> { scores | threes = minScore }
       4 -> { scores | fours = minScore }
       5 -> { scores | fives = minScore }
       6 -> { scores | sixes = minScore }
       7 -> { scores | sevens = minScore }
       8 -> { scores | eights = minScore }
       9 -> { scores | nines = minScore }
       10 -> { scores | tens = minScore }
       _ -> scores

getQuestion : Int -> NumberScores -> Random.Generator Rectangle 
getQuestion max scores =
    let
        weighted =
            Random.weighted
                (toFloat <| boundedScore <| maxScore - scores.ones , 1)
                [ (toFloat <| boundedScore <| maxScore - scores.twos , 2)
                , (toFloat <| boundedScore <| maxScore - scores.threes , 3)
                , (toFloat <| boundedScore <| maxScore - scores.fours , 4)
                , (toFloat <| boundedScore <| maxScore - scores.fives , 5)
                , (toFloat <| boundedScore <| maxScore - scores.sixes , 6)
                , (toFloat <| boundedScore <| maxScore - scores.sevens , 7)
                , (toFloat <| boundedScore <| maxScore - scores.eights , 8)
                , (toFloat <| boundedScore <| maxScore - scores.nines , 9)
                , (toFloat <| boundedScore <| maxScore - scores.tens , 10)
                ]
        lg = Debug.log "scores" scores
        lg2 = Debug.log "weights" 
            <|  [ (toFloat <| boundedScore <|maxScore - scores.twos , 2)
                , (toFloat <| boundedScore <| maxScore - scores.threes , 3)
                , (toFloat <| boundedScore <| maxScore - scores.fours , 4)
                , (toFloat <| boundedScore <| maxScore - scores.fives , 5)
                , (toFloat <| boundedScore <| maxScore - scores.sixes , 6)
                , (toFloat <| boundedScore <| maxScore - scores.sevens , 7)
                , (toFloat <| boundedScore <| maxScore - scores.eights , 8)
                , (toFloat <| boundedScore <| maxScore - scores.nines , 9)
                , (toFloat <| boundedScore <| maxScore - scores.tens , 10)
                ]

    in
    Random.map2 Rectangle (Random.int 1 max) weighted



