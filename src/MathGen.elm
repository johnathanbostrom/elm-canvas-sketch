module MathGen exposing (Rectangle, randomRectangle, updateNumberScores, NumberScores, getQuestion, initNumberScores)
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

updateNumberScores : Int -> Int -> Bool -> NumberScores -> NumberScores
updateNumberScores multiplicand multiplier correct scores =
    let
        change = 
            if correct then 
                1
            else
                -2

        update i s =
            case i of
               1 -> { s | ones = min (s.ones + change) 20 }
               2 -> { s | twos = min (s.twos + change) 20 }
               3 -> { s | threes = min (s.threes + change) 20 }
               4 -> { s | fours = min (s.fours + change) 20 }
               5 -> { s | fives = min (s.fives + change) 20 }
               6 -> { s | sixes = min (s.sixes + change) 20 }
               7 -> { s | sevens = min (s.sevens + change) 20 }
               8 -> { s | eights = min (s.eights + change) 20 }
               9 -> { s | nines = min (s.nines + change) 20 }
               10 -> { s | tens = min (s.tens + change) 20 }
               _ -> s
    in        
        update multiplicand scores
        |> update multiplier


getQuestion : Int -> NumberScores -> Random.Generator Rectangle 
getQuestion max scores =
    let
        weighted =
            Random.weighted
                (toFloat <| 20 - scores.ones , 1)
                [ (toFloat <| 20 - scores.twos , 2)
                , (toFloat <| 20 - scores.threes , 3)
                , (toFloat <| 20 - scores.fours , 4)
                , (toFloat <| 20 - scores.fives , 5)
                , (toFloat <| 20 - scores.sixes , 6)
                , (toFloat <| 20 - scores.sevens , 7)
                , (toFloat <| 20 - scores.eights , 8)
                , (toFloat <| 20 - scores.nines , 9)
                , (toFloat <| 20 - scores.tens , 10)
                ]
        lg = Debug.log "scores" scores

    in
    Random.map2 Rectangle (Random.int 1 max) weighted



