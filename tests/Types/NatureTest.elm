module Types.NatureTest exposing (..)

import Expect
import Test exposing (..)
import Types.Nature as Nature exposing (Nature)
import Types.Parameter exposing (ParamCategory(..))


natureTest : Test
natureTest =
    describe "すべての性格補正が表現されているかのテスト"
        [ test "陽気の性格補正が表現されているかのテスト" <|
            \() ->
                let
                    nature =
                        Nature "jolly" "陽気" (Just Speed) (Just SpAttack)
                in
                validateCorrect nature
                    |> Expect.equal ( Just Speed, Just SpAttack )
        ]


type alias NatureCorrect =
    Maybe ParamCategory


validateCorrect : Nature -> ( NatureCorrect, NatureCorrect )
validateCorrect nature =
    Tuple.pair nature.up nature.down


paramList : List ParamCategory
paramList =
    [ Attack, Defence, SpAttack, SpDefence, Speed ]
