module Types.Parameter exposing (..)

import Types.Status exposing (Status)


type alias Parameters =
    { hitPoint : Status, attack : Status, defence : Status, spAttack : Status, spDefence : Status, speed : Status }


type ParamCategory
    = HitPoint
    | Attack
    | Defence
    | SpAttack
    | SpDefence
    | Speed


updateParam : ParamCategory -> Parameters -> Status -> Parameters
updateParam pc params status =
    case pc of
        HitPoint ->
            { params | hitPoint = status }

        Attack ->
            { params | attack = status }

        Defence ->
            { params | defence = status }

        SpAttack ->
            { params | spAttack = status }

        SpDefence ->
            { params | spDefence = status }

        Speed ->
            { params | speed = status }


accessFieldStatus : ParamCategory -> (Parameters -> Status)
accessFieldStatus pc =
    case pc of
        HitPoint ->
            .hitPoint

        Attack ->
            .attack

        Defence ->
            .defence

        SpAttack ->
            .spAttack

        SpDefence ->
            .spDefence

        Speed ->
            .speed


describe : ParamCategory -> String
describe pc =
    case pc of
        HitPoint ->
            "HP"

        Attack ->
            "こうげき"

        Defence ->
            "ぼうぎょ"

        SpAttack ->
            "とくこう"

        SpDefence ->
            "とくぼう"

        Speed ->
            "すばやさ"
