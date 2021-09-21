module Pages.Test exposing (Model, Msg, page)

import Gen.Params.Test exposing (Params)
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (class, placeholder, style, type_, value)
import Html.Events exposing (onInput)
import Page
import Request
import Shared
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type alias Status =
    { baseStats : Int, effortValue : Int, individualValue : Int, value : Maybe Int }


type alias Parameters =
    { hitPoint : Status, attack : Status, defence : Status, spAttack : Status, spDefence : Status, speed : Status }


type alias Model =
    { content : String, level : Int, attack : Status, parameters : Parameters }


init : ( Model, Cmd Msg )
init =
    ( { content = ""
      , level = 50
      , attack = Status 0 0 31 Nothing
      , parameters =
            { hitPoint = Status 0 0 31 Nothing
            , attack = Status 0 0 31 Nothing
            , defence = Status 0 0 31 Nothing
            , spAttack = Status 0 0 31 Nothing
            , spDefence = Status 0 0 31 Nothing
            , speed = Status 0 0 31 Nothing
            }
      }
    , Cmd.none
    )



-- UPDATE


type StatusCategory
    = BaseStats
    | EffortValue
    | IndividualValue


type ParamCategory
    = HitPoint
    | Attack
    | Defence
    | SpAttack
    | SpDefence
    | Speed


type Msg
    = Level String
    | ChangeValue ParamCategory StatusCategory String


calculateStatus : Int -> ParamCategory -> Status -> Status
calculateStatus level paramCategory status =
    let
        newValue =
            case paramCategory of
                HitPoint ->
                    (status.baseStats * 2 + status.individualValue + status.effortValue // 4) * level // 100 + level + 10

                _ ->
                    (status.baseStats * 2 + status.individualValue + status.effortValue // 4) * level // 100 + 5
    in
    { status | value = Just newValue }


updateStatus : StatusCategory -> Int -> Status -> Status
updateStatus category val status =
    case category of
        BaseStats ->
            { status | baseStats = val }

        EffortValue ->
            { status | effortValue = val }

        IndividualValue ->
            { status | individualValue = val }


getStatusFromParameters : ParamCategory -> Parameters -> Status
getStatusFromParameters pc params =
    case pc of
        HitPoint ->
            params.hitPoint

        Attack ->
            params.attack

        Defence ->
            params.defence

        SpAttack ->
            params.spAttack

        SpDefence ->
            params.spDefence

        Speed ->
            params.speed


updateParams : ParamCategory -> StatusCategory -> Int -> Int -> Parameters -> Parameters
updateParams paramCategory statusCategory val level params =
    case paramCategory of
        HitPoint ->
            { params
                | hitPoint =
                    params.hitPoint
                        |> updateStatus statusCategory val
                        |> calculateStatus level paramCategory
            }

        Attack ->
            { params
                | attack =
                    params.attack
                        |> updateStatus statusCategory val
                        |> calculateStatus level paramCategory
            }

        Defence ->
            { params
                | defence =
                    params.defence
                        |> updateStatus statusCategory val
                        |> calculateStatus level paramCategory
            }

        SpAttack ->
            { params
                | spAttack =
                    params.spAttack
                        |> updateStatus statusCategory val
                        |> calculateStatus level paramCategory
            }

        SpDefence ->
            { params
                | spDefence =
                    params.spDefence
                        |> updateStatus statusCategory val
                        |> calculateStatus level paramCategory
            }

        Speed ->
            { params
                | speed =
                    params.speed
                        |> updateStatus statusCategory val
                        |> calculateStatus level paramCategory
            }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newModel =
            case msg of
                Level levelInput ->
                    case validateLevel levelInput of
                        Nothing ->
                            model

                        Just newLevel ->
                            { model | level = newLevel }

                ChangeValue paramCategory statusCategory input ->
                    case validateStatusValue statusCategory input of
                        Nothing ->
                            model

                        Just val ->
                            { model | parameters = updateParams paramCategory statusCategory val model.level model.parameters }
    in
    ( newModel, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "pokelm"
    , body =
        [ div []
            [ viewInput "text" "level" model.content Level
            ]
        , div [] <| viewRowInput HitPoint model
        , div [] <| viewRowInput Attack model
        , div [] <| viewRowInput Defence model
        , div [] <| viewRowInput SpAttack model
        , div [] <| viewRowInput SpDefence model
        , div [] <| viewRowInput Speed model
        ]
    }


viewRowInput : ParamCategory -> Model -> List (Html Msg)
viewRowInput pc model =
    [ viewInput "text" "種族値" (String.fromInt model.attack.baseStats) (ChangeValue pc BaseStats)
    , viewInput "text" "個体値" (String.fromInt model.attack.individualValue) (ChangeValue pc IndividualValue)
    , viewInput "text" "努力値" (String.fromInt model.attack.effortValue) (ChangeValue pc EffortValue)
    , text (resultView <| .value <| getStatusFromParameters pc model.parameters)
    ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    if isValid v then
        input [ type_ t, placeholder p, onInput toMsg ] []

    else
        input [ style "border-color" "red", type_ t, placeholder p, onInput toMsg ] []


resultView : Maybe Int -> String
resultView maybeInt =
    case maybeInt of
        Nothing ->
            ""

        Just val ->
            String.fromInt val


isValid : String -> Bool
isValid string =
    case String.toInt string of
        Just _ ->
            True

        Nothing ->
            if string == "" then
                True

            else
                False


validateLevel : String -> Maybe Int
validateLevel input =
    case String.toInt input of
        Nothing ->
            Nothing

        Just level ->
            if level >= 1 then
                Just level

            else
                Nothing


validateStatusValue : StatusCategory -> String -> Maybe Int
validateStatusValue statusCategory input =
    case String.toInt input of
        Nothing ->
            Nothing

        Just val ->
            case statusCategory of
                EffortValue ->
                    validateEffortValue val

                BaseStats ->
                    Just val

                IndividualValue ->
                    validateIndividualValue val


validateEffortValue : Int -> Maybe Int
validateEffortValue ev =
    if ev <= 252 && ev >= 0 then
        Just ev

    else
        Nothing


validateIndividualValue : Int -> Maybe Int
validateIndividualValue iv =
    if iv <= 31 && iv >= 0 then
        Just iv

    else
        Nothing
