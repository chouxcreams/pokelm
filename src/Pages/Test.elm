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


type alias Value =
    { value : Int, input : String }


type alias Status =
    { baseStats : Value, effortValue : Value, individualValue : Value, realNumber : Maybe Int }


type alias Parameters =
    { hitPoint : Status, attack : Status, defence : Status, spAttack : Status, spDefence : Status, speed : Status }


type alias Model =
    { content : String, level : Int, attack : Status, parameters : Parameters }


initValue : Int -> Value
initValue val =
    Value val <| String.fromInt val


initStatus : Status
initStatus =
    Status (initValue 0) (initValue 0) (initValue 31) Nothing


init : ( Model, Cmd Msg )
init =
    ( { content = ""
      , level = 50
      , attack = initStatus
      , parameters =
            { hitPoint = initStatus
            , attack = initStatus
            , defence = initStatus
            , spAttack = initStatus
            , spDefence = initStatus
            , speed = initStatus
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
                    (status.baseStats.value * 2 + status.individualValue.value + status.effortValue.value // 4) * level // 100 + level + 10

                _ ->
                    (status.baseStats.value * 2 + status.individualValue.value + status.effortValue.value // 4) * level // 100 + 5
    in
    { status | realNumber = Just newValue }


updateStatus : StatusCategory -> Int -> Status -> Status
updateStatus category val status =
    case category of
        BaseStats ->
            { status | baseStats = initValue val }

        EffortValue ->
            { status | effortValue = initValue val }

        IndividualValue ->
            { status | individualValue = initValue val }


getParamFieldAccess : ParamCategory -> (Parameters -> Status)
getParamFieldAccess pc =
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
                            { model
                                | parameters =
                                    model.parameters
                                        |> updateParams paramCategory statusCategory val model.level
                            }
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
    [ viewInput "text" "種族値" (String.fromInt model.attack.baseStats.value) (ChangeValue pc BaseStats)
    , viewInput "text" "個体値" (String.fromInt model.attack.individualValue.value) (ChangeValue pc IndividualValue)
    , viewInput "text" "努力値" (String.fromInt model.attack.effortValue.value) (ChangeValue pc EffortValue)
    , text <| resultView <| .realNumber <| getParamFieldAccess pc <| model.parameters
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
