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


type alias Model =
    { content : String, level : Int, attack : Status }


init : ( Model, Cmd Msg )
init =
    ( { content = "", level = 50, attack = Status 0 0 31 Nothing }, Cmd.none )



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
    | Param ParamCategory StatusCategory String


calculateStatus : Int -> ParamCategory -> Status -> Status
calculateStatus level paramCategory status =
    { status | value = Just case paramCategory of
            HitPoint ->
                (status.baseStats * 2 + status.individualValue + status.effortValue // 4) * level // 100 + level + 10
            _ ->
                (status.baseStats * 2 + status.individualValue + status.effortValue // 4) * level // 100 + 5
    }


updateStatus : StatusCategory -> Int -> Status -> Status
updateStatus category val status =
    case category of
        BaseStats ->
            { status | baseStats = val }

        EffortValue ->
            { status | effortValue = val }

        IndividualValue ->
            { status | individualValue = val }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newModel =
            case msg of
                Level levelInput ->
                    case String.toInt levelInput of
                        Nothing ->
                            model

                        Just newLevel ->
                            { model | level = newLevel }

                Param paramCategory statusCategory valString ->
                    case String.toInt valString of
                        Nothing ->
                            model

                        Just val ->
                            { model | attack = model.attack |> updateStatus statusCategory val |> calculateStatus model.level paramCategory }
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
        , div []
            [ viewInput "text" "種族値" (String.fromInt model.attack.baseStats) (Param HitPoint BaseStats)
            , viewInput "text" "個体値" (String.fromInt model.attack.individualValue) (Param HitPoint IndividualValue)
            , viewInput "text" "努力値" (String.fromInt model.attack.effortValue) (Param HitPoint EffortValue)
            , text (resultView model.attack.value)
            ]
        , div []
            [ viewInput "text" "種族値" (String.fromInt model.attack.baseStats) (Param Attack BaseStats)
            , viewInput "text" "個体値" (String.fromInt model.attack.individualValue) (Param Attack IndividualValue)
            , viewInput "text" "努力値" (String.fromInt model.attack.effortValue) (Param Attack EffortValue)
            , text (resultView model.attack.value)
            ]
        , div []
            [ viewInput "text" "種族値" (String.fromInt model.attack.baseStats) (Param Defence BaseStats)
            , viewInput "text" "個体値" (String.fromInt model.attack.individualValue) (Param Defence IndividualValue)
            , viewInput "text" "努力値" (String.fromInt model.attack.effortValue) (Param Defence EffortValue)
            , text (resultView model.attack.value)
            ]
        , div []
            [ viewInput "text" "種族値" (String.fromInt model.attack.baseStats) (Param SpAttack BaseStats)
            , viewInput "text" "個体値" (String.fromInt model.attack.individualValue) (Param SpAttack IndividualValue)
            , viewInput "text" "努力値" (String.fromInt model.attack.effortValue) (Param SpAttack EffortValue)
            , text (resultView model.attack.value)
            ]
        , div []
            [ viewInput "text" "種族値" (String.fromInt model.attack.baseStats) (Param SpDefence BaseStats)
            , viewInput "text" "個体値" (String.fromInt model.attack.individualValue) (Param SpDefence IndividualValue)
            , viewInput "text" "努力値" (String.fromInt model.attack.effortValue) (Param SpDefence EffortValue)
            , text (resultView model.attack.value)
            ]
        , div []
            [ viewInput "text" "種族値" (String.fromInt model.attack.baseStats) (Param Speed BaseStats)
            , viewInput "text" "個体値" (String.fromInt model.attack.individualValue) (Param Speed IndividualValue)
            , viewInput "text" "努力値" (String.fromInt model.attack.effortValue) (Param Speed EffortValue)
            , text (resultView model.attack.value)
            ]
        ]
    }


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
