module Pages.Test exposing (Model, Msg, ParamCategory(..), Status, page)

import Gen.Params.Test exposing (Params)
import Html exposing (Attribute, Html, button, div, h2, input, nav, option, select, span, text)
import Html.Attributes exposing (class, placeholder, step, style, type_, value)
import Html.Events as Events exposing (on, onInput)
import Json.Decode as Json
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


onChange : (String -> msg) -> Attribute msg
onChange handler =
    on "change" (Json.map handler Events.targetValue)


type alias Level =
    Int


type alias Value =
    { value : Int, input : String }


type alias Status =
    { baseStats : Value, effortValue : Value, individualValue : Value, realNumber : Maybe Int }


type alias Parameters =
    { hitPoint : Status, attack : Status, defence : Status, spAttack : Status, spDefence : Status, speed : Status }


type alias Model =
    { content : String, level : Level, nature : Nature, attack : Status, parameters : Parameters }


type alias NatureCode =
    String


type alias Nature =
    { code : NatureCode
    , label : String
    , up : Maybe ParamCategory
    , down : Maybe ParamCategory
    }


type NatureCategory
    = Adamant
    | Brave
    | Bold
    | Calm
    | Careful
    | Gentle
    | Hasty
    | Impish
    | Lax
    | Lonely
    | Mild
    | Modest
    | Naive
    | Naughty
    | Quiet
    | Relaxed
    | Sassy
    | Serious
    | Timid


listNatureCode : List NatureCode
listNatureCode =
    [ "serious"
    , "adamant"
    , "brave"
    , "bold"
    , "calm"
    , "careful"
    , "gentle"
    , "hasty"
    , "impish"
    , "lax"
    , "lonely"
    , "mild"
    , "modest"
    , "naive"
    , "naughty"
    , "quiet"
    , "relaxed"
    , "sassy"
    , "timid"
    ]


categoryToNature : NatureCategory -> Nature
categoryToNature natureCategory =
    case natureCategory of
        Serious ->
            Nature "serious" "まじめ" Nothing Nothing

        Adamant ->
            Nature "adamant" "いじっぱり" (Just Attack) (Just SpAttack)

        Brave ->
            Nature "brave" "ゆうかん" (Just Attack) (Just Speed)

        Bold ->
            Nature "bold" "ずぶとい" (Just Defence) (Just Attack)

        Calm ->
            Nature "calm" "おだやか" (Just SpDefence) (Just Attack)

        Careful ->
            Nature "careful" "しんちょう" (Just SpDefence) (Just SpAttack)

        Gentle ->
            Nature "gentle" "おとなしい" (Just SpDefence) (Just Defence)

        Hasty ->
            Nature "hasty" "せっかち" (Just Speed) (Just Defence)

        Impish ->
            Nature "impish" "わんぱく" (Just Defence) (Just SpAttack)

        Lax ->
            Nature "lax" "のうてんき" (Just Defence) (Just SpDefence)

        Lonely ->
            Nature "lonely" "さみしがり" (Just Attack) (Just Defence)

        Mild ->
            Nature "mild" "おっとり" (Just SpAttack) (Just Defence)

        Modest ->
            Nature "modest" "ひかえめ" (Just SpAttack) (Just Attack)

        Naive ->
            Nature "naive" "むじゃき" (Just Speed) (Just SpDefence)

        Naughty ->
            Nature "naughty" "やんちゃ" (Just Attack) (Just SpDefence)

        Quiet ->
            Nature "quiet" "れいせい" (Just SpAttack) (Just Speed)

        Relaxed ->
            Nature "relaxed" "のんき" (Just Defence) (Just Speed)

        Sassy ->
            Nature "sassy" "なまいき" (Just SpDefence) (Just Speed)

        Timid ->
            Nature "timid" "おくびょう" (Just Speed) (Just Attack)


codeToCategory : NatureCode -> NatureCategory
codeToCategory code =
    if code == "adamant" then
        Adamant

    else if code == "brave" then
        Brave

    else if code == "bold" then
        Bold

    else if code == "calm" then
        Calm

    else if code == "careful" then
        Careful

    else if code == "gentle" then
        Gentle

    else if code == "hasty" then
        Hasty

    else if code == "impish" then
        Impish

    else if code == "lax" then
        Lax

    else if code == "lonely" then
        Lonely

    else if code == "mild" then
        Mild

    else if code == "modest" then
        Modest

    else if code == "naive" then
        Naive

    else if code == "naughty" then
        Naughty

    else if code == "quiet" then
        Quiet

    else if code == "relaxed" then
        Relaxed

    else if code == "sassy" then
        Sassy

    else if code == "timid" then
        Timid

    else
        Serious


codeToNature : NatureCode -> Nature
codeToNature =
    codeToCategory >> categoryToNature


selectNature : Nature -> Html msg
selectNature nature =
    option [ value nature.code ] [ text nature.label ]


listNatureSelect : List (Html msg)
listNatureSelect =
    List.map (codeToNature >> selectNature) listNatureCode


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
      , nature = Nature "serious" "まじめ" Nothing Nothing
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
    | ChangeNature NatureCode


calculateStatus : Level -> Nature -> ParamCategory -> Status -> Status
calculateStatus level nature paramCategory status =
    let
        corrector : Maybe ParamCategory -> Float -> (Int -> Int)
        corrector pcMaybe rate =
            case pcMaybe of
                Just pc ->
                    if pc == paramCategory then
                        toFloat >> (\x -> x * rate) >> floor

                    else
                        \x -> x

                Nothing ->
                    \x -> x

        newValue =
            case paramCategory of
                HitPoint ->
                    (status.baseStats.value * 2 + status.individualValue.value + status.effortValue.value // 4) * level // 100 + level + 10

                _ ->
                    (status.baseStats.value * 2 + status.individualValue.value + status.effortValue.value // 4)
                        * level
                        // 100
                        + 5
                        |> corrector nature.up 1.1
                        |> corrector nature.down 0.9
    in
    { status | realNumber = Just newValue }


calculateParameters : Nature -> Level -> Parameters -> Parameters
calculateParameters nature level params =
    let
        curriedCalculateStatus : ParamCategory -> Status -> Status
        curriedCalculateStatus =
            calculateStatus level nature
    in
    { hitPoint = curriedCalculateStatus HitPoint params.hitPoint
    , attack = curriedCalculateStatus Attack params.attack
    , defence = curriedCalculateStatus Defence params.defence
    , spAttack = curriedCalculateStatus SpAttack params.spAttack
    , spDefence = curriedCalculateStatus SpDefence params.spDefence
    , speed = curriedCalculateStatus Speed params.speed
    }


updateValue : String -> Value -> Value
updateValue input value =
    case String.toInt input of
        Just num ->
            Value num input

        Nothing ->
            { value | input = input }


updateStatus : StatusCategory -> Status -> Value -> Status
updateStatus category status value =
    case category of
        BaseStats ->
            { status | baseStats = value }

        EffortValue ->
            { status | effortValue = value }

        IndividualValue ->
            { status | individualValue = value }


accessFieldValue : StatusCategory -> (Status -> Value)
accessFieldValue sc =
    case sc of
        BaseStats ->
            .baseStats

        EffortValue ->
            .effortValue

        IndividualValue ->
            .individualValue


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
                            { model
                                | level = newLevel
                                , parameters = calculateParameters model.nature newLevel model.parameters
                            }

                ChangeValue pc sc input ->
                    { model
                        | parameters =
                            let
                                statusToUpdate =
                                    model.parameters
                                        |> accessFieldStatus pc
                            in
                            statusToUpdate
                                |> accessFieldValue sc
                                |> updateValue input
                                |> updateStatus sc statusToUpdate
                                |> calculateStatus model.level model.nature pc
                                |> updateParam pc model.parameters
                    }

                ChangeNature input ->
                    let
                        newNature =
                            codeToNature input
                    in
                    { model
                        | nature = newNature
                        , parameters = calculateParameters newNature model.level model.parameters
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
        [ nav [ class "navbar is-primary" ]
            [ div
                [ class "navbar-brand"
                , style "font-size" "35px"
                , style "margin-left" "30px"
                ]
                [ text "Pokelm" ]
            ]
        , div [ class "container", style "margin-top" "20px" ]
            [ div [ class "columns" ]
                [ viewInput "number" "level" model.content Level
                , select [ class "column", onChange ChangeNature ] listNatureSelect
                ]
            , viewRowInput HitPoint model
            , viewRowInput Attack model
            , viewRowInput Defence model
            , viewRowInput SpAttack model
            , viewRowInput SpDefence model
            , viewRowInput Speed model
            ]
        ]
    }


viewRowInput : ParamCategory -> Model -> Html Msg
viewRowInput pc model =
    let
        status =
            model.parameters |> accessFieldStatus pc
    in
    div [ class "field", style "margin-top" "20px" ]
        [ div [ class "label" ] [ text <| describeParam pc ]
        , div [ class "columns control" ]
            [ input [ type_ "number", placeholder "種族値", viewStatusClass BaseStats status.baseStats.input, onInput (ChangeValue pc BaseStats) ] []
            , input [ type_ "number", placeholder "個体値", viewStatusClass IndividualValue status.individualValue.input, onInput (ChangeValue pc IndividualValue) ] []
            , input [ type_ "number", placeholder "努力値", viewStatusClass EffortValue status.effortValue.input, onInput (ChangeValue pc EffortValue), step "4" ] []
            , div [ class "column" ] [ button [ class "button" ] [ text "↑" ], button [ class "button" ] [ text "↓" ] ]
            , div [ class "column", style "font-size" "20px" ] [ text <| resultView <| .realNumber <| accessFieldStatus pc <| model.parameters ]
            ]
        ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    if isValid v then
        input [ type_ t, placeholder p, onInput toMsg, class "column input is-medium is-one-quarter" ] []

    else
        input [ style "border-color" "red", type_ t, placeholder p, onInput toMsg, class "column input is-medium is-danger" ] []


viewStatusClass : StatusCategory -> String -> Attribute msg
viewStatusClass sc v =
    case validateStatusInput sc v of
        Just _ ->
            class "column input is-medium"

        Nothing ->
            class "column input is-medium is-danger"


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


validateLevel : String -> Maybe Level
validateLevel input =
    case String.toInt input of
        Nothing ->
            Nothing

        Just level ->
            if level >= 1 then
                Just level

            else
                Nothing


validateStatusInput : StatusCategory -> String -> Maybe Int
validateStatusInput statusCategory input =
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


describeParam : ParamCategory -> String
describeParam pc =
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
