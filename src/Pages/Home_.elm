module Pages.Home_ exposing (Model, Msg, calculateParameters, page)

import Gen.Params.Home_ exposing (Params)
import Html exposing (Attribute, Html, button, div, input, nav, select, text)
import Html.Attributes exposing (class, placeholder, style, type_, value)
import Html.Events as Events exposing (on, onInput)
import Http
import Json.Decode as Json exposing (Decoder, field, int, list, map2, string)
import Page
import Request
import Shared
import Types.Nature as Nature exposing (Nature, NatureCode)
import Types.Parameter as Parameter exposing (ParamCategory(..), Parameters)
import Types.Status as Status exposing (Status, StatusCategory(..))
import Types.Value as Value exposing (Value)
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page _ _ =
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


type alias InputValidator =
    String -> Maybe Int


type alias Level =
    Value


type alias Model =
    { name : String, id : String, level : Level, nature : Nature, attack : Status, parameters : Parameters }


initValue : Int -> Value
initValue val =
    Value val <| String.fromInt val


initStatus : Status
initStatus =
    Status (initValue 0) (initValue 0) (initValue 31) Nothing


init : ( Model, Cmd Msg )
init =
    ( { name = ""
      , id = "1"
      , level = { value = 50, input = "50" }
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


type Msg
    = ChangeLevel String
    | ChangeValue ParamCategory StatusCategory String
    | ChangeNature NatureCode
    | ChangeId String
    | GotPokemon (Result Http.Error Pokemon)


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
                    (status.baseStats.value * 2 + status.individualValue.value + status.effortValue.value // 4) * level.value // 100 + level.value + 10

                _ ->
                    (status.baseStats.value * 2 + status.individualValue.value + status.effortValue.value // 4)
                        * level.value
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeLevel levelInput ->
            let
                newLevel =
                    case validateLevel levelInput of
                        Nothing ->
                            { value = model.level.value, input = levelInput }

                        Just newVal ->
                            { value = newVal, input = levelInput }
            in
            ( { model
                | level = newLevel
                , parameters = calculateParameters model.nature newLevel model.parameters
              }
            , Cmd.none
            )

        ChangeValue pc sc input ->
            ( { model
                | parameters =
                    let
                        statusToUpdate =
                            model.parameters
                                |> Parameter.accessFieldStatus pc
                    in
                    statusToUpdate
                        |> Status.accessFieldValue sc
                        |> Value.updateValue input
                        |> Status.updateStatus sc statusToUpdate
                        |> calculateStatus model.level model.nature pc
                        |> Parameter.updateParam pc model.parameters
              }
            , Cmd.none
            )

        ChangeNature input ->
            let
                newNature =
                    Nature.fromCode input
            in
            ( { model
                | nature = newNature
                , parameters = calculateParameters newNature model.level model.parameters
              }
            , Cmd.none
            )

        ChangeId idInput ->
            case String.toInt idInput of
                Just id ->
                    ( { model | id = idInput }, getPokemon id )

                Nothing ->
                    ( { model | id = idInput }, Cmd.none )

        GotPokemon result ->
            case result of
                Ok res ->
                    ( { model
                        | name = res.name
                        , parameters = updateParametersFromApi res.stats model.parameters
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
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
                [ viewValueInput "level" validateLevel [ class "column is-medium is-one-quarter" ] model.level.input ChangeLevel
                , select [ class "column is-one-quarter", onChange ChangeNature ] Nature.listNatureSelect
                , viewValueInput "pokemon" (\x -> String.toInt x) [ class "column is-medium is-one-quarter" ] model.id ChangeId
                , text model.name
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
            model.parameters |> Parameter.accessFieldStatus pc
    in
    div [ class "field", style "margin-top" "20px" ]
        [ div [ class "label" ] [ text <| Parameter.describe pc ]
        , div [ class "columns control" ]
            [ viewValueInput "種族値" (Status.validateStatusInput BaseStats) [ class "column is-medium" ] status.baseStats.input <| ChangeValue pc BaseStats
            , viewValueInput "個体値" (Status.validateStatusInput IndividualValue) [ class "column is-medium" ] status.individualValue.input <| ChangeValue pc IndividualValue
            , viewValueInput "努力値" (Status.validateStatusInput EffortValue) [ class "column is-medium" ] status.effortValue.input <| ChangeValue pc EffortValue
            , div [ class "column" ] [ button [ class "button" ] [ text "↑" ], button [ class "button" ] [ text "↓" ] ]
            , div [ class "column", style "font-size" "20px" ] [ text <| resultView <| .realNumber <| Parameter.accessFieldStatus pc <| model.parameters ]
            ]
        ]


viewValueInput : String -> InputValidator -> List (Attribute msg) -> String -> (String -> msg) -> Html msg
viewValueInput p validator attributes inputValue toMsg =
    let
        myAttributes =
            List.append [ type_ "tel", placeholder p, value inputValue, onInput toMsg, class "input" ] <|
                List.append attributes <|
                    case validator inputValue of
                        Just _ ->
                            []

                        Nothing ->
                            [ class "is-danger" ]
    in
    input myAttributes []


resultView : Maybe Int -> String
resultView maybeInt =
    case maybeInt of
        Nothing ->
            ""

        Just val ->
            String.fromInt val


validateLevel : InputValidator
validateLevel input =
    case String.toInt input of
        Nothing ->
            Nothing

        Just level ->
            if level >= 1 then
                Just level

            else
                Nothing



-- HTTP


getPokemon : Int -> Cmd Msg
getPokemon id =
    Http.get
        { url = "https://pokeapi.co/api/v2/pokemon/" ++ String.fromInt id
        , expect = Http.expectJson GotPokemon pokemonDecoder
        }


type alias Pokemon =
    { name : String, stats : List ApiStat }


type alias ApiStat =
    { baseStat : Int, name : String }


apiStatDecoder : Decoder ApiStat
apiStatDecoder =
    map2 ApiStat
        (field "base_stat" int)
        (field "stat" (field "name" string))


pokemonDecoder : Decoder Pokemon
pokemonDecoder =
    map2 Pokemon
        (field "name" string)
        (field "stats" <| list apiStatDecoder)


updateValueFromApi : ApiStat -> Value
updateValueFromApi stat =
    Value stat.baseStat <| String.fromInt stat.baseStat


updateStatusFromApi : ApiStat -> Status -> Status
updateStatusFromApi stat status =
    { status | baseStats = updateValueFromApi stat }


updateParameterFromApi : ApiStat -> Parameters -> Parameters
updateParameterFromApi stat params =
    let
        pc =
            Parameter.fromCode stat.name

        newStatus =
            updateStatusFromApi stat <| Parameter.accessFieldStatus pc params
    in
    Parameter.updateParam pc params newStatus


updateParametersFromApi : List ApiStat -> Parameters -> Parameters
updateParametersFromApi stats params =
    List.foldl updateParameterFromApi params stats
