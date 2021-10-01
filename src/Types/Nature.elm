module Types.Nature exposing (..)

import Html exposing (Html, option, text)
import Html.Attributes exposing (value)
import Types.Parameter exposing (ParamCategory(..))


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


fromCode : NatureCode -> Nature
fromCode =
    codeToCategory >> categoryToNature


selectNature : Nature -> Html msg
selectNature nature =
    option [ value nature.code ] [ text nature.label ]


listNatureSelect : List (Html msg)
listNatureSelect =
    List.map (fromCode >> selectNature) listNatureCode
