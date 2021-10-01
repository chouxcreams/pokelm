module Types.Status exposing (Status, StatusCategory(..), accessFieldValue, updateStatus, validateStatusInput)

import Types.Value exposing (Value)


type alias Status =
    { baseStats : Value, effortValue : Value, individualValue : Value, realNumber : Maybe Int }


type StatusCategory
    = BaseStats
    | EffortValue
    | IndividualValue


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
