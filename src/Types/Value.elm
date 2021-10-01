module Types.Value exposing (..)


type alias Value =
    { value : Int, input : String }


updateValue : String -> Value -> Value
updateValue input value =
    case String.toInt input of
        Just num ->
            Value num input

        Nothing ->
            { value | input = input }
