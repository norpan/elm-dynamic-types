module Dynamic.Json.Encoder exposing (encoder)

import Dynamic exposing (..)
import Json.Encode as Json


encoder : Data -> Json.Value
encoder value =
    case value of
        Bool bool ->
            Json.bool bool

        Char char ->
            Json.string (String.fromChar char)

        Float float ->
            Json.float float

        Int int ->
            Json.int int

        List values ->
            List.map encoder values
                |> Json.list

        Record values ->
            values
                |> List.map (\( k, v ) -> ( k, encoder v ))
                |> Json.object

        String string ->
            Json.string string

        Constructor name values ->
            Json.object [ ( name, Json.list (List.map encoder values) ) ]
