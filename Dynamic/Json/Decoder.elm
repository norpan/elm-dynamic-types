module Dynamic.Json.Decoder exposing (decoder)

import Dynamic exposing (..)
import Json.Decode as Json
import EveryDict exposing (EveryDict)


type alias Dictionary =
    EveryDict TypeName (List ConstructorType)


decoder : Dictionary -> Type -> Json.Decoder Data
decoder dictionary typ =
    case typ of
        NamedType ( "Bool", [] ) ->
            Json.bool |> Json.map Bool

        NamedType ( "Char", [] ) ->
            Json.string
                |> Json.andThen
                    (\s ->
                        case String.uncons s of
                            Just ( c, "" ) ->
                                Json.succeed c

                            Just ( c, _ ) ->
                                Json.fail "String to long for Char"

                            Nothing ->
                                Json.fail "Cannot decode empty string to Char"
                    )
                |> Json.map Char

        NamedType ( "Date", [] ) ->
            Json.float |> Json.map Float

        NamedType ( "Float", [] ) ->
            Json.float |> Json.map Float

        NamedType ( "Int", [] ) ->
            Json.int |> Json.map Int

        NamedType ( "List", [ t ] ) ->
            decoder dictionary t
                |> Json.list
                |> Json.map List

        RecordType list ->
            Json.map Record (recordDecoder dictionary list)

        NamedType ( "String", [] ) ->
            Json.string |> Json.map String

        UnionType constructors ->
            unionDecoder dictionary constructors |> Json.map (uncurry Constructor)

        NamedType typeName ->
            case EveryDict.get typeName dictionary of
                Nothing ->
                    Json.fail ("Cannot decode to type `" ++ toString typeName ++ "`")

                Just constructors ->
                    Json.lazy (\_ -> decoder dictionary (UnionType constructors))


recordDecoder : Dictionary -> List ( String, Type ) -> Json.Decoder (List ( String, Data ))
recordDecoder dictionary list =
    case list of
        [] ->
            Json.succeed []

        ( k, t ) :: rest ->
            Json.map2 (\v vs -> ( k, v ) :: vs)
                (Json.field k (decoder dictionary t))
                (recordDecoder dictionary rest)


tupleDecoder : Dictionary -> Int -> List Type -> Json.Decoder (List Data)
tupleDecoder dictionary index list =
    case list of
        [] ->
            Json.succeed []

        typ :: rest ->
            Json.map2 (::)
                (Json.index index (decoder dictionary typ))
                (tupleDecoder dictionary (index + 1) rest)


unionDecoder : Dictionary -> List ( String, List Type ) -> Json.Decoder ( String, List Data )
unionDecoder dictionary list =
    Json.oneOf (List.map (unionElementDecoder dictionary) list)


unionElementDecoder : Dictionary -> ( String, List Type ) -> Json.Decoder ( String, List Data )
unionElementDecoder dictionary ( name, types ) =
    (Json.field name (tupleDecoder dictionary 0 types)) |> Json.map (\v -> ( name, v ))
