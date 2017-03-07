module Dynamic.Json exposing (decoder, encoder)

import Dynamic exposing (..)
import Dynamic.Json.Decoder as DJD
import Dynamic.Json.Encoder as DJE
import Json.Decode as JD
import Json.Encode as JE
import Dynamic.Show as Show


decoder : Dynamic a -> JD.Decoder a
decoder dynamic =
    DJD.decoder (unions dynamic) (typ dynamic)
        |> JD.map (toElm dynamic)
        |> JD.map (Result.mapError (Show.error))
        |> JD.andThen resultToDecoder


resultToDecoder : Result String a -> JD.Decoder a
resultToDecoder r =
    case r of
        Ok a ->
            JD.succeed a

        Err e ->
            JD.fail e


encoder : Dynamic a -> a -> JE.Value
encoder dynamic value =
    DJE.encoder (fromElm dynamic value)
