module Dynamic.Build
    exposing
        ( namedType
          -- union builders
        , unionFromElm
        , constructorFromElm
        , unionToElm
        , buildUnion
          -- record builders
        , recordFromElm
        , recordToElm
        , buildRecord
          -- error
        , internalError
          -- built-ins
        , t_Bool
        , t_Char
        , t_Date
        , t_Float
        , t_Int
        , t_List
        , t_String
        )

{-| This module can be used to build ```Dynamic``` values.
It could be used by a compiler extension to generate dynamics when writing ```#Typename```}
-}

import Dynamic as D
import Date exposing (Date)
import EveryDict exposing (EveryDict)


namedType : String -> List D.Type -> D.Type
namedType name arguments =
    D.NamedType ( name, arguments )


unionFromElm : D.Data -> D.Data
unionFromElm value =
    value


constructorFromElm : String -> D.FromElm (List D.Data)
constructorFromElm =
    D.Constructor


unionToElm : List D.ConstructorType -> D.ToElm ( String, List D.Data )
unionToElm constructors data =
    let
        typ =
            D.UnionType constructors

        checkConstructor constructor values typ =
            case lookup constructor typ of
                Nothing ->
                    False

                Just types ->
                    List.length types == List.length values
    in
        case data of
            D.Constructor constructor values ->
                if checkConstructor constructor values constructors then
                    Ok ( constructor, values )
                else
                    wrongTypeError typ data

            _ ->
                wrongTypeError typ data


buildUnion :
    D.TypeName
    -> List D.ConstructorType
    -> List D.Dictionary
    -> D.FromElm a
    -> D.ToElm a
    -> D.Dynamic a
buildUnion name types unions =
    build (D.NamedType name) (EveryDict.singleton name types :: unions)


recordFromElm : D.FromElm (List ( String, D.Data ))
recordFromElm =
    D.Record


recordToElm : List ( String, D.Type ) -> D.ToElm (List D.Data)
recordToElm types data =
    let
        typ =
            D.RecordType types
    in
        case data of
            D.Record values ->
                let
                    keys_types =
                        List.map Tuple.first types

                    values_types =
                        List.map Tuple.first values
                in
                    if (keys_types == values_types) then
                        Ok (List.map Tuple.second values)
                    else
                        wrongTypeError typ data

            _ ->
                wrongTypeError typ data


buildRecord :
    List ( String, D.Type )
    -> List D.Dictionary
    -> D.FromElm a
    -> D.ToElm a
    -> D.Dynamic a
buildRecord types =
    build (D.RecordType types)


internalError : Result D.Error a
internalError =
    Err D.InternalError


t_Bool : D.Dynamic Bool
t_Bool =
    let
        typ =
            namedType "Bool" []

        fromElm x =
            D.Bool x

        toElm data =
            case data of
                D.Bool v ->
                    Ok v

                _ ->
                    wrongTypeError typ data
    in
        build typ [] fromElm toElm


t_Char : D.Dynamic Char
t_Char =
    let
        typ =
            namedType "Char" []

        fromElm x =
            D.Char x

        toElm data =
            case data of
                D.Char v ->
                    Ok v

                _ ->
                    wrongTypeError typ data
    in
        build typ [] fromElm toElm


t_Date : D.Dynamic Date
t_Date =
    let
        typ =
            namedType "Date" []

        fromElm x =
            D.Float (Date.toTime x)

        toElm data =
            case data of
                D.Float v ->
                    Ok (Date.fromTime v)

                _ ->
                    wrongTypeError typ data
    in
        build typ [] fromElm toElm


t_Float : D.Dynamic Float
t_Float =
    let
        typ =
            namedType "Float" []

        fromElm x =
            D.Float x

        toElm data =
            case data of
                D.Float v ->
                    Ok v

                _ ->
                    wrongTypeError typ data
    in
        build typ [] fromElm toElm


t_Int : D.Dynamic Int
t_Int =
    let
        typ =
            namedType "Int" []

        fromElm x =
            D.Int x

        toElm data =
            case data of
                D.Int v ->
                    Ok v

                _ ->
                    wrongTypeError typ data
    in
        build typ [] fromElm toElm


t_List : D.Dynamic a -> D.Dynamic (List a)
t_List a =
    let
        typ =
            namedType "List" [ D.typ a ]

        fromElm x =
            List.map (D.fromElm a) x |> D.List

        toElm data =
            case data of
                D.List [] ->
                    Ok []

                D.List (x :: xs) ->
                    D.toElm a x
                        |> Result.andThen
                            (\v ->
                                toElm (D.List xs)
                                    |> Result.andThen
                                        (\vs -> Ok (v :: vs))
                            )

                _ ->
                    wrongTypeError typ data
    in
        build typ [ D.unions a ] fromElm toElm


t_String : D.Dynamic String
t_String =
    let
        typ =
            namedType "String" []

        fromElm x =
            D.String x

        toElm data =
            case data of
                D.String v ->
                    Ok v

                _ ->
                    wrongTypeError typ data
    in
        build typ [] fromElm toElm



-- INTERNAL --


wrongTypeError : D.Type -> D.Data -> Result D.Error a
wrongTypeError typ data =
    Err (D.WrongType typ data)


build :
    D.Type
    -> List D.Dictionary
    -> D.FromElm a
    -> D.ToElm a
    -> D.Dynamic a
build typ unions fromElm toElm =
    D.Dynamic
        { typ = typ
        , unions = List.foldl EveryDict.union EveryDict.empty unions
        , fromElm = fromElm
        , toElm = toElm
        }


lookup : a -> List ( a, b ) -> Maybe b
lookup a list =
    case list of
        [] ->
            Nothing

        ( a_, b ) :: bs ->
            if a == a_ then
                Just b
            else
                lookup a bs
