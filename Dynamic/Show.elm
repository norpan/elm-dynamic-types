module Dynamic.Show exposing (..)

import Dynamic exposing (..)


typ : Type -> String
typ t =
    case t of
        NamedType ( name, [] ) ->
            name

        NamedType ( name, types ) ->
            "(" ++ name ++ " " ++ spaceSeparated typ types ++ ")"

        RecordType types ->
            "{" ++ commaSeparated (\( k, t ) -> k ++ " : " ++ typ t) types ++ "}"

        UnionType constructors ->
            String.join " | " (List.map constructor constructors)


constructor : ( String, List Type ) -> String
constructor ( c, types ) =
    c ++ " " ++ spaceSeparated typ types


data : Data -> String
data d =
    case d of
        Bool a ->
            toString a

        Char a ->
            toString a

        Float a ->
            toString a

        Int a ->
            toString a

        List tvs ->
            "[ " ++ commaSeparated data tvs ++ " ]"

        String s ->
            "\"" ++ escapeString s ++ "\""

        Constructor constructor tvs ->
            constructor ++ spaceSeparated data tvs

        Record tvs ->
            "{ " ++ commaSeparated (\( k, tv ) -> k ++ " = " ++ data tv) tvs ++ " }"


spaceSeparated : (a -> String) -> List a -> String
spaceSeparated f =
    List.map f >> String.join " "


commaSeparated : (a -> String) -> List a -> String
commaSeparated f =
    List.map f >> String.join ", "


escapeString : String -> String
escapeString =
    String.split "\\"
        >> String.join "\\\""
        >> String.split "\""
        >> String.join "\\\""


error : Error -> String
error e =
    case e of
        WrongType t d ->
            "Expected type`" ++ typ t ++ "` but got data `" ++ data d ++ "`"

        InternalError ->
            "Internal error"
