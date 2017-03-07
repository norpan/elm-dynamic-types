module Dynamic exposing (..)

import EveryDict exposing (EveryDict)


{-| The result of doing #Typename
   * typ is a representation of the type
   * unions is a dict of all the union types we know of in `typ`
     since we cannot have cyclic data structures.
   * fromElm is a function to convert from Elm to Data
   * toElm is a function to convert from Data to Elm
-}
type Dynamic a
    = Dynamic
        { typ : Type
        , unions : Dictionary
        , fromElm : FromElm a
        , toElm : ToElm a
        }


type alias Dictionary =
    EveryDict TypeName (List ConstructorType)


type alias FromElm a =
    a -> Data


type alias ToElm a =
    Data -> Result Error a


{-| Get the type from a Dynamic
-}
typ : Dynamic a -> Type
typ (Dynamic dynamic) =
    dynamic.typ


{-| Get the union types from a Dynamic
-}
unions : Dynamic a -> Dictionary
unions (Dynamic dynamic) =
    dynamic.unions


{-| Convert an Elm value to a Data
-}
fromElm : Dynamic a -> FromElm a
fromElm (Dynamic dynamic) =
    dynamic.fromElm


{-| Convert a Data to an Elm value.
-}
toElm : Dynamic a -> ToElm a
toElm (Dynamic dynamic) =
    dynamic.toElm


{-| Three kinds of types
* NamedType is a named type, either a built-in or a user-defined union type
  (not type aliases, they should be inlined)
* RecordType is a record type, (field name, type)
* UnionType is a union type, (constructors)
-}
type Type
    = NamedType TypeName
    | RecordType (List ( String, Type ))
    | UnionType (List ConstructorType)


{-| A type name
List Int is ("List", (_, NamedType "Int" []))
-}
type alias TypeName =
    ( String, List Type )


{-| Constructor type
Cons Type1 Type2 is ("Cons", (NamedType "Type1" [], NamedType "Type2" []))
-}
type alias ConstructorType =
    ( String, List Type )


{-| Basic Elm data types

Other types can be stored using a combination of these.
(Yes, we could store everything as "String" but I'm thinking no parsing here)
-}
type Data
    = Bool Bool
    | Char Char
    | Constructor String (List Data)
    | Float Float
    | Int Int
    | List (List Data)
    | Record (List ( String, Data ))
    | String String


type Error
    = WrongType Type Data
    | InternalError
