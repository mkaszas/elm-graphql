-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Hasura.Object.User_min_fields exposing (age, name)

import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Hasura.InputObject
import Hasura.Interface
import Hasura.Object
import Hasura.Scalar
import Hasura.ScalarCodecs
import Hasura.Union
import Json.Decode as Decode


age : SelectionSet (Maybe Int) Hasura.Object.User_min_fields
age =
    Object.selectionForField "(Maybe Int)" "age" [] (Decode.int |> Decode.nullable)


name : SelectionSet (Maybe String) Hasura.Object.User_min_fields
name =
    Object.selectionForField "(Maybe String)" "name" [] (Decode.string |> Decode.nullable)
