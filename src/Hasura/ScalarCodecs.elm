-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Hasura.ScalarCodecs exposing (Id, Timestamptz, Uuid, codecs)

import Hasura.Scalar exposing (defaultCodecs)
import Json.Decode as Decode exposing (Decoder)


type alias Id =
    Hasura.Scalar.Id


type alias Timestamptz =
    Hasura.Scalar.Timestamptz


type alias Uuid =
    Hasura.Scalar.Uuid


codecs : Hasura.Scalar.Codecs Id Timestamptz Uuid
codecs =
    Hasura.Scalar.defineCodecs
        { codecId = defaultCodecs.codecId
        , codecTimestamptz = defaultCodecs.codecTimestamptz
        , codecUuid = defaultCodecs.codecUuid
        }
