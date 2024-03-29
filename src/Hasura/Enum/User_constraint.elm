-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Hasura.Enum.User_constraint exposing (User_constraint(..), decoder, fromString, list, toString)

import Json.Decode as Decode exposing (Decoder)


{-| unique or primary key constraints on table "user"

  - User\_pkey - unique or primary key constraint

-}
type User_constraint
    = User_pkey


list : List User_constraint
list =
    [ User_pkey ]


decoder : Decoder User_constraint
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "user_pkey" ->
                        Decode.succeed User_pkey

                    _ ->
                        Decode.fail ("Invalid User_constraint type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representating the Enum to a string that the GraphQL server will recognize.
-}
toString : User_constraint -> String
toString enum =
    case enum of
        User_pkey ->
            "user_pkey"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe User_constraint
fromString enumString =
    case enumString of
        "user_pkey" ->
            Just User_pkey

        _ ->
            Nothing
