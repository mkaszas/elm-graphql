module Main exposing (main)

import Browser
import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Hasura.Object
import Hasura.Object.Follow_connection_aggregate as FollowConnectionAggregate
import Hasura.Object.Follow_connection_aggregate_fields as FollowConnectionAggregateFields
import Hasura.Object.User as User
import Hasura.Query as Query
import Html exposing (Html, div, li, p, text, ul)
import RemoteData exposing (RemoteData(..))


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( RemoteData.Loading, makeRequest )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


mainQuery : SelectionSet Response RootQuery
mainQuery =
    Query.user identity userInfoSelection


userInfoSelection : SelectionSet Person Hasura.Object.User
userInfoSelection =
    SelectionSet.succeed Person
        |> with User.name
        |> with User.age
        |> with (User.inviter nameSelection)
        |> with followerCountSelection


nameSelection : SelectionSet String Hasura.Object.User
nameSelection =
    User.name


followerCountSelection : SelectionSet Int Hasura.Object.User
followerCountSelection =
    User.followers_aggregate identity (FollowConnectionAggregate.aggregate (FollowConnectionAggregateFields.count identity))
        |> SelectionSet.map (flattenWithDefault 0)


flattenWithDefault : a -> Maybe (Maybe a) -> a
flattenWithDefault default value =
    value
        |> Maybe.withDefault Nothing
        |> Maybe.withDefault default


type Msg
    = GotResponse Model


type alias Person =
    { name : String
    , age : Maybe Int
    , inviterName : Maybe String
    , followerCount : Int
    }


type alias Response =
    List Person


type alias Model =
    RemoteData (Graphql.Http.Error Response) Response


makeRequest =
    mainQuery
        |> Graphql.Http.queryRequest "https://mk-1.herokuapp.com/v1/graphql"
        |> Graphql.Http.send (RemoteData.fromResult >> GotResponse)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        GotResponse res ->
            ( res, Cmd.none )


view : Model -> Html Msg
view model =
    case model of
        RemoteData.Loading ->
            text "Loading..."

        RemoteData.Success people ->
            div []
                [ p [] [ text "People:" ]
                , ul [] (List.map personInfo people)
                ]

        RemoteData.Failure _ ->
            text "ERROR"

        _ ->
            text "???"


personInfo : Person -> Html Msg
personInfo { name, age, inviterName, followerCount } =
    let
        ageString =
            age
                |> Maybe.map String.fromInt
                |> Maybe.map (\s -> " (" ++ s ++ ")")
                |> Maybe.withDefault ""

        inviterString =
            inviterName
                |> Maybe.map (\s -> ", Inviter: " ++ s)
                |> Maybe.withDefault ""

        nameString =
            name ++ ageString ++ inviterString ++ " [" ++ String.fromInt followerCount ++ " followers]"
    in
    li [] [ text nameString ]
