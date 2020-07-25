module Main exposing (GraphQLType(..), Query(..), query, typedef)

import Dict exposing (Dict)
import Parser exposing ((|.), (|=), Parser, Step(..))


type Query
    = Query String (List Query)


query : Parser (List Query)
query =
    Parser.succeed identity
        |. Parser.token "{"
        |. Parser.spaces
        |= Parser.loop [] queryHelp


queryHelp : List Query -> Parser (Step (List Query) (List Query))
queryHelp revQueries =
    Parser.oneOf
        [ Parser.succeed ()
            |. Parser.spaces
            |. Parser.token "}"
            |. Parser.spaces
            |> Parser.map (\_ -> Done (List.reverse revQueries))
        , Parser.succeed (\name queryList -> Loop (Query name queryList :: revQueries))
            |. Parser.spaces
            |= alphas
            |. Parser.spaces
            |= Parser.oneOf
                [ query
                , Parser.succeed []
                ]
        ]


alphas : Parser String
alphas =
    Parser.chompWhile Char.isAlpha
        |> Parser.getChompedString


type GraphQLType
    = StringType
    | IntType
    | ObjectType (Dict String GraphQLType)


typedef : Parser (Dict String GraphQLType)
typedef =
    Parser.succeed (\key val -> Dict.fromList [ ( key, val ) ])
        |. Parser.keyword "type"
        |. Parser.spaces
        |= alphas
        |. Parser.spaces
        |= graphQLType


graphQLType : Parser GraphQLType
graphQLType =
    Parser.oneOf
        [ Parser.keyword "String"
            |> Parser.map (\_ -> StringType)
        , Parser.succeed ObjectType
            |. Parser.spaces
            |. Parser.token "{"
            |. Parser.spaces
            |= Parser.loop Dict.empty loopKeyVals
        ]


loopKeyVals : Dict String GraphQLType -> Parser (Step (Dict String GraphQLType) (Dict String GraphQLType))
loopKeyVals keyVals =
    Parser.oneOf
        [ Parser.succeed (\key valType -> Loop (Dict.insert key valType keyVals))
            |= alphas
            |. Parser.token ":"
            |. Parser.spaces
            |= Parser.oneOf
                [ Parser.keyword "String" |> Parser.map (\_ -> StringType)
                , Parser.keyword "Int" |> Parser.map (\_ -> IntType) 
                ]
            |. Parser.spaces
        , Parser.succeed (\_ -> Done keyVals)
            |= Parser.token "}"
        ]
