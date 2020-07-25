module Main exposing (Query(..), Typedef(..), query, typedef)

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


type Typedef
    = StringType
    | ObjectType (Dict String Typedef)


typedef : Parser (Dict String Typedef)
typedef =
    Parser.succeed (\key val -> Dict.fromList [ ( key, val ) ])
        |. Parser.keyword "type"
        |. Parser.spaces
        |= alphas
        |. Parser.spaces
        |= Parser.oneOf
            [ Parser.keyword "String"
                |> Parser.map (\_ -> StringType)
            , Parser.succeed (\key -> ObjectType (Dict.fromList [ ( key, StringType ) ]))
                |. Parser.spaces
                |. Parser.token "{"
                |. Parser.spaces
                |= alphas
                |. Parser.token ":"
                |. Parser.spaces
                |. alphas
                |. Parser.spaces
            ]
