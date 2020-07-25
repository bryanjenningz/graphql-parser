module TestMain exposing (suite)

import Dict
import Expect
import Main exposing (GraphQLType(..), Query(..), query, typedef)
import Parser
import Test exposing (Test)


suite : Test
suite =
    Test.describe "GraphQL parsers"
        [ Test.describe "Query parser"
            [ Test.test "Parses a simple query" <|
                \_ ->
                    Expect.equal
                        (Parser.run query "{ name }")
                        (Ok [ Query "name" [] ])
            , Test.test "Parser nested query" <|
                \_ ->
                    Expect.equal
                        (Parser.run query
                            """{
                            hero {
                                name
                                friends {
                                    name
                                    homeWorld {
                                        name
                                        climate
                                    }
                                    species {
                                        name
                                        lifespan
                                        origin {
                                            name
                                        }
                                    }
                                }
                            }
                        }
                        """
                        )
                        (Ok
                            [ Query "hero"
                                [ Query "name" []
                                , Query "friends"
                                    [ Query "name" []
                                    , Query "homeWorld"
                                        [ Query "name" []
                                        , Query "climate" []
                                        ]
                                    , Query "species"
                                        [ Query "name" []
                                        , Query "lifespan" []
                                        , Query "origin"
                                            [ Query "name" [] ]
                                        ]
                                    ]
                                ]
                            ]
                        )
            ]
        , Test.describe "Typedef parser"
            [ Test.test "Parses a simple typedef" <|
                \_ ->
                    Expect.equal
                        (Parser.run typedef "type Query { hero: String }")
                        (Ok <|
                            Dict.fromList
                                [ ( "Query"
                                  , ObjectType
                                        (Dict.fromList
                                            [ ( "hero", StringType ) ]
                                        )
                                  )
                                ]
                        )
            ]
        ]
