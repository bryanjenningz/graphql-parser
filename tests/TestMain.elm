module TestMain exposing (suite)

import Expect
import Main exposing (Query(..), query)
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
        ]
