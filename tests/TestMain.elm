module TestMain exposing (suite)

import Expect
import Main exposing (Field(..), fields)
import Parser
import Test exposing (Test)


suite : Test
suite =
    Test.describe "GraphQL parsers"
        [ Test.describe "Query parser"
            [ Test.test "Parses a simple query field" <|
                \_ ->
                    Expect.equal
                        (Parser.run fields "{ name }")
                        (Ok [ Field "name" [] ])
            , Test.test "Parser nested fields" <|
                \_ ->
                    Expect.equal
                        (Parser.run fields
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
                            [ Field "hero"
                                [ Field "name" []
                                , Field "friends"
                                    [ Field "name" []
                                    , Field "homeWorld"
                                        [ Field "name" []
                                        , Field "climate" []
                                        ]
                                    , Field "species"
                                        [ Field "name" []
                                        , Field "lifespan" []
                                        , Field "origin"
                                            [ Field "name" [] ]
                                        ]
                                    ]
                                ]
                            ]
                        )
            ]
        ]
