module TestMain exposing (suite)

import Dict
import Expect
import Main exposing (GraphQLType(..), Query(..), allValidTypedefs, query, typedef, typedefs)
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
            , Test.test "Parses a typedef with multiple key vals" <|
                \_ ->
                    Expect.equal
                        (Parser.run typedef """type Hero {
                            id: String
                            name: String
                            age: Int
                            friend: Friend
                        }""")
                        (Ok <|
                            Dict.fromList
                                [ ( "Hero"
                                  , ObjectType
                                        (Dict.fromList
                                            [ ( "id", StringType )
                                            , ( "name", StringType )
                                            , ( "age", IntType )
                                            , ( "friend", DefinedType "Friend" )
                                            ]
                                        )
                                  )
                                ]
                        )
            , Test.describe "Typedefs parser"
                [ Test.test "Parses multiple typedefs" <|
                    \_ ->
                        Expect.equal
                            (Parser.run typedefs """
                            type Hero {
                                id: String
                                name: String
                                age: Int
                                friend: Friend
                            }
                            type Friend {
                                friendliness: Int
                                picture: String
                            }
                            """)
                            (Ok <|
                                Dict.fromList
                                    [ ( "Hero"
                                      , ObjectType
                                            (Dict.fromList
                                                [ ( "id", StringType )
                                                , ( "name", StringType )
                                                , ( "age", IntType )
                                                , ( "friend", DefinedType "Friend" )
                                                ]
                                            )
                                      )
                                    , ( "Friend"
                                      , ObjectType
                                            (Dict.fromList
                                                [ ( "friendliness", IntType )
                                                , ( "picture", StringType )
                                                ]
                                            )
                                      )
                                    ]
                            )
                ]
            , Test.describe "allValidTypedefs"
                [ Test.test "Is valid if all typedef references are actually defined" <|
                    \_ ->
                        Expect.equal
                            (allValidTypedefs <|
                                Dict.fromList
                                    [ ( "Hero"
                                      , ObjectType
                                            (Dict.fromList
                                                [ ( "id", StringType )
                                                , ( "name", StringType )
                                                , ( "age", IntType )
                                                , ( "friend", DefinedType "Friend" )
                                                ]
                                            )
                                      )
                                    , ( "Friend"
                                      , ObjectType
                                            (Dict.fromList
                                                [ ( "friendliness", IntType )
                                                , ( "picture", StringType )
                                                ]
                                            )
                                      )
                                    ]
                            )
                            True
                , Test.test "Is not valid if there's a type used that's not actually defined" <|
                    \_ ->
                        Expect.equal
                            (allValidTypedefs <|
                                Dict.fromList
                                    [ ( "Hero"
                                      , ObjectType
                                            (Dict.fromList
                                                [ ( "id", StringType )
                                                , ( "name", StringType )
                                                , ( "age", IntType )
                                                , ( "friend", DefinedType "Friend" )
                                                ]
                                            )
                                      )
                                    ]
                            )
                            False
                ]
            ]
        ]
