module Main exposing (Field(..), fields)

import Parser exposing ((|.), (|=), Parser, Step(..))


type Field
    = Field String (List Field)


fields : Parser (List Field)
fields =
    Parser.succeed identity
        |. Parser.token "{"
        |. Parser.spaces
        |= Parser.loop [] fieldsHelp


fieldsHelp : List Field -> Parser (Step (List Field) (List Field))
fieldsHelp revFields =
    Parser.oneOf
        [ Parser.succeed ()
            |. Parser.spaces
            |. Parser.token "}"
            |. Parser.spaces
            |> Parser.map (\_ -> Done (List.reverse revFields))
        , Parser.succeed (\name fieldList -> Loop (Field name fieldList :: revFields))
            |. Parser.spaces
            |= chompName
            |. Parser.spaces
            |= Parser.oneOf
                [ fields
                , Parser.succeed []
                ]
        ]


chompName : Parser String
chompName =
    Parser.chompWhile Char.isAlpha
        |> Parser.getChompedString
