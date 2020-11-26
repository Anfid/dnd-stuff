module Style exposing
    ( bgColor
    , buttonStyle
    , fgColor
    , greenBrightColor
    , greenColor
    , greenFadedColor
    , headingStyle
    , inputFieldStyle
    , redBrightColor
    , redColor
    , redFadedColor
    , textStyle
    , toColor
    )

import Color
import Element exposing (Color, centerX, focused, mouseDown, mouseOver, padding, rgb255)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font


textStyle : List (Element.Attribute msg) -> List (Element.Attribute msg)
textStyle s =
    s
        ++ [ padding 5
           , Font.color fgColor
           , Font.family [ Font.serif ]
           ]


headingStyle : List (Element.Attribute msg) -> List (Element.Attribute msg)
headingStyle s =
    s
        ++ [ padding 5
           , centerX
           , Font.color fgColor
           , Font.size 40
           , Font.family [ Font.serif ]
           ]


buttonStyle : List (Element.Attribute msg) -> List (Element.Attribute msg)
buttonStyle s =
    s
        ++ [ padding 5
           , Background.color greenColor
           , Font.color fgColor
           , Font.family [ Font.serif ]
           , mouseDown [ Background.color greenFadedColor ]
           , mouseOver [ Background.color greenBrightColor ]
           , focused [ Background.color greenBrightColor ]
           ]


inputFieldStyle : List (Element.Attribute msg) -> List (Element.Attribute msg)
inputFieldStyle s =
    s
        ++ [ padding 5
           , Background.color <| rgb255 0 0 0
           , Border.color <| rgb255 0 0 0
           , Font.color fgColor
           , Font.family [ Font.serif ]
           , focused [ Border.color greenBrightColor ]
           ]



-- COLORS


toColor : Color -> Color.Color
toColor c =
    let
        { red, green, blue, alpha } =
            Element.toRgb c
    in
    Color.rgba red green blue alpha


bgColor : Color
bgColor =
    rgb255 40 40 40


fgColor : Color
fgColor =
    rgb255 251 241 199


greenColor : Color
greenColor =
    rgb255 76 175 80


greenBrightColor : Color
greenBrightColor =
    rgb255 92 207 96


greenFadedColor : Color
greenFadedColor =
    rgb255 60 159 64


redColor : Color
redColor =
    rgb255 204 36 29


redBrightColor : Color
redBrightColor =
    rgb255 251 73 52


redFadedColor : Color
redFadedColor =
    rgb255 157 0 6
