module Style exposing
    ( bgColor
    , buttonStyle
    , headingStyle
    , fgColor
    , greenBrightColor
    , greenColor
    , greenFadedColor
    , inputFieldStyle
    , redBrightColor
    , redColor
    , redFadedColor
    , textStyle
    )

import Element exposing (Color, focused, mouseDown, mouseOver, padding, rgb255, centerX)
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
