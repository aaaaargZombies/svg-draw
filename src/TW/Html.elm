module TW.Html exposing (..)

import Html
import Html.Attributes


button : List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
button attr children =
    Html.button
        (attr
            ++ [ Html.Attributes.classList
                    [ ( "bg-gray-800", True )
                    , ( "dark:bg-gray-800", True )
                    , ( "dark:border-gray-700", True )
                    , ( "dark:focus:ring-gray-700", True )
                    , ( "dark:hover:bg-gray-700", True )
                    , ( "focus:outline-none", True )
                    , ( "focus:ring-4", True )
                    , ( "focus:ring-gray-300", True )
                    , ( "font-medium", True )
                    , ( "hover:bg-gray-900", True )
                    , ( "px-5", True )
                    , ( "py-2.5", True )
                    , ( "rounded-lg", True )
                    , ( "text-sm", True )
                    , ( "text-white", True )
                    ]
               ]
        )
        children
