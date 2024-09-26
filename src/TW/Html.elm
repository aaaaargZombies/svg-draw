module TW.Html exposing (..)

import Html
import Html.Attributes


button : List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
button attr children =
    Html.button
        (attr
            ++ [ Html.Attributes.classList
                    [ ( "text-white", True )
                    , ( "bg-gray-800", True )
                    , ( "hover:bg-gray-900", True )
                    , ( "focus:outline-none", True )
                    , ( "focus:ring-4", True )
                    , ( "focus:ring-gray-300", True )
                    , ( "font-medium", True )
                    , ( "rounded-lg", True )
                    , ( "text-sm", True )
                    , ( "px-5", True )
                    , ( "py-2.5", True )
                    , ( "me-2", True )
                    , ( "mb-2", True )
                    , ( "dark:bg-gray-800", True )
                    , ( "dark:hover:bg-gray-700", True )
                    , ( "dark:focus:ring-gray-700", True )
                    , ( "dark:border-gray-700", True )
                    ]
               ]
        )
        children
