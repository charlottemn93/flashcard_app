module Format exposing (prettyDate, prettyTime)

import Time as Time exposing (Month(..), Posix, utc)


prettyMonth : Month -> String
prettyMonth month =
    case month of
        Jan ->
            "Jan"

        Feb ->
            "Feb"

        Mar ->
            "Mar"

        Apr ->
            "Apr"

        May ->
            "May"

        Jun ->
            "Jun"

        Jul ->
            "Jul"

        Aug ->
            "Aug"

        Sep ->
            "Sep"

        Oct ->
            "Oct"

        Nov ->
            "Nov"

        Dec ->
            "Dec"


prettyDate : Posix -> String
prettyDate posix =
    String.fromInt (Time.toDay utc posix)
        ++ " "
        ++ prettyMonth (Time.toMonth utc posix)
        ++ " "
        ++ String.fromInt (Time.toYear utc posix)


prettyTime : Posix -> String
prettyTime posix =
    String.fromInt (Time.toHour utc posix)
        ++ ":"
        ++ String.fromInt (Time.toMinute utc posix)
