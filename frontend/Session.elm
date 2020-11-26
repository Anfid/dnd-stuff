module Session exposing (Session)

import Browser.Navigation as Navigation


type alias Session =
    { key : Navigation.Key
    }
