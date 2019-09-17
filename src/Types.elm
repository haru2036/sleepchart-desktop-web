module Types exposing (..)

type OAuthProvider
    = Google

type alias Profile =
    { name : String
    , picture : String
    }