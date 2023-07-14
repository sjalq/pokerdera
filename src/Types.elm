module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , message : String
    }


type alias BackendModel =
    { message : String
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend


type Suit
    = Spades
    | Hearts
    | Diamonds
    | Clubs


type Rank
    = Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King
    | Ace


type alias Card =
    ( Suit, Rank )


type PokerHand
    = Nada
    | HighCard Rank
    | Pair Rank
    | TwoPair (Rank, Rank)
    | ThreeOfAKind Rank
    | Straight Rank
    | Flush Suit
    | FullHouse (Rank, Rank)
    | FourOfAKind Rank
    | StraightFlush Rank
    | RoyalFlush
