module PokerLogic exposing (..)

import Dict exposing (..)
import Dict.Extra exposing (..)
import List.Extra exposing (..)
import Set exposing (..)
import Types exposing (..)


nextRank : Rank -> Rank
nextRank rank =
    case rank of
        Ace ->
            Two

        Two ->
            Three

        Three ->
            Four

        Four ->
            Five

        Five ->
            Six

        Six ->
            Seven

        Seven ->
            Eight

        Eight ->
            Nine

        Nine ->
            Ten

        Ten ->
            Jack

        Jack ->
            Queen

        Queen ->
            King

        King ->
            Ace


rankToInt : Rank -> Int
rankToInt rank =
    case rank of
        Two ->
            2

        Three ->
            3

        Four ->
            4

        Five ->
            5

        Six ->
            6

        Seven ->
            7

        Eight ->
            8

        Nine ->
            9

        Ten ->
            10

        Jack ->
            11

        Queen ->
            12

        King ->
            13

        Ace ->
            14


intToRank : Int -> Rank
intToRank i =
    case i of
        2 ->
            Two

        3 ->
            Three

        4 ->
            Four

        5 ->
            Five

        6 ->
            Six

        7 ->
            Seven

        8 ->
            Eight

        9 ->
            Nine

        10 ->
            Ten

        11 ->
            Jack

        12 ->
            Queen

        13 ->
            King

        14 ->
            Ace

        _ ->
            Two


suitToAlphaChar : Suit -> Char
suitToAlphaChar suit =
    case suit of
        Spades ->
            'S'

        Hearts ->
            'H'

        Diamonds ->
            'D'

        Clubs ->
            'C'


alphaCharToSuit : Char -> Suit
alphaCharToSuit c =
    case c of
        'S' ->
            Spades

        'H' ->
            Hearts

        'D' ->
            Diamonds

        'C' ->
            Clubs

        _ ->
            Spades


compareRank : Rank -> Rank -> Order
compareRank r1 r2 =
    compare (rankToInt r1) (rankToInt r2)


compareSuit : Suit -> Suit -> Order
compareSuit s1 s2 =
    compare (suitToAlphaChar s1) (suitToAlphaChar s2)


compareRankTuple : ( Rank, Rank ) -> ( Rank, Rank ) -> Order
compareRankTuple ( r1a, r1b ) ( r2a, r2b ) =
    case compareRank r1a r2a of
        EQ ->
            compareRank r1b r2b

        order ->
            order

comparePokerHand : PokerHand -> PokerHand -> Order
comparePokerHand h1 h2 =
    case (h1, h2) of
        ( Nada, Nada ) ->
            EQ

        ( Nada, _ ) ->
            LT

        ( _, Nada ) ->
            GT

        ( HighCard r1, HighCard r2 ) ->
            compareRank r1 r2

        ( Pair r1, Pair r2 ) ->
            compareRank r1 r2

        ( TwoPair rt1, TwoPair rt2 ) ->
            compareRankTuple rt1 rt2

        ( ThreeOfAKind r1, ThreeOfAKind r2 ) ->
            compareRank r1 r2

        ( Straight r1, Straight r2 ) ->
            compareRank r1 r2

        ( Flush s1, Flush s2 ) ->
            compareSuit s1 s2

        ( FullHouse rt1, FullHouse rt2 ) ->
            compareRankTuple rt1 rt2

        ( FourOfAKind r1, FourOfAKind r2 ) ->
            compareRank r1 r2

        ( StraightFlush r1, StraightFlush r2 ) ->
            compareRank r1 r2

        ( RoyalFlush, RoyalFlush ) ->
            EQ

        ( RoyalFlush, _ ) ->
            GT

        ( _, RoyalFlush ) ->
            LT


allSuits : List Suit
allSuits =
    [ Spades, Hearts, Diamonds, Clubs ]


allRanks : List Rank
allRanks =
    [ Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace ]


fullDeck : List Card
fullDeck =
    List.concat (List.map (\suit -> List.map (\rank -> ( suit, rank )) allRanks) allSuits)


pick : Int -> List a -> ( List a, List a )
pick n deck =
    ( List.take n deck, List.drop n deck )


removeCard : Card -> List Card -> List Card
removeCard card deck =
    List.filter (\c -> c /= card) deck


removeCards : List Card -> List Card -> List Card
removeCards cards deck =
    List.foldl (\c d -> removeCard c d) deck cards


removeRank : Rank -> List Card -> List Card
removeRank rank deck =
    List.filter (\( _, r ) -> r /= rank) deck


removeSuit : Suit -> List Card -> List Card
removeSuit suit deck =
    List.filter (\( s, _ ) -> s /= suit) deck


maybeToPokerHand : Maybe PokerHand -> PokerHand
maybeToPokerHand hand =
    case hand of
        Just h ->
            h

        Nothing ->
            Nada


bestNOfAKind : Int -> Set Card -> Maybe Rank
bestNOfAKind n cards =
    cards
        |> Set.toList
        |> Dict.Extra.groupBy (\( _, rank ) -> rankToInt rank)
        |> Dict.filter (\_ l -> List.length l >= n)
        |> Dict.keys
        |> List.maximum
        |> Maybe.map (\i -> intToRank i)


bestHighCard : Set Card -> Maybe PokerHand
bestHighCard =
    bestNOfAKind 1
        >> Maybe.map HighCard


bestPair : Set Card -> Maybe PokerHand
bestPair =
    bestNOfAKind 2
        >> Maybe.map Pair


bestThreeOfAKind : Set Card -> Maybe PokerHand
bestThreeOfAKind =
    bestNOfAKind 3
        >> Maybe.map ThreeOfAKind


bestFourOfAKind : Set Card -> Maybe PokerHand
bestFourOfAKind =
    bestNOfAKind 4
        >> Maybe.map FourOfAKind


bestTwoPair : Set ( Suit, Rank ) -> number
bestTwoPair cards =
    let
        bestpair =
            bestNOfAKind 2 cards

        cardsWithoutBestPair =
            cards
                |> Set.toList
                |> List.filter (\( _, rank ) -> Just rank /= bestpair)
                |> Set.fromList
                |> bestNOfAKind 2
    in
    1
