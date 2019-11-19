module BlackJack where
import Cards
import RunGame
import Test.QuickCheck

-- | -------------2A------------- | --
-- | -------------2A------------- | --
-- | -------------2A------------- | --

hand2 :: Hand
hand2 = Add (Card (Numeric 2) Hearts)
        (Add (Card Jack Spades) Empty)

-- | The size of a hand using manual steps.
sizeSteps :: [Integer]
sizeSteps = [size hand2,
            size (Add (Card Ace Hearts)
            (Add (Card Jack Spades) Empty))
            , 1 + size (Add (Card Jack Spades) Empty)
            , 2 + size Empty
            , 2]

-- | Takes a hand and converts it to a String.
display :: Hand -> String
display Empty = ""
display (Add c h) = displayCard c ++ "\n" ++ display h

-- | Takes a card and converts it to a String.
displayCard :: Card -> String
displayCard (Card (Numeric i) s) = show i ++ " of " ++ show s
displayCard (Card r           s) = show r ++ " of " ++ show s

-- | Takes a hand and calculates its value using Ace as 11 to begin
-- | and changes Ace to 1 if value becomes larger than 21.
value :: Hand -> Integer
value h 
    | calculateWithAceAs h 11 <= 21 = calculateWithAceAs h 11
    |                      otherwise = calculateWithAceAs h 1

-- | Calculates the value of a hand with Ace as the selected integer.
calculateWithAceAs :: Hand -> Integer -> Integer
calculateWithAceAs Empty _ = 0
calculateWithAceAs (Add (Card Ace _) h) i 
                            = i + calculateWithAceAs h i
calculateWithAceAs (Add (Card r   _) h) i 
                            = valueRank r + calculateWithAceAs h i

-- | Converts a Rank to its Integer value.
valueRank :: Rank -> Integer
valueRank (Numeric i) = i
valueRank _           = 10

-- | Takes a hand and checks if its value is over 21.
gameOver :: Hand -> Bool
gameOver h 
        | value h < 21 = False
        | otherwise    = True

-- | Checks who has won out of Bank and Guest.
winner:: Hand -> Hand -> Player
winner hg hb 
        | gameOver hg            = Bank
        | value hg <= value hb 
                && value hb < 21 = Bank
        |              otherwise = Guest



-- | -------------2B------------- | --
-- | -------------2B------------- | --
-- | -------------2B------------- | --