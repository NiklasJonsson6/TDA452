module BlackJack where
import Cards
import RunGame
import Test.QuickCheck
import System.Random

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
            , 1 + 1 + size Empty
            , 1 + 1 + 0
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
gameOver h = value h > 21

-- | Checks who has won out of Bank and Guest.
winner:: Hand -> Hand -> Player
winner hg hb 
        | gameOver hg               = Bank
        | value hg <= value hb 
                && not(gameOver hb) = Bank
        |               otherwise   = Guest



-- | -------------2B------------- | --
-- | -------------2B------------- | --
-- | -------------2B------------- | --

-- | Takes two hands and puts the first on top of the other.
-- | Begins putting the card before empty on top of the second hand.
(<+) :: Hand -> Hand -> Hand
(<+) Empty h2     = h2
(<+) (Add c h) h2 = Add c (h <+ h2) 

-- | Tests if <+ is associative.
prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 =
        p1<+(p2<+p3) == (p1<+p2)<+p3

-- | Tests so that the size of two combined hand is the sum of their sizes.
prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf p1 p2 =
        size p1 + size p2 == size(p1<+p2) 

-- | Converts a list of cards to a hand.
convert :: [Card] -> Hand 
convert = foldr Add Empty

-- | Creates a full deck.
fullDeck :: Hand
fullDeck = convert [Card r s | s <- [Hearts,Spades,Diamonds,Clubs], 
                    r <- [Numeric x | x <- [2 .. 10]]++[Jack,Queen,King,Ace]]

-- | Draws the first card from the first hand and puts it on top of the second hand.
draw :: Hand -> Hand -> (Hand,Hand)
draw Empty hand = error "draw: The deck is empty."
draw (Add c rest) hand = (rest, Add c hand)

-- | Plays for the bank.
playBank :: Hand -> Hand
playBank deck = playBankHelper deck Empty

-- | Helper function for playBank.
playBankHelper :: Hand -> Hand -> Hand
playBankHelper deck hand = if value hand < 16 
                             then playBankHelper smallerDeck biggerHand
                             else hand
                         where (smallerDeck, biggerHand) = draw deck hand

                         -- | Takes a random number generator and a deck and shuffles the deck.
shuffleDeck :: StdGen -> Hand -> Hand
shuffleDeck gen deck = shuffleDeckHelper gen deck Empty

-- | Helper function to shuffleDeck.
shuffleDeckHelper :: StdGen -> Hand -> Hand -> Hand
shuffleDeckHelper gen Empty   newDeck = newDeck
shuffleDeckHelper gen oldDeck newDeck = 
                        shuffleDeckHelper gen2 pickedDeck (Add card newDeck) 
                        where (i, gen2) = randomR (0, size oldDeck - 1) gen
                              (pickedDeck, card) = pickCard i oldDeck

-- | Picks the i:th card in the deck and returns the deck without this card.
pickCard :: Int -> Hand -> (Hand, Card)
pickCard i = pickCardHelper i Empty

-- | Helper function to pickCard.
pickCardHelper :: Int -> Hand -> Hand -> (Hand, Card)
pickCardHelper i passedCards (Add c h) = 
                        if i == 0
                           then (reverseHand passedCards Empty <+ h, c) 
                           else pickCardHelper (i-1) newPassed rest
                           where (rest, newPassed) = draw (Add c h) passedCards

-- | Takes a hand and reverses it.
reverseHand :: Hand -> Hand -> Hand
reverseHand Empty        newH = newH 
reverseHand (Add c oldH) newH = reverseHand oldH (Add c newH)

-- | Tests that no cards disappear after a shuffle.
prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h = (c `belongsTo` h) == (c `belongsTo` shuffleDeck g h)

-- | Checks if a card belongs to a hand.
belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty = False
c `belongsTo` (Add c' h) = c==c' || c `belongsTo` h

-- | Checks so that the size of a hand remains the same after a shuffle.
prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle gen hand = size hand == size (shuffleDeck gen hand)

implementation = Interface
  { iFullDeck = fullDeck
  , iValue    = value
  , iDisplay  = display
  , iGameOver = gameOver
  , iWinner   = winner
  , iDraw     = draw
  , iPlayBank = playBank
  , iShuffle  = shuffleDeck
  }

main :: IO ()
main = runGame implementation  
