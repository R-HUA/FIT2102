-- | This is the file you need to implement to complete the assignment. Remember
-- to comment where appropriate, use generic types and have fun!

module Player where

import Parser.Parser -- This is the source for the parser from the course notes
import Parser.Instances
import Rummy.Rules
import Rummy.Types   -- Here you will find types used in the game of Rummy
import Cards         -- Finally, the generic card type(s)

-- You can add more imports if you need them

-- | This card is called at the beginning of your turn, you need to decide which
-- pile to draw from.
pickCard :: ActionFunc
pickCard discarded (yourScore, opponentScore) memory Nothing hand = 
    if foldl (\b x -> twoCards discarded x || b) False hand 
        then (Discard, memoryO) 
        else (Stock, memoryO)
    where memoryO = fromJust (newMemory (yourScore, opponentScore) memory discarded Nothing)
pickCard discarded (yourScore, opponentScore) memory opponent hand 
    | length memory < 40 = if foldl (\b x -> twoCards discarded x || b) False hand 
        then (Discard, memoryO) 
        else (Stock, memoryO)
    | otherwise = if canMakeMeld (sort (deadwoods (makeMelds (yourScore, opponentScore) (fromJust memory) hand) ++ [discarded])) 
        then (Discard,memoryO)
        else (Stock,memoryO)
        where memoryO = fromJust (newMemory (yourScore, opponentScore) memory discarded opponent)


-- | This function is called once you have drawn a card, you need to decide
-- which action to call.
playCard :: PlayFunc
playCard drew (ps,os) memory hand 
    | length allDeadwoods == 1 && ((last allDeadwoods) /= drew) = ((Action Gin (last allDeadwoods)),(memoryOut memory drew (last allDeadwoods)))
    | length allDeadwoods > 1 && (foldr (\x acc-> toPoints x + acc ) 0 (drew:(init restCards))) < 10 = (Action Knock (last restCards),(memoryOut memory drew (last restCards)))
    | otherwise = if length restCards > 0 
                    then ((Action Drop (todrop memory restCards)) ,(memoryOut memory drew (todrop memory restCards)))
                    else ((Action Drop thecard) , (memoryOut memory drew thecard))
        where restCards = sort (difference (deadwoods (makeMelds (ps,os) memory (hand ++ [drew]))) [drew])
              allDeadwoods = deadwoods (makeMelds (ps,os) memory (hand ++ [drew]))
              thecard = last (sort (deadwoods (makeMelds (ps,os) memory hand)))

todrop :: String -> [Card] -> Card 
-- using the memory to determin which card should be dropped           
todrop memory restCards = if length should > 0 then should !! 0 else if length shouldnot > 0 then last shouldnot else last restCards
    where should = shouldDrop restCards (allDiscards memory)
          shouldnot = filter (\x -> not (cannotDiscard x (opponent'sDraws memory))) restCards
             

-- | This function is called at the end of the game when you need to return the
-- melds you formed with your last hand.
makeMelds :: MeldFunc
makeMelds _ _ c = forming $ sort c
    
sort :: [Card] -> [Card]
-- sort the list of cards 
sort [] = []
sort (pivot:rest) = lower rest ++ pivot : upper rest
  where lower = part (pivot>) 
        upper = part (pivot<=)
        part = (sort .) . filter

sameRank :: Card -> Card -> Bool
-- if the two cards have the same rank
sameRank (Card _ r1) (Card _ r2) = r1 == r2

isStraight3 :: Card -> Card -> Card -> Bool
-- check if the three cards are Straight3
isStraight3 (Card s1 r1) (Card s2 r2) (Card s3 r3) 
    | r1 == King || r2 == King  = False
    | otherwise = (s1 == s2) && (s2 == s3) && (r2 == succ r1) && (r3 == succ r2) 

isStraight4 :: Card -> Card -> Card -> Card -> Bool
-- check if the three cards are Straight4
isStraight4 (Card s1 r1) (Card s2 r2) (Card s3 r3) (Card s4 r4)
    | r1 == King || r2 == King || r3 == King = False
    | otherwise = (s1 == s2) && (s2 == s3) && (s3 == s4) && (r2 == succ r1) && (r3 == succ r2) && (r4 == succ r3)

forming :: [Card] -> [Meld]
-- forming a sorted list of card become a list of Meld
forming [] = []
forming (x:xs) 
    | length xs >= 3 && length (filter (\a-> sameRank x a) xs) < 2 = 
        if isStraight4 x (xs !! 0) (xs !! 1) (xs !! 2) 
            then [(Straight4 x (xs !! 0) (xs !! 1) (xs !! 2))] ++ forming (drop 3 xs)
            else if isStraight3 x (xs !! 0) (xs !! 1) 
                then [(Straight3 x (xs !! 0) (xs !! 1))] ++ forming (drop 2 xs) 
                else (Deadwood x):(forming xs)
    | length xs == 2 && length (filter (\a-> sameRank x a) xs) < 2 = 
        if isStraight3 x (xs !! 0) (xs !! 1) 
            then (Straight3 x (xs !! 0) (xs !! 1)):(forming (drop 2 xs))
            else (Deadwood x):(forming xs)
    | length withSameRank == 2 = (Set3 x (withSameRank !! 0) (withSameRank !! 1)):(forming (filter  (\a-> not (sameRank x a)) xs))
    | length withSameRank == 3 = (Set4 x (withSameRank !! 0) (withSameRank !! 1) (withSameRank !! 2)):(forming (filter (\a-> not (sameRank x a)) xs))
    | otherwise = (Deadwood x):(forming xs)
        where withSameRank = filter (\a-> sameRank x a) xs

deadwoods :: [Meld] -> [Card]
-- make all the Deadwood in the list of Meld become simple cards
deadwoods [] = [] 
deadwoods ((Deadwood c):xs) = c:(deadwoods xs)
deadwoods (_:xs) = deadwoods xs

canMakeMeld :: [Card] -> Bool  --the input list should be sorted list
-- check whether the list of cards can make Meld
canMakeMeld [] = False
canMakeMeld (x:xs) 
    | length xs >= 3 = isStraight4 x (xs !! 0) (xs !! 1) (xs !! 2) || isStraight3 x (xs !! 0) (xs !! 1) || canMakeMeld xs
    | length xs == 2 = isStraight3 x (xs !! 0) (xs !! 1) || canMakeMeld xs
    | otherwise = length (filter (\a-> sameRank x a) xs) >= 2 || canMakeMeld xs
    
twoCards :: Card -> Card -> Bool
-- check whether the two crads are ajacent or have same rank
twoCards (Card s1 r1) (Card s2 r2) = isAjacent (Card s1 r1) (Card s2 r2) || ((s1 /= s2) && (r1 == r2))

fromJust :: Maybe String -> String
-- The fromJust function extracts the element out of a Just
fromJust (Just s) = s
fromJust Nothing = ""

newMemory :: (Score, Score) -> Maybe String -> Card -> Maybe Draw -> Maybe String
-- generate new memory to output
-- add oppent's discard and oppent's drew
-- if it is the first turn, it will creat basic memory
newMemory (yourScore, opponentScore) _ discard Nothing = pure (cardtoString discard ++ "nn" ++ scoreToString yourScore ++ scoreToString opponentScore)
newMemory (yourScore, opponentScore) Nothing discard _ = pure (cardtoString discard ++ "nn" ++ scoreToString yourScore ++ scoreToString opponentScore)
newMemory (yourScore, opponentScore) memory discard opponent 
     | notsameScore yourScore (getYScore memory) || notsameScore opponentScore (getOScore memory) = pure (cardtoString discard ++ "nn" ++ scoreToString yourScore ++ scoreToString opponentScore)
     | otherwise = memory >>= (\s -> Just (cardtoString discard ++ (opponentPicked opponent s) ++ s))

memoryOut :: String -> Card -> Card -> String
-- add player's discard and player's drew
memoryOut memory drew droptCard = cardtoString droptCard ++ (cardtoString drew) ++ memory

cardtoString ::  Card -> String
-- covert card to string
cardtoString (Card s r) = suitToString s ++ rankToString r

stringToCard :: String -> Maybe Card
-- convert the head of the memory to Cards
stringToCard  ""  = Nothing
stringToCard "nn" = Nothing
stringToCard (x:xs) = Just (Card (charToSuit x) (charToRank xs))

suitToString :: Suit -> String 
suitToString Spade    = "S"
suitToString Club     = "C"
suitToString Diamond  = "D"
suitToString Heart    = "H"

charToSuit :: Char -> Suit
charToSuit c 
    | c == 'S' = Spade
    | c == 'C' = Club
    | c == 'D' = Diamond
    | c == 'H' = Heart
charToSuit _ = Spade

charToRank :: String -> Rank
charToRank c
    | c == "1" =  Ace  
    | c == "2" =  Two  
    | c == "3" =  Three
    | c == "4" =  Four 
    | c == "5" =  Five 
    | c == "6" =  Six  
    | c == "7" =  Seven
    | c == "8" =  Eight
    | c == "9" =  Nine 
    | c == "0" =  Ten  
    | c == "J" =  Jack 
    | c == "Q" =  Queen
    | c == "K" =  King 
charToRank _ = Ace
    

rankToString :: Rank -> String
rankToString Ace    ="1"
rankToString Two    ="2"
rankToString Three  ="3"
rankToString Four   ="4"
rankToString Five   ="5"
rankToString Six    ="6"
rankToString Seven  ="7"
rankToString Eight  ="8"
rankToString Nine   ="9"
rankToString Ten    ="0"
rankToString Jack   ="J"
rankToString Queen  ="Q"
rankToString King   ="K"

opponentPicked :: Maybe Draw -> String -> String
-- get the cads which opponent picked form the memory
opponentPicked (Just Discard) s = latestCard s
opponentPicked _ _ = "nn"

latestCard :: String -> String
-- the head two character in the list
latestCard s
    | length latest < 1 = ""
    | head latest ==  'S' = latest
    | head latest ==  'C' = latest
    | head latest ==  'D' = latest
    | head latest ==  'H' = latest
    | otherwise = "nn"
    where latest = getCurrent (parse parseMemory s) 

getRest :: ParseResult String -> String
-- get the rest of the ParseResult
getRest (Result rest _) = rest
getRest (Error _) = ""

getCurrent :: ParseResult String -> String
-- the the current ParseResult
getCurrent (Result _ current) = current
getCurrent (Error _) = ""

parseMemory :: Parser String
-- Return a parser that succeeds with a string off the input or fails with an error if the input is empty.
parseMemory = P parseit
  where parseit "" = Error UnexpectedEof
        parseit (c1:(c2:s)) = Result s [c1,c2]
        parseit [_] = Error UnexpectedEof


notsameScore :: Score -> String -> Bool
-- if the string represent the same value of the score
notsameScore _ ""  = False
notsameScore sco str 
    | sco < 1 = True
    | sco > 99 = True
    | sco > 9 = (isErrorResult (parse (is (intTensChar sco)) (safehead str))) 
                    || (isErrorResult (parse (is (intOnesChar sco)) [(last str)]))
    | otherwise = (isErrorResult (parse (is (intOnesChar sco)) [(last str)]))

scoreToString :: Score -> String
-- covert the Score(Int) to String
scoreToString sco
    | sco < 10 = "0" ++ (show sco)
    | sco > 99 = "00"
    | otherwise = show sco

intTensChar :: Int -> Char
intTensChar i = head $ show i

intOnesChar ::  Int -> Char
intOnesChar i = last $ show i

getOScore :: Maybe String -> String
-- get the opponent's score
getOScore (Just s) = [last (init s)] ++ [last s]
getOScore Nothing = ""

getYScore :: Maybe String -> String
-- get the player's score
getYScore (Just s) = [last (init (init (init s)))] ++ [last (init (init s))]
getYScore Nothing = ""

opponent'sDraws :: String -> [Card]
-- get all the card opponent picked from memory
opponent'sDraws memory
    | length s <= 6 = if isCard s then safeadd (toCard s) [] else []
    | otherwise = (if isCard s then safeadd (toCard s) [] else []) ++ (opponent'sDraws $ drop 8 memory)
        where s = drop 2 memory
        
allDiscards :: String -> [Card]
-- get all cards in the discard pile from memory
allDiscards s
    | length s <= 8 = safeadd (toCard s) []
    | otherwise = safeadd (toCard s) (allDiscards (drop 4 s))

safeadd :: Maybe Card -> [Card] -> [Card]
-- add the card to the list
-- if the Maybe Card is nothing, then keep the list 
safeadd Nothing ls = ls 
safeadd (Just c) ls = c:ls


toCard :: String -> Maybe Card
-- convert the string to card
toCard = stringToCard . latestCard 


isCard :: String -> Bool
-- check whether the card is card
isCard card = suit == "S" || suit == "D" || suit == "C" || suit == "H" 
    where suit = safehead card 

difference :: Eq a => [a] -> [a] -> [a]
-- A - B (difference set)
difference a b = filter (\x -> not(elem x b)) a

cannotDiscard :: Card -> [Card] -> Bool
cannotDiscard a l = canMakeMeld $ sort (a:l)

shouldDrop :: [Card] -> [Card] -> [Card]
-- find what card the player should drop
shouldDrop [] _ = []
shouldDrop (x:[]) discards = 
    if length (filter (\a -> isAjacent x a) discards) == 2 || length (filter (\a -> sameRank x a) discards) >=2 then [x] else []
shouldDrop (x1:(x2:xs)) discards 
    | isAjacent x1 x2 = if noAjacent x1 x2 discards then [x1,x2] ++ next else next
    | otherwise = if length (filter (\a -> isAjacent x1 a) discards) == 2 || length (filter (\a -> sameRank x1 a) discards) >=2 then x1:next else next
        where next = shouldDrop xs discards

isAjacent :: Card -> Card -> Bool 
-- check whether the two card is ajacent
isAjacent (Card s1 King) (Card s2 r2) = (s1 == s2 && r2 == Queen) 
isAjacent (Card s1 r1) (Card s2 King) = (s1 == s2 && r1 == Queen) 
isAjacent (Card s1 r1) (Card s2 r2) = (s1 == s2 && (r2 == succ r1 || r1 == succ r2)) 

noAjacent :: Card -> Card -> [Card] -> Bool
-- all the ajacent card of that two card is in the list
noAjacent (Card s1 Ace) (Card _ _) ls = elem (Card s1 Three) ls
noAjacent (Card s1 _) (Card _ King) ls = elem (Card s1 Jack) ls
noAjacent (Card s1 r1) (Card _ r2) ls = (elem (Card s1 (pred r1)) ls) || (elem (Card s1 (succ r2)) ls)


safehead :: [a] -> [a]
-- a safe head function, its return type is [a] rather than a
safehead []= []
safehead (x:_) = [x]




