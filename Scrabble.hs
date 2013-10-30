import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad
import qualified System.IO as SIO
import Data.Char
import System.Environment (getArgs)
import System.Random

-- my own modules
import qualified Bag
import qualified PrefixTree as PT

type Pos = (Int,Int)
type Board = Map.Map Pos (Either Bonus Char)
data Bonus = LetterBonus Int | WordBonus Int deriving (Eq,Show)
type Hand = Bag.Bag Char
type Dict = PT.PrefixTree Char
-- Representing dictionary as a prefix tree helps not chase too many dead ends
-- when searching for moves


posOccupied :: Pos -> Board -> Bool
posOccupied pos board = case Map.lookup pos board of
    Just (Right c) -> True
    _ -> False  -- either off board, or bonus, or nothing there

-- Helper function to ease building Maps from association lists
assign :: [a] -> b -> [(a,b)]
assign keys val = zip keys $ repeat val

charValueMap :: Map.Map Char Int
charValueMap = Map.fromList $ concatMap (uncurry assign)
    [("AEIOULNSTR",1),("DG",2),("BCMP",3),("FHVWY",4),("K",5),("JX",8),("QZ",10)]

charValue :: Char -> Int
charValue c = case Map.lookup c charValueMap of
    Just x -> x
    Nothing -> 0  -- score lower case letters (from blank tiles) as 0

baseScore :: String -> Int
baseScore word = sum $ map charValue word

-- Standard placement of bonus squares. Pretty easy with assign.
startingBoard :: Board
startingBoard = Map.fromList $ whole
  where
    ul :: [(Pos, Either Bonus Char)]
    ul = assign [(1,1),(8,1)] (Left (WordBonus 3))
      ++ assign [(i,i) | i <- [2,3,4,5,8]] (Left (WordBonus 2))
      ++ assign [(6,2),(6,6)] (Left (LetterBonus 3))
      ++ assign [(4,1),(7,3),(7,7),(8,4)] (Left (LetterBonus 2))
    left = ul ++ [((16-i,j),val) | ((i,j),val) <- ul]
    half = left ++ [((16-j,i),val) | ((i,j),val) <- left]
    whole = half ++ [((j,i),val) | ((i,j),val) <- half]

-- Will be easier to work with (namely to shuffle) than a Bag Char
startingLetters :: String
startingLetters = "AAAAAAAAABBCCDDDDEEEEEEEEEEEEFFGGGHHIIIIIIIIIJKLLL"
               ++ "LMMNNNNNNOOOOOOOOPPQRRRRRRSSSSTTTTTTUUUUVVWWXYYZ__"

-- Lookup word, ignoring case (so blanks will work)
realWord :: String -> Dict -> Bool
realWord word dict = map toUpper word `PT.member` dict

-- A hack, especially because infinity + 1 /= infinity, but 15 is small enough
-- that infinity = 100 will work.
infinity :: Int
infinity = 100

validPos :: Pos -> Bool
validPos (x,y) = 1 <= min x y && max x y <= 15

(<+>) :: Pos -> Pos -> Pos
(x,y) <+> (z,w) = (x+z,y+w)


-- Internal representation of Moves, to be used both in searching for legal moves
-- (for the computer player) and verifying the legality of moves (for the human).
data Move = Move { mStart :: Pos  -- start position
                 , mDict :: Dict  -- where we are in PrefixTree
                 , mHand :: Hand  -- what's left in hand
                 , mPlaces :: [Place]  --
                 , mCrosswords :: [Crossword]  -- cross words so far
                 , mMultipliers :: [Int]  -- multipliers for later
                 , mScore :: Int  -- tentative score, ignoring vcws and mults
                 } deriving (Eq,Show)

-- Representation of placement of a Char at a Pos, designating whether or not
-- that Char actually came from the Hand (vs already on the Board)
type Place = (Pos,Char,Bool)

-- Starting position, the word, and the score you'll get from it
type Crossword = (Pos,String,Int)


-- We really just need to know how to search/check horizontal moves. Vertical
-- moves can then be treated with the same functions via transposition.


-- When starting a horizontal move at a given position in a given row, determine
-- the minimum number of letters one would have to play to make a valid move for a
-- word that starts at that position.
-- The recursive structure of this problem suggests building the list of all
-- minimum lengths for a given row "dynamically", so we give a length 16 list with
-- all such numbers. The last element (infinity) of the list corresponds to the
-- non-existant 16th column.
minHLengths :: Board -> Int -> [Int]
minHLengths board row = foldl loop [infinity] [15,14..1]
  where
    loop :: [Int] -> Int -> [Int]
    loop list col = (new:list) where
      new = if letter (col-1) then infinity  -- the word can't start *here*
        else if letter col then 1  -- have to play at least 1 letter
        -- The following trick makes the opening move possible. Note that after
        -- the opening move is played, (8,8) is necessarily occupied, so one of
        -- the above two clauses will match, and no harm is done.
        -- Note: this trick relies on having no one-letter words in the dict.
        else if (row,col) == (8,8) then 1
        else if letter (col+1) then 1
        else if aboveOrBelowLetter col then 2
        else if aboveOrBelowLetter (col+1) then 2
        else 1 + (head list)  -- "recursive" call

    letter :: Int -> Bool
    letter col = posOccupied (row,col) board

    aboveOrBelowLetter :: Int -> Bool
    aboveOrBelowLetter col = posOccupied (row-1,col) board
                          || posOccupied (row+1,col) board


-- Next position that will be filled for an HMove
hmNextPos :: Move -> Pos
hmNextPos hm = case mPlaces hm of
    [] -> mStart hm
    ((pos,_,_):_) -> pos <+> (0,1)

-- Test that a Move spans a valid horizontal word. (Still have to check enough
-- letters have been played from hand, as in minHLengths.)
hmWordEnd :: Board -> Move -> Bool
hmWordEnd board hm = PT.wordEnd (mDict hm) &&
    not (posOccupied (hmNextPos hm) board)  -- word does not continue

highestOccupiedAbove :: Board -> Pos -> Pos
highestOccupiedAbove board pos = let up = pos <+> (-1,0) in
    if posOccupied up board then highestOccupiedAbove board up else pos

wordDownFrom :: Board -> Pos -> String
wordDownFrom board pos = case Map.lookup pos board of
    Just (Right char) -> char:(wordDownFrom board (pos <+> (1,0)))
    _ -> ""

-- Attempt to extend a *horizontal* Move with a particular Char.
-- Kinda ugly and complex.
extendHMWithLetter :: Board -> Dict -> Move -> Char -> Either String Move
extendHMWithLetter board dict hm@(Move hms hmd hmh hmp hmv hmm hmsc) char = do
    let nxt = hmNextPos hm  -- check positioning:
    unless (validPos nxt) $ Left $ "Word goes off board"
    let fromHand = not (posOccupied nxt board)  -- check sufficient letters in hand:
    newHand <- if fromHand then takeFromHand char hmh else return hmh

    case Map.lookup (toUpper char) (PT.children hmd) of -- check valid prefix:
        Nothing -> Left $ "No word starting " ++ mGetWord hm ++ [char]
                       ++ " in dictionary" -- User shouldn't actually see this

        Just newPT -> do { -- THIS DO BLOCK CONTINUES TO END OF DEFINITION

    if not fromHand  -- then updating is pretty simple, no new crosswords etc:
    then case Map.lookup nxt board of
        Just (Right c) | c == char -> return $ hm {mDict = newPT,
                                  mPlaces = ((nxt,char,False):hmp),
                                  mScore = (hmsc + charValue char)}
        Just (Right c) -> Left $ "Can't place " ++ [char] ++ " on top of " ++ [c]
        _ -> error "Internal Error in function extendHMWithLetter!"

    else do  -- updating from hand is more complex:
    let vcwStart = highestOccupiedAbove board nxt
        vcw = wordDownFrom (Map.insert nxt (Right char) board) vcwStart
    when (length vcw > 1 && not (vcw `realWord` dict))  -- check valid/no crossword
         (Left $ "Invalid cross word " ++ vcw)
    -- otherwise keep going
    let vcwBaseScore = baseScore vcw  -- discard later if it's 1 char
        (vcwScore,newScore,newMultipliers) = case Map.lookup nxt board of
            Just (Left (LetterBonus n)) -> (vcwBaseScore + (n-1)*(charValue char),
                                            hmsc + n*(charValue char), hmm)
            Just (Left (WordBonus n)) -> (n*vcwBaseScore,
                                          hmsc + (charValue char), (n:hmm))
            _ -> (vcwBaseScore, hmsc + (charValue char), hmm)  -- no bonus
    let newHMV = if length vcw == 1 then hmv else ((vcwStart,vcw,vcwScore):hmv)
    
    return $ Move hms newPT newHand ((nxt,char,True):hmp) newHMV
                  newMultipliers newScore
    }

mGetWord :: Move -> String
mGetWord m = reverse [char | (_,char,_) <- mPlaces m]

takeFromHand :: Char -> Hand -> Either String Hand
takeFromHand char hand = let toRemove = if isLower char then '_' else char in
    if toRemove `Map.member` hand
    then Right $ Bag.remove toRemove hand
    else Left $ "Insufficient " ++ [toRemove] ++ " characters in hand"

mNumFromHand :: Move -> Int
mNumFromHand m = length [() | (_,_,True) <- mPlaces m]

-- Sometimes List is the more convenient monad to work in
rightToList :: Either e a -> [a]
rightToList (Right a) = [a]
rightToList _ = []

-- Find all possible extensions of a horizontal Move, via extendHMWithLetter above
hmSuccessors :: Board -> Dict -> Move -> [Move]
hmSuccessors board dict hm = let nxt = hmNextPos hm in
    case Map.lookup nxt board of
        Just (Right char) -> rightToList $ extendHMWithLetter board dict hm char
        _ -> do
            char <- Map.keys $ mHand hm
            char' <- if char /= '_' then [char] else ['a'..'z']  -- blank to lower
            rightToList $ extendHMWithLetter board dict hm char'

-- Depth-first search function for tree search spaces (sufficient for findHMovesAt)
dfs :: (a -> [a]) -> a -> (a -> Bool) -> [a]
dfs successors start isComplete = loop [start] [] where
    loop stack results = case stack of
        [] -> results
        (x:xs) -> let stack' = (successors x) ++ xs
                      results' = if isComplete x then x:results else results
                  in  loop stack' results'

-- Use a depth-first search to find all horizontal Moves, at a position, of
-- sufficient length.
findHMovesAt :: Board -> Dict -> Hand -> Pos -> Int -> [Move]
findHMovesAt board dict hand pos minLength = dfs successors start isComplete
  where
    successors = hmSuccessors board dict
    start = Move pos dict hand [] [] [] 0
    isComplete hm = hmWordEnd board hm && mNumFromHand hm >= minLength

-- Find valid horizontal moves everywhere.
findHMoves :: Board -> Dict -> Hand -> [Move]
findHMoves board dict hand = do
    let handSize = Bag.size hand
    row <- [1..15]
    (col,minLength) <- zip [1..] $ minHLengths board row
    guard $ minLength <= handSize  -- No need to chase dead ends
    findHMovesAt board dict hand (row,col) minLength

-- Now easily find vertical moves via transposition

swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)

transposeMap :: Map.Map Pos a -> Map.Map Pos a
transposeMap mp = Map.fromList [(swap p,a) | (p,a) <- Map.toList mp]

transposeMove :: Move -> Move
transposeMove m@(Move ms _ _ mp mc _ _) =
    m {mStart = swap ms, mPlaces = [(swap p,c,b) | (p,c,b) <- mp],
       mCrosswords = [(swap p,s,i) | (p,s,i) <- mc]}

-- Find all valid vertical moves.
findVMoves :: Board -> Dict -> Hand -> [Move]
findVMoves b d h = map transposeMove $ findHMoves (transposeMap b) d h

-- Certain somewhat rare one-letter moves can be duplicated here (found as both
-- horizontal and vertical moves), I don't think it's worth filtering them out.
findAllMoves :: Board -> Dict -> Hand -> [Move]
findAllMoves b d h = findHMoves b d h ++ findVMoves b d h


-- Now on to checking user-input moves

-- Representation of user input
type UserMove = (Dir,Pos,String)
data Dir = Hor | Vert deriving (Eq,Show)

-- Read properly formatted user input to a UserMove
readUserMove :: String -> Either String UserMove
readUserMove s = case readUserMoveList s of
    [] -> Left $ "Improperly formatted move"
    (um:_) -> Right um

readUserMoveList :: String -> [UserMove]
readUserMoveList s = case words s of
    [sdir,srow,scol,sletters] -> do
        dir <- case sdir of
            "H" -> return Hor
            "V" -> return Vert
            _ -> []
        (row,"") <- reads srow
        (col,"") <- reads scol
        return (dir,(row,col),sletters)
    _ -> []


-- Checks a UserMove, converting it to a Move if valid
userMoveToMove :: Board -> Dict -> Hand -> UserMove -> Either String Move
-- Horizontal case:
userMoveToMove board dict hand (Hor,pos,str) = do
    unless (str `realWord` dict) (Left $ "Word " ++ map toUpper str
                                                 ++ " not in dictionary")
    let startHM = Move pos dict hand [] [] [] 0
        loop = extendHMWithLetter board dict
    hm <- foldM loop startHM str
    unless (hmWordEnd board hm) $ Left $ "Incomplete word " ++ str
    if length (mGetWord hm) < 2 then Left "Words must be at least 2 characters"
    else do
    let (r,c) = pos
        k = minHLengths board r !! (c-1)  -- tricky off-by-one
    unless (mNumFromHand hm >= k) $ Left $ "Illegal word placement"
    return hm
-- Vertical case:
userMoveToMove board dict hand (Vert,pos,str) = fmap transposeMove $
    userMoveToMove (transposeMap board) dict hand (Hor,swap pos,str)


-- Score a valid Move and generate the "report" message that ought to be printed
reportMove :: Move -> (Int,String)
reportMove m = (total,report)
  where
    wordScore = mScore m * product (mMultipliers m)
    format (pos,str,int) = str ++ " at " ++ show pos ++ ": " ++ show int ++ " points"
    wordLine = "Word " ++ format (mStart m, mGetWord m, wordScore)
    xwordLines = map (\s -> "Crossword " ++ format s) $ mCrosswords m
    bonus = length [() | (_,_,True) <- mPlaces m] == 7
    bonusScore = if bonus then 50 else 0
    bonusLines = if bonus then ["BINGO!!! 50 bonus points"] else []
    total = wordScore + sum [x | (_,_,x) <- mCrosswords m] + bonusScore
    report = unlines $ (wordLine:xwordLines) ++ bonusLines ++
                       ["Total: " ++ show total ++ " points"]

-- Internal representation of the game state. Player 1 is human; 2 is computer
data GameState = GameState { gsBoard :: Board
                           , gsLetters :: String
                           -- easier to shuffle/sample from list than Bag
                           , gsScore1 :: Int
                           , gsScore2 :: Int
                           , gsHand1 :: Hand
                           , gsHand2 :: Hand
                           , gsZeros :: Int
                           , gsRand :: StdGen } deriving (Show)
                           -- No Eq instance for StdGen

data Player = P1 | P2 deriving (Eq,Show)

putMoveOnBoard :: Board -> Move -> Board
putMoveOnBoard board m = foldl loop board (mPlaces m)
  where
    loop brd (pos,char,_) = Map.insert pos (Right char) brd

-- Update GameState based on Move, and also return the message from reportMove
recordMoveForPlayer :: Player -> GameState -> Move -> (GameState,String)
recordMoveForPlayer player gs m =
    let newBoard = putMoveOnBoard (gsBoard gs) m
        (pts,msg) = reportMove m
        -- Technically possible corner case:
        newZs = if pts > 0 then 0 else 1 + gsZeros gs in
    ( case player of
        P1 -> let newScore1 = (gsScore1 gs) + pts
                  (newHand1, newLetters) = refillHand (mHand m) (gsLetters gs)
              in  gs {gsBoard = newBoard, gsScore1 = newScore1, gsHand1 = newHand1,
                      gsLetters = newLetters, gsZeros = newZs}
        P2 -> let newScore2 = (gsScore2 gs) + pts
                  (newHand2, newLetters) = refillHand (mHand m) (gsLetters gs)
              in  gs {gsBoard = newBoard, gsScore2 = newScore2, gsHand2 = newHand2,
                      gsLetters = newLetters, gsZeros = newZs}
     , msg )  -- it's a tuple

-- Assumes the letters are already shuffled
refillHand :: Hand -> String -> (Hand,String)
refillHand hand letters = let n = 7 - Bag.size hand in
    (foldl (flip Bag.insert) hand (take n letters), drop n letters)


-- As an alternative to moving, a player may pass (aka exchange)
data Action = MoveAction Move | PassAction Pass deriving (Eq,Show)
type Pass = (String,Hand)  -- what you're giving up vs keeping

emptyPass :: Hand -> Pass
emptyPass h = ("",h)

recordPassForPlayer :: Player -> GameState -> Pass -> (GameState,String)
recordPassForPlayer player gs (give,keep) =
    let (newHand,tempLetters) = refillHand keep (gsLetters gs)
        newZeros = 1 + gsZeros gs
        (newLetters,newRand) = shuffle (give ++ tempLetters) (gsRand gs)
        gs' = gs {gsLetters = newLetters, gsZeros = newZeros, gsRand = newRand}
    in  ( case player of
            P1 -> gs' {gsHand1 = newHand}
            P2 -> gs' {gsHand2 = newHand}
        , "pass " ++ show (length give) )

-- O(n*log n) functional shuffle is good enough here
shuffle :: (RandomGen g) => [a] -> g -> ([a],g)
shuffle xs gen = let (mp, gen') = foldl step start [1..n-1]
                 in  (Map.elems mp, gen')
  where
    n = length xs
    start = (Map.fromList (zip [1..n] xs), gen)
    swap mp i j = Map.insert j (mp Map.! i) (Map.insert i (mp Map.! j) mp)
    step (mp,gen) k = let (j,gen') = randomR (k,n) gen
                      in  (swap mp k j, gen')

recordActionForPlayer :: Player -> GameState -> Action -> (GameState,String)
recordActionForPlayer p g (PassAction pass) = recordPassForPlayer p g pass
recordActionForPlayer p g (MoveAction move) = recordMoveForPlayer p g move



-- Now we can get Actions from both the computer and human.


-- TODO (maybe): think of a nice way to constrain the computer's move/pass actions
-- to only the obviously valid ones?
-- Probably by restructuring modules, hiding constructors of *everything*, i.e.
-- not just Pass and Move, but also Board, Dict, Hand...
-- Or a less nice way but easier way: feed the actions back to userMoves and
-- Strings, then send them through userMoveToMove and checkPlayerPass, and raise
-- an error on Left string.

-- TODO (maybe): more sophisticated AI could e.g. avoid opening up double/triple
-- word scores, or play the endgame perfectly by searching the whole game tree.

getCompAction :: Board -> Dict -> Hand -> Int -> Int -> Int -> Int -> Action
getCompAction b d h numLettersRem playerScore compScore numZeros = 
    case findAllMoves b d h of
        [] -> PassAction $ emptyPass h
        -- If there are letters remaining, play highest scoring move, adjusted
        -- with very simple heuristic favoring saving blanks and S's, hoping to
        -- get better value out of them later
        moves | numLettersRem > 0 -> MoveAction $ maximumWithKey util moves
          where
            util move = fst (reportMove move) +
                        5 * quantSaved 'S' move + 10 * quantSaved '_' move
            quantSaved char move = Bag.quantity char (mHand move)
        -- Otherwise, when there are no letters remaining, deduce the opponent's
        -- hand and reap extra points for moves using all letters in own hand.
        -- An imperfect (but probably reasonably good) endgame heuristic.
        moves -> MoveAction $ maximumWithKey util moves
          where
            onBoard = Bag.fromList [if isUpper c then c else '_' |
                                       Right c <- Map.elems b]
            startBag = Bag.fromList startingLetters
            oppHand = (startBag `Bag.difference` onBoard) `Bag.difference` h
            bonus = 2 * baseScore (Bag.toList oppHand)
            util move = fst (reportMove move) +
                        if Map.null (mHand move) then bonus else 0

-- Unsafe like Prelude.maximum
maximumWithKey :: (Ord k) => (a -> k) -> [a] -> a
maximumWithKey key (x:xs) = fst $ foldl loop start xs
  where
    start = (x,key x)
    loop (a1,k1) a2 = let k2 = key a2 in
        if k2 > k1 then (a2,k2) else (a1,k1)
maximumWithKey _ [] = error "maximumWithKey: empty list"

-- This can *only* come up with valid Actions. Safe to record them.
getPlayerAction :: GameState -> Dict -> IO Action
getPlayerAction gs dict = do
    input <- getLine
    case words input of
      w:letters | map toUpper w == "PASS" ->
        case checkPlayerPass gs $ join letters of
            Left s -> putStrLn s >> getPlayerAction gs dict
            Right pass -> return $ PassAction pass
      _ ->
        case readUserMove input >>= userMoveToMove (gsBoard gs) dict (gsHand1 gs) of
            Left s -> putStrLn s >> getPlayerAction gs dict
            Right hm -> return $ MoveAction hm

-- Checks validity of an attempted Pass
checkPlayerPass :: GameState -> String -> Either String Pass
checkPlayerPass gs letters = if null letters then Right ("", gsHand1 gs)
    else if length (gsLetters gs) < 7
    then Left "Can't exchange letters when there are fewer than 7 remaining"
    else foldM loop ("", gsHand1 gs) letters
      where
        loop (pass,hand) char = do
            rem <- takeFromHand char hand  -- checks that letter is in hand
            return (char:pass,rem)


-- More game control


displayBoard :: Board -> IO ()
displayBoard = putStrLn . showBoard

displayGameState :: GameState -> IO ()
displayGameState = putStrLn . showGameState

showBoard :: Board -> String
showBoard board = unlines $ ["  | 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5",
                             "--+------------------------------"] ++
    [show (r `mod` 10) ++ " | " ++ unwords [[posToChar (r,c)] | c <- [1..15]]
         | r <- [1..15]]
  where
    posToChar pos = case Map.lookup pos board of
        Just (Right char) -> char
        Just (Left (WordBonus 2)) -> '@'
        Just (Left (WordBonus 3)) -> '#'
        Just (Left (LetterBonus 2)) -> '2'
        Just (Left (LetterBonus 3)) -> '3'
        _                           -> ' '

showGameState :: GameState -> String
showGameState gs = unlines $
    [ showBoard (gsBoard gs)
    , "Your score: " ++ show (gsScore1 gs)
    , "My score: " ++ show (gsScore2 gs)
    , "Your hand: " ++ Bag.toList (gsHand1 gs)
    , "My hand: " ++ show (Bag.size (gsHand2 gs))
    , "Letters remaining: " ++ show (length (gsLetters gs)) ]
    ++ if gsZeros gs > 0
       then ["Consecutive no-score turns: " ++ show (gsZeros gs)]
       else []

newGameState :: Dict -> StdGen -> GameState
newGameState dict gen = GameState startingBoard letters2 0 0 h1 h2 0 gen'
  where
    (letters,gen') = shuffle startingLetters gen
    (h1,letters1) = refillHand Bag.empty letters
    (h2,letters2) = refillHand Bag.empty letters1

startNewGame :: Dict -> StdGen -> Player -> IO ()
startNewGame dict gen startingPlayer = do
    let gs = newGameState dict gen
    displayGameState gs
    mainLoop dict gs startingPlayer

mainLoop :: Dict -> GameState -> Player -> IO ()
-- Human case
mainLoop dict gs P1 = do
    putStrLn "YOUR MOVE:"
    act <- getPlayerAction gs dict
    let (gs',msg) = recordActionForPlayer P1 gs act
    putStrLn msg
    displayGameState gs'
    if gsOver gs' then endGame dict gs' else mainLoop dict gs' P2
-- Computer case
mainLoop dict gs P2 = do
    putStrLn "MY MOVE:"
    let act = getCompAction (gsBoard gs) dict (gsHand2 gs) (length (gsLetters gs))
                            (gsScore1 gs) (gsScore2 gs) (gsZeros gs)
        (gs',msg) = recordActionForPlayer P2 gs act
    putStrLn msg
    displayGameState gs'
    if gsOver gs' then endGame dict gs' else mainLoop dict gs' P1

gsOver :: GameState -> Bool
gsOver gs = gsZeros gs >= 6 ||
    null (gsLetters gs) && (Map.null (gsHand1 gs) || Map.null (gsHand2 gs))

-- Irritatingly, endGame needs the dictionary to pass to the new game
endGame :: Dict -> GameState -> IO ()
endGame dict gs = do
    putStrLn "GAME OVER"
    let hand1 = Bag.toList $ gsHand1 gs
        hand2 = Bag.toList $ gsHand2 gs
        ext1 = baseScore hand1
        ext2 = baseScore hand2

    (score1,score2) <- if null hand1 then do
            putStrLn $ "My hand: " ++ hand2 ++ " (" ++ show ext2
                       ++ " excesss points)"
            return (gsScore1 gs + 2*ext2, gsScore2 gs)
        else if null hand2 then do
            putStrLn $ "Your hand: " ++ hand1 ++ " (" ++ show ext1
                       ++ " excesss points)"
            return (gsScore1 gs, gsScore2 gs + 2*ext1)
        else do
            putStrLn $ "My hand: " ++ hand2 ++ " (" ++ show ext2
                       ++ " excesss points)"
            putStrLn $ "Your hand: " ++ hand2 ++ " (" ++ show ext1
                       ++ " excesss points)"
            return (gsScore1 gs - ext1, gsScore2 gs - ext2)

    putStrLn $ "FINAL SCORE: You: " ++ show score1 ++ ", Me: " ++ show score2
    if score1 > score2 then putStrLn "YOU WIN!"
    else if score1 < score2 then putStrLn "I WIN!"
    else putStrLn $ "IT'S A TIE! Broken by preadjusted scores: your " ++
                    show (gsScore1 gs) ++ " to my " ++ show (gsScore2 gs) ++
                    " says... " ++ if gsScore1 gs > gsScore2 gs then "YOU WIN!"
                              else if gsScore1 gs < gsScore2 gs then "I WIN!"
                                 else "IT'S STILL A TIE!"

    putStrLn $ "\nPress Enter to play again. I'll let you go first. Or type the"
             ++ " number 2 before entering if you'd prefer to go second."
    answer <- getLine
    let startingPlayer = if null answer || head answer /= '2' then P1 else P2
    startNewGame dict (gsRand gs) startingPlayer


main :: IO ()
main = do
    args <- getArgs
    if null args
    then putStrLn $ "Usage: please provide a dictionary file as command line "
                 ++ "argument. Optionally follow this argument with number 2 "
                 ++ "to play second, e.g.\n./Scrabble dictionary.txt 2"
    else do
    let dictPath = head args
        startingPlayer = case tail args of
            ("2":_) -> P2
            _ -> P1
    putStrLn $ "Loading dictionary from file " ++ dictPath ++ "..."
    dictHandle <- SIO.openFile dictPath SIO.ReadMode
    (dict,size) <- hGetDictionaryAndSize dictHandle
    -- Printing size forces enough evaluation that I can close the file handle
    putStrLn $ "Done. " ++ show size ++ " words loaded.\n"
    SIO.hClose dictHandle

    gen <- getStdGen
    startNewGame dict gen startingPlayer

hGetDictionaryAndSize :: SIO.Handle -> IO (Dict,Int)
hGetDictionaryAndSize handle = do
    contents <- SIO.hGetContents handle
    let usableWords = Set.fromList  -- Set.size is faster than PT.size
          [map toUpper w | w <- words contents, all isAlpha w && length w >= 2]
    return $ (PT.fromList (Set.toList usableWords), Set.size usableWords)





