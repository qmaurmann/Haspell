-- Biggest TODO: split off a smallish piece as Main.hs, and only export a small
-- number of definitions from this module.

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

-- Depth-first search function for tree search spaces (sufficient for word search)
dfs :: (a -> [a]) -> a -> (a -> Bool) -> [a]
dfs successors start isComplete = loop [start] [] where
    loop stack results = case stack of
        [] -> results
        (x:xs) -> let stack' = (successors x) ++ xs
                      results' = if isComplete x then x:results else results
                  in  loop stack' results'

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

-- Lookup word, ignoring case (so blanks will work)
realWord :: String -> Dict -> Bool
realWord word dict = map toUpper word `PT.member` dict

size :: Int
size = 15

-- A hack, especially because infinity + 1 /= infinity, but close enough that
-- it will work.
infinity :: Int
infinity = 100

validPos :: Pos -> Bool
validPos (x,y) = 1 <= min x y && max x y <= size

(<+>) :: Pos -> Pos -> Pos
(x,y) <+> (z,w) = (x+z,y+w)


-- Most of the rest of the definitions facilitate searching for horizontal moves.
-- Once we can do that, searching for vertical moves is easy via transposing the
-- board.



-- When starting a horizontal move at a given position in a given row, determine
-- the minimum number of letters one would have to play to make a valid move for a
-- word that starts at that position.

-- The recursive structure of this problem suggests building the list of all
-- minimum lengths for a given row "dynamically", so we give a length 16 list with
-- all such numbers. The last element (infinity) of the list corresponds to the
-- non-existant 16th column.

minHLengths :: Board -> Int -> [Int]
minHLengths board row = foldl loop [infinity] [size,size-1..1]
  where
    loop :: [Int] -> Int -> [Int]
    loop list col = (new:list) where
      new = if letter (col-1) then infinity  -- the word can't start *here*
        else if letter col then 1  -- have to play at least 1 letter
        else if (row,col) == (8,8) then 1 -- this trick makes opening move possible!
        else if letter (col+1) then 1     -- ^ can break if one-letter word in Dict
        else if aboveOrBelowLetter col then 2
        else if aboveOrBelowLetter (col+1) then 2
        else 1 + (head list)  -- "recursive" call

    letter :: Int -> Bool
    letter col = posOccupied (row,col) board

    aboveOrBelowLetter :: Int -> Bool
    aboveOrBelowLetter col = posOccupied (row-1,col) board
                          || posOccupied (row+1,col) board


-- Representation of placement of a Char at a Pos, designating whether or not
-- that Char actually came from the Hand (vs from the Board)
type Place = (Pos,Char,Bool)

-- Starting position, the word, and the score you'll get from it
type VCWord = (Pos,String,Int)

-- Representation for partial matches of horizontal words, to be used in a depth-
-- first search.

-- Nothing fundamentally forces these to be horizontal though.
-- TODO: rename to Move

data HMove = HMove { hmStart :: Pos  -- start position
                   , hmDict :: Dict  -- where we are in PrefixTree
                   , hmHand :: Hand  -- what's left in hand
                   , hmPlaces :: [Place]  --
                   , hmVCWords :: [VCWord]  -- vertical cross words so far
                   , hmMultipliers :: [Int]  -- multipliers for later
                   , hmScore :: Int  -- tentative score, ignoring vcws and mults
                   } deriving (Eq,Show)

-- Next position that will be filled for an HMove
hmNextPos :: HMove -> Pos
hmNextPos hm = case hmPlaces hm of
    [] -> hmStart hm
    ((pos,_,_):_) -> pos <+> (0,1)

-- Test that an HMove spans a valid horizontal word. (Still have to check enough
-- letters have been played from hand, as in minHLengths.)
hmWordEnd :: Board -> HMove -> Bool
hmWordEnd board hm = PT.wordEnd (hmDict hm) &&
    not (posOccupied (hmNextPos hm) board)  -- word does not continue

highestOccupiedAbove :: Board -> Pos -> Pos
highestOccupiedAbove board pos = let up = pos <+> (-1,0) in
    if posOccupied up board then highestOccupiedAbove board up else pos

wordDownFrom :: Board -> Pos -> String
wordDownFrom board pos = case Map.lookup pos board of
    Just (Right char) -> char:(wordDownFrom board (pos <+> (1,0)))
    _ -> ""


rightToList :: Either e a -> [a]
rightToList (Right a) = [a]
rightToList _ = []

-- Attempt to extend an HMove with a particular (valid!) Place.
-- Kinda ugly and complex.

extendHMWithLetter :: Board -> Dict -> HMove -> Char -> Either String HMove
extendHMWithLetter board dict hm@(HMove hms hmd hmh hmp hmv hmm hmsc) char = do
    let nxt = hmNextPos hm  -- check positioning:
    unless (validPos nxt) $ Left $ "Position " ++ show nxt ++ " off board"
    let fromHand = not (posOccupied nxt board)  -- check sufficient letters in hand:
    newHand <- if fromHand then takeFromHand char hmh else return hmh

    case Map.lookup (toUpper char) (PT.children hmd) of -- check valid prefix:
        Nothing -> Left $ "Invalid word starting " ++ hmGetWord hm ++ [char]
                          ++ " at " ++ show (hmStart hm)

        Just newPT -> do { -- THIS DO BLOCK CONTINUES TO END OF DEFINITION

    if not fromHand  -- then updating is pretty simple, no new crosswords etc:
    then case Map.lookup nxt board of
        Just (Right c) | c == char -> return $ hm {hmDict = newPT,
                                  hmPlaces = ((nxt,char,False):hmp),
                                  hmScore = (hmsc + charValue char)}
        Just (Right c) -> Left $ "Can't place " ++ [char] ++ " on top of " ++ [c]
        _ -> error "Internal Error in function extendHMWithLetter!"

    else do  -- updating from hand is more complex:
    let vcwStart = highestOccupiedAbove board nxt
        vcw = wordDownFrom (Map.insert nxt (Right char) board) vcwStart
    when (length vcw > 1 && not (vcw `realWord` dict))  -- check valid/no crossword
         (Left $ "Invalid cross word " ++ vcw ++ " at " ++ show (hmStart hm))
    -- otherwise keep going
    let vcwBaseScore = baseScore vcw  -- discard later if it's 1 char
        (vcwScore,newScore,newMultipliers) = case Map.lookup nxt board of
            Just (Left (LetterBonus n)) -> (vcwBaseScore + (n-1)*(charValue char),
                                            hmsc + n*(charValue char), hmm)
            Just (Left (WordBonus n)) -> (n*vcwBaseScore,
                                          hmsc + (charValue char), (n:hmm))
            _ -> (vcwBaseScore, hmsc + (charValue char), hmm)  -- no bonus
    let newHMV = if length vcw == 1 then hmv else ((vcwStart,vcw,vcwScore):hmv)
    
    return $ HMove hms newPT newHand ((nxt,char,True):hmp) newHMV
                   newMultipliers newScore
    }

hmGetWord :: HMove -> String
hmGetWord hm = reverse [char | (_,char,_) <- hmPlaces hm]

takeFromHand :: Char -> Hand -> Either String Hand
takeFromHand char hand = let toRemove = if isLower char then '_' else char in
    if toRemove `Map.member` hand
    then Right $ Bag.remove toRemove hand
    else Left $ "Insufficient " ++ [toRemove] ++ " characters in hand."


hmNumFromHand :: HMove -> Int
hmNumFromHand hm = length [() | (_,_,True) <- hmPlaces hm]

-- Find all possible extensions of the HMove, by generating the appropriate
-- Places to pass to extendHMWithPlace.

hmSuccessors :: Board -> Dict -> HMove -> [HMove]
hmSuccessors board dict hm = let nxt = hmNextPos hm in
    case Map.lookup nxt board of
        Just (Right char) -> rightToList $ extendHMWithLetter board dict hm char
        _ -> do
            char <- Map.keys $ hmHand hm
            char' <- if char /= '_' then [char] else ['a'..'z']  -- blank to lower
            rightToList $ extendHMWithLetter board dict hm char'

-- Use a depth-first search to find all HMoves at a position of sufficient length.
findHMovesAt :: Board -> Dict -> Hand -> Pos -> Int -> [HMove]
findHMovesAt board dict hand pos minLength = dfs successors start isComplete
  where
    successors = hmSuccessors board dict
    start = HMove pos dict hand [] [] [] 0
    isComplete hm = hmWordEnd board hm && hmNumFromHand hm >= minLength

-- Find all valid horizontal moves.
findHMoves :: Board -> Dict -> Hand -> [HMove]
findHMoves board dict hand = do  -- fancy list comprehension
    let handSize = Bag.size hand
    row <- [1..size]
    (col,minLength) <- zip [1..] $ minHLengths board row
    guard $ minLength <= handSize  -- No need to chase dead ends
    findHMovesAt board dict hand (row,col) minLength

-- Find all valid vertical moves.
findVMoves :: Board -> Dict -> Hand -> [HMove]
findVMoves b d h = map transposeHMove $ findHMoves (transposeMap b) d h

-- Certain somewhat one-letter moves can be duplicated here (found as both
-- horizontal and vertical moves here), but I don't think it's a real problem.
findAllMoves :: Board -> Dict -> Hand -> [HMove]
findAllMoves b d h = findHMoves b d h ++ findVMoves b d h

swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)

transposeMap :: Map.Map Pos a -> Map.Map Pos a
transposeMap mp = Map.fromList [(swap p,a) | (p,a) <- Map.toList mp]

transposeHMove :: HMove -> HMove
transposeHMove hm@(HMove hms _ _ hmp hmv _ _) =
    hm {hmStart = swap hms, hmPlaces = [(swap p,c,b) | (p,c,b) <- hmp],
        hmVCWords = [(swap p,s,i) | (p,s,i) <- hmv]}

-- hack with r.e.'s; i won't have to do it that often, so okay
-- TODO, or remove the corresponding functionality.
transposeMessage :: String -> String
transposeMessage m = "TODO: transpose(" ++ m ++ ")"

transposeEither :: Either String HMove -> Either String HMove
transposeEither (Right hm) = Right (transposeHMove hm)    -- fmap
transposeEither (Left msg) = Left (transposeMessage msg)  -- not fmap




-- Closer representation of user input
type UserMove = (Dir,Pos,String)
data Dir = Hor | Vert deriving (Eq,Show)

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

readUserMove :: String -> Either String UserMove
readUserMove s = case readUserMoveList s of
    [] -> Left $ "Improperly formatted move!"
    (um:_) -> Right um

-- Checks a UserMove, converting it to an HMove if valid
umToHM :: Board -> Dict -> Hand -> UserMove -> Either String HMove
umToHM board dict hand (Hor,pos,str) = do
    let startHM = HMove pos dict hand [] [] [] 0
        loop = extendHMWithLetter board dict
    hm <- foldM loop startHM str
    unless (hmWordEnd board hm) $ Left $ "Incomplete word " ++ str
    if length (hmGetWord hm) < 2 then Left "Words must be at least 2 characters"
    else do
    let (r,c) = pos
        m = minHLengths board r !! (c-1)  -- tricky off-by-one
    unless (hmNumFromHand hm >= m) $ Left $ "Illegal word placement"
    return hm
-- Vertical case:
umToHM board dict hand (Vert,pos,str) = transposeEither $
    umToHM (transposeMap board) dict hand (Hor,swap pos,str)


-- Score a valid HMove and generate the "report" message that ought to be printed
reportHMove :: HMove -> (Int,String)
reportHMove hm = (total,report)
  where
    wordScore = hmScore hm * product (hmMultipliers hm)
    format (pos,str,int) = str ++ " at " ++ show pos ++ ": " ++ show int ++ " points"
    wordLine = "Word " ++ format (hmStart hm, hmGetWord hm, wordScore)
    xwordLines = map (\s -> "Crossword " ++ format s) $ hmVCWords hm
    bonus = length [() | (_,_,True) <- hmPlaces hm] == 7
    bonusScore = if bonus then 50 else 0
    bonusLines = if bonus then ["BINGO!!! 50 bonus points"] else []
    total = wordScore + sum [x | (_,_,x) <- hmVCWords hm] + bonusScore
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

putHMoveOnBoard :: Board -> HMove -> Board
putHMoveOnBoard board hm = foldl loop board (hmPlaces hm)
  where
    loop brd (pos,char,_) = Map.insert pos (Right char) brd

recordHMoveForPlayer :: Player -> GameState -> HMove -> (GameState,String)
recordHMoveForPlayer player gs hm =
    let newBoard = putHMoveOnBoard (gsBoard gs) hm
        (pts,msg) = reportHMove hm
        -- Technically possible corner case:
        newZs = if pts > 0 then 0 else 1 + gsZeros gs in
    ( case player of
        P1 -> let newScore1 = (gsScore1 gs) + pts
                  (newHand1, newLetters) = refillHand (hmHand hm) (gsLetters gs)
              in  gs {gsBoard = newBoard, gsScore1 = newScore1, gsHand1 = newHand1,
                      gsLetters = newLetters, gsZeros = newZs}
        P2 -> let newScore2 = (gsScore2 gs) + pts
                  (newHand2, newLetters) = refillHand (hmHand hm) (gsLetters gs)
              in  gs {gsBoard = newBoard, gsScore2 = newScore2, gsHand2 = newHand2,
                      gsLetters = newLetters, gsZeros = newZs}
     , msg )  -- it's a tuple


-- Unsafe like Prelude.maximum
maximumWithKey :: (Ord k) => (a -> k) -> [a] -> a
maximumWithKey key (x:xs) = fst $ foldl loop start xs
  where
    start = (x,key x)
    loop (a1,k1) a2 = let k2 = key a2 in
        if k2 > k1 then (a2,k2) else (a1,k1)
maximumWithKey _ [] = error "maximumWithKey: empty list"



data Action = MoveAction HMove | PassAction Pass deriving (Eq,Show)
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

recordActionForPlayer :: Player -> GameState -> Action -> (GameState,String)
recordActionForPlayer p g (PassAction pass) = recordPassForPlayer p g pass
recordActionForPlayer p g (MoveAction move) = recordHMoveForPlayer p g move


-- TODO: make explicit all possible arguments, which the computer may ignore.
-- Also, restructure modules to hide all HMove constructors except umToHM and
-- findAllMoves. Possibly includes renaming HMove to Move!

-- TODO: think of a way to constrain the computer's pass actions to only the
-- obviously valid ones, ala umToHM and findAllMoves???

-- TODO: it's painful to see the computer "throw away" blanks and S's for little
-- to no gain. Implement a simple saving heuristic?

getCompAction :: Board -> Dict -> Hand -> Int -> Int -> Int -> Int -> Action
getCompAction b d h numLettersRem playerScore compScore numZeros = 
    case findAllMoves b d h of
        [] -> PassAction $ emptyPass h
        moves -> MoveAction $ maximumWithKey util moves
          where
            util = fst . reportHMove


-- This can *only* come up with valid Actions. Safe to record them.
getPlayerAction :: GameState -> Dict -> IO Action
getPlayerAction gs dict = do
    input <- getLine
    case words input of
        "pass":letters ->
            case checkPlayerPass gs $ join letters of
                Left s -> putStrLn s >> getPlayerAction gs dict
                Right pass -> return $ PassAction pass
        _ ->
            case readUserMove input >>= umToHM (gsBoard gs) dict (gsHand1 gs) of
                Left s -> putStrLn s >> getPlayerAction gs dict
                Right hm -> return $ MoveAction hm




checkPlayerPass :: GameState -> String -> Either String Pass
checkPlayerPass gs letters = if null letters then Right ("", gsHand1 gs)
    else if length (gsLetters gs) < 7
    then Left "Can't exchange letters when there are fewer than 7 remaining"
    else foldM loop ("", gsHand1 gs) letters
      where
        loop (pass,hand) char = do
            rem <- takeFromHand char hand
            return (char:pass,rem)



displayGameState :: GameState -> IO ()
displayGameState gs = do
    displayBoard $ gsBoard gs
    putStrLn $ "\nYour score: " ++ show (gsScore1 gs)
    putStrLn $ "My score: " ++ show (gsScore2 gs)
    putStrLn $ "Your hand: " ++ Bag.toList (gsHand1 gs)
    putStrLn $ "My hand: " ++ show (Bag.size (gsHand2 gs))
    putStrLn $ "Letters remaining: " ++ show (length (gsLetters gs))
    if gsZeros gs > 0
    then putStrLn $ "Consecutive no-score turns: " ++ show (gsZeros gs)
    else return ()


refillHand :: Hand -> String -> (Hand,String)
refillHand hand letters = let n = 7 - Bag.size hand in
    (foldl (flip Bag.insert) hand (take n letters), drop n letters)


main :: IO ()
main = do
    args <- getArgs
    if null args
    then putStrLn $ "Usage: please provide a dictionary file as command line "
                 ++ "argument. Optionally follow this argument with number 2 "
                 ++ "to play second, e.g.\n./Rules dictionary.txt 2"
    else do
    let dictPath = head args
        startingPlayer = case tail args of
            ("2":_) -> P2
            _ -> P1
    putStrLn $ "Loading dictionary from file " ++ dictPath ++ "..."
    dictHandle <- SIO.openFile dictPath SIO.ReadMode
    dict <- hGetDictionary dictHandle
    -- This is a hack to force evaluation so I can close the file.
    -- TODO: replace with somethat that doesn't slow the loading down as much.
    putStrLn $ "Done. " ++ show (PT.size dict) ++ " words loaded.\n"
    SIO.hClose dictHandle
    gen <- getStdGen
    newGame dict gen startingPlayer


shuffle :: (RandomGen g) => [a] -> g -> ([a],g)
shuffle xs gen = let (mp, gen') = foldl step start [1..n-1]
                 in  (Map.elems mp, gen')
  where
    n = length xs
    start = (Map.fromList (zip [1..n] xs), gen)
    swap mp i j = Map.insert j (mp Map.! i) (Map.insert i (mp Map.! j) mp)
    step (mp,gen) k = let (j,gen') = randomR (k,n) gen
                      in  (swap mp k j, gen')


startingLetters :: String
startingLetters = "AAAAAAAAABBCCDDDDEEEEEEEEEEEEFFGGGHHIIIIIIIIIJKLLL"
               ++ "LMMNNNNNNOOOOOOOOPPQRRRRRRSSSSTTTTTTUUUUVVWWXYYZ__"

newGame :: Dict -> StdGen -> Player -> IO ()
newGame dict gen startingPlayer = do
    let (letters,gen') = shuffle startingLetters gen
        (h1,letters1) = refillHand Bag.empty letters
        (h2,letters2) = refillHand Bag.empty letters1
        gs = GameState startingBoard letters2 0 0 h1 h2 0 gen'
    displayGameState gs
    mainLoop dict gs startingPlayer


mainLoop :: Dict -> GameState -> Player -> IO ()
mainLoop dict gs P1 = do
    putStrLn "\nYOUR MOVE:"
    act <- getPlayerAction gs dict
    let (gs',msg) = recordActionForPlayer P1 gs act
    putStrLn msg
    displayGameState gs'
    if gsOver gs' then endGame dict gs' else mainLoop dict gs' P2

mainLoop dict gs P2 = do
    putStrLn "\nMY MOVE:"
    -- TODO: make sure Int params in right order!
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
    putStrLn "\nGAME OVER"
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
    else putStrLn $ "IT'S A TIE! (Broken by preadjusted scores: your " ++
                    show (gsScore1 gs) ++ " to my " ++ show (gsScore2 gs) ++ ")." --TODO

    putStrLn $ "\nPress Enter to play again. I'll let you go first. Or type the"
             ++ " number 2 before entering if you'd prefer to go second."
    answer <- getLine
    let startingPlayer = if null answer || head answer /= '2' then P1 else P2
    newGame dict (gsRand gs) startingPlayer
















testFindH :: Board -> Dict -> Hand -> IO ()
testFindH b d h = do
    mapM_ (putStrLn . snd . reportHMove) $ findHMoves b d h

testFindV :: Board -> Dict -> Hand -> IO ()
testFindV b d h = do
    mapM_ (putStrLn . snd . reportHMove) $ findVMoves b d h




displayBoard :: Board -> IO ()
displayBoard = putStr . showBoard

showBoard :: Board -> String
showBoard board = unlines $ ["  | 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5",
                             "--+------------------------------"] ++
    [show (r `mod` 10) ++ " | " ++ unwords [[posToChar (r,c)] | c <- [1..size]]
         | r <- [1..size]]
  where
    posToChar pos = case Map.lookup pos board of
        Just (Right char) -> char
        Just (Left (WordBonus 2)) -> '@'
        Just (Left (WordBonus 3)) -> '#'
        Just (Left (LetterBonus 2)) -> '2'
        Just (Left (LetterBonus 3)) -> '3'
        _                           -> ' '

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



-- Take all usable words (alpha characters only, length >= 2) out of a file handle
hGetDictionary :: SIO.Handle -> IO Dict
hGetDictionary handle = do
    contents <- SIO.hGetContents handle
    let usableWords = [map toUpper w | w <- words contents,
                                       all isAlpha w && length w >= 2]
    return $ PT.fromList usableWords





