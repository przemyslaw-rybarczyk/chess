module ChessIO (startGame) where

import ChessTypes
import ChessMoves
import Control.Monad
import Control.Monad.State
import Data.Array
import Data.Function
import Data.Maybe

showPiece :: Piece -> String
showPiece piece = case pieceType piece of
    Pawn   -> "P"
    Knight -> "N"
    Bishop -> "B"
    Rook   -> "R"
    Queen  -> "Q"
    King   -> "K"

displaySquare :: Position -> Board -> String
displaySquare (x,y) board = bg ++ fg ++ letter
    where
        bg = "\ESC[48;5;" ++ (if even (x+y) then "166" else "172") ++ "m"
        fg = case fmap pieceColor (board ! (x,y)) of
            Nothing    -> ""
            Just White -> "\ESC[38;5;231m"
            Just Black -> "\ESC[38;5;16m"
        letter = "\ESC[1m" ++ maybe " " showPiece (board ! (x,y))

displayBoard :: Board -> Color -> String
displayBoard board color =
    unlines ([showNs] ++ map displayRow rows ++ [showNs ++ "\ESC[0m"])
    where
        displayRow y = showN y ++ concatMap (\x -> displaySquare (x,y) board) cols ++ showN y ++ "\ESC[49m"
        rows = case color of
            White -> [8,7..1]
            Black -> [1..8]
        cols = case color of
            White -> [1..8]
            Black -> [8,7..1]
        showN y = "\ESC[0m\ESC[48;5;22m\ESC[37m" ++ show y
        showNs  = "\ESC[0m\ESC[48;5;22m\ESC[37m " ++ map (toEnum . (+96)) cols ++ " \ESC[49m"

initPiece :: Position -> Maybe Piece
initPiece (x,y) = case y of
    1 -> Just $ makePiece xType White
    2 -> Just $ makePiece Pawn  White
    7 -> Just $ makePiece Pawn  Black
    8 -> Just $ makePiece xType Black
    _ -> Nothing
    where
        xType
            | x `elem` [1,8] = Rook
            | x `elem` [2,7] = Knight
            | x `elem` [3,6] = Bishop
            | x == 4         = Queen
            | x == 5         = King

initialBoard = array ((1,1),(8,8)) [((x,y), initPiece (x,y)) | x <- [1..8], y <- [1..8]]
emptyHistory = History { pastBoards = [], counter = 0 }

readPiece :: Char -> Maybe PieceType
readPiece c = case c of
    'N' -> Just Knight
    'B' -> Just Bishop
    'R' -> Just Rook
    'Q' -> Just Queen
    'K' -> Just King
    _   -> Nothing

readLetter :: Char -> Maybe Int
readLetter c =
    let n = fromEnum c - fromEnum 'a' + 1
     in if n > 0 && n <= 8 then Just n else Nothing

readNumber :: Char -> Maybe Int
readNumber c =
    let n = fromEnum c - fromEnum '0'
     in if n > 0 && n <= 8 then Just n else Nothing

readsHead :: (a -> Maybe b) -> [a] -> (Maybe b, [a])
readsHead _ [] = (Nothing, [])
readsHead f xxs@(x:xs) = case f x of
    Just y  -> (Just y, xs)
    Nothing -> (Nothing, xxs)

stateHead :: (a -> Maybe b) -> State [a] (Maybe b)
stateHead = state . readsHead

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither x y = case y of
    Nothing -> Left x
    Just y' -> Right y'

numbers :: Maybe Int -> [Int]
numbers x = case x of
    Nothing -> [1..8]
    Just y  -> [y]

readMove :: Board -> Color -> String -> Either String Move
readMove _ _ "0-0" = Right $ Castling Kingside
readMove _ _ "0-0-0" = Right $ Castling Queenside
readMove board color input = flip evalState input $ do
    mPiece <- stateHead readPiece
    let piece = fromMaybe Pawn mPiece
    xa <- stateHead readLetter
    ya <- stateHead readNumber
    xb <- stateHead readLetter
    yb <- stateHead readNumber
    promoted <- stateHead readPiece
    let ((startX, startY), mEnd) = if isNothing xb && isNothing yb
            then ((Nothing, Nothing), uncurry (liftM2 (,)) (xa, ya))
            else ((xa, ya), uncurry (liftM2 (,)) (xb, yb))
     in return $ do
        end <- maybeToEither "Invalid input" mEnd
        let pieces = filter isValidPiece [(x,y) | x <- numbers startX, y <- numbers startY]
            isValidPiece pos =
                   fmap pieceType (board ! pos) == Just piece
                && isJust (tryMove move board color)
                where move = Move pos end promoted
        case pieces of
            [start] -> Right $ Move start end promoted
            []      -> Left "Invalid move"
            _       -> Left "Ambiguous move"

doMove :: Board -> Color -> String -> Either String Board
doMove board color input = do
    move <- readMove board color input
    case tryMove move board color of
        Just board' -> Right board'
        Nothing     -> Left "Invalid move"

turn :: History -> Board -> Color -> IO ()
turn history board color 
    | noMoves && inCheck = do
        putStrLn "Checkmate"
        putStrLn $ show (nextColor color) ++ " wins"
        printBoard
    | noMoves = do
        putStrLn $ show color ++ " cannot move"
        putStrLn "Stalemate - Draw"
        printBoard
    | repetition = do
        putStrLn "Threefold repetition - Draw"
        printBoard
    | counter history >= 100 = do
        putStrLn "Fifty move rule - Draw"
        printBoard
    | otherwise = do
        when inCheck $ putStrLn "Check"
        putStrLn $ show color ++ "'s turn"
        printBoard
        input <- getLine
        case doMove board color input of
            Left msg -> do
                putStrLn msg
                turn history board color
            Right board' ->
                let movedPawn = ((==) `on` (filter isPawn . elems)) board board'
                    captured  = ((/=) `on` (length . filter isJust . elems)) board board'
                    counter' = if movedPawn || captured
                        then 0
                        else counter history + 1
                    history' = History
                        { pastBoards = board : pastBoards history
                        , counter = counter'
                        }
                 in turn history' board' (nextColor color)
    where
        inCheck = check board color
        noMoves = null (moves board color)
        repetition = length (filter (== board) (pastBoards history)) >= 2
        printBoard = putStr $ displayBoard board color
        isPawn piece = fmap pieceType piece == Just Pawn

startGame = turn emptyHistory initialBoard White
