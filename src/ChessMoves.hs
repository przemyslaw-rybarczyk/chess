module ChessMoves
    ( tryMove
    , check
    , moves
    ) where

import ChessTypes
import Data.Array
import Data.Foldable

forward :: Color -> Int
forward White = 1
forward Black = -1

exRange :: (Num a, Enum a, Ord a) => a -> a -> [a]
exRange a b
    | a <= b    = [a+1..b-1]
    | otherwise = reverse [b+1..a-1]

canMoveDiagonally :: Position -> Position -> Board -> Bool
canMoveDiagonally from to board =
    abs (fst to - fst from) == abs (snd to - snd from)
    && (all (== Nothing) . map (board !) $ zip (exRange (fst from) (fst to)) (exRange (snd from) (snd to)))

canMoveStraight :: Position -> Position -> Board -> Bool
canMoveStraight from to board
    | fst to == fst from = all (== Nothing) . map ((board !) . (\y -> (fst to, y))) $ exRange (snd from) (snd to)
    | snd to == snd from = all (== Nothing) . map ((board !) . (\x -> (x, snd to))) $ exRange (fst from) (fst to)
    | otherwise          = False

symmetries :: Position -> [Position]
symmetries (x,y) = [(x,y), (x,-y), (-x,y), (-x,-y), (y,x), (y,-x), (-y,x), (-y,-x)]

canBasicMove :: Position -> Position -> Board -> Bool
canBasicMove from to board = case pieceType piece of
    Pawn   ->
        dy == forward (pieceColor piece)
        && abs dx == case board ! to of
            Nothing -> 0
            Just _  -> 1
    Knight -> diff `elem` symmetries (1,2)
    Bishop -> canMoveDiagonally from to board
    Rook   -> canMoveStraight from to board
    Queen  -> canMoveDiagonally from to board || canMoveStraight from to board
    King   -> diff `elem` concatMap symmetries [(0,1), (1,1)]
    where
        diff@(dx, dy) = (fst to - fst from, snd to - snd from)
        Just piece = board ! from

doubleMove :: Position -> Position -> Board -> Maybe Board
doubleMove from@(x,y) to@(x',y') board = do
    pawn <- board ! from
    if     pieceType pawn == Pawn
        && not (moved pawn)
        && dx == 0
        && board ! to == Nothing
        && dy == 2 * forward (pieceColor pawn)
        && canMoveStraight from to board
    then return . movePassant from to $ board
    else Nothing
    where (dx, dy) = (x' - x, y' - y)

castle :: Position -> Position -> Board -> Maybe Board
castle from@(x,y) to@(x',y') board = do
    (rookFrom, rookTo) <- rookMove
    king <- board ! from
    rook <- board ! rookFrom
    let board' = movePiece from to . movePiece rookFrom rookTo $ board
    if     not (moved king)
        && not (moved rook)
        && pieceType king == King
        && canMoveStraight from rookFrom board
        && not (check board' (pieceColor king))
    then return board'
    else Nothing
    where
        rookMove
            | y' /= y = Nothing
            | x' == x-2 = Just ((1,y),(x-1,y))
            | x' == x+2 = Just ((8,y),(x+1,y))
            | otherwise = Nothing

enPassant :: Position -> Position -> Board -> Maybe Board
enPassant from@(x,y) to@(x',y') board = do
    pawn <- board ! from
    captured <- board ! capturedPos
    let board' = removePiece capturedPos . movePiece from to $ board
    if     pieceType pawn == Pawn
        && abs dx == 1
        && dy == forward (pieceColor pawn)
        && board ! to == Nothing
        && passant captured
    then return board'
    else Nothing
    where
        (dx, dy) = (x' - x, y' - y)
        capturedPos = (x', y)

trySimpleMove' :: Position -> Position -> Board -> Color -> Maybe Board
trySimpleMove' from to board color
    | fmap pieceColor (board ! from) /= Just color = Nothing
    | fmap pieceColor (board ! to)   == Just color = Nothing
    | canBasicMove from to board = Just $ movePiece from to board
    | otherwise = asum $ [f from to board | f <- [doubleMove, castle, enPassant]]

trySimpleMove :: Position -> Position -> Board -> Color -> Maybe Board
trySimpleMove from to board color = case trySimpleMove' from to board color of
    Nothing -> Nothing
    Just board' -> if check board' color
        then Nothing
        else Just $ removePassants (nextColor color) board'

tryMove :: Move -> Board -> Color -> Maybe Board
tryMove move board color = do
    board' <- trySimpleMove from to board color
    case promotion move of
        Nothing -> if promotes
            then Nothing
            else return board'
        Just t  -> if promotes
            then if t == Pawn || t == King
                then Nothing
                else return $ promotePiece to t board'
            else Nothing
    where
        from = moveStart move
        to   = moveEnd move
        promotes =
               fmap pieceType (board ! from) == Just Pawn
            && snd to == case color of
                White -> 8
                Black -> 1

check :: Board -> Color -> Bool
check board color = any (\from -> trySimpleMove' from king board (nextColor color) /= Nothing) pieces
    where
        king = fst . head . filter isKing $ assocs board
        isKing (_,piece) = fmap pieceType piece == Just King && fmap pieceColor piece == Just color
        pieces = map fst . filter (\(_,piece) -> fmap pieceColor piece == Just (nextColor color)) $ assocs board

moves :: Board -> Color -> [(Position, Position)]
moves board color = filter (\(start, end) -> trySimpleMove start end board color /= Nothing) $ (,) <$> starts <*> ends
    where
        starts = filter ((== Just color) . fmap pieceColor . (board !)) ends
        ends   = (,) <$> [1..8] <*> [1..8]
