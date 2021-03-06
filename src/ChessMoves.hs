module ChessMoves
    ( tryMove
    , trySimpleMove
    , check
    , moves
    , noMaterial
    , updateCounter
    ) where

import ChessTypes
import Data.Array
import Data.Foldable
import Data.Function
import Data.Maybe

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

trySimpleMove :: Position -> Position -> Board -> Color -> Maybe Board
trySimpleMove from to board color
    | fmap pieceColor (board ! from) /= Just color = Nothing
    | fmap pieceColor (board ! to)   == Just color = Nothing
    | canBasicMove from to board = Just $ movePiece from to board
    | otherwise = asum [f from to board | f <- [doubleMove, enPassant]]

tryMove' :: Move -> Board -> Color -> Either String Board
tryMove' (Move from to promotion) board color = do
    board' <- maybeToEither "Invalid move" $ trySimpleMove from to board color
    case promotion of
        Nothing -> if promotes
            then Left "No promotion specified"
            else return board'
        Just t  -> if promotes
            then if t == Pawn || t == King
                then Left "Can't promote to this piece"
                else return $ promotePiece to t board'
            else Left "No promotion in this move"
    where
        promotes =
               fmap pieceType (board ! from) == Just Pawn
            && snd to == case color of
                White -> 8
                Black -> 1

tryMove' (Castling side) board color = maybeToEither "Invalid move" $ do
    king <- board ! kingFrom
    rook <- board ! rookFrom
    let board' = movePiece kingFrom kingTo . movePiece rookFrom rookTo $ board
    if     pieceType king == King
        && pieceType rook == Rook
        && not (moved king)
        && not (moved rook)
        && canMoveStraight kingFrom rookFrom board
        && not (attacked board color kingFrom)
        && not (attacked board color rookTo)
    then return board'
    else Nothing
    where
        (kingFrom, kingTo, rookFrom, rookTo) = case (color, side) of
            (White, Kingside)  -> ((5,1),(7,1),(8,1),(6,1))
            (White, Queenside) -> ((5,1),(3,1),(1,1),(4,1))
            (Black, Kingside)  -> ((5,8),(7,8),(8,8),(6,8))
            (Black, Queenside) -> ((5,8),(3,8),(1,8),(4,8))

tryMove :: Move -> Board -> Color -> Either String Board
tryMove move board color = do
    board' <- tryMove' move board color
    if check board' color
        then Left "Move causes check"
        else return $ removePassants (nextColor color) board'

attacked :: Board -> Color -> Position -> Bool
attacked board color pos = any canMove pieces
    where
        canMove from = isJust $ trySimpleMove from pos board (nextColor color)
        pieces = map fst . filter (rightColor . snd) $ assocs board
        rightColor (Just piece) = pieceColor piece == nextColor color
        rightColor _ = False

check :: Board -> Color -> Bool
check board color = attacked board color king
    where
        king = fst . head . filter (isKing . snd) $ assocs board
        isKing (Just Piece {pieceType = King, pieceColor = color'}) = color' == color
        isKing _ = False
        
moves :: Board -> Color -> [(Position, Position)]
moves board color = filter canMove $ (,) <$> starts <*> ends
    where
        canMove (start, end) = isJust $ trySimpleMove start end board color
        starts = filter ((== Just color) . fmap pieceColor . (board !)) ends
        ends   = (,) <$> [1..8] <*> [1..8]

noMaterial :: Board -> Bool
noMaterial board =
       types == []
    || types == [Knight]
    || all (== Bishop) types && all (== head colors) colors
    where
        types = map (pieceType . fromJust . snd) pieces
        colors = map (squareColor . fst) pieces
        pieces = filter (notKingOrEmpty . snd) $ assocs board
        notKingOrEmpty Nothing = False
        notKingOrEmpty (Just piece) = pieceType piece /= King
        squareColor (x,y) = (x+y) `mod` 2

updateCounter :: Board -> Board -> Int -> Int
updateCounter board board' counter =
    if movedPawn || captured
        then 0
        else succ counter
    where
        movedPawn = ((==) `on` (filter isPawn . elems)) board board'
        captured  = ((/=) `on` (length . filter isJust . elems)) board board'
        isPawn piece = fmap pieceType piece == Just Pawn
