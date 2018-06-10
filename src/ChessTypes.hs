module ChessTypes
    ( PieceType (..)
    , Color (..)
    , Piece (..)
    , nextColor
    , makePiece
    , movePiece
    , movePassant
    , removePiece
    , removePassants
    , promotePiece
    , Position
    , Board
    , Move (..)
    , History (..)
    ) where

import Data.Array

data PieceType = Pawn | Knight | Bishop | Rook | Queen | King
    deriving (Show, Eq)
data Color = White | Black
    deriving (Show, Eq)
data Piece = Piece
    { pieceType  :: PieceType
    , pieceColor :: Color
    , moved      :: Bool
    , passant    :: Bool
    } deriving (Show)

instance Eq Piece where
    a == b = pieceType a == pieceType b && pieceColor a == pieceColor b

nextColor :: Color -> Color
nextColor White = Black
nextColor Black = White

makePiece :: PieceType -> Color -> Piece
makePiece t c = Piece t c False False

type Position = (Int, Int)
type Board = Array Position (Maybe Piece)
data Move = Move
    { moveStart :: Position
    , moveEnd   :: Position
    , promotion :: Maybe PieceType
    } deriving (Show)

movePiece :: Position -> Position -> Board -> Board
movePiece from to board = board // [(from, Nothing), (to, piece')]
    where
        piece' = do
            piece <- board ! from
            return $ piece { moved = True }

movePassant :: Position -> Position -> Board -> Board
movePassant from to board = board // [(from, Nothing), (to, piece')]
    where
        piece' = do
            piece <- board ! from
            return $ piece { moved = True, passant = True }

removePiece :: Position -> Board -> Board
removePiece pos board = board // [(pos, Nothing)]

removePassants :: Color -> Board -> Board
removePassants color = fmap (fmap makeNotPassant)
    where
        makeNotPassant piece
            | pieceColor piece == color = piece { passant = False }
            | otherwise                 = piece

promotePiece :: Position -> PieceType -> Board -> Board
promotePiece pos t board = board // [(pos, piece')]
    where
        piece' = do
            piece <- board ! pos
            return $ piece { pieceType = t }

data History = History
    { pastBoards :: [Board]
    , counter    :: Int
    }
