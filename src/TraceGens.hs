module TraceGens where

import Data.Array.BitArray
import Data.List
import Trace
import Sim
import Generator
import Algorithms
import Control.Monad
import Control.Monad.State
import Data.Word

type Matrix3d = BitArray P3

-- task with 3d images of source/target figures.
data Task = Assemble {tgt :: Matrix3d}
          | Disassemble {src :: Matrix3d}
          | Reassemble {src :: Matrix3d, tgt :: Matrix3d}

data Rect3d = Rect3dIncl {rMin :: P3, rMax :: P3}

dummyTraceGen :: Matrix3d -> [Command]
dummyTraceGen matrix = [Halt]
-- generator for solving given Task
matrixTraceGen :: Task -> Generator ()
matrixTraceGen task@(Assemble tgt) = do
    -- move to corner 'a' of figure
    moveTo $ botPositionUnderFigure tgt
    -- gen
    matrixTraceGen' task
    -- move home
    moveTo (0,0,0)

matrixTraceGen task@(Disassemble src) = do
    -- move to corner 'a' of figure
    moveTo $ botPositionUnderFigure src
    matrixTraceGen' task
    -- move home
    moveTo (0,0,0)
matrixTraceGen task@(Reassemble src tgt) = do
    -- move to corner 'a' of figure
    moveTo $ botPositionUnderFigure src
    matrixTraceGen' task
    -- move home
    moveTo (0,0,0)

-- without start/finish moves.
-- initial: 1 bot, under figure (botPositionUnderFigure).
-- finish: 1 bot, under figure.
matrixTraceGen' :: Task -> Generator ()
matrixTraceGen' (Assemble tgt) =  snakeMatrixAssemble tgt
matrixTraceGen' (Disassemble src) = do
    clearFigureBy8Bots src
matrixTraceGen' (Reassemble src tgt) = do
    matrixTraceGen' (Disassemble src)
    moveTo $ botPositionUnderFigure src
    matrixTraceGen' (Assemble tgt)

-- initial: 1 bot, under figure (botPositionUnderFigure).
-- finish: 1 bot, under figure.snakeMatrixAssemble :: Matrix3d -> Generator ()
snakeMatrixAssemble tgt = do
    -- fork bot in spare matrix (1 bot in 3х3 cell)
    setHarmonics 0 High
    step
    -- move snake-like
    setHarmonics 0 Low
    step
    -- join bots, move to src
    undefined

clearFigureBy8Bots :: Matrix3d -> Generator ()
clearFigureBy8Bots src = do
    let bounds = getBoundingBox src
    -- fork bot to 8.

    -- move bots to bounds

    -- gvoid

    -- move bot

    -- join bot
    undefined

maxBots = 40
-- fork bot (bid=0) in XZ plane.
forkBotToMatrix :: (Int,Int) -> Generator [BID]
forkBotToMatrix (nx,nz) | nx * nz <= maxBots = do
    undefined

-- join active bots (in matrix form) to single bot (bid=0).
joinBots :: (Int,Int) -> Generator ()
joinBots (nx,ny) = do
    undefined

-- Figure bounds, inclusive.
getBoundingBox :: Matrix3d -> Rect3d
getBoundingBox m =
    if null inds then error "no Fill'ed cells in matrix"
                 else Rect3dIncl (minimum xs, minimum ys, minimum zs)
                                 (maximum xs, maximum ys, maximum zs)
    where
        inds = filter (m!) (indices m)
        xs = Prelude.map (\(x,_,_)->x) inds
        ys = Prelude.map (\(_,y,_)->y) inds
        zs = Prelude.map (\(_,_,z)->z) inds

-- move 1 bot (bid=0) in void
moveTo :: P3 -> Generator ()
moveTo = move 0

botPositionUnderFigure :: Matrix3d -> P3
botPositionUnderFigure m = (x,y-1,z) where
    (x,y,z) = rMin bounds
    bounds = getBoundingBox m

data BotMatrix = BotMatrix { bmSize :: (Int,Int)
                           , bmY :: Int
                           , bmFirstBotXZ :: (Int,Int)
                           }
data BotDir = Up | Down
-- columns for "snake". Each column (by Y) contains bot matrices.
genSnakeColumns :: (Int,Int,Int) -> (Int,Int,Int) -> (Int,Int) -> [(BotDir,[BotMatrix])]
genSnakeColumns start@(sx,sy,sz) end@(ex,ey,ez) sizes@(dx,dz) = matrices
    where
        repeatEach n vs = concatMap (take n . repeat) vs
        -- TODO: boundary conditions.
        nx = (ex - sx + 1) `div` (dx * 3)
        nz = (ex - sz + 1) `div` (dz * 3)
        ny = (ey - sy + 1)
        dirYsColumns :: [(BotDir,[Int])]
        dirYsColumns = cycle [(Up,[sy..ey]),(Down,reverse [sy,ey])]
        xs = Data.List.map (\i -> sx + i * 3) $ cycle $ [0..nx] ++ reverse [0..nx]
        zs = Data.List.map (\i -> sz + i * 3) $ cycle $ [0..nz] ++ reverse [0..nz]
        zxys :: [(Int,Int,(BotDir,[Int]))]
        zxys = zip3 (repeatEach nx zs) xs dirYsColumns
        matrices :: [(BotDir,[BotMatrix])]
        matrices = Data.List.map (\(z,x,(dir,ys))-> (dir, Data.List.map (\y-> BotMatrix sizes y (x,z)) ys)) zxys
