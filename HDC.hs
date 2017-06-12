{-# OPTIONS
 
 -XMultiParamTypeClasses
 -XFunctionalDependencies
 -XFlexibleInstances
 -XRank2Types
 -XGADTs
 -XPolyKinds
 -XDeriveFunctor
 -XTemplateHaskell
 -XLambdaCase
#-}

module HDC where
import System.Environment
import Control.Monad
import Data.Tree
import Data.List
import qualified Data.Set as S
import qualified Data.Map.Strict as Map
import qualified Data.Hashable
import Data.Either
import Data.Maybe
import Control.Lens
import Control.Monad.Free
import Control.Monad.Random
import Control.Monad.Trans.State
import Prelude hiding ((*), (+), (-), negate)
import System.Random.Shuffle

import Numeric.Algebra
import TypeSyns
import qualified Data.Vector as Vec

import Utilities

data R = RRef I

data V = VRef I | Bind V V | Unbind V V | Add V V | Rot R V | RotInv R V

instance Additive V where 
    (+) = Add

instance Multiplicative V where 
    (*) = Bind

(*-) = Unbind

rot = Rot
rotInv = RotInv

-- every memory is of type a
-- no saving for now
-- cf. https://github.com/holdenlee/HasFlow/blob/master/Graph.hs
data Program' a next = 
    NewRef a (V -> next)
    | Extract V ([V] -> next)
    | NewRot (R -> next) deriving (Functor)
    
type Program a = Free (Program' a)

newRef :: a -> Program a V
newRef a = liftF (NewRef a id)

extract :: V -> Program a [V]
extract v = liftF (Extract v id)

newRot :: Program a R
newRot = liftF (NewRot id)

{-
class HDC m r v where
    bind :: v -> v -> v
    unbind :: v -> v -> v
    add :: v -> v -> v
    mkNewRef :: m v
    mkNewRot :: m r
    extracts :: m [v]
-}
data HDC r v m = HDC {bind :: v -> v -> v, 
    unbind :: v -> v -> v,
    add :: v -> v -> v,
    mkNewRef :: m V, -- should be ref here
    mkNewRot :: m R,
    extracts :: v -> m [I],
    sortByActivation :: v -> m [(I, I)]}

--permutations
type KanervaRot = Vec.Vector I

data KanervaMapping = KanervaMapping {_mems :: [Vec.Vector I], _rots :: [KanervaRot], _dims :: I, _threshold :: I}

makeLenses ''KanervaMapping

type KanervaMonad = StateT (KanervaMapping) (Rand StdGen)

--type KanervaMonad = RandT StdGen (State KanervaMapping)

instance (Additive a) => Additive (Vec.Vector a) where 
    (+) = Vec.zipWith (+)

instance (Multiplicative a) => Multiplicative (Vec.Vector a) where 
    (*) = Vec.zipWith (*)

dot :: (Additive a, Multiplicative a) => Vec.Vector a -> Vec.Vector a -> a
dot v1 v2 = Vec.foldl1 (+) (v1 * v2)

randomPM :: MonadRandom m => m I
randomPM = fmap (\case {True -> 1; False -> -1}) getRandom

kanerva :: HDC KanervaRot (Vec.Vector I) KanervaMonad
kanerva = HDC
    {bind = (*), 
     unbind = \v1 v2 -> (Vec.map negate v1) * v2,
     add = (+),
     mkNewRef = do
       s <- get
       let d = s ^. dims
       randVec <- lift $ fmap Vec.fromList $ forM [1..d] (\_ -> randomPM)
       let l = length (s ^. mems)
       modify (& mems %~ (++[randVec]))
       return (VRef l),
     mkNewRot = do
       s <- get
       let d = s ^. dims
       randPerm <- fmap Vec.fromList $ shuffleM [0..(d-1)]
       let l = length (s ^. rots)
       modify (& rots %~ (++[randPerm]))
       return (RRef l),
     extracts = \v -> do
                  s <- get
                  sorted <- f v
                  return $ map snd $ filter ((>= s ^. threshold) . fst) sorted,
     sortByActivation = f} where
        f = \v -> do
              s <- get
              return $ reverse $ sort $ zimap (\i x -> (i, x `dot` v)) (s ^. mems)
--map (\x -> (x, x `dot` v)) (s ^. mems)

{-
instance HDC Kanerva KanervaRot KanervaMapping where
    bind v1 v2 = v1 * v2
    unbind v1 v2 = (Vec.map negate v1) * v2
    add v1 v2 = v1 + v2
    mkNewRef r = do
      s <- getState
      randVec <- fromList $ for [1..dim] (\_ -> randomPM)
      let l = length (s ^. mems)
      modify (& mems %~ (++[randVec]))
      return (VRef i)
    mkNewRot = do
      s <- getState
      randPerm <- shuffleM [0..((s ^. dims)-1)]
      let l = length (s ^. rots)
      modify (& rots %~ (++[randPerm]))
      return (VRef i)
-}
{-
data HDC r v mem = HDC {bind :: v -> v -> v, 
    unbind :: v -> v -> v,
    add :: v -> v -> v,
    mkNewRef :: Rand StdGen v,
    mkNewRot :: Rand StdGen r,
    extracts :: mem -> v -> [I]}
-}
{-
--permutations
data KanervaRot = Vector I

--remember the mapping between vectors and rotations
--data KanervaMapping a = {_mems :: M I a, _rots :: M I KanervaRot, _memInd :: I, _rotInd :: I, _dims :: I, _threshold :: I}
data KanervaMapping a = {_mems :: [a], _rots :: [KanervaRot], _dims :: I, _threshold :: I}

makeLenses ''KanervaMapping

type Kanerva a = State (KanervaMapping a)

instance (Additive a) => Additive (Vec.Vector a) where 
    (+) = Vec.zipWith (+)

instance (Multiplicative a) => Multiplicative (Vec.Vector a) where 
    (*) = Vec.zipWith (*)

randomPM :: MonadRandom m => m I
randomPM = fmap (\case {True -> 1; False -> -1}) getRandom 

--https://hackage.haskell.org/package/random-shuffle-0.0.4/docs/System-Random-Shuffle.html
--random shuffle
instance HDC Kanerva KanervaRot KanervaMapping where
    bind v1 v2 = v1 * v2
    unbind v1 v2 = (Vec.map negate v1) * v2
    add v1 v2 = v1 + v2
    mkNewRef r = do
      s <- getState
      randVec <- fromList $ for [1..dim] (\_ -> randomPM)
      let l = length (s ^. mems)
      modify (& mems %~ (++[randVec]))
      return (VRef i)
    mkNewRot = do
      s <- getState
      randPerm <- shuffleM [0..((s ^. dims)-1)]
      let l = length (s ^. rots)
      modify (& rots %~ (++[randPerm]))
      return (VRef i)
-}
{-
    mkNewRef r = do
      s <- getState
      randVec <- fromList $ for [1..dim] (\_ -> randomPM)
      let i = s ^. memInd
      modify (& mems %~ (M.insert i r)
              & memInd %~ (+1))
      return (VRef i)
    mkNewRot = do
      s <- getState
      randPerm <- shuffleM [0..((s ^. dims)-1)]
      let i = s ^. rotInd
      modify (& rots %~ (M.insert i randPerm)
              & rotInd %~ (+1))
      return (VRef i)

-}
