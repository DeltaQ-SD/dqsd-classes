{-# LANGUAGE DefaultSignatures, MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstrainedClassMethods #-}

-- TODO: restructure so this isn't needed
{-# LANGUAGE AllowAmbiguousTypes #-}

module DeltaQ.Class
  ( ImproperRandomVariable(..)
  , Convolvable(..)
  , NonConcurrentCombination(..)
  , ConcurrentCombination(..)
  ) where

-- | The class of improper random variables; we have a means of constructing and
--   extracting basic values (for visualisation).
--
--   DelayModel is a non-negative real number. ProbabiltyModel is a real number
--   over the (inclusive) unit interval.
class ImproperRandomVariable a where
  type DelayModel a
  type ProbabilityModel a

  {-# MINIMAL perfection, bottom, diracDelta, uniform0, (tangibleMass | intangibleMass) #-}

  -- value space
  perfection :: a
  bottom     :: a
  diracDelta :: DelayModel a -> a
  uniform0   :: DelayModel a -> a

  -- probablity mass
  tangibleMass :: a -> ProbabilityModel a
  default tangibleMass :: (Num (ProbabilityModel a)) => a -> ProbabilityModel a
  tangibleMass x = 1 - intangibleMass x

  intangibleMass :: a -> ProbabilityModel a
  default intangibleMass :: (Num (ProbabilityModel a)) => a -> ProbabilityModel a
  intangibleMass x = 1 - tangibleMass x

  -- ? making an improper random variable 'proper'

  -- ? means of sampling

  -- ? means of extracting lower order moments (mean, variance ..)

-- | Wrapper for convolution (since it doesn't need the weight type param)
class Convolvable a where
  {-# MINIMAL ((<+>) | convolve) #-}
  (<+>) :: a -> a -> a
  (<+>) = convolve

  convolve :: a -> a -> a
  convolve = (<+>)

-- | Combinations of Improper Random Variables that occur either sequentially or
--   in a mutally exclusive fashion. Single use of a computatonal capability.
class Convolvable a => NonConcurrentCombination w a where
  {-# MINIMAL (weightedChoice | probabilisticChoice | weightedChoice') #-}

  weightedChoice :: (ImproperRandomVariable a) => (w, a) -> (w, a) -> a
  default weightedChoice :: (Num w, Fractional w, ImproperRandomVariable a)
                         => (w, a) -> (w, a) -> a
  weightedChoice (wa, a) (wb, b) = let
      p = wa / (wa + wb)
    in probabilisticChoice p a b

  weightedChoice' :: (ImproperRandomVariable a) => [(w, a)] -> a
  default weightedChoice' :: (Num w, Fractional w, ImproperRandomVariable a)
                          => [(w, a)] -> a
  weightedChoice' [] = perfection
  weightedChoice' (x:xs) = weightedChoice x (ws, weightedChoice' xs)
    where ws = sum $ map fst xs

  probabilisticChoice :: (ImproperRandomVariable a) => w -> a -> a -> a
  default probabilisticChoice :: (Num w, ImproperRandomVariable a) => w -> a -> a -> a
  probabilisticChoice n a b = weightedChoice' [(n, a), (1 - n, b)]


-- | Combination of Improper Random Variables that occur concurrent with
--   sufficient computational capabilities available so that they do not need to
--   compute for computational resources.
class ConcurrentCombination a where

  {-# MINIMAL (firstToFinish | firstToFinish'), (allToFinish | allToFinish') #-}

  firstToFinish :: (ImproperRandomVariable a) => a -> a -> a
  firstToFinish x y = firstToFinish' [x, y]

  firstToFinish' :: (ImproperRandomVariable a) => [a] -> a
  firstToFinish' = foldl firstToFinish bottom

  allToFinish :: (ImproperRandomVariable a) => a -> a -> a
  allToFinish x y = allToFinish' [x,y]

  allToFinish' :: (ImproperRandomVariable a) => [a] -> a
  allToFinish' = foldl allToFinish perfection

  -- we may want to include the special case of timeout. Why? it can be easily
  -- simplified and timeout may be considered something that doesn't need
  -- additional computation resource to operate
