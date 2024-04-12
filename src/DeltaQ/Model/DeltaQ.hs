{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies      #-}

{-|
Module      : DeltaQ
Description : Class descriptions for the ∆Q systems development approach.
Copyright   : Neil Davies, 2024
License     : BSD-3
Maintainer  : neil.davies@pnsol.com
-}


module DeltaQ.Model.DeltaQ
  (
    -- * Improper probabilty distributions
    -- ** Probability
    Probability (..)
    -- ** ∆Q
  , DeltaQ (..)
    -- * `DeltaQ` operations
    -- ** choice, serial / concurrent composition
  , DeltaQOps (..)
    -- ** Time-shifting
  , DeltaQ𝛩 (..)
    -- ** Timeout
  , DeltaQTimeout (..)
    -- * Useful distributions
  , DeltaQUniform (..)
    -- * Utility functions
  , shiftedHeaviside
  )
where

{-| A type @a@ can be an instance of the 'Probability' class
if it represents the unit interval.

For reasons of accuracy or speed, we want to allow different
implementations based on different numeric types, say,
'Double' or 'Rational'. The type family 'NumericType'
records this underlying numeric type.
-}
class ( Fractional (NumericType p)
      , Ord (NumericType p)
      )
      => Probability p where
  type  NumericType p
  never         :: p
  -- ^ No possibility, 0% probability.
  always        :: p
  -- ^ Certainty, 100% probability.
  complement    :: p -> p
  -- ^ Difference from always.
  --
  -- We expect that the properties
  --
  -- prop> complement . complement = id
  -- prop> complement never = always
  --
  -- hold up to rounding errors.
  toNumericType   :: p -> NumericType p
  -- ^ Projection to the underlying numeric type.
  fromNumericType :: NumericType p -> p
  -- ^ Projection from the underlying numeric type.
  -- Values outside the range [0,1] will be mapped to @0@ or @1@,
  -- whichever is closer.

  never        = fromNumericType 0
  always       = fromNumericType 1

-- | ∆Q - a relationship between timeliness and probability that admits the
--   notion of non-occurrence.
class ( Probability (ProbMass icdf)
      , Num (Time icdf) 
      ) => DeltaQ icdf where
  type ProbMass icdf
  type Time     icdf

  -- | Perfection - instantaneous 100% __success__.
  perfection :: icdf
  -- | Bottom - never occurs, 100% __failure__.
  bottom :: icdf
  -- | cumulative probability mass - the probability that /success/ will have
  --   occurred at or before the given time.
  cumulativeMass :: icdf -> Time icdf -> ProbMass icdf
  -- | As above, but only defined over the support.
  cumulativeMass' :: icdf -> Time icdf -> Maybe (ProbMass icdf)
  -- | The support. The values of time before (and after) which the cumulative
  --   distribution does not change. The upper bound is present if the support
  --   has a finite upper bound.
  support :: icdf -> (Time icdf, Maybe (Time icdf))
  -- | The limit of the cumulative probability at and beyond the upper bound of
  --   support.
  tangibleMass :: icdf -> ProbMass icdf
  -- | The time at which the request probability mass has accumulated.
  centile :: icdf -> ProbMass icdf -> Maybe (Time icdf)

  centiles :: icdf -> [ProbMass icdf] -> [Maybe (Time icdf)]

  {-# MINIMAL perfection, bottom, (cumulativeMass | cumulativeMass'), support,  tangibleMass, (centile | centiles) #-}

  default cumulativeMass :: Ord (Time icdf)
                         => icdf -> Time icdf -> ProbMass icdf
  cumulativeMass icdf t
    | t < l                 = never
    | maybe False (t >) u   = tangibleMass icdf
    | otherwise             = maybe err1 id $ cumulativeMass' icdf t
    where
      (l,u) = support icdf
      err1  = error "cumulativeMass: DeltaQ model error - not defined over support"

  default cumulativeMass' :: Ord (Time icdf)
                          => icdf -> Time icdf -> Maybe (ProbMass icdf)

  cumulativeMass' icdf t
    | t < l                 = Just $ never
    | maybe False (t >) u   = Nothing
    | otherwise             = Just $ cumulativeMass icdf t
    where
      (l,u) = support icdf

  centile icdf p = head $ centiles icdf [p]

  centiles icdf ps = map (centile icdf) ps



-- | All these operators are transitive, we offer up the ability to create a
--   multi-way version of the functions to permit exploitation of optimisations.
--
--   Care is taken in the default multi-way case to ensure that the empty list
--   returns the unit of the operator.

class (DeltaQ icdf) => DeltaQOps icdf where

  -- | Normalise, including any representation into canonical form
  normalise :: icdf -> icdf

  -- | Left biased probabilistic choice.
  choice :: ProbMass icdf -> icdf -> icdf -> icdf

  -- | Given a weighted sequence derive the weighted sum
  nWayChoice :: [(NumericType (ProbMass icdf), icdf)] -> icdf

  -- | Sequential composition of two expressions
  convolve :: icdf -> icdf -> icdf

  -- | multiple sequence steps
  nWayConvolve :: [icdf] -> icdf

  -- | First to finish progression
  ftf :: icdf -> icdf -> icdf

  -- | multi-way case
  nWayFtf :: [icdf] -> icdf

  -- | last to finish synchronisation
  ltf :: icdf -> icdf -> icdf

  -- | multi-way case
  nWayLtf :: [icdf] -> icdf
  {-# MINIMAL (choice | nWayChoice)
            , (convolve | nWayConvolve)
            , (ftf | nWayFtf)
            , (ltf | nWayLtf)  #-}


  -- | default is the identify function.
  normalise = id

  choice p a b = nWayChoice [(pl, a), (pr, b)]
    where
      pl = toNumericType p
      pr = 1 - pl

  nWayChoice [] = bottom
  nWayChoice [(_,a)] = a
  nWayChoice ((w_x,dq_x):xs) =
    choice (fromNumericType $ w_x / (w_x + sumW xs)) dq_x (nWayChoice xs)
    where
     sumW = sum . map fst


  convolve a b = nWayConvolve [a, b]

  -- | could have defined it as @foldr perfection convolve@, however where the
  --   underlying representation is retaining structure this would introduce a
  --   trailing `perfection` which was not present in the source - with
  --   potential to confuse the user.
  nWayConvolve [] = perfection
  nWayConvolve ps = foldr1 convolve ps


  ftf a b = nWayFtf [a, b]

  -- | The choice of the empty list case is the 'unit' that would make no
  --   difference to the outcome for this operator.
  nWayFtf [] = bottom
  nWayFtf ps = foldr1 ftf ps

  ltf a b = nWayLtf [a,b]

  nWayLtf [] = perfection
  nWayLtf ps = foldr1 ltf ps

-- | Time shifting ∆Q

class (DeltaQOps icdf) => DeltaQ𝛩 icdf where
  -- | introduce a fixed additional delay
  delay :: icdf -> Time icdf -> icdf
  -- | The shifted Heavyside function. `cumulativeMass` is `never` for times
  --   below the value, and `always` for times above the value
  shifted𝛩     :: Time icdf -> icdf

  delay  a t = a `convolve` shifted𝛩 t
  shifted𝛩 t = delay perfection t

  {-# MINIMAL (shifted𝛩 | delay) #-}


-- | A synonym for `shifted𝛩`.
shiftedHeaviside :: (DeltaQ𝛩 icdf) => Time icdf -> icdf
shiftedHeaviside = shifted𝛩

-- | Those models that support construction through use of uniform distributions
class (DeltaQ𝛩 icdf) => DeltaQUniform icdf where
-- | Uniform distribution from 0
  uniform0 :: Time icdf -> icdf
-- | Uniform distribution over a range
  uniform  :: Time icdf -> Time icdf -> icdf

  uniform0  b = uniform 0 b
  uniform a b = delay (uniform0 $ b - a) a

  {-# MINIMAL (uniform0 | uniform) #-}


-- | Specialised version of first to finish for timeout
class (DeltaQ𝛩 icdf) => DeltaQTimeout icdf where
  -- | Ensure progress at no later than the specified timeout. This can
  --   _increase_ `tangibleMass` as it will be `always` for times greater than
  --   the supplied timeout.
  timeout :: icdf -> Time icdf -> icdf

  timeout icdf to = icdf `ftf` shifted𝛩 to
