{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies      #-}

module DeltaQ.Model.DeltaQ
  ( ProbabilityMass (..)
  , DeltaQ (..)
  , DeltaQOps (..)
  , DeltaQ𝛩 (..)
  , DeltaQTimeout (..)
  , DeltaQUniform (..)
  , shiftedHeaviside
  )
where

-- | Probability mass. This is a value that can range over the unit interval,
--   [0,1].


class ( Fractional (ProbMassModel a)
      , Ord (ProbMassModel a)
      )
      => ProbabilityMass a where
  data  ProbMassModel a
  never         :: a
  always        :: a
  complement    :: a -> a
  toMassModel   :: a -> ProbMassModel a
  fromMassModel :: ProbMassModel a -> a

  never        = fromMassModel 0
  always       = fromMassModel 1
  complement a = fromMassModel (toMassModel always - toMassModel a)

class ( ProbabilityMass (ProbMass icdf)
      , Num (Time icdf) 
      ) => DeltaQ icdf where
  type ProbMass icdf
  type Time     icdf

  -- | Perfection - instantaneous 100% 'success'.
  perfection :: icdf

  -- | Bottom - never occurs, 100% 'failure'.
  bottom :: icdf

  -- | cumulative probability mass - the probability that 'success' will have
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

  default cumulativeMass :: Ord (Time icdf)
                         => icdf -> Time icdf -> ProbMass icdf
  cumulativeMass icdf t
    | t < l                 = never
    | maybe False (t >) u   = tangibleMass icdf
    | otherwise             = maybe err1 id $ cumulativeMass' icdf t
    where
      (l,u) = support icdf
      err1  = error "cumulativeMass: DeltaQ model error - not defined over support"

  -- | All these operators are transitive, we offer up the ability to create a
  --   multi-way version of the functions to permit exploitation of
  --   optimisations.
  --
  --   Care is taken in the default multi-way case to ensure that the empty list
  --   returns the `unit` of the operator.

class (DeltaQ icdf) => DeltaQOps icdf where

  -- | Normalise, including any representation into canonical form
  normalise :: icdf -> icdf

  -- | Left biased probabilistic choice.
  choice :: ProbMass icdf -> icdf -> icdf -> icdf

  -- | Given a weighted sequence derive the weighted sum
  nWayChoice :: [(ProbMassModel (ProbMass icdf), icdf)] -> icdf

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
      pl = toMassModel p
      pr = 1 - pl

  nWayChoice [] = bottom
  nWayChoice [(_,a)] = a
  nWayChoice ((w_x,dq_x):xs) =
    choice (fromMassModel $ w_x / (w_x + sumW xs)) dq_x (nWayChoice xs)
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


class (DeltaQOps icdf) => DeltaQ𝛩 icdf where
  -- | introduce a fixed additional delay
  delay :: icdf -> Time icdf -> icdf
  -- | The shifted Heavyside function. `cumulativeMass` is `never` for times
  --   below the value, and `always` for times above the value
  shifted𝛩     :: Time icdf -> icdf

  delay  a t = a `convolve` shifted𝛩 t
  shifted𝛩 t = delay perfection t

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

class (DeltaQ𝛩 icdf) => DeltaQTimeout icdf where
  -- | Ensure progress at no later than the specified timeout. This can
  --   _increase_ `tangibleMass` as it will be `always` for times greater than
  --   the supplied timeout.
  timeout :: icdf -> Time icdf -> icdf

  timeout icdf to = icdf `ftf` shifted𝛩 to
