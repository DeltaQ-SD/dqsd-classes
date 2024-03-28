{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies      #-}

module DeltaQ.Model.DeltaQ
where

class ( Fractional (ProbMassModel a)
      , Real (ProbMassModel a)
      , Num a
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
  complement a = (always - a)


class (ProbabilityMass (ProbMass icdf)) => DeltaQ icdf where
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
  centiles :: icdf -> [ProbMass icdf] -> [Maybe (Time icdf)]

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

  nWayChoice cs = undefined
    where
      _s = sum (map fst cs)
      _n = undefined

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
class (DeltaQ icdf)=> DeltaQUniform icdf where
  uniform0 :: Time icdf -> icdf
  uniform  :: Time icdf -> Time icdf -> icdf

class (DeltaQ𝛩 icdf) => DeltaQTimeout icdf where
  -- | Ensure progress at no later than the specified timeout. This can
  --   _increase_ `tangibleMass` as it will be `always` for times greater than
  --   the supplied timeout.
  timeout :: icdf -> Time icdf -> icdf

  timeout icdf to = icdf `ftf` shifted𝛩 to


-- | Ability to extract internal detail of aspects of the expressions.
class (DeltaQOps icdf) => DeltaQIntrospection icdf where
  -- | Extract the probability that the timeout was invoked.
  probTimedout :: icdf -> Time icdf -> ProbMass icdf
  -- | Extract the 'slack' (or 'hazard') for a single (time, probability) point -
  --   the degenerative QTA (Quantitative Timeliness Agreement)
  pointSlackHazard :: icdf -> (Time icdf, ProbMass icdf)
                   -> Either (Time icdf, ProbMass icdf) (Time icdf, ProbMass icdf)

-- here? Things that might inform a scheduler, for example

  probTimedout icdf to = complement $ cumulativeMass icdf to

  pointSlackHazard = undefined

class (DeltaQ icdf) => DeltaQSupport icdf where
  type InvProbMass icdf

  fromQTA :: [(Time icdf, ProbMass icdf)] -> icdf

  inverseCummulativeMass :: icdf -> Time icdf -> InvProbMass icdf

  toDensity :: icdf -> Time icdf -> {- ProbabilityMass -} icdf

  -- views for plotting
