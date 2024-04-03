{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies      #-}

module DeltaQ.Model.Introspection
  ( DeltaQIntrospection (..)
  , Slazard (..)
  )
where

import Data.Maybe (isNothing)
import DeltaQ.Model.DeltaQ ( ProbabilityMass (..)
                           , DeltaQ (..))

-- | The slack \/ hazard - the difference between the point in time \/
--  probability mass space and the given `DeltaQ`. `Slack` represents the
--  reference point being achieved; `Hazard` represents the point not being
--  achieved a measure of the degree of it being missed.
data (DeltaQ icdf) => Slazard icdf
  = Slack (Time icdf) (ProbMass icdf)
  -- ^ the 'slack' time / probability mass
  | Hazard (Maybe (Time icdf)) (ProbMass icdf)
  -- ^ the 'hazard' time / probability mass

-- | Ability to extract internal detail of aspects of the expressions.
class (DeltaQ icdf) => DeltaQIntrospection icdf where
  -- | Extract the probability that the timeout would occur.
  probTimedout :: icdf -> Time icdf -> ProbMass icdf
  -- | Extract the 'slack' (or 'hazard') for a single (time, probability) point
  --   - the degenerative QTA (Quantitative Timeliness Agreement)
  pointSlackHazard :: icdf
                   -> (Time icdf, ProbMass icdf)
                   -> Slazard icdf

-- here? Things that might inform a scheduler, for example

  probTimedout icdf to = complement $ cumulativeMass icdf to

  pointSlackHazard icdf (t,p)
    | dp >= 0      = Slack  dt (fromMassModel dp)
    -- ^ There must exist a non-negative time difference.
    | isNothing t' = Hazard Nothing (fromMassModel $ negate dp)
    -- ^ There is no upper bound on the time.
    | otherwise    = Hazard (Just $ negate dt) (fromMassModel $ negate dp)
    where
      dp = toMassModel p' - toMassModel p
      dt =  t - (maybe err id t')

      t' = centile icdf p
      p' = cumulativeMass icdf t

      err = error "pointSlackHazard: inconsistency"