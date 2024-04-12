{-# LANGUAGE TypeFamilies      #-}

module DeltaQ.Model.Utilities
where

import DeltaQ.Model.DeltaQ

class (DeltaQ irv) => DeltaQVisualisation irv where
  -- | Return a sequence of Time and Probability Mass points of a given length
  --   over the support of the ΔQ. The sequence is monotonitically increasing in
  --   both Time and Probablity Mass.
  asDiscreteCDF :: irv -> Int -> [(Time irv, ProbMass irv)]
  -- | Return a sequence of (Left) Impulse Probablity mass (equivalent to the
  --   integral of the Heaviside function at that point) or (Right) a sequence
  --   of Time and Probability Density. The sequence is monotonitcally increasing in Time.
  asDiscretePDF :: irv -> Int -> [Either (Time irv, ProbMass irv) [(Time irv, ProbMass irv)]]

{-
class (DeltaQ icdf) => DeltaQSupport icdf where
  type InvProbMass icdf

  fromQTA :: [(Time icdf, ProbMass icdf)] -> icdf

  inverseCummulativeMass :: icdf -> Time icdf -> InvProbMass icdf

  toDensity :: icdf -> Time icdf -> {- ProbabilityMass -} icdf

  -- views for plotting
-}
