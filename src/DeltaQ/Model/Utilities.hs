{-# LANGUAGE TypeFamilies      #-}

module DeltaQ.Model.Utilities
where

import DeltaQ.Model.DeltaQ

class (DeltaQ icdf) => DeltaQSupport icdf where
  type InvProbMass icdf

  fromQTA :: [(Time icdf, ProbMass icdf)] -> icdf

  inverseCummulativeMass :: icdf -> Time icdf -> InvProbMass icdf

  toDensity :: icdf -> Time icdf -> {- ProbabilityMass -} icdf

  -- views for plotting
