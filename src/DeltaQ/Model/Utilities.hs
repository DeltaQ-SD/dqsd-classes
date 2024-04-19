{-# LANGUAGE FlexibleContexts      #-}
module DeltaQ.Model.Utilities
  ( DeltaQVisualisation(..)
  , plotCDF
  , plotCDFs
  , plotCDFWithCentiles
  , plotInverseCDF
  , plotPDF
  , plotPDFs
  )
where

import DeltaQ.Model.DeltaQ

import Graphics.Rendering.Chart.Easy

import Data.Bifunctor (second)

class (DeltaQ irv) => DeltaQVisualisation irv where
  -- | Return a sequence of Time and Probability Mass points of a given length
  --   over the support of the ΔQ. The sequence is monotonitically increasing in
  --   both Time and Probablity Mass.
  asDiscreteCDF :: irv -> Int -> [(Time irv, ProbMass irv)]
  -- | Return a sequence of (Left) Impulse Probablity mass (equivalent to the
  --   integral of the Heaviside function at that point) or (Right) a sequence
  --   of Time and Probability Density. The sequence is monotonitcally increasing in Time.
  asDiscretePDF :: irv -> Int -> [Either (Time irv, ProbMass irv) [(Time irv, ProbMass irv)]]
  -- | Given a (monotonically increasing in both paramters) list of `Time`,
  --   `ProbMass` pairs, create a stepwise QTA.
  fromQTA :: [(Time irv , ProbMass irv)] -> irv
  -- | Given a (monotonically increasing in both paramters) list of `Time`,
  --   `ProbMass` pairs, create a linearaly interpolated improper random
  --   variable. It is assumed that the first point is at (0,0).
  fromEmpirical :: [(Time irv, ProbMass irv)] -> irv


plotCDF :: ( PlotValue (ProbMass irv), PlotValue (Time irv)
           , RealFloat (Time irv), Show (Time irv)
           , DeltaQVisualisation irv)
        => String
        -> irv
        -> Layout (Time irv) (ProbMass irv)
plotCDF title irv = execEC $ do
  layout_title .=  title
  layout_x_axis . laxis_title .= "Time"
  layout_x_axis . laxis_generate .= maybe autoAxis (\u' -> scaledAxis def (0, factor * u')) u
  layout_y_axis . laxis_title .= "Prob. Mass"
  plot $ line "" [asDiscreteCDF irv 1000 ++ maybe [] (\u' -> [(2 * factor * u', tangibleMass irv)]) u]
  where
   factor = 1.1
   (_,u) = support irv

plotCDFs :: ( PlotValue (ProbMass irv), PlotValue (Time irv)
            , RealFloat (Time irv), Show (Time irv)
            , DeltaQVisualisation irv)
         => String
         -> [(String, irv)]
         -> Layout (Time irv) (ProbMass irv)

plotCDFs = error "plotCDFs: TBW"

plotCDFWithCentiles :: ( PlotValue (ProbMass irv), PlotValue (Time irv)
                       , RealFloat (Time irv), Show (Time irv)
                       , DeltaQVisualisation irv)
                    => String
                    -> irv
                    -> [ProbMass irv]
                    -> Layout (Time irv) (ProbMass irv)

plotCDFWithCentiles = error "plotCDFWithCentiles: TBW"

plotInverseCDF :: ( PlotValue (ProbMass irv), PlotValue (Time irv)
                  , RealFloat (Time irv), Show (Time irv)
                  , Real (ProbMass irv)
                  , DeltaQVisualisation irv)
               => String
               -> irv
               -> Layout (Time irv) LogValue
plotInverseCDF title irv = execEC $ do
  layout_title .=  title
  layout_x_axis . laxis_title .= "Time"
  layout_x_axis . laxis_generate .= maybe autoAxis (\u' -> scaledAxis def (0, factor * u')) u
  layout_y_axis . laxis_title .= "Log Inv. Prob. Mass"
  plot $ line "" [map (second cv) (asDiscreteCDF irv 1000)
         ++ maybe [] (\u' -> [(2 * factor * u', cv $ tangibleMass irv)]) u]
  where
   factor = 1.1
   (_,u) = support irv
   cv = fromRational . toRational . complement

plotPDF :: ( PlotValue (ProbMass irv), PlotValue (Time irv)
           , RealFloat (Time irv), Show (Time irv)
           , DeltaQVisualisation irv)
        => String
        -> irv
        -> Layout (Time irv) (ProbMass irv)
plotPDF title irv = execEC $ do
  layout_title .=  title
  layout_x_axis . laxis_title .= "Time"
  layout_x_axis . laxis_generate .= maybe autoAxis (\u' -> scaledAxis def (0, factor * u')) u
  layout_y_axis . laxis_title .= "Prob. Density"
  plot $ line "" $ map (either (\(t,p) -> [(t,never), (t, p)]) id) $ asDiscretePDF irv 1000
  where
   factor = 1.1
   (_,u) = support irv

plotPDFs :: ( PlotValue (ProbMass irv), PlotValue (Time irv)
            , RealFloat (Time irv), Show (Time irv)
            , DeltaQVisualisation irv)
         => String
         -> [(String, irv)]
         -> Layout (Time irv) (ProbMass irv)
plotPDFs = error "plotPDFs: TBW"
