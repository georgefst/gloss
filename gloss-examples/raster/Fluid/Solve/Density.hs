
module Solve.Density
        (densitySteps)
where
import Stage.Diffusion
import Stage.Advection
import Stage.Sources
import Config
import Model

-- | Run the stages for processing the density field in one time step
densitySteps 
        :: Config
        -> DensityField 
        -> Maybe (Source Float) 
        -> VelocityField 
        -> IO DensityField

densitySteps config df ds vf 
 = {-# SCC "Solve.densitySteps" #-}
   do   df1     <- addSources (configDelta config) (configDensity   config) ds df
        df2     <- diffusion  (configDelta config) (configDiffusion config) df1
        df'     <- advection  (configDelta config) vf df2
        return  df'
