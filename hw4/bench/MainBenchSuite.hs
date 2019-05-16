import Criterion.Main (defaultMain)

import Bench.CHT (chtBench)
import Bench.Gauss (gaussBench)
import Bench.Geometry (perimeterBench)
import Bench.Multiply (multiplyBench)

main :: IO ()
main = defaultMain [
    multiplyBench
  , perimeterBench
  , gaussBench
  , chtBench
  ]
