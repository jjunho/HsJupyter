-- Test HsJupyter GHC evaluation directly
module TestKernel where

import HsJupyter.Runtime.GHCRuntime
import HsJupyter.Runtime.GHCSession
import Control.Concurrent.STM

-- Test basic expression evaluation
testBasicEvaluation :: IO ()
testBasicEvaluation = do
  putStrLn "🧪 Testing HsJupyter GHC Evaluation"
  
  let config = defaultGHCConfig
  session <- atomically $ newGHCSession config
  
  -- Test simple arithmetic
  result1 <- evaluateExpression session "2 + 3"
  putStrLn $ "2 + 3 = " ++ show result1
  
  -- Test list operations
  result2 <- evaluateExpression session "length [1,2,3,4,5]"
  putStrLn $ "length [1,2,3,4,5] = " ++ show result2
  
  -- Test higher-order functions
  result3 <- evaluateExpression session "map (*2) [1,2,3]"
  putStrLn $ "map (*2) [1,2,3] = " ++ show result3
  
  putStrLn "✅ GHC evaluation test complete!"

main :: IO ()
main = testBasicEvaluation