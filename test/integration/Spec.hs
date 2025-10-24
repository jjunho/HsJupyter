import Test.Hspec

import qualified ExecuteEchoSpec
import qualified RuntimeNotebookSpec

main :: IO ()
main = hspec $ do
  ExecuteEchoSpec.spec
  RuntimeNotebookSpec.spec
