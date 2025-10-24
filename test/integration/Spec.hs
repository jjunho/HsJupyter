import Test.Hspec

import qualified ExecuteEchoSpec
import qualified GHCNotebookSpec
import qualified RuntimeNotebookSpec

main :: IO ()
main = hspec $ do
  ExecuteEchoSpec.spec
  GHCNotebookSpec.spec
  RuntimeNotebookSpec.spec
