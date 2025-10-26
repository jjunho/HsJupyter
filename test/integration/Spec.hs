import Test.Hspec

import qualified CLIIntegrationSpec
import qualified ExecuteEchoSpec
import qualified GHCNotebookSpec
import qualified RuntimeNotebookSpec

main :: IO ()
main = hspec $ do
  CLIIntegrationSpec.spec
  ExecuteEchoSpec.spec
  GHCNotebookSpec.spec
  RuntimeNotebookSpec.spec
