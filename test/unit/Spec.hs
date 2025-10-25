import Test.Hspec

import qualified DiagnosticsSpec
import qualified ErrorHandlingSpec
import qualified GHCDiagnosticsSpec
import qualified GHCRuntimeSpec
import qualified GHCSessionSpec
import qualified ProtocolEnvelopeSpec
import qualified SignatureValidationSpec
import qualified ObservabilitySpec
import qualified ResourceGuardSpec
import qualified SessionStateSpec
import qualified RuntimeManagerSpec
import qualified CLITypesSpec

main :: IO ()
main = hspec $ do
  DiagnosticsSpec.spec
  ErrorHandlingSpec.spec
  GHCDiagnosticsSpec.spec
  GHCRuntimeSpec.spec
  GHCSessionSpec.spec
  ProtocolEnvelopeSpec.spec
  SignatureValidationSpec.spec
  ObservabilitySpec.spec
  ResourceGuardSpec.spec
  SessionStateSpec.spec
  RuntimeManagerSpec.spec
  CLITypesSpec.spec
