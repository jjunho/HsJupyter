import Test.Hspec

import qualified CLICommandsSpec
import qualified CLIInstallSpec
import qualified CLITypesSpec
import qualified CrossPlatformSpec
import qualified DiagnosticsSpec
import qualified DoctorSpec
import qualified ErrorHandlingSpec
import qualified GHCDiagnosticsSpec
import qualified GHCRuntimeSpec
import qualified GHCSessionSpec
import qualified PerformanceSpec
import qualified ProtocolEnvelopeSpec
import qualified SignatureValidationSpec
import qualified ObservabilitySpec
import qualified ResourceGuardSpec
import qualified SessionStateSpec
import qualified RuntimeManagerSpec
import qualified SystemIntegrationSpec

main :: IO ()
main = hspec $ do
  CLICommandsSpec.spec
  CLIInstallSpec.spec
  CLITypesSpec.spec
  CrossPlatformSpec.spec
  DiagnosticsSpec.spec
  DoctorSpec.spec
  ErrorHandlingSpec.spec
  GHCDiagnosticsSpec.spec
  GHCRuntimeSpec.spec
  GHCSessionSpec.spec
  PerformanceSpec.spec
  ProtocolEnvelopeSpec.spec
  SignatureValidationSpec.spec
  ObservabilitySpec.spec
  ResourceGuardSpec.spec
  SessionStateSpec.spec
  RuntimeManagerSpec.spec
  SystemIntegrationSpec.spec
