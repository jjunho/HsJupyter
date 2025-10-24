import Test.Hspec

import qualified DiagnosticsSpec
import qualified ObservabilitySpec
import qualified ProtocolEnvelopeSpec
import qualified ResourceGuardSpec
import qualified RuntimeManagerSpec
import qualified SessionStateSpec
import qualified SignatureValidationSpec
import qualified TelemetrySpec

main :: IO ()
main = hspec $ do
  DiagnosticsSpec.spec
  ObservabilitySpec.spec
  ProtocolEnvelopeSpec.spec
  ResourceGuardSpec.spec
  RuntimeManagerSpec.spec
  SessionStateSpec.spec
  SignatureValidationSpec.spec
  TelemetrySpec.spec
