import Test.Hspec

import qualified DiagnosticsSpec
import qualified ProtocolEnvelopeSpec
import qualified SignatureValidationSpec
import qualified ObservabilitySpec
import qualified ResourceGuardSpec
import qualified SessionStateSpec
import qualified RuntimeManagerSpec

main :: IO ()
main = hspec $ do
  DiagnosticsSpec.spec
  ProtocolEnvelopeSpec.spec
  SignatureValidationSpec.spec
  ObservabilitySpec.spec
  ResourceGuardSpec.spec
  SessionStateSpec.spec
  RuntimeManagerSpec.spec
