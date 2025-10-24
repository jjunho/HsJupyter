import Test.Hspec

import qualified ProtocolEnvelopeSpec
import qualified SignatureValidationSpec
import qualified ObservabilitySpec

main :: IO ()
main = hspec $ do
  ProtocolEnvelopeSpec.spec
  SignatureValidationSpec.spec
  ObservabilitySpec.spec
