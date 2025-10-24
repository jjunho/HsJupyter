module HsJupyter.Router.RequestRouter
  ( Router(..)
  , mkRouter
  , routeExecuteRequest
  , acknowledgeInterrupt
  ) where

import Data.Aeson (Value)
import HsJupyter.Bridge.Protocol.Envelope
  ( ExecuteRequest
  , InterruptReply(..)
  , ProtocolEnvelope(..)
  )
import HsJupyter.Runtime.EchoRuntime
  ( EchoRuntime
  , ExecutionOutcome(..)
  , executeRequest
  )

-- | Request router orchestrating calls into the runtime stub.
newtype Router = Router
  { routerRuntime :: EchoRuntime
  }

-- | Construct a router from an echo runtime handle.
mkRouter :: EchoRuntime -> Router
mkRouter = Router

-- | Route an execute request to the runtime and return the resulting outcome.
routeExecuteRequest :: Router -> ProtocolEnvelope ExecuteRequest -> IO ExecutionOutcome
routeExecuteRequest (Router runtime) env = executeRequest runtime (envelopeContent env)

-- | Produce an interrupt acknowledgement payload.
acknowledgeInterrupt :: Router -> ProtocolEnvelope Value -> InterruptReply
acknowledgeInterrupt _ _ = InterruptReply "ok"
