{-# LANGUAGE OverloadedStrings #-}

module HsJupyter.Router.RequestRouter
  ( Router(..)
  , mkRouter
  , routeExecuteRequest
  , acknowledgeInterrupt
  ) where

import Data.Aeson (Value)
import Data.Text (Text)

import HsJupyter.Bridge.Protocol.Envelope
  ( ExecuteRequest(..)
  , InterruptReply(..)
  , MessageHeader(..)
  , ProtocolEnvelope(..)
  )
import HsJupyter.Runtime.Manager
  ( RuntimeManager
  , enqueueInterrupt
  , submitExecute
  )
import HsJupyter.Runtime.SessionState
  ( ExecuteContext(..)
  , ExecutionOutcome
  , JobMetadata(..)
  )

newtype Router = Router
  { routerManager :: RuntimeManager
  }

mkRouter :: RuntimeManager -> Router
mkRouter = Router

routeExecuteRequest
  :: Router
  -> ProtocolEnvelope ExecuteRequest
  -> IO ExecutionOutcome
routeExecuteRequest (Router manager) env = do
  let req = envelopeContent env
      header = envelopeHeader env
      ctx = ExecuteContext
        { ecMessageId = msgId header
        , ecSessionId = session header
        , ecUsername  = username header
        , ecParentId  = msgId <$> envelopeParent env
        }
      metadata = JobMetadata
        { jmSilent = erSilent req
        , jmStoreHistory = erStoreHistory req
        , jmAllowStdin = erAllowStdin req
        , jmUserExpressions = envelopeMetadata env
        }
  submitExecute manager ctx metadata (erCode req)

acknowledgeInterrupt
  :: Router
  -> ProtocolEnvelope Value
  -> IO InterruptReply
acknowledgeInterrupt (Router manager) env = do
  enqueueInterrupt manager (msgId (envelopeHeader env))
  pure (InterruptReply "ok")
