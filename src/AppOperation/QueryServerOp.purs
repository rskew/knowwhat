-- | QueryServerOp DSL for asking the server/database questions from the client
module AppOperation.QueryServerOp where

import Prelude

import Core (GraphId, NodeId)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.UUID as UUID
import Foreign (Foreign)
import Foreign as Foreign
import Foreign.Class (class Encode, class Decode, decode)
import Foreign.Generic (genericEncode, genericDecode, defaultOptions)
import Foreign.Utils (parseUUIDEither, toExceptT)
import Run (Run, FProxy, Step(..))
import Run as Run


data QueryServerOpF next
  = ConnectSubgraphIfTitleExists NodeId String next
  | OpenGraphsWithSubgraph GraphId next
  | CreateGraph GraphId String next

derive instance functorQueryServerOpF :: Functor QueryServerOpF

type QUERYSERVEROP = FProxy QueryServerOpF

_queryServerOp :: SProxy "queryServerOp"
_queryServerOp = SProxy

handleQueryServerOpOnClient :: forall a b. QueryServerOpF a -> Tuple (b -> b) a
handleQueryServerOpOnClient = case _ of
  ConnectSubgraphIfTitleExists nodeId title next -> Tuple (identity) next
  OpenGraphsWithSubgraph graphId next -> Tuple (identity) next
  CreateGraph graphId title next -> Tuple (identity) next

interpretQueryServerOpOnClient :: forall r a b.
                                  Run (queryServerOp :: QUERYSERVEROP | r) a -> Run r (Tuple (b -> b) a)
interpretQueryServerOpOnClient =
  Run.runAccumPure
  (\accumulator ->
    Run.on _queryServerOp (Loop <<< lmap ((>>>) accumulator) <<< handleQueryServerOpOnClient) Done)
  Tuple
  identity

showQueryServerOp :: forall a. QueryServerOpF a -> Tuple String a
showQueryServerOp = case _ of
  ConnectSubgraphIfTitleExists nodeId title next ->
    Tuple ("ConnectSubgraphIfTitleExists title: " <> title <> " nodeId: " <> show nodeId) next
  OpenGraphsWithSubgraph graphId next ->
    Tuple ("OpenGraphsWithSubgraph " <> show graphId) next
  CreateGraph graphId title next ->
    Tuple ("CreateNewGraph " <> show graphId <> " with title: " <> title) next

interpretShowQueryServerOp :: forall r a. Run (queryServerOp :: QUERYSERVEROP | r) a -> Run r String
interpretShowQueryServerOp op =
  (op # Run.runAccumPure
   (\accumulator -> Run.on _queryServerOp (Loop <<< lmap (append accumulator) <<< showQueryServerOp) Done)
   (\accumulator a -> accumulator)
   "")

------
-- Interface

connectSubgraphIfTitleExists :: forall r. NodeId -> String -> Run (queryServerOp :: QUERYSERVEROP | r) Unit
connectSubgraphIfTitleExists nodeId title = Run.lift _queryServerOp $ ConnectSubgraphIfTitleExists nodeId title unit

openGraphsWithSubgraph :: forall r. GraphId -> Run (queryServerOp :: QUERYSERVEROP | r) Unit
openGraphsWithSubgraph graphId = Run.lift _queryServerOp $ OpenGraphsWithSubgraph graphId unit

createGraph :: forall r. GraphId -> String -> Run (queryServerOp :: QUERYSERVEROP | r) Unit
createGraph graphId title = Run.lift _queryServerOp $ CreateGraph graphId title unit


--------
---- Serialisation/deserialisation

instance decodeQueryServerOpF :: Decode (QueryServerOpF Unit) where
  decode x = x # genericDecode defaultOptions >>= toExceptT <<< fromForeignQueryServerOpF

data ForeignQueryServerOpF
  = ForeignConnectSubgraphIfTitleExists String String
  | ForeignOpenGraphsWithSubgraph String
  | ForeignCreateGraph String String

derive instance genericForeignQueryServerOpF :: Generic ForeignQueryServerOpF _

instance encodeForeignQueryServerOpF :: Encode ForeignQueryServerOpF where
  encode x = x # genericEncode defaultOptions

instance decodeForeignQueryServerOpF :: Decode ForeignQueryServerOpF where
  decode x = x # genericDecode defaultOptions

toForeignQueryServerOpF :: forall a. QueryServerOpF a -> Tuple Foreign a
toForeignQueryServerOpF = lmap (genericEncode defaultOptions) <<< case _ of
  ConnectSubgraphIfTitleExists nodeId title next ->
    Tuple (ForeignConnectSubgraphIfTitleExists (UUID.toString nodeId) title) next
  OpenGraphsWithSubgraph graphId next ->
    Tuple (ForeignOpenGraphsWithSubgraph (UUID.toString graphId)) next
  CreateGraph graphId title next ->
    Tuple (ForeignCreateGraph (UUID.toString graphId) title) next

fromForeignQueryServerOpF :: ForeignQueryServerOpF -> Either String (QueryServerOpF Unit)
fromForeignQueryServerOpF = case _ of
  ForeignConnectSubgraphIfTitleExists nodeIdStr title -> do
    nodeId <- parseUUIDEither nodeIdStr
    pure $ ConnectSubgraphIfTitleExists nodeId title unit
  ForeignOpenGraphsWithSubgraph graphIdStr -> do
    graphId <- parseUUIDEither graphIdStr
    pure $ OpenGraphsWithSubgraph graphId unit
  ForeignCreateGraph graphIdStr title -> do
    graphId <- parseUUIDEither graphIdStr
    pure $ CreateGraph graphId title unit

newtype ForeignQueryServerOp = ForeignQueryServerOp (Run (queryServerOp :: QUERYSERVEROP) Unit)

instance encodeForeignQueryServerOp' :: Encode ForeignQueryServerOp where
  encode = Foreign.unsafeToForeign <<< encodeForeignQueryServerOp

instance decodeForeignQueryServerOp' :: Decode ForeignQueryServerOp where
  decode = decodeForeignQueryServerOp

encodeForeignQueryServerOp :: ForeignQueryServerOp -> Array Foreign
encodeForeignQueryServerOp (ForeignQueryServerOp op) =
  Run.extract $
  (op # Run.runAccumPure
    (\accumulator -> Run.on
      _queryServerOp (Loop <<< lmap (\encodedOp -> accumulator <> [encodedOp]) <<< toForeignQueryServerOpF) Done)
    (\accumulator a -> accumulator)
    [])

decodeForeignQueryServerOp :: Foreign -> Foreign.F ForeignQueryServerOp
decodeForeignQueryServerOp foreignOpArray =
  let
    decodeQueryServerOp :: Foreign -> Foreign.F (Run (queryServerOp :: QUERYSERVEROP) Unit)
    decodeQueryServerOp = map (Run.lift _queryServerOp) <<< (decode :: Foreign -> Foreign.F (QueryServerOpF Unit))
  in do
    arrayForeign <- Foreign.readArray foreignOpArray
    decodedOperations <- traverse decodeQueryServerOp arrayForeign
    pure $ ForeignQueryServerOp $ foldl bind (pure unit) $ map const decodedOperations
