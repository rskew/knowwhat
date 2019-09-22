module Workflow.Synth.SynthOp where

import Prelude

import Audio.WebAudio.Oscillator (stopOscillator) as WebAudio
import Audio.WebAudio.Types (AudioNode(..), Value, connect, disconnect) as WebAudio
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Foldable (for_)
import Data.Generic.Rep (class Generic)
import Data.Lens ((^.), (.~))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Foreign.Class (class Encode, class Decode)
import Foreign.Generic (genericEncode, genericDecode, defaultOptions)
import Foreign (Foreign)
import Foreign.Unit (ForeignUnit(..))
import Run (FProxy, Run, Step(..))
import Run as Run
import Workflow.Core (_children, _nodeId, _parents)
import Workflow.Synth (Synth, SynthParams(..), SynthNodeParams, SynthNodeState(..), SynthParameter, freshSynthNodeParams, newSynthNodeState, inputPort, outputPort, parseSynthNodeType, updateParam)
import Workflow.UIGraph.Types (UINode, _nodeText)
import Workflow.UIGraph.UIGraphOp (UIGraphOpF(..))

data SynthOpF next
  = CreateSynthNode UINode SynthNodeParams next
  | DeleteSynthNode UINode SynthNodeParams next
  | ConnectSynthNodes UINode UINode next
  | DisconnectSynthNodes UINode UINode next
  | UpdateParameter UINode SynthParameter WebAudio.Value WebAudio.Value next
derive instance functorSynthOpF :: Functor SynthOpF
showSynthOp :: forall a. SynthOpF a -> Tuple String a
showSynthOp = case _ of
  CreateSynthNode node synthNodeParams a ->
    Tuple ("CreateSynthNode " <> show node <> " " <> show synthNodeParams) a
  DeleteSynthNode node synthNodeParams a ->
    Tuple ("DeleteSynthNode " <> show node <> " " <> show synthNodeParams) a
  ConnectSynthNodes source target a ->
    Tuple ("ConnectSynthNodes " <> show source <> " " <> show target) a
  DisconnectSynthNodes source target a ->
    Tuple ("DisconnectSynthNodes " <> show source <> " " <> show target) a
  UpdateParameter node synthParameter from to a ->
    Tuple ("UpdateParameter " <> show node <> " " <> show synthParameter <> " from: " <> show from <> " to: " <> show to) a

invertSynthOp :: SynthOpF ~> SynthOpF
invertSynthOp = case _ of
  CreateSynthNode node synthNodeParams next ->        DeleteSynthNode node synthNodeParams next
  DeleteSynthNode node synthNodeParams next ->        CreateSynthNode node synthNodeParams next
  ConnectSynthNodes sourceId targetId next ->         DisconnectSynthNodes sourceId targetId next
  DisconnectSynthNodes sourceId targetId next ->      ConnectSynthNodes sourceId targetId next
  UpdateParameter node synthParameter from to next -> UpdateParameter node synthParameter to from next

collapseSynthOp :: forall a. SynthOpF a -> SynthOpF a -> Maybe (SynthOpF Unit)
collapseSynthOp opA opB = case Tuple opA opB of
  Tuple (UpdateParameter nodeA parameterA fromA toA nextA)
        (UpdateParameter nodeB parameterB fromB toB nextB) ->
    if (nodeA ^. _nodeId) == (nodeB ^. _nodeId)
       && parameterA == parameterB
    then Just $ UpdateParameter nodeA parameterA fromB toA unit
    else Nothing
  _ -> Nothing

type SYNTHOP = FProxy SynthOpF

_synthOp :: SProxy "synthOp"
_synthOp = SProxy

handleSynthOp :: forall a. SynthOpF a -> Tuple (Synth -> Effect Synth) a
handleSynthOp = case _ of
  CreateSynthNode node synthNodeParams next ->
    Tuple (\synth -> do
             log "handleSynthOp CreateSynthNode"
             log $ node ^. _nodeText
             synthNodeState <- newSynthNodeState synthNodeParams synth.synthState.audioContext
             connectParentChildSynthNodes synthNodeState node synth
             let
               updatedNodeStates = Map.insert (node ^. _nodeId) synthNodeState synth.synthState.synthNodeStates
               synthParams = SynthParams $ Map.insert (node ^. _nodeId) synthNodeParams (unwrap synth.synthParams)
             pure $ synth { synthState = synth.synthState { synthNodeStates = updatedNodeStates }
                          , synthParams = synthParams
                          })
          next

  DeleteSynthNode node synthNodeParams next ->
    Tuple (\synth ->
            case Map.lookup (node ^. _nodeId) synth.synthState.synthNodeStates of
              Nothing -> pure synth
              Just synthNodeState -> do
                disconnectParentChildSynthNodes synthNodeState node synth
                case synthNodeState of
                  OscillatorState oscNode -> WebAudio.stopOscillator 0.0 oscNode
                  --WebAudio.AudioBufferSource audioBufferSource -> WebAudio.stopBufferSource 0.0 audioBufferSource
                  _ -> pure unit
                let updatedNodeStates = Map.delete (node ^. _nodeId) synth.synthState.synthNodeStates
                pure $ synth { synthState = synth.synthState { synthNodeStates = updatedNodeStates }
                             , synthParams = SynthParams $ Map.delete (node ^. _nodeId) (unwrap synth.synthParams)
                             })
          next

  ConnectSynthNodes source target next ->
    Tuple (\synth -> do
              log "interpretSynth InsertEdge"
              case Tuple (Map.lookup (source ^. _nodeId) synth.synthState.synthNodeStates)
                         (Map.lookup (target ^. _nodeId) synth.synthState.synthNodeStates) of
                Tuple (Just sourceNodeState) (Just targetNodeState) -> do
                  log "connecting two audio nodes"
                  connectSynthNodeStatesWith connectAudioNodes sourceNodeState targetNodeState
                _ -> do
                  log "interpretSynth InsertEdge case failed"
                  pure unit
              pure synth)
          next

  DisconnectSynthNodes source target next ->
    Tuple (\synth -> do
              case Tuple (Map.lookup (source ^. _nodeId) synth.synthState.synthNodeStates)
                         (Map.lookup (target ^. _nodeId) synth.synthState.synthNodeStates) of
                Tuple (Just sourceNodeState) (Just targetNodeState) -> do
                  log $ "disconnecting " <> show sourceNodeState <> " from " <> show targetNodeState
                  connectSynthNodeStatesWith disconnectAudioNodes sourceNodeState targetNodeState
                _ -> pure unit
              pure synth)
          next

  UpdateParameter node parameter from to next ->
    Tuple (\synth -> updateParam synth (node ^. _nodeId) parameter to) next

interpretSynthOp :: forall r a. Run (synthOp :: SYNTHOP | r) a
                    -> Run r (Tuple (Synth -> Effect Synth) a)
interpretSynthOp =
  Run.runAccumPure
  (\accumulator ->
    Run.on
    _synthOp
    (\synthOp ->
      let
        Tuple synthUpdater next = handleSynthOp synthOp
      in
        Loop $ Tuple (\synth -> accumulator synth >>= synthUpdater) next)
    Done)
  (\accumulator a -> Tuple accumulator a)
  pure


--discardSynthOp :: forall a. SynthOpF a -> a
--discardSynthOp = case _ of
--  CreateSynthNode _ _ next      -> next
--  DeleteSynthNode _ _ next      -> next
--  ConnectSynthNodes _ _ next    -> next
--  DisconnectSynthNodes _ _ next -> next
--  UpdateParameter _ _ _ _ next  -> next
--
--discardSynthOps :: forall r a. Run (synthOp :: SYNTHOP | r) a -> Run r a
--discardSynthOps = Run.run $ Run.on _synthOp discardSynthOp Run.send

-- | Compile an operation on the graph to an operation on the synthesiser
-- | that changes the synth context as an Effect and returns a function that
-- | changes the AppState.synthState
handleUIGraphOpAsSynthUpdate :: forall a. UIGraphOpF a -> Tuple (Synth -> Effect Synth) a
handleUIGraphOpAsSynthUpdate = case _ of
  -- If the node text is a valid synth node type, create the corresponding
  -- synth node
  InsertNode node next ->
    case parseSynthNodeType (node ^. _nodeText) of
      Nothing -> Tuple (\synth -> do
                           log $ "failed parse of node text: " <> node ^. _nodeText
                           pure synth)
                       next
      Just synthNodeType ->
        let
          synthNodeParams = freshSynthNodeParams synthNodeType
          synthOp = createSynthNode node synthNodeParams
          Tuple synthStateUpdate _ = Run.extract $ interpretSynthOp synthOp
        in
          Tuple synthStateUpdate next

  -- If the node text is a valid synth node type, delete the corresponding
  -- synth node
  DeleteNode node next ->
    Tuple (\synth ->
            case Map.lookup (node ^. _nodeId) (unwrap synth.synthParams) of
              Nothing -> pure synth
              Just synthNodeParams ->
                let
                  synthOp = deleteSynthNode node synthNodeParams
                  Tuple synthStateUpdate _ = Run.extract $ interpretSynthOp synthOp
                in
                  synthStateUpdate synth)
          next

  -- If the node text is a valid synth node type, create the corresponding
  -- connection between synth nodes
  ConnectNodes source target next ->
    let
      synthOp = connectSynthNodes source target
      Tuple synthStateUpdate _ = Run.extract $ interpretSynthOp synthOp
    in
      Tuple synthStateUpdate next

  -- If the node text is a valid synth node type, delete the corresponding
  -- connection between synth nodes
  DisconnectNodes source target next ->
    let
      synthOp = disconnectSynthNodes source target
      Tuple synthStateUpdate _ = Run.extract $ interpretSynthOp synthOp
    in
     Tuple synthStateUpdate next

  -- Moving a node doesn't affect the synth
  MoveNode node from to next ->
    Tuple pure next

  -- If the synth node text goes from invalid to valid, create the
  -- corresponding synth node.
  -- If the synth node text goes from valid to invalid, delete the
  -- corresponding synth node.
  UpdateNodeText node from to next ->
    case Tuple (parseSynthNodeType from) (parseSynthNodeType to) of
      Tuple Nothing (Just nodeType) ->
        handleUIGraphOpAsSynthUpdate $ InsertNode (node # _nodeText .~ to) next
      Tuple (Just _) Nothing -> do
        handleUIGraphOpAsSynthUpdate $ DeleteNode (node # _nodeText .~ to) next
      _ -> Tuple pure next

  -- Edge text doesn't affect the synth
  UpdateEdgeText edge from to next ->
    Tuple pure next


connectSynthNodeStatesWith :: (WebAudio.AudioNode -> WebAudio.AudioNode -> Effect Unit)
                              -> SynthNodeState -> SynthNodeState -> Effect Unit
connectSynthNodeStatesWith f source target =
  let
    sourceNode = outputPort source
    targetNode = inputPort target
  in
    f sourceNode targetNode

connectAudioNodes :: WebAudio.AudioNode -> WebAudio.AudioNode -> Effect Unit
connectAudioNodes source target = case source of
  WebAudio.Oscillator rawOscillatorNode ->         WebAudio.connect rawOscillatorNode target
  WebAudio.Gain rawGainNode ->                     WebAudio.connect rawGainNode target
  WebAudio.Delay rawDelayNode ->                   WebAudio.connect rawDelayNode target
  WebAudio.BiquadFilter rawFilterNode ->           WebAudio.connect rawFilterNode target
  WebAudio.Analyser rawAnalyserNode ->             WebAudio.connect rawAnalyserNode target
  WebAudio.AudioBufferSource rawAudioBufferNode -> WebAudio.connect rawAudioBufferNode target
  WebAudio.Convolver rawConvolverNode ->           WebAudio.connect rawConvolverNode target
  WebAudio.DynamicsCompressor rawCompressorNode -> WebAudio.connect rawCompressorNode target
  WebAudio.StereoPanner rawPannerNode ->           WebAudio.connect rawPannerNode target
  WebAudio.Destination rawDestinationNode ->       WebAudio.connect rawDestinationNode target

disconnectAudioNodes :: WebAudio.AudioNode -> WebAudio.AudioNode -> Effect Unit
disconnectAudioNodes source target = case source of
  WebAudio.Oscillator rawOscillatorNode ->         WebAudio.disconnect rawOscillatorNode target
  WebAudio.Gain rawGainNode ->                     WebAudio.disconnect rawGainNode target
  WebAudio.Delay rawDelayNode ->                   WebAudio.disconnect rawDelayNode target
  WebAudio.BiquadFilter rawFilterNode ->           WebAudio.disconnect rawFilterNode target
  WebAudio.Analyser rawAnalyserNode ->             WebAudio.disconnect rawAnalyserNode target
  WebAudio.AudioBufferSource rawAudioBufferNode -> WebAudio.disconnect rawAudioBufferNode target
  WebAudio.Convolver rawConvolverNode ->           WebAudio.disconnect rawConvolverNode target
  WebAudio.DynamicsCompressor rawCompressorNode -> WebAudio.disconnect rawCompressorNode target
  WebAudio.StereoPanner rawPannerNode ->           WebAudio.disconnect rawPannerNode target
  WebAudio.Destination rawDestinationNode ->       WebAudio.disconnect rawDestinationNode target

-- It should be ok to ignore graph.isDual since this will
-- be executed outside of any graph operations. isDual
-- should only be used internally in graph operations.
mapParentChildSynthNodes :: (SynthNodeState -> SynthNodeState -> Effect Unit)
                            -> SynthNodeState -> UINode -> Synth -> Effect Unit
mapParentChildSynthNodes connectOrDisconnect synthNodeState node synth = do
  let parentSynthNodes = Map.keys (node ^. _parents)
                         # Array.fromFoldable
                         # Array.mapMaybe (\parentId -> Map.lookup parentId synth.synthState.synthNodeStates)
  for_ parentSynthNodes \parentSynthNode ->
                          connectOrDisconnect parentSynthNode synthNodeState
  let childSynthNodes = Map.keys (node ^. _children)
                        # Array.fromFoldable
                        # Array.mapMaybe (\childId -> Map.lookup childId synth.synthState.synthNodeStates)
  for_ childSynthNodes \childSynthNode ->
                         connectOrDisconnect synthNodeState childSynthNode

connectParentChildSynthNodes :: SynthNodeState -> UINode -> Synth -> Effect Unit
connectParentChildSynthNodes = mapParentChildSynthNodes (connectSynthNodeStatesWith connectAudioNodes)

disconnectParentChildSynthNodes :: SynthNodeState -> UINode -> Synth -> Effect Unit
disconnectParentChildSynthNodes = mapParentChildSynthNodes (connectSynthNodeStatesWith disconnectAudioNodes)

createSynthNode :: forall r. UINode -> SynthNodeParams -> Run (synthOp :: SYNTHOP | r) Unit
createSynthNode node synthNodeParams =
  Run.lift _synthOp $ CreateSynthNode node synthNodeParams unit

deleteSynthNode :: forall r. UINode -> SynthNodeParams -> Run (synthOp :: SYNTHOP | r) Unit
deleteSynthNode node synthNodeParams =
  Run.lift _synthOp $ DeleteSynthNode node synthNodeParams unit

connectSynthNodes :: forall r. UINode -> UINode -> Run (synthOp :: SYNTHOP | r) Unit
connectSynthNodes source target =
  Run.lift _synthOp $ ConnectSynthNodes source target unit

disconnectSynthNodes :: forall r. UINode -> UINode -> Run (synthOp :: SYNTHOP | r) Unit
disconnectSynthNodes source target =
  Run.lift _synthOp $ DisconnectSynthNodes source target unit

updateSynthParam :: forall r. UINode -> SynthParameter -> WebAudio.Value -> WebAudio.Value -> Run (synthOp :: SYNTHOP | r) Unit
updateSynthParam node parameter oldValue newValue =
  Run.lift _synthOp $ UpdateParameter node parameter oldValue newValue unit


------
-- Serialisation/deserialisation

derive instance genericSynthOpF :: (Generic a z) => Generic (SynthOpF a) _
instance encodeSynthOpF' :: (Generic a z, Encode a) => Encode (SynthOpF a) where
  encode x = x # genericEncode defaultOptions
instance decodeSynthOpF' :: (Generic a z, Decode a) => Decode (SynthOpF a) where
  decode x = x # genericDecode defaultOptions

encodeSynthOpF :: forall a. SynthOpF a -> Tuple Foreign a
encodeSynthOpF = lmap (genericEncode defaultOptions) <<< case _ of
  CreateSynthNode node synthNodeParams next ->
    Tuple (CreateSynthNode node synthNodeParams ForeignUnit) next
  DeleteSynthNode node synthNodeParams next ->
    Tuple (DeleteSynthNode node synthNodeParams ForeignUnit) next
  ConnectSynthNodes source target next ->
    Tuple (ConnectSynthNodes source target ForeignUnit) next
  DisconnectSynthNodes source target next ->
    Tuple (DisconnectSynthNodes source target ForeignUnit) next
  UpdateParameter node synthParameter from to next ->
    Tuple (UpdateParameter node synthParameter from to ForeignUnit) next
