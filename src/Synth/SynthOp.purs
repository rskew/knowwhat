module Synth.SynthOp where

import Prelude

import AppOperation.GraphOp (GraphOpF(..))
import Audio.WebAudio.Oscillator (stopOscillator) as WebAudio
import Audio.WebAudio.Types (AudioNode(..), Value, connect, disconnect) as WebAudio
import Core (NodeId, EdgeId)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Data.UUID as UUID
import Effect (Effect)
import Effect.Console as Console
import Foreign (Foreign)
import Foreign.Class (class Encode, class Decode)
import Foreign.Generic (genericEncode, genericDecode, defaultOptions)
import Foreign.Utils (parseUUIDEither, edgeIdToString, parseEdgeIdEither, toExceptT)
import Run (FProxy, Run, Step(..))
import Run as Run
import Synth (Synth, SynthParams, SynthNodeParams(..), SynthNodeState(..), SynthParameter(..), defaultOscillatorFreq, defaultAmplifierGain, defaultDelayPeriod, defaultFilterCutoff, freshSynthNodeParams, inputPort, newSynthNodeState, outputPort, parseSynthNodeType, updateParam)

data SynthOpF next
  = CreateSynthNode NodeId SynthNodeParams next
  | DeleteSynthNode NodeId SynthNodeParams next
  | ConnectSynthNodes EdgeId next
  | DisconnectSynthNodes EdgeId next
  | UpdateParameter NodeId SynthParameter WebAudio.Value WebAudio.Value next

derive instance functorSynthOpF :: Functor SynthOpF

showSynthOp :: forall a. SynthOpF a -> Tuple String a
showSynthOp = case _ of
  CreateSynthNode node synthNodeParams a ->
    Tuple ("CreateSynthNode node: " <> show node   <> " " <> show synthNodeParams) a
  DeleteSynthNode node synthNodeParams a ->
    Tuple ("DeleteSynthNode node: " <> show node   <> " " <> show synthNodeParams) a
  ConnectSynthNodes edgeId a ->
    Tuple ("ConnectSynthNodes node: " <> show edgeId.source <> " " <> show edgeId.target) a
  DisconnectSynthNodes edgeId a ->
    Tuple ("DisconnectSynthNodes node: " <> show edgeId.source <> " " <> show edgeId.target) a
  UpdateParameter node synthParameter from to a ->
    Tuple ("UpdateParameter node: " <> show node   <> " " <> show synthParameter <> " from: " <> show from <> " to: " <> show to) a

invertSynthOp :: SynthOpF ~> SynthOpF
invertSynthOp = case _ of
  CreateSynthNode nodeId synthNodeParams next        -> DeleteSynthNode nodeId synthNodeParams        next
  DeleteSynthNode nodeId synthNodeParams next        -> CreateSynthNode nodeId synthNodeParams        next
  ConnectSynthNodes edgeId               next        -> DisconnectSynthNodes edgeId                   next
  DisconnectSynthNodes edgeId            next        -> ConnectSynthNodes edgeId                      next
  UpdateParameter nodeId synthParameter from to next -> UpdateParameter nodeId synthParameter to from next

collapseSynthOp :: forall a. SynthOpF a -> SynthOpF a -> Maybe (SynthOpF Unit)
collapseSynthOp opA opB = case Tuple opA opB of
  Tuple (UpdateParameter nodeAId parameterA fromA toA nextA)
        (UpdateParameter nodeBId parameterB fromB toB nextB) ->
    if nodeAId == nodeBId
       && parameterA == parameterB
    then Just $ UpdateParameter nodeAId parameterA fromB toA unit
    else Nothing
  _ -> Nothing

type SYNTHOP = FProxy SynthOpF

_synthOp :: SProxy "synthOp"
_synthOp = SProxy

handleSynthOp :: forall a. SynthOpF a -> Tuple (Synth -> Effect Synth) a
handleSynthOp = case _ of
  CreateSynthNode nodeId synthNodeParams next ->
    Tuple
    (\synth -> do
      Console.log "handleSynthOp CreateSynthNode"
      synthNodeState <- newSynthNodeState synthNodeParams synth.synthState.audioContext
      let
        updatedNodeStates  = Map.insert nodeId synthNodeState synth.synthState.synthNodeStates
        updatedSynthParams = Map.insert nodeId synthNodeParams synth.synthParams
      pure $ synth { synthState  = synth.synthState { synthNodeStates = updatedNodeStates }
                   , synthParams = updatedSynthParams
                   })
    next

  DeleteSynthNode nodeId synthNodeParams next ->
    Tuple
    (\synth ->
      case Map.lookup nodeId synth.synthState.synthNodeStates of
        Nothing -> pure synth
        Just synthNodeState -> do
          case synthNodeState of
            OscillatorState oscNode -> WebAudio.stopOscillator 0.0 oscNode
            -- TODO: uncomment when AudioBufferSource is implemented
            --WebAudio.AudioBufferSource audioBufferSource -> WebAudio.stopBufferSource 0.0 audioBufferSource
            _ -> pure unit
          let
            updatedNodeStates  = Map.delete nodeId synth.synthState.synthNodeStates
            updatedSynthParams = Map.delete nodeId synth.synthParams
          pure $ synth { synthState  = synth.synthState { synthNodeStates = updatedNodeStates }
                       , synthParams = updatedSynthParams
                       }
    )
    next

  ConnectSynthNodes edgeId next ->
    Tuple
    (\synth -> do
      Console.log "interpretSynth InsertEdge"
      case Tuple (Map.lookup edgeId.source synth.synthState.synthNodeStates)
                 (Map.lookup edgeId.target synth.synthState.synthNodeStates) of
        Tuple (Just sourceNodeState) (Just targetNodeState) -> do
          Console.log "connecting two audio nodes"
          connectSynthNodeStatesWith connectAudioNodes sourceNodeState targetNodeState
        _ -> do
          pure unit
      pure synth)
    next

  DisconnectSynthNodes edgeId next ->
    Tuple
    (\synth -> do
      case Tuple (Map.lookup edgeId.source synth.synthState.synthNodeStates)
                 (Map.lookup edgeId.target synth.synthState.synthNodeStates) of
        Tuple (Just sourceNodeState) (Just targetNodeState) -> do
          Console.log $ "disconnecting " <> show sourceNodeState <> " from " <> show targetNodeState
          connectSynthNodeStatesWith disconnectAudioNodes sourceNodeState targetNodeState
        _ -> pure unit
      pure synth)
    next

  UpdateParameter nodeId parameter from to next ->
    Tuple (\synth -> updateParam synth nodeId parameter to) next

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

-- | Interpret an operation on graphs to an operation on synthesisers
handleGraphOpAsSynthUpdate :: forall a. GraphOpF a -> Tuple (Synth -> Effect Synth) a
handleGraphOpAsSynthUpdate = case _ of
  -- Synth node creation happens when the text field is set to a
  -- synth node name. Don't do anything on InsertNode operations.
  InsertNode _ nodeId next ->
    Tuple pure next

  -- If the node text is a valid synth node type, delete the corresponding
  -- synth node
  DeleteNode _ nodeId next ->
    Tuple
    (\synth ->
      case Map.lookup nodeId synth.synthParams of
        Nothing -> pure synth
        Just synthNodeParams ->
          let
            synthOp = deleteSynthNode nodeId synthNodeParams
            Tuple synthStateUpdate _ = Run.extract $ interpretSynthOp synthOp
          in
            synthStateUpdate synth)
    next

  -- If the node text is a valid synth node type, create the corresponding
  -- connection between synth nodes
  InsertEdge edgeId next ->
    let
      synthOp = connectSynthNodes edgeId
      Tuple synthStateUpdate _ = Run.extract $ interpretSynthOp synthOp
    in
      Tuple synthStateUpdate next

  -- If the node text is a valid synth node type, delete the corresponding
  -- connection between synth nodes
  DeleteEdge edgeId next ->
    let
      synthOp = disconnectSynthNodes edgeId
      Tuple synthStateUpdate _ = Run.extract $ interpretSynthOp synthOp
    in
     Tuple synthStateUpdate next

  -- Moving a node doesn't affect the synth
  MoveNode nodeId from to next ->
    Tuple pure next

  -- If the synth node text goes from invalid to valid, create the
  -- corresponding synth node.
  -- If the synth node text goes from valid to invalid, delete the
  -- corresponding synth node.
  UpdateNodeText nodeId from to next ->
    case Tuple (parseSynthNodeType from) (parseSynthNodeType to) of
      Tuple Nothing (Just nodeType) ->
        let
          synthNodeParams = freshSynthNodeParams nodeType
          synthOp = createSynthNode nodeId synthNodeParams
          Tuple synthStateUpdate _ = Run.extract $ interpretSynthOp synthOp
        in
          Tuple synthStateUpdate next
      Tuple (Just _) Nothing ->
        -- We're going to consume the DeleteNode object immediately
        -- and we don't use its GraphId field.
        let dummyGraphId = nodeId in
        handleGraphOpAsSynthUpdate $ DeleteNode dummyGraphId nodeId next
      _ -> Tuple (\synth -> do
                    Console.log $ "node text updated but not synthy, from: " <> from <> " to: " <> to
                    pure synth
                 ) next

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
  WebAudio.Oscillator rawOscillatorNode         -> WebAudio.connect rawOscillatorNode target
  WebAudio.Gain rawGainNode                     -> WebAudio.connect rawGainNode target
  WebAudio.Delay rawDelayNode                   -> WebAudio.connect rawDelayNode target
  WebAudio.BiquadFilter rawFilterNode           -> WebAudio.connect rawFilterNode target
  WebAudio.Analyser rawAnalyserNode             -> WebAudio.connect rawAnalyserNode target
  WebAudio.AudioBufferSource rawAudioBufferNode -> WebAudio.connect rawAudioBufferNode target
  WebAudio.Convolver rawConvolverNode           -> WebAudio.connect rawConvolverNode target
  WebAudio.DynamicsCompressor rawCompressorNode -> WebAudio.connect rawCompressorNode target
  WebAudio.StereoPanner rawPannerNode           -> WebAudio.connect rawPannerNode target
  WebAudio.Destination rawDestinationNode       -> WebAudio.connect rawDestinationNode target

disconnectAudioNodes :: WebAudio.AudioNode -> WebAudio.AudioNode -> Effect Unit
disconnectAudioNodes source target = case source of
  WebAudio.Oscillator rawOscillatorNode         -> WebAudio.disconnect rawOscillatorNode target
  WebAudio.Gain rawGainNode                     -> WebAudio.disconnect rawGainNode target
  WebAudio.Delay rawDelayNode                   -> WebAudio.disconnect rawDelayNode target
  WebAudio.BiquadFilter rawFilterNode           -> WebAudio.disconnect rawFilterNode target
  WebAudio.Analyser rawAnalyserNode             -> WebAudio.disconnect rawAnalyserNode target
  WebAudio.AudioBufferSource rawAudioBufferNode -> WebAudio.disconnect rawAudioBufferNode target
  WebAudio.Convolver rawConvolverNode           -> WebAudio.disconnect rawConvolverNode target
  WebAudio.DynamicsCompressor rawCompressorNode -> WebAudio.disconnect rawCompressorNode target
  WebAudio.StereoPanner rawPannerNode           -> WebAudio.disconnect rawPannerNode target
  WebAudio.Destination rawDestinationNode       -> WebAudio.disconnect rawDestinationNode target

createSynthNode :: forall r. NodeId -> SynthNodeParams -> Run (synthOp :: SYNTHOP | r) Unit
createSynthNode nodeId synthNodeParams =
  Run.lift _synthOp $ CreateSynthNode nodeId synthNodeParams unit

deleteSynthNode :: forall r. NodeId -> SynthNodeParams -> Run (synthOp :: SYNTHOP | r) Unit
deleteSynthNode nodeId synthNodeParams =
  let
    deleteSynthNodeOp = Run.lift _synthOp $ DeleteSynthNode nodeId synthNodeParams unit
  in
    case synthNodeParams of
      OscillatorParams freq ->
        updateSynthParam nodeId OscillatorFreq freq defaultOscillatorFreq
        >>= const deleteSynthNodeOp
      AmplifierParams gain ->
        updateSynthParam nodeId AmplifierGain gain defaultAmplifierGain
        >>= const deleteSynthNodeOp
      DelayParams periodSeconds ->
        updateSynthParam nodeId DelayPeriod periodSeconds defaultDelayPeriod
        >>= const deleteSynthNodeOp
      FilterParams filterParams ->
        updateSynthParam nodeId FilterLowpassCutoff filterParams.lowpassParams.cutoff defaultFilterCutoff
        >>= const (updateSynthParam nodeId FilterHighpassCutoff filterParams.highpassParams.cutoff defaultFilterCutoff)
        >>= const deleteSynthNodeOp
      DestinationParams ->
        deleteSynthNodeOp

connectSynthNodes :: forall r. EdgeId -> Run (synthOp :: SYNTHOP | r) Unit
connectSynthNodes edgeId =
  Run.lift _synthOp $ ConnectSynthNodes edgeId unit

disconnectSynthNodes :: forall r. EdgeId -> Run (synthOp :: SYNTHOP | r) Unit
disconnectSynthNodes edgeId =
  Run.lift _synthOp $ DisconnectSynthNodes edgeId unit

updateSynthParam :: forall r. NodeId -> SynthParameter -> WebAudio.Value -> WebAudio.Value -> Run (synthOp :: SYNTHOP | r) Unit
updateSynthParam nodeId parameter oldValue newValue =
  Run.lift _synthOp $ UpdateParameter nodeId parameter oldValue newValue unit

encodeSynthNodeParamsAsSynthOp :: forall r. NodeId -> SynthNodeParams -> Run (synthOp :: SYNTHOP | r) Unit
encodeSynthNodeParamsAsSynthOp nodeId = case _ of
  OscillatorParams freq ->
    updateSynthParam nodeId OscillatorFreq freq freq
  AmplifierParams gain ->
    updateSynthParam nodeId AmplifierGain gain gain
  DelayParams periodSeconds ->
    updateSynthParam nodeId DelayPeriod periodSeconds periodSeconds
  FilterParams filterParams -> do
    updateSynthParam nodeId FilterLowpassCutoff  filterParams.lowpassParams.cutoff  filterParams.lowpassParams.cutoff
    updateSynthParam nodeId FilterHighpassCutoff filterParams.highpassParams.cutoff filterParams.highpassParams.cutoff
  DestinationParams ->
    pure unit

encodeSynthParamsAsSynthOp :: forall r. SynthParams -> Run (synthOp :: SYNTHOP | r) Unit
encodeSynthParamsAsSynthOp synthParams =
  ((Map.toUnfoldable synthParams) :: Array (Tuple NodeId SynthNodeParams))
  <#> (\(Tuple nodeId synthNodeParams) -> encodeSynthNodeParamsAsSynthOp nodeId synthNodeParams)
  <#> const
  # foldl bind (pure unit)

------
-- Serialisation/deserialisation

data ForeignSynthOpF
  = ForeignCreateSynthNode String SynthNodeParams
  | ForeignDeleteSynthNode String SynthNodeParams
  | ForeignConnectSynthNodes String
  | ForeignDisconnectSynthNodes String
  | ForeignUpdateParameter String SynthParameter WebAudio.Value WebAudio.Value

derive instance genericForeignSynthOpF :: Generic ForeignSynthOpF _

instance encodeForeignSynthOpF :: Encode ForeignSynthOpF where
  encode x = x # genericEncode defaultOptions

instance decodeForeignSynthOpF :: Decode ForeignSynthOpF where
  decode x = x # genericDecode defaultOptions

toForeignSynthOpF :: forall a. SynthOpF a -> Tuple Foreign a
toForeignSynthOpF = lmap (genericEncode defaultOptions) <<< case _ of
  CreateSynthNode nodeId synthNodeParams next ->
    Tuple (ForeignCreateSynthNode (show nodeId) synthNodeParams) next
  DeleteSynthNode nodeId synthNodeParams next ->
    Tuple (ForeignDeleteSynthNode (show nodeId) synthNodeParams) next
  ConnectSynthNodes edgeId next ->
    Tuple (ForeignConnectSynthNodes (edgeIdToString edgeId)) next
  DisconnectSynthNodes edgeId next ->
    Tuple (ForeignDisconnectSynthNodes (edgeIdToString edgeId)) next
  UpdateParameter nodeId synthParameter from to next ->
    Tuple (ForeignUpdateParameter (UUID.toString nodeId) synthParameter from to) next

fromForeignSynthOpF :: ForeignSynthOpF -> Either String (SynthOpF Unit)
fromForeignSynthOpF = case _ of
  ForeignCreateSynthNode nodeIdStr synthNodeParams -> do
    nodeId <- parseUUIDEither nodeIdStr
    pure $ CreateSynthNode nodeId synthNodeParams unit
  ForeignDeleteSynthNode nodeIdStr synthNodeParams -> do
    nodeId <- parseUUIDEither nodeIdStr
    pure $ DeleteSynthNode nodeId synthNodeParams unit
  ForeignConnectSynthNodes edgeIdStr -> do
    edgeId <- parseEdgeIdEither edgeIdStr
    pure $ ConnectSynthNodes edgeId unit
  ForeignDisconnectSynthNodes edgeIdStr -> do
    edgeId <- parseEdgeIdEither edgeIdStr
    pure $ DisconnectSynthNodes edgeId unit
  ForeignUpdateParameter nodeIdStr synthParameter from to -> do
    nodeId <- parseUUIDEither nodeIdStr
    pure $ UpdateParameter nodeId synthParameter from to unit

instance decodeSynthOpF :: Decode (SynthOpF Unit) where
  decode x = x # genericDecode defaultOptions >>= toExceptT <<< fromForeignSynthOpF
