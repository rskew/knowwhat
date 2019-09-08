module Workflow.Synth where

import Prelude

import Audio.WebAudio.AnalyserNode (setSmoothingTimeConstant, setMaxDecibels) as WebAudio
import Audio.WebAudio.AudioBufferSourceNode (stopBufferSource) as WebAudio
import Audio.WebAudio.AudioParam (linearRampToValueAtTime) as WebAudio
import Audio.WebAudio.BaseAudioContext (createOscillator, createGain, createDelay, createBiquadFilter, createAnalyser, destination, currentTime) as WebAudio
import Audio.WebAudio.BiquadFilterNode (BiquadFilterType(..), setFilterType, filterFrequency, quality, gain) as WebAudio
import Audio.WebAudio.DelayNode (delayTime) as WebAudio
import Audio.WebAudio.GainNode (setGain) as WebAudio
import Audio.WebAudio.Oscillator (OscillatorType(..), setFrequency, setOscillatorType, startOscillator, stopOscillator) as WebAudio
import Audio.WebAudio.Types (AudioContext, AudioNode(..), Seconds, class Connectable, GainNode, DelayNode, AnalyserNode, AudioParam, Value, connect, disconnect, connectParam) as WebAudio
import Data.Array as Array
import Data.ArrayBuffer.Types (Uint8Array, ByteLength)
import Data.ArrayBuffer.Typed (empty)
import Data.Foldable (for_)
import Data.Lens (Lens', Prism', Optic', lens, prism', (^.))
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Strong (class Strong)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Ref as Ref
import Effect.Ref (Ref)
import Effect.Console (log)
import Workflow.Core (NodeId, _nodeId, _parents, _children, _source, _target)
import Workflow.UIGraph.Types (UINode, _nodeText)
import Workflow.UIGraph.UIGraphOp (UIGraphOp(..), UIGraphOpF(..), Free(..))

updateValueRampTime :: WebAudio.Seconds
updateValueRampTime = 0.1

defaultOscillatorFreq :: Freq
defaultOscillatorFreq = 220.0

defaultOscillatorType :: WebAudio.OscillatorType
defaultOscillatorType = WebAudio.Sine

defaultAmplifierGain :: Gain
defaultAmplifierGain = 0.5

defaultDelayPeriod :: PeriodSeconds
defaultDelayPeriod = 0.3

defaultFilterType :: WebAudio.BiquadFilterType
defaultFilterType = WebAudio.Lowpass

defaultFilterCutoff :: Cutoff
defaultFilterCutoff = 4000.0

defaultFilterQFactor :: QFactor
defaultFilterQFactor = 1.0

defaultFilterGain :: Gain
defaultFilterGain = 1.0

defaultFrequencyBinCount :: FrequencyBinCount
defaultFrequencyBinCount = 1024

defaultMinDecibels :: MinDecibels
defaultMinDecibels = -100.0

defaultMaxDecibels :: MaxDecibels
defaultMaxDecibels = 10.0

defaultSmoothingTimeConstant :: SmoothingTimeConstant
defaultSmoothingTimeConstant = 0.2

defaultLoopStart :: LoopStart
defaultLoopStart = 0.0

defaultLoopEnd :: LoopEnd
defaultLoopEnd = 9999.9

defaultLoopOn :: LoopOn
defaultLoopOn = false

defaultNormalize :: Normalize
defaultNormalize = true

defualtCompressorThreshold :: Threshold
defualtCompressorThreshold = -24.0

defaultCompressorKnee :: Knee
defaultCompressorKnee = 30.0

defaultCompressorRatio :: Ratio
defaultCompressorRatio = 12.0

defaultCompressorReduction :: Reduction
defaultCompressorReduction = 0.0

defaultCompressorAttack :: Attack
defaultCompressorAttack = 0.003

defaultCompressorRelease :: Release
defaultCompressorRelease = 0.25

defaultCompressorPan :: Pan
defaultCompressorPan = 0.0

type SynthState = { synthNodes :: Map NodeId SynthNodeState
                  , audioContext :: WebAudio.AudioContext
                  }

type SynthNodeState_ = { audioNode :: WebAudio.AudioNode
                       , synthNodeType :: SynthNodeType
                       , synthNodeParams :: SynthNodeParams
                       }

newtype SynthNodeState = SynthNodeState SynthNodeState_
derive instance newtypeSynthNodeState :: Newtype SynthNodeState _

instance connectableSynthNodeState :: WebAudio.Connectable SynthNodeState where
  connect rawAudioNode      (SynthNodeState synthNodeState) = WebAudio.connect rawAudioNode synthNodeState.audioNode
  disconnect rawAudioNode   (SynthNodeState synthNodeState) = WebAudio.disconnect rawAudioNode synthNodeState.audioNode
  connectParam rawAudioNode (SynthNodeState synthNodeState) = WebAudio.connectParam rawAudioNode synthNodeState.audioNode

data SynthNodeType
  = NodeTypeOscillator
  | NodeTypeAmplifier
  | NodeTypeDelay
  | NodeTypeFilter
  | NodeTypeDestination
  | NodeTypeAnalyser
  --| NodeTypeAudioBufferSource
  --| NodeTypeConvolver
  --| NodeTypeDynamicsCompressor
  --| NodeTypeMediaElementAudioSource
  --| NodeTypeStereoPanner
derive instance eqSynthNodeType :: Eq SynthNodeType
derive instance ordSynthNodeType :: Ord SynthNodeType
instance showSynthNodeType :: Show SynthNodeType where
  show = case _ of
    NodeTypeOscillator -> "NodeTypeOscillator"
    NodeTypeAmplifier -> "NodeTypeAmplifier"
    NodeTypeDelay -> "NodeTypeDelay"
    NodeTypeFilter -> "NodeTypeFilter"
    NodeTypeDestination -> "NodeTypeDestination"
    NodeTypeAnalyser -> "NodeTypeAnalyser"

data SynthNodeParams
  = OscillatorParams Freq
  | AmplifierParams Gain
  | DelayParams PeriodSeconds
  | FilterParams WebAudio.BiquadFilterType Cutoff QFactor Gain
  | DestinationParams
  | AnalyserParams FrequencyBinCount MinDecibels MaxDecibels SmoothingTimeConstant Uint8Array (Ref Boolean)
  | AudioBufferSourceParams LoopStart LoopEnd LoopOn
  | ConvolverParams Normalize
  | DynamicsCompressorParams Threshold Knee Ratio Reduction Attack Release
  | MediaElementAudioSourceParams
  | StereoPannerParams Pan

type Freq = Number
type Gain = Number
type PeriodSeconds = Number
type Cutoff = Number
type QFactor = Number
type FrequencyBinCount = ByteLength
type SmoothingTimeConstant = Number
type MinDecibels = Number
type MaxDecibels = Number
type LoopStart = WebAudio.Seconds
type LoopEnd = WebAudio.Seconds
type LoopOn = Boolean
type Normalize = Boolean
type Threshold = Number
type Knee = Number
type Ratio = Number
type Reduction = Number
type Attack = Number
type Release = Number
type Pan = Number

_SynthNodeState :: Lens' SynthNodeState SynthNodeState_
_SynthNodeState = lens unwrap (\_ -> SynthNodeState)

_synthNodeParams :: Lens' SynthNodeState SynthNodeParams
_synthNodeParams = _SynthNodeState <<< prop (SProxy :: SProxy "synthNodeParams")

_gain :: Prism' SynthNodeParams Gain
_gain = prism' AmplifierParams
               (case _ of
                   AmplifierParams gain -> Just gain
                   _ -> Nothing
               )

_synthNodeGain :: forall p. Choice p => Strong p =>
                  Optic' p SynthNodeState Gain
_synthNodeGain = _synthNodeParams <<< _gain

_delayPeriod :: Prism' SynthNodeParams PeriodSeconds
_delayPeriod = prism' DelayParams
                      (case _ of
                          DelayParams period -> Just period
                          _ -> Nothing
                      )

_synthNodeDelayPeriod :: forall p. Choice p => Strong p =>
                         Optic' p SynthNodeState PeriodSeconds
_synthNodeDelayPeriod = _synthNodeParams <<< _delayPeriod

_audioNode :: Lens' SynthNodeState WebAudio.AudioNode
_audioNode = _SynthNodeState <<< prop (SProxy :: SProxy "audioNode")

_gainNode :: forall p. Choice p => Strong p =>
             Optic' p SynthNodeState WebAudio.GainNode
_gainNode =
  _audioNode <<< prism' WebAudio.Gain
                        (case _ of
                            WebAudio.Gain gainNode -> Just gainNode
                            _ -> Nothing
                        )

_delayNode :: forall p. Choice p => Strong p =>
              Optic' p SynthNodeState WebAudio.DelayNode
_delayNode =
  _audioNode <<< prism' WebAudio.Delay
                        (case _ of
                            WebAudio.Delay delayNode -> Just delayNode
                            _ -> Nothing
                        )

_analyserNode :: forall p. Choice p => Strong p =>
                 Optic' p SynthNodeState WebAudio.AnalyserNode
_analyserNode =
  _audioNode <<< prism' WebAudio.Analyser
                        (case _ of
                            WebAudio.Analyser analyserNode -> Just analyserNode
                            _ -> Nothing
                        )

-- | Compile an operation on the graph to an operation on the synthesiser
-- | that changes the synth context as an Effect and returns a function that
-- | changes the AppState.synthState
interpretSynth :: forall a. UIGraphOp a -> (SynthState -> Effect SynthState)
interpretSynth (UIGraphOp op) = interpretSynthM op where
  interpretSynthM = case _ of
    Pure _ -> \asdf -> do
      pure asdf

    -- If the node text is a valid synth node type, create the corresponding
    -- synth node
    Free (InsertNode node next) -> \synthState -> do
      log "interpretSynthM InsertNode"
      log $ node ^. _nodeText
      newState <- tryCreateSynthNode node synthState
      interpretSynthM next newState

    -- If the node text is a valid synth node type, delete the corresponding
    -- synth node
    Free (DeleteNode node next) -> \synthState -> do
      log $ "interpretSynthM DeleteNode " <> node ^. _nodeText
      newState <- deleteSynthNode node synthState
      interpretSynthM next newState

    -- If the node text is a valid synth node type, create the corresponding
    -- connection between synth nodes
    Free (InsertEdge edge next) -> \synthState -> do
      log "interpretSynth InsertEdge"
      case Tuple (Map.lookup (edge ^. _source) synthState.synthNodes)
                 (Map.lookup (edge ^. _target) synthState.synthNodes) of
        Tuple (Just sourceNodeState) (Just targetNodeState) -> do
          log "connecting two audio nodes"
          connectAudioNode sourceNodeState targetNodeState
        _ -> do
          log "interpretSynth InsertEdge case failed"
          pure unit
      interpretSynthM next synthState

    -- If the node text is a valid synth node type, delete the corresponding
    -- connection between synth nodes
    Free (DeleteEdge edge next) -> \synthState -> do
      case Tuple (Map.lookup (edge ^. _source) synthState.synthNodes)
                 (Map.lookup (edge ^. _target) synthState.synthNodes) of
        Tuple (Just sourceNodeState) (Just targetNodeState) -> do
          log $ "disconnecting " <> show (unwrap sourceNodeState).synthNodeType <> " from " <> show (unwrap targetNodeState).synthNodeType
          disconnectAudioNode sourceNodeState targetNodeState
        _ -> pure unit
      interpretSynthM next synthState

    -- Moving a node doesn't affect the synth
    Free (MoveNode node from to next) ->
      interpretSynthM next

    -- If the synth node text goes from invalid to valid, create the
    -- corresponding synth node.
    -- If the synth node text goes from valid to invalid, delete the
    -- corresponding synth node.
    Free (UpdateNodeText node from to next) -> \synthState -> do
      log $ "interpretSynthM UpdateNodeText from " <> from <> " to " <> to
      newState <- case Tuple (parseSynthNodeType from) (parseSynthNodeType to) of
        -- This update makes the node text a valid synth node
        Tuple Nothing (Just nodeType) -> do
          log "UpdateNodeText tryCreateSynthNode"
          createSynthNode node nodeType synthState
        -- This update takes the node text from a valid synth node type to a
        -- value that does not represent a synth node type
        Tuple (Just _) Nothing -> do
          log "UpdateNodeText tryDeleteSynthNode"
          deleteSynthNode node synthState
        _ -> pure synthState
      interpretSynthM next newState

    -- Edge text doesn't affect the synth
    Free (UpdateEdgeText edge from to next) ->
      interpretSynthM next

parseSynthNodeType :: String -> Maybe SynthNodeType
parseSynthNodeType = case _ of
  "oscillator" -> Just NodeTypeOscillator
  "gain" -> Just NodeTypeAmplifier
  "delay" -> Just NodeTypeDelay
  "filter" -> Just NodeTypeFilter
  "output" -> Just NodeTypeDestination
  "analyser" -> Just NodeTypeAnalyser
  --"analyser" -> Just NodeTypeAnalyser
  --"audio-buffer" -> Just NodeTypeAudioBufferSource
  --"convolver" -> Just NodeTypeConvolver
  --"compressor" -> Just NodeTypeDynamicsCompressor
  --"media-element" -> Just NodeTypeMediaElementAudioSource
  --"panner" -> Just NodeTypeStereoPanner
  _ -> Nothing

freshSynthNode :: SynthNodeType -> WebAudio.AudioContext -> Effect SynthNodeState
freshSynthNode synthNodeType audioContext = case synthNodeType of
  NodeTypeOscillator -> do
    log "creating oscillator"
    oscillatorNode <- WebAudio.createOscillator audioContext
    WebAudio.setFrequency defaultOscillatorFreq oscillatorNode
    WebAudio.setOscillatorType defaultOscillatorType oscillatorNode
    WebAudio.startOscillator 0.0 oscillatorNode
    pure $ SynthNodeState { audioNode :       WebAudio.Oscillator oscillatorNode
                          , synthNodeType :   NodeTypeOscillator
                          , synthNodeParams : OscillatorParams defaultOscillatorFreq
                          }

  NodeTypeAmplifier -> do
    amplifierNode <- WebAudio.createGain audioContext
    WebAudio.setGain defaultAmplifierGain amplifierNode
    pure $ SynthNodeState { audioNode :       WebAudio.Gain amplifierNode
                          , synthNodeType :   NodeTypeAmplifier
                          , synthNodeParams : AmplifierParams defaultAmplifierGain
                          }

  NodeTypeDelay -> do
    delayNode <- WebAudio.createDelay audioContext
    setTargetValue defaultDelayPeriod audioContext =<< WebAudio.delayTime delayNode
    pure $ SynthNodeState { audioNode :       WebAudio.Delay delayNode
                          , synthNodeType :   NodeTypeDelay
                          , synthNodeParams : DelayParams defaultDelayPeriod
                          }

  NodeTypeFilter -> do
    filterNode <- WebAudio.createBiquadFilter audioContext
    WebAudio.setFilterType defaultFilterType filterNode
    setTargetValue defaultFilterCutoff  audioContext =<< WebAudio.filterFrequency filterNode
    setTargetValue defaultFilterQFactor audioContext =<< WebAudio.quality filterNode
    setTargetValue defaultFilterGain    audioContext =<< WebAudio.gain filterNode
    pure $ SynthNodeState { audioNode :       WebAudio.BiquadFilter filterNode
                          , synthNodeType :   NodeTypeFilter
                          , synthNodeParams : FilterParams defaultFilterType defaultFilterCutoff defaultFilterQFactor defaultFilterGain
                          }

  NodeTypeDestination -> do
    log "creating destination"
    destinationNode <- WebAudio.destination audioContext
    pure $ SynthNodeState { audioNode :       WebAudio.Destination destinationNode
                          , synthNodeType :   NodeTypeDestination
                          , synthNodeParams : DestinationParams
                          }

  NodeTypeAnalyser -> do
    log "creating analyser"
    analyserNode <- WebAudio.createAnalyser audioContext
    WebAudio.setSmoothingTimeConstant defaultSmoothingTimeConstant analyserNode
    WebAudio.setMaxDecibels defaultMaxDecibels analyserNode
    spectrumBuffer <- empty defaultFrequencyBinCount
    drawLoopStopSignal <- Ref.new false
    pure $ SynthNodeState { audioNode : WebAudio.Analyser analyserNode
                          , synthNodeType : NodeTypeFilter
                          , synthNodeParams : AnalyserParams
                                              defaultFrequencyBinCount
                                              defaultMinDecibels
                                              defaultMaxDecibels
                                              defaultSmoothingTimeConstant
                                              spectrumBuffer
                                              drawLoopStopSignal
                          }

  --NodeTypeAudioBufferSource ->
  --NodeTypeConvolver ->
  --NodeTypeDynamicsCompressor ->
  --NodeTypeMediaElementAudioSource ->
  --NodeTypeStereoPanner ->

connectAudioNode :: SynthNodeState -> SynthNodeState -> Effect Unit
connectAudioNode (SynthNodeState source) target = case source.audioNode of
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

disconnectAudioNode :: SynthNodeState -> SynthNodeState -> Effect Unit
disconnectAudioNode (SynthNodeState source) target = case source.audioNode of
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
                            -> SynthNodeState -> UINode -> SynthState -> Effect Unit
mapParentChildSynthNodes connectOrDisconnect synthNodeState node synthState = do
  let parentSynthNodes = Map.keys (node ^. _parents)
                         # Array.fromFoldable
                         # Array.mapMaybe (\parentId -> Map.lookup parentId synthState.synthNodes)
  for_ parentSynthNodes \parentSynthNode ->
                          connectOrDisconnect parentSynthNode synthNodeState
  let childSynthNodes = Map.keys (node ^. _children)
                        # Array.fromFoldable
                        # Array.mapMaybe (\childId -> Map.lookup childId synthState.synthNodes)
  for_ childSynthNodes \childSynthNode ->
                         connectOrDisconnect synthNodeState childSynthNode

connectParentChildSynthNodes :: SynthNodeState -> UINode -> SynthState -> Effect Unit
connectParentChildSynthNodes = mapParentChildSynthNodes connectAudioNode

disconnectParentChildSynthNodes :: SynthNodeState -> UINode -> SynthState -> Effect Unit
disconnectParentChildSynthNodes = mapParentChildSynthNodes disconnectAudioNode

tryCreateSynthNode :: UINode -> SynthState -> Effect SynthState
tryCreateSynthNode node synthState = do
  case parseSynthNodeType (node ^. _nodeText) of
    Nothing -> pure synthState
    Just nodeType -> do
      createSynthNode node nodeType synthState

createSynthNode :: UINode -> SynthNodeType -> SynthState -> Effect SynthState
createSynthNode node nodeType synthState = do
  synthNodeState <- freshSynthNode nodeType synthState.audioContext
  connectParentChildSynthNodes synthNodeState node synthState
  let updatedNodes = Map.insert (node ^. _nodeId) synthNodeState synthState.synthNodes
  pure $ synthState { synthNodes = updatedNodes }

deleteSynthNode :: UINode -> SynthState -> Effect SynthState
deleteSynthNode node synthState = do
  case Map.lookup (node ^. _nodeId) synthState.synthNodes of
    Nothing -> pure synthState
    Just (SynthNodeState synthNodeState) -> do
      disconnectParentChildSynthNodes (SynthNodeState synthNodeState) node synthState
      case synthNodeState.audioNode of
        WebAudio.Oscillator oscillatorNode -> WebAudio.stopOscillator 0.0 oscillatorNode
        WebAudio.AudioBufferSource audioBufferSource -> WebAudio.stopBufferSource 0.0 audioBufferSource
        _ -> pure unit
      let updatedNodes = Map.delete (node ^. _nodeId) synthState.synthNodes
      pure $ synthState { synthNodes = updatedNodes }

setTargetValue :: WebAudio.Value -> WebAudio.AudioContext -> WebAudio.AudioParam -> Effect Unit
setTargetValue value audioContext param = do
  now <- WebAudio.currentTime audioContext
  _ <- WebAudio.linearRampToValueAtTime value (now + updateValueRampTime) param
  pure unit
