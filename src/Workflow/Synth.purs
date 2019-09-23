module Workflow.Synth where

import Prelude

import Audio.WebAudio.AnalyserNode (setSmoothingTimeConstant, setMaxDecibels, setMinDecibels) as WebAudio
import Audio.WebAudio.AudioParam (linearRampToValueAtTime) as WebAudio
import Audio.WebAudio.BaseAudioContext (createOscillator, createGain, createDelay, createBiquadFilter, createAnalyser, destination, currentTime) as WebAudio
import Audio.WebAudio.BiquadFilterNode (BiquadFilterType(..), setFilterType, filterFrequency, quality, gain) as WebAudioBiquad
import Audio.WebAudio.DelayNode (delayTime) as WebAudioDelay
import Audio.WebAudio.GainNode (gain, setGain) as WebAudioGain
import Audio.WebAudio.Oscillator (OscillatorType(..), frequency, setFrequency, setOscillatorType, startOscillator) as WebAudioOsc
import Audio.WebAudio.Types (AudioContext, AudioNode(..), Seconds, AudioParam, Value, GainNode, DelayNode, AnalyserNode, BiquadFilterNode, OscillatorNode, DestinationNode, connect) as WebAudio
import Data.ArrayBuffer.Typed (empty)
import Data.ArrayBuffer.Types (Uint8Array, ByteLength)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens', lens, traversed, Prism', Optic', prism', (^?), (.~))
import Data.Lens.At (at)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Strong (class Strong)
import Data.Symbol (SProxy(..))
import Data.UUID as UUID
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Foreign.Class (class Encode, class Decode)
import Foreign.Generic (defaultOptions, genericEncode, genericDecode)
import Foreign.Object (Object)
import Workflow.Core (NodeId, toForeignMap, fromForeignMap, parseUUIDEither, toExceptT)

updateValueRampTime :: WebAudio.Seconds
updateValueRampTime = 0.1

defaultOscillatorFreq :: Freq
defaultOscillatorFreq = 220.0

defaultOscillatorType :: WebAudioOsc.OscillatorType
defaultOscillatorType = WebAudioOsc.Sine

defaultAmplifierGain :: Gain
defaultAmplifierGain = 0.5

defaultDelayPeriod :: PeriodSeconds
defaultDelayPeriod = 0.3

defaultFilterType :: WebAudioBiquad.BiquadFilterType
defaultFilterType = WebAudioBiquad.Lowpass

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

type Synth = { synthState :: SynthState
             , synthParams :: SynthParams
             }

type SynthState = { audioContext :: WebAudio.AudioContext
                  , synthNodeStates :: Map NodeId SynthNodeState
                  }

type FilterState_ = { analyserNode :: WebAudio.AnalyserNode
                    , lowpassFilterNode :: WebAudio.BiquadFilterNode
                    , highpassFilterNode :: WebAudio.BiquadFilterNode
                    , analyserBuffer :: Uint8Array
                    , drawLoopStopSignal :: Ref Boolean
                    }

data SynthNodeState
  = OscillatorState WebAudio.OscillatorNode
  | AmplifierState WebAudio.GainNode
  | DelayState WebAudio.DelayNode
  | FilterState FilterState_
  | DestinationState WebAudio.DestinationNode

instance showSynthNodeState :: Show SynthNodeState where
  show = case _ of
    OscillatorState _ -> "Oscillator"
    AmplifierState _ -> "Amplifier"
    DelayState _ -> "Delay"
    FilterState _ -> "Filter"
    DestinationState _ -> "Destination"

inputPort :: SynthNodeState -> WebAudio.AudioNode
inputPort = case _ of
  OscillatorState oscNode          -> WebAudio.Oscillator oscNode
  AmplifierState gainNode          -> WebAudio.Gain gainNode
  DelayState delayNode             -> WebAudio.Delay delayNode
  FilterState filterState          -> WebAudio.Analyser filterState.analyserNode
  DestinationState destinationNode -> WebAudio.Destination destinationNode

outputPort :: SynthNodeState -> WebAudio.AudioNode
outputPort = case _ of
  OscillatorState oscNode          -> WebAudio.Oscillator oscNode
  AmplifierState gainNode          -> WebAudio.Gain gainNode
  DelayState delayNode             -> WebAudio.Delay delayNode
  FilterState filterState          -> WebAudio.BiquadFilter filterState.highpassFilterNode
  DestinationState destinationNode -> WebAudio.Destination destinationNode

newtype SynthParams = SynthParams (Map NodeId SynthNodeParams)

derive instance newtypeSynthParams :: Newtype SynthParams _

instance showSynthParams :: Show SynthParams where
  show (SynthParams synthParams) = "SynthParams: " <> show synthParams

type FilterParams_ = { analyserParams :: AnalyserParams
                     , lowpassParams :: BiquadFilterParams
                     , highpassParams :: BiquadFilterParams
                     }

data SynthNodeParams
  = OscillatorParams Freq
  | AmplifierParams Gain
  | DelayParams PeriodSeconds
  | FilterParams FilterParams_
  | DestinationParams
  -- Not implemented:
  --| DynamicsCompressorParams Threshold Knee Ratio Reduction Attack Release
  --| AudioBufferSourceParams LoopStart LoopEnd LoopOn
  --| ConvolverParams Normalize
  --| MediaElementAudioSourceParams
  --| StereoPannerParams Pan

instance showSynthNodeParams :: Show SynthNodeParams where
  show = case _ of
    OscillatorParams freq -> "OscillatorParams " <> show freq
    AmplifierParams gain -> "AmplifierParams " <> show gain
    DelayParams periodSeconds -> "DelayParams " <> show periodSeconds
    FilterParams filterParams -> "FilterParams " <> show filterParams
    DestinationParams -> "DestinationParams"

data BiquadFilterParams = BiquadFilterParams WebAudioBiquad.BiquadFilterType Cutoff QFactor Gain
instance showBiquadFilterParams :: Show BiquadFilterParams where
  show (BiquadFilterParams filterType cutoff q gain) =
    "BiquadFilterParams " <> show filterType <> " " <> show cutoff <> " " <> show q <> " " <> show gain

data AnalyserParams = AnalyserParams FrequencyBinCount MinDecibels MaxDecibels SmoothingTimeConstant
instance showAnalyserParams :: Show AnalyserParams where
  show (AnalyserParams freqBinCount minDecibels maxDecibels smoothingTimeConstant) =
    "AnalyserParams " <> show freqBinCount <> " " <> show minDecibels <> " " <> show maxDecibels <> " " <> show smoothingTimeConstant

data SynthNodeType
  = NodeTypeOscillator
  | NodeTypeAmplifier
  | NodeTypeDelay
  | NodeTypeFilter
  | NodeTypeDestination

derive instance eqSynthNodeType :: Eq SynthNodeType
derive instance ordSynthNodeType :: Ord SynthNodeType
instance showSynthNodeType :: Show SynthNodeType where
  show = case _ of
    NodeTypeOscillator -> "Oscillator"
    NodeTypeAmplifier -> "Amplifier"
    NodeTypeDelay -> "Delay"
    NodeTypeFilter -> "Filter"
    NodeTypeDestination -> "Destination"

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

-- | Just the parameters which are directly updated via UI interactions
data SynthParameter
  = OscillatorFreq
  | AmplifierGain
  | DelayPeriod
  | FilterLowpassCutoff
  | FilterHighpassCutoff

derive instance eqSynthParameter :: Eq SynthParameter

instance showSynthParameter :: Show SynthParameter where
  show = case _ of
    OscillatorFreq -> "OscillatorFreq"
    AmplifierGain -> "AmplifierGain"
    DelayPeriod -> "DelayPeriod"
    FilterLowpassCutoff -> "FilterLowpassCutoff"
    FilterHighpassCutoff -> "FilterHighpassCutoff"

updateParam :: Synth -> NodeId -> SynthParameter -> WebAudio.Value -> Effect Synth
updateParam synth nodeId synthParameter newValue =
  case Map.lookup nodeId synth.synthState.synthNodeStates of
    Nothing -> pure synth
    Just synthNodeState ->
      case synthParameter of
        OscillatorFreq -> case synthNodeState ^? _oscNode of
          Nothing -> pure synth
          Just oscNode -> do
            freqParam <- WebAudioOsc.frequency oscNode
            setTargetValue newValue synth.synthState.audioContext freqParam
            pure $ synth # _synthNodeParams nodeId <<< traversed <<< _oscFreq .~ newValue

        AmplifierGain -> case synthNodeState ^? _gainNode of
          Nothing -> pure synth
          Just gainNode -> do
            gainParam <- WebAudioGain.gain gainNode
            setTargetValue newValue synth.synthState.audioContext gainParam
            pure $ synth # _synthNodeParams nodeId <<< traversed <<< _amplifierGain .~ newValue

        DelayPeriod -> case synthNodeState ^? _delayNode of
          Nothing -> pure synth
          Just delayNode -> do
            delayParam <- WebAudioDelay.delayTime delayNode
            setTargetValue newValue synth.synthState.audioContext delayParam
            pure $ synth # _synthNodeParams nodeId <<< traversed <<< _delayPeriod .~ newValue

        FilterLowpassCutoff -> case synthNodeState ^? _lowpassNode of
          Nothing -> pure synth
          Just lowpassNode -> do
            lowpassCutoffParam <- WebAudioBiquad.filterFrequency lowpassNode
            setTargetValue newValue synth.synthState.audioContext lowpassCutoffParam
            pure $ synth # _synthNodeParams nodeId <<< traversed <<< _lowpassCutoff .~ newValue

        FilterHighpassCutoff -> case synthNodeState ^? _highpassNode of
          Nothing -> pure synth
          Just highpassNode -> do
            highpassCutoffParam <- WebAudioBiquad.filterFrequency highpassNode
            setTargetValue newValue synth.synthState.audioContext highpassCutoffParam
            pure $ synth # _synthNodeParams nodeId <<< traversed <<< _highpassCutoff .~ newValue

_SynthParams :: Lens' SynthParams (Map NodeId SynthNodeParams)
_SynthParams = lens (\(SynthParams synthParams) -> synthParams) (\_ -> SynthParams)

_synthNodeParams :: NodeId -> Lens' Synth (Maybe SynthNodeParams)
_synthNodeParams nodeId = prop (SProxy :: SProxy "synthParams") <<< _SynthParams <<< at nodeId

_oscNode :: Prism' SynthNodeState WebAudio.OscillatorNode
_oscNode = prism'
           OscillatorState
           (case _ of
               OscillatorState oscNode -> Just oscNode
               _ -> Nothing
           )

_oscFreq :: Prism' SynthNodeParams Freq
_oscFreq = prism'
           OscillatorParams
           (case _ of
               OscillatorParams freq -> Just freq
               _ -> Nothing)

_gainNode :: Prism' SynthNodeState WebAudio.GainNode
_gainNode = prism'
            AmplifierState
            (case _ of
                AmplifierState gainNode -> Just gainNode
                _ -> Nothing)

_amplifierGain :: Prism' SynthNodeParams Gain
_amplifierGain = prism'
                 AmplifierParams
                 (case _ of
                     AmplifierParams gain -> Just gain
                     _ -> Nothing)

_delayNode :: Prism' SynthNodeState WebAudio.DelayNode
_delayNode = prism'
             DelayState
             (case _ of
                 DelayState delayNode -> Just delayNode
                 _ -> Nothing
             )

_delayPeriod :: Prism' SynthNodeParams PeriodSeconds
_delayPeriod = prism' DelayParams
               (case _ of
                   DelayParams period -> Just period
                   _ -> Nothing)

_FilterState :: Prism' SynthNodeState FilterState_
_FilterState = prism' FilterState (case _ of
                                      FilterState filterState_ -> Just filterState_
                                      _ -> Nothing)

_lowpassNode :: forall p. Choice p => Strong p =>
                Optic' p SynthNodeState WebAudio.BiquadFilterNode
_lowpassNode = _FilterState <<< prop (SProxy :: SProxy "lowpassFilterNode")

_filterParams :: Prism' SynthNodeParams FilterParams_
_filterParams = prism'
                FilterParams
                (case _ of
                    FilterParams filterParams -> Just filterParams
                    _ -> Nothing)

_biquadFilterCutoff :: Lens' BiquadFilterParams Cutoff
_biquadFilterCutoff = lens
                      (\(BiquadFilterParams filterType cutoff q gain) -> gain)
                      (\(BiquadFilterParams filterType _ q gain) newCutoff ->
                        BiquadFilterParams filterType newCutoff q gain)

_lowpassCutoff :: forall p. Choice p => Strong p =>
                  Optic' p SynthNodeParams Gain
_lowpassCutoff = _filterParams <<< prop (SProxy :: SProxy "lowpassParams") <<< _biquadFilterCutoff

_highpassNode :: forall p. Choice p => Strong p =>
                Optic' p SynthNodeState WebAudio.BiquadFilterNode
_highpassNode = _FilterState <<< prop (SProxy :: SProxy "highpassFilterNode")

_highpassCutoff :: forall p. Choice p => Strong p =>
                  Optic' p SynthNodeParams Gain
_highpassCutoff = _filterParams <<< prop (SProxy :: SProxy "highpassParams") <<< _biquadFilterCutoff

parseSynthNodeType :: String -> Maybe SynthNodeType
parseSynthNodeType = case _ of
  "oscillator" -> Just NodeTypeOscillator
  "gain"       -> Just NodeTypeAmplifier
  "delay"      -> Just NodeTypeDelay
  "filter"     -> Just NodeTypeFilter
  "output"     -> Just NodeTypeDestination
  -- Not implemented:
  --"compressor" -> Just NodeTypeDynamicsCompressor
  --"media-element" -> Just NodeTypeMediaElementAudioSource
  --"audio-buffer" -> Just NodeTypeAudioBufferSource
  --"convolver" -> Just NodeTypeConvolver
  --"panner" -> Just NodeTypeStereoPanner
  --"analyser" -> Just NodeTypeAnalyser
  --"listener"
  --"waveshaper"
  _ -> Nothing

freshSynthNodeParams :: SynthNodeType -> SynthNodeParams
freshSynthNodeParams =
  let
    biquadFilterParams = BiquadFilterParams defaultFilterType defaultFilterCutoff defaultFilterQFactor defaultFilterGain
    analyserParams = AnalyserParams defaultFrequencyBinCount defaultMinDecibels defaultMaxDecibels defaultSmoothingTimeConstant
  in case _ of
    NodeTypeOscillator -> OscillatorParams defaultOscillatorFreq
    NodeTypeAmplifier -> AmplifierParams defaultAmplifierGain
    NodeTypeDelay -> DelayParams defaultDelayPeriod
    NodeTypeFilter -> FilterParams { lowpassParams : biquadFilterParams
                                   , highpassParams : biquadFilterParams
                                   , analyserParams : analyserParams
                                   }
    NodeTypeDestination -> DestinationParams

newSynthNodeState :: SynthNodeParams -> WebAudio.AudioContext -> Effect SynthNodeState
newSynthNodeState synthNodeType audioContext =
  let
    newAnalyserState analyserParams =
      let
        AnalyserParams frequencyBinCount minDecibels maxDecibels smoothingTimeConstant = analyserParams
      in do
        log "creating analyser"
        analyserNode <- WebAudio.createAnalyser audioContext
        WebAudio.setSmoothingTimeConstant smoothingTimeConstant analyserNode
        WebAudio.setMinDecibels minDecibels analyserNode
        WebAudio.setMaxDecibels maxDecibels analyserNode
        spectrumBuffer <- empty frequencyBinCount
        drawLoopStopSignal <- Ref.new false
        pure { analyserNode : analyserNode
             , spectrumBuffer : spectrumBuffer
             , drawLoopStopSignal : drawLoopStopSignal
             }

    newFilterAudioNodeState filterParams =
      let
        BiquadFilterParams filterType filterCutoff filterQFactor filterGain = filterParams
      in do
        filterAudioNode <- WebAudio.createBiquadFilter audioContext
        WebAudioBiquad.setFilterType filterType filterAudioNode
        setTargetValue filterCutoff  audioContext =<< WebAudioBiquad.filterFrequency filterAudioNode
        setTargetValue filterQFactor audioContext =<< WebAudioBiquad.quality filterAudioNode
        setTargetValue filterGain    audioContext =<< WebAudioBiquad.gain filterAudioNode
        pure filterAudioNode

  in case synthNodeType of
    OscillatorParams freq -> do
      log "creating oscillator"
      oscillatorNode <- WebAudio.createOscillator audioContext
      WebAudioOsc.setFrequency defaultOscillatorFreq oscillatorNode
      WebAudioOsc.setOscillatorType defaultOscillatorType oscillatorNode
      WebAudioOsc.startOscillator 0.0 oscillatorNode
      pure $ OscillatorState oscillatorNode

    AmplifierParams gain -> do
      amplifierNode <- WebAudio.createGain audioContext
      WebAudioGain.setGain defaultAmplifierGain amplifierNode
      pure $ AmplifierState amplifierNode

    DelayParams periodSeconds -> do
      delayNode <- WebAudio.createDelay audioContext
      setTargetValue defaultDelayPeriod audioContext =<< WebAudioDelay.delayTime delayNode
      pure $ DelayState delayNode

    FilterParams filterParams -> do
      lowpassFilter  <- newFilterAudioNodeState filterParams.lowpassParams
      highpassFilter <- newFilterAudioNodeState filterParams.highpassParams
      analyserState <- newAnalyserState filterParams.analyserParams
      WebAudio.connect analyserState.analyserNode lowpassFilter
      WebAudio.connect lowpassFilter highpassFilter
      pure $ FilterState { lowpassFilterNode : lowpassFilter
                         , highpassFilterNode : highpassFilter
                         , analyserNode : analyserState.analyserNode
                         , analyserBuffer : analyserState.spectrumBuffer
                         , drawLoopStopSignal : analyserState.drawLoopStopSignal
                         }

    DestinationParams -> do
      log "creating destination"
      destinationNode <- WebAudio.destination audioContext
      pure $ DestinationState destinationNode

  -- Not implemented:
  --NodeTypeDynamicsCompressor ->
  --NodeTypeAudioBufferSource ->
  --NodeTypeConvolver ->
  --NodeTypeMediaElementAudioSource ->
  --NodeTypeStereoPanner ->

setTargetValue :: WebAudio.Value -> WebAudio.AudioContext -> WebAudio.AudioParam -> Effect Unit
setTargetValue value audioContext param = do
  now <- WebAudio.currentTime audioContext
  _ <- WebAudio.linearRampToValueAtTime value (now + updateValueRampTime) param
  pure unit


------
-- Serialisation/deserialisation

derive instance genericSynthParams :: Generic SynthParams _

newtype ForeignSynthParams = ForeignSynthParams (Object SynthNodeParams)
derive instance newtypeForeignSynthParams :: Newtype ForeignSynthParams _
derive instance genericForeignSynthParams :: Generic ForeignSynthParams _
instance encodeForeignSynthParams :: Encode ForeignSynthParams where
  encode x = x # genericEncode defaultOptions
instance decodeForeignSynthParams :: Decode ForeignSynthParams where
  decode x = x # genericDecode defaultOptions

toForeignSynthParams :: SynthParams -> ForeignSynthParams
toForeignSynthParams =
  unwrap >>> toForeignMap identity UUID.toString >>> wrap

fromForeignSynthParams :: ForeignSynthParams -> Either String SynthParams
fromForeignSynthParams =
  unwrap >>> fromForeignMap pure parseUUIDEither >>> map wrap

instance encodeSynthParams :: Encode SynthParams where
  encode x = x # toForeignSynthParams >>> genericEncode defaultOptions
instance decodeSynthParams :: Decode SynthParams where
  decode x = x # genericDecode defaultOptions >>= fromForeignSynthParams >>> toExceptT

derive instance genericSynthNodeParams :: Generic SynthNodeParams _
instance encodeSynthNodeParams :: Encode SynthNodeParams where
  encode x = x # genericEncode defaultOptions
instance decodeSynthNodeParams :: Decode SynthNodeParams where
  decode x = x # genericDecode defaultOptions

derive instance genericBiquadFilterParams :: Generic BiquadFilterParams _
instance encodeBiquadFilterParams :: Encode BiquadFilterParams where
  encode x = x # genericEncode defaultOptions
instance decodeBiquadFilterParams :: Decode BiquadFilterParams where
  decode x = x # genericDecode defaultOptions

derive instance genericAnalyserParams :: Generic AnalyserParams _
instance encodeAnalyserParams :: Encode AnalyserParams where
  encode x = x # genericEncode defaultOptions
instance decodeAnalyserParams :: Decode AnalyserParams where
  decode x = x # genericDecode defaultOptions

derive instance genericSynthParameter :: Generic SynthParameter _
instance encodeSynthParameter' :: Encode SynthParameter where
  encode x = x # genericEncode defaultOptions
instance decodeSynthParameter' :: Decode SynthParameter where
  decode x = x # genericDecode defaultOptions
