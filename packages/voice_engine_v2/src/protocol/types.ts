// SPDX-License-Identifier: AGPL-3.0-or-later

export type VoiceEngineV2ImplementationKind = 'js' | 'native';

export type VoiceEngineV2ConnectionStatus =
	| 'idle'
	| 'connecting'
	| 'connected'
	| 'disconnecting'
	| 'reconnecting'
	| 'failed';

export type VoiceEngineV2MediaStatus = 'idle' | 'publishing' | 'published' | 'unpublishing' | 'failed';

export type VoiceEngineV2OperationId = number;

export type VoiceEngineV2DisconnectReason = 'user' | 'server' | 'network' | 'replaced' | 'shutdown';

export type VoiceEngineV2LifecycleReason =
	| 'rendererDisposed'
	| 'windowClosed'
	| 'appShutdown'
	| 'sessionReplaced'
	| 'logout'
	| 'test';

export type VoiceEngineV2TrackSource = 'microphone' | 'camera' | 'screen' | 'screenAudio' | 'unknown';

export type VoiceEngineV2TrackKind = 'audio' | 'video';

export type VoiceEngineV2ConnectionQuality = 'excellent' | 'good' | 'poor' | 'lost';

export type VoiceEngineV2RemoteTrackQuality = 'low' | 'medium' | 'high';

export type VoiceEngineV2VideoCodec = '' | 'vp8' | 'vp9' | 'h264' | 'h265' | 'av1';

export type VoiceEngineV2ScreenPacing = 'sender' | 'source';
export type VoiceEngineV2CameraBackgroundMode = 'none' | 'non' | 'blur' | 'custom';
export type VoiceEngineV2CameraBackgroundCustomMediaKind = 'static' | 'animated' | 'video';

export type VoiceEngineV2ResourceKey =
	| 'implementation'
	| 'connection'
	| 'gateway'
	| 'livekit'
	| 'microphone'
	| 'camera'
	| 'screen'
	| 'screenAudio'
	| 'audioControls'
	| 'outputDevice'
	| 'participantVolume'
	| 'remoteTrackSubscription'
	| 'dataChannel'
	| 'stats'
	| 'capabilities'
	| 'devices'
	| 'permissions'
	| 'nativeCapture'
	| 'nativeAudioTap'
	| 'nativeFrameSink'
	| 'e2ee'
	| 'storage'
	| 'telemetry'
	| 'ui'
	| 'timer'
	| 'diagnostics'
	| 'lifecycle';

export interface VoiceEngineV2Error {
	code:
		| 'unsupportedCapability'
		| 'notConnected'
		| 'invalidState'
		| 'invalidArgument'
		| 'staleOperation'
		| 'permissionDenied'
		| 'deviceUnavailable'
		| 'gatewayError'
		| 'liveKitError'
		| 'nativeCaptureError'
		| 'cancelled'
		| 'timeout'
		| 'implementationError';
	message: string;
	capability?: keyof VoiceEngineV2Capabilities | string;
	cause?: unknown;
}

export interface VoiceEngineV2Capabilities {
	connect: boolean;
	microphone: boolean;
	camera: boolean;
	screen: boolean;
	screenAudio: boolean;
	outputDevice: boolean;
	participantVolume: boolean;
	remoteTrackSubscription: boolean;
	dataChannel: boolean;
	stats: boolean;
	nativeVideoFrames: boolean;
	hardwareEncoding: boolean;
	zeroCopyScreenTransport: boolean;
	nativeAudioTaps: boolean;
}

export interface VoiceEngineV2HardwareEncoderCapabilities {
	available: boolean;
	backend: 'none' | 'videotoolbox' | 'nvenc' | 'qsv' | 'amf' | string;
	compiled?: boolean;
	runtime?: boolean;
	codecs: Array<VoiceEngineV2VideoCodec | string>;
	zeroCopy: boolean;
	nativeInputs: Array<'dmabuf' | 'd3d11Texture' | 'cvPixelBuffer' | 'sharedTexture' | string>;
	reason?: string;
	detail?: string;
}

export interface VoiceEngineV2ConnectOptions {
	url: string;
	token: string;
	e2eeKey?: ArrayBuffer | null;
	metadata?: Record<string, string>;
}

export interface VoiceEngineV2GatewayVoiceState {
	guildId: string | null;
	channelId: string | null;
	userId: string;
	sessionId: string | null;
	selfMute: boolean;
	selfDeaf: boolean;
	selfVideo: boolean;
	selfStream: boolean;
	suppress: boolean;
	requestToSpeakTimestamp: string | null;
}

export interface VoiceEngineV2GatewayVoiceServer {
	guildId: string | null;
	endpoint: string | null;
	token: string;
}

export interface VoiceEngineV2GatewayVoiceStateWrite {
	guildId: string | null;
	channelId: string | null;
	selfMute: boolean;
	selfDeaf: boolean;
	selfVideo?: boolean;
	selfStream?: boolean;
}

export interface VoiceEngineV2GatewayDesiredVoiceState {
	guildId: string | null;
	channelId: string | null;
	selfMute: boolean;
	selfDeaf: boolean;
	selfVideo: boolean;
	selfStream: boolean;
}

export type VoiceEngineV2LiveKitConnectionState =
	| 'disconnected'
	| 'connecting'
	| 'connected'
	| 'reconnecting'
	| 'failed';

export interface VoiceEngineV2LiveKitRoomState {
	connectionState: VoiceEngineV2LiveKitConnectionState;
	roomSid: string | null;
	roomName: string | null;
	serverRegion: string | null;
}

export interface VoiceEngineV2MicrophoneOptions {
	deviceId?: string;
	echoCancellation?: boolean;
	noiseSuppression?: boolean;
	autoGainControl?: boolean;
	deepFilter?: boolean;
	deepFilterNoiseReductionLevel?: number;
	maxBitrateBps?: number;
}

export interface VoiceEngineV2CameraOptions {
	deviceId?: string;
	width?: number;
	height?: number;
	frameRate?: number;
	codec?: VoiceEngineV2VideoCodec;
	maxBitrateBps?: number;
	mirror?: boolean;
	backgroundMode?: VoiceEngineV2CameraBackgroundMode;
	backgroundBlurStrength?: number;
	backgroundCustomMediaPath?: string;
	backgroundCustomMediaKind?: VoiceEngineV2CameraBackgroundCustomMediaKind;
	sendUpdate?: boolean;
}

export interface VoiceEngineV2CameraEncodingOptions {
	deviceId?: string;
	width?: number;
	height?: number;
	frameRate?: number;
	codec?: VoiceEngineV2VideoCodec;
	maxBitrateBps?: number;
	mirror?: boolean;
	backgroundMode?: VoiceEngineV2CameraBackgroundMode;
	backgroundBlurStrength?: number;
	backgroundCustomMediaPath?: string;
	backgroundCustomMediaKind?: VoiceEngineV2CameraBackgroundCustomMediaKind;
}

export interface VoiceEngineV2ScreenOptions {
	captureId: string;
	width: number;
	height: number;
	codec?: VoiceEngineV2VideoCodec;
	hardwareEncoding?: boolean;
	zeroCopyRequired?: boolean;
	maxBitrateBps?: number;
	maxFramerate?: number;
	adaptiveSend?: boolean;
	minVideoFps?: number;
	maxAudioBufferMs?: number;
	pacing?: VoiceEngineV2ScreenPacing;
}

export interface VoiceEngineV2ScreenEncodingOptions {
	captureId: string;
	width: number;
	height: number;
	frameRate?: number;
	maxBitrateBps?: number;
	codec?: VoiceEngineV2VideoCodec;
	hardwareEncoding?: boolean;
	zeroCopyRequired?: boolean;
}

export interface VoiceEngineV2ScreenAudioOptions {
	sampleRate: number;
	numChannels: number;
	route?: 'browser' | 'native';
	captureId?: string;
	tapId?: string;
}

export type VoiceEngineV2NativeCaptureKind = 'screen' | 'window' | 'game' | 'camera';

export interface VoiceEngineV2NativeCaptureSource {
	id: string;
	kind: VoiceEngineV2NativeCaptureKind;
	title: string;
	appName?: string;
	width?: number;
	height?: number;
	displayId?: string;
	windowId?: string;
	processId?: number;
}

export interface VoiceEngineV2NativeCaptureOptions {
	captureId: string;
	source: VoiceEngineV2NativeCaptureSource;
	width: number;
	height: number;
	frameRate: number;
	includeCursor: boolean;
	includeAudio: boolean;
	zeroCopyRequired: true;
}

export interface VoiceEngineV2NativeCaptureFrame {
	captureId: string;
	frameId: string;
	width: number;
	height: number;
	timestampMs: number;
	format: 'i420' | 'nv12' | 'rgba' | 'bgra' | 'native';
	zeroCopy: true;
	handle?: string;
	byteLength?: number;
}

export interface VoiceEngineV2NativeAudioTapOptions {
	tapId: string;
	source: 'system' | 'application' | 'window';
	sourceId?: string;
	sampleRate: number;
	numChannels: number;
}

export interface VoiceEngineV2NativeFrameSinkOptions {
	sinkId: string;
	captureId: string;
	trackSid?: string;
	zeroCopyRequired: true;
}

export interface VoiceEngineV2PcmFrame {
	sampleRate: number;
	numChannels: number;
	samples: ArrayBuffer;
}

export interface VoiceEngineV2OutputDeviceOptions {
	deviceId: string;
}

export interface VoiceEngineV2ParticipantVolumeOptions {
	participantIdentity: string;
	volume: number;
}

export interface VoiceEngineV2RemoteTrackSubscriptionOptions {
	participantIdentity: string;
	source: VoiceEngineV2TrackSource | string;
	subscribed: boolean;
	enabled?: boolean;
	quality?: VoiceEngineV2RemoteTrackQuality;
}

export interface VoiceEngineV2DataOptions {
	payload: ArrayBuffer | ArrayBufferView;
	reliable?: boolean;
	topic?: string;
	destinationIdentities?: Array<string>;
}

export type VoiceEngineV2AudioDeviceRole = 'default' | 'communications' | 'endpoint';

export interface VoiceEngineV2AudioInputDevice {
	deviceId: string;
	label: string;
	isDefault: boolean;
	role?: VoiceEngineV2AudioDeviceRole;
	endpointLabel?: string;
	isDefaultRoute?: boolean;
}

export interface VoiceEngineV2AudioOutputDevice {
	deviceId: string;
	label: string;
	isDefault: boolean;
	role?: VoiceEngineV2AudioDeviceRole;
	endpointLabel?: string;
	isDefaultRoute?: boolean;
}

export interface VoiceEngineV2CameraDevice {
	deviceId: string;
	label: string;
	description?: string;
}

export interface VoiceEngineV2DeviceInventory {
	audioInputs: Array<VoiceEngineV2AudioInputDevice>;
	audioOutputs: Array<VoiceEngineV2AudioOutputDevice>;
	cameras: Array<VoiceEngineV2CameraDevice>;
	selectedAudioInputId: string | null;
	selectedAudioOutputId: string | null;
	selectedCameraId: string | null;
}

export type VoiceEngineV2DeviceChangeReason = 'initial' | 'hotplug' | 'selectionChanged' | 'permissionChanged';

export type VoiceEngineV2PermissionName =
	| 'microphone'
	| 'camera'
	| 'screen'
	| 'screenAudio'
	| 'systemAudio'
	| 'windowCapture'
	| 'accessibility'
	| 'notifications'
	| string;

export type VoiceEngineV2PermissionStatus = 'unknown' | 'prompt' | 'granted' | 'denied' | 'restricted' | 'unsupported';

export interface VoiceEngineV2PermissionResult {
	name: VoiceEngineV2PermissionName;
	status: VoiceEngineV2PermissionStatus;
	canPrompt: boolean;
	detail?: string;
}

export type VoiceEngineV2AudioMode = 'voiceActivity' | 'pushToTalk' | 'pushToMute';

export interface VoiceEngineV2AudioControls {
	mode: VoiceEngineV2AudioMode;
	locallyMuted: boolean;
	preferredLocallyMuted: boolean;
	locallyDeafened: boolean;
	mutedByPermission: boolean;
	hasUserSetMute: boolean;
	hasUserSetDeaf: boolean;
	shouldUnmuteOnUndeafen: boolean;
	pushToTalkActive: boolean;
	pushToMuteActive: boolean;
	inputVolume: number;
	outputVolume: number;
}

export interface VoiceEngineV2AudioControlsPatch {
	mode?: VoiceEngineV2AudioMode;
	locallyMuted?: boolean;
	preferredLocallyMuted?: boolean;
	locallyDeafened?: boolean;
	mutedByPermission?: boolean;
	hasUserSetMute?: boolean;
	hasUserSetDeaf?: boolean;
	shouldUnmuteOnUndeafen?: boolean;
	pushToTalkActive?: boolean;
	pushToMuteActive?: boolean;
	inputVolume?: number;
	outputVolume?: number;
}

export type VoiceEngineV2E2eeStatus = 'disabled' | 'pendingKey' | 'enabled' | 'failed';

export interface VoiceEngineV2E2eeState {
	status: VoiceEngineV2E2eeStatus;
	keyId: string | null;
	failure: VoiceEngineV2Error | null;
}

export interface VoiceEngineV2Participant {
	sid: string;
	identity: string;
	name: string;
}

export interface VoiceEngineV2Track {
	participantIdentity: string;
	participantSid: string;
	trackSid: string;
	trackName: string;
	kind: VoiceEngineV2TrackKind;
	source: VoiceEngineV2TrackSource | string;
	muted: boolean;
}

export interface VoiceEngineV2OutboundStats {
	trackSid: string;
	source: VoiceEngineV2TrackSource | string;
	kind: VoiceEngineV2TrackKind;
	bitrateKbps: number;
	packetsLost: number;
	packetsSent?: number;
	codec?: string;
	width?: number;
	height?: number;
	fps?: number;
	audioLevel?: number;
	sourceWidth?: number;
	sourceHeight?: number;
	targetBitrateKbps?: number;
	configuredFps?: number;
	targetFps?: number;
	effectiveFps?: number;
	framesProduced?: number;
	framesAccepted?: number;
	framesDropped?: number;
	framesCoalesced?: number;
	framesCaptured?: number;
	captureFailures?: number;
	maxQueueAgeMs?: number;
	maxPushLatencyMs?: number;
	adaptiveSendTier?: string;
	adaptiveSendReason?: string;
	zeroCopy?: boolean;
}

export interface VoiceEngineV2InboundStats {
	participantIdentity?: string;
	participantSid?: string;
	trackSid: string;
	source?: string;
	kind: VoiceEngineV2TrackKind;
	bitrateKbps: number;
	packetsLost: number;
	packetsReceived?: number;
	codec?: string;
	jitterMs?: number;
	audioLevel?: number;
	width?: number;
	height?: number;
	fps?: number;
	sourceWidth?: number;
	sourceHeight?: number;
}

export type VoiceEngineV2VideoAccelerationStatus = 'hardware' | 'software' | 'unknown';

export interface VoiceEngineV2PerTrackStats {
	direction: 'send' | 'recv';
	kind: 'audio' | 'video' | 'unknown';
	ssrc?: number;
	rid?: string;
	mid?: string;
	trackIdentifier?: string;
	mediaSourceId?: string;
	codec?: string;
	payloadType?: number;
	bitrateKbps: number;
	packetsLost?: number;
	packetsLossPercent?: number;
	jitterMs?: number;
	framesPerSecond?: number;
	sourceFramesPerSecond?: number;
	configuredFramesPerSecond?: number;
	targetFramesPerSecond?: number;
	effectiveFramesPerSecond?: number;
	frameWidth?: number;
	frameHeight?: number;
	sourceFrameWidth?: number;
	sourceFrameHeight?: number;
	framesProduced?: number;
	framesAccepted?: number;
	framesCoalesced?: number;
	framesCaptured?: number;
	captureFailures?: number;
	maxQueueAgeMs?: number;
	maxPushLatencyMs?: number;
	adaptiveSendTier?: string;
	adaptiveSendReason?: string;
	sourceFrames?: number;
	framesEncoded?: number;
	framesDecoded?: number;
	framesDropped?: number;
	framesSent?: number;
	freezeCount?: number;
	totalFreezesDurationMs?: number;
	totalEncodeTimeMs?: number;
	encoderImplementation?: string;
	powerEfficientEncoder?: boolean;
	encoderAcceleration?: VoiceEngineV2VideoAccelerationStatus;
	scalabilityMode?: string;
	qualityLimitationReason?: string;
	qualityLimitationResolutionChanges?: number;
	targetBitrateKbps?: number;
	totalPacketSendDelayMs?: number;
	nackCount?: number;
	pliCount?: number;
	firCount?: number;
	retransmittedPacketsSent?: number;
	retransmittedBytesSent?: number;
	keyFramesEncoded?: number;
	keyFramesDecoded?: number;
	decoderImplementation?: string;
	powerEfficientDecoder?: boolean;
	decoderAcceleration?: VoiceEngineV2VideoAccelerationStatus;
	totalDecodeTimeMs?: number;
	jitterBufferDelayMs?: number;
	jitterBufferEmittedCount?: number;
	concealedSamples?: number;
	silentConcealedSamples?: number;
	totalSamplesReceived?: number;
}

export interface VoiceEngineV2TransportInfo {
	candidatePairState?: string;
	localCandidateType?: string;
	localProtocol?: string;
	localNetworkType?: string;
	remoteCandidateType?: string;
	remoteProtocol?: string;
	currentRoundTripTimeMs?: number;
	availableOutgoingBitrate?: number;
	availableIncomingBitrate?: number;
	dtlsState?: string;
	iceState?: string;
	selectedCandidatePairChanges?: number;
	dtlsCipher?: string;
	srtpCipher?: string;
	tlsVersion?: string;
	networkType?: string;
}

export interface VoiceEngineV2VoiceStats {
	audioSendBitrate: number;
	audioRecvBitrate: number;
	videoSendBitrate: number;
	videoRecvBitrate: number;
	audioPacketLoss: number;
	videoPacketLoss: number;
	rtt: number;
	jitter: number;
	participantCount: number;
	duration: number;
}

export interface VoiceEngineV2StatsSample {
	timestamp: number;
	rtt: number;
	jitter: number;
	audioPacketLoss: number;
	videoPacketLoss: number;
	audioSendBitrate: number;
	audioRecvBitrate: number;
	videoSendBitrate: number;
	videoRecvBitrate: number;
}

export interface VoiceEngineV2LatencyDataPoint {
	timestamp: number;
	latency: number;
}

export interface VoiceEngineV2SendStats {
	outgoingVideoQueueDepth: number;
	outgoingVideoQueueCapacity: number;
	outgoingVideoMaxQueueDepth: number;
	outgoingVideoFramesProduced: number;
	outgoingVideoFramesAccepted: number;
	outgoingVideoFramesDropped: number;
	outgoingVideoFramesCoalesced: number;
	outgoingVideoFramesCaptured: number;
	outgoingVideoCaptureFailures: number;
	outgoingVideoEffectiveFps: number;
	outgoingVideoTargetFps: number;
	outgoingVideoPacingTargetFps: number;
	outgoingVideoMaxQueueAgeMs: number;
	outgoingVideoMaxPushLatencyMs: number;
	outgoingVideoPacingMode: string;
	outgoingVideoBusActive: boolean;
	outgoingAudioBufferTargetMs: number;
	outgoingAudioBufferMaxMs: number;
	outgoingAudioUnderruns: number;
	outgoingAudioRebuffers: number;
	outgoingAudioMaxFrameGapMs: number;
	adaptiveSendTier: string;
	adaptiveSendReason: string;
}

export interface VoiceEngineV2Stats {
	rttMs: number | null;
	outbound: Array<VoiceEngineV2OutboundStats>;
	inbound: Array<VoiceEngineV2InboundStats>;
	droppedNativeVideoFrames?: number;
	droppedVideoFrameCallbacks?: number;
	send?: VoiceEngineV2SendStats | null;
}

export type VoiceEngineV2DiagnosticLevel = 'debug' | 'info' | 'warn' | 'error';

export interface VoiceEngineV2DiagnosticEntry {
	id: string;
	atMs: number;
	level: VoiceEngineV2DiagnosticLevel;
	code: string;
	message: string;
	detail?: unknown;
}

export interface VoiceEngineV2TimerOptions {
	timerId: string;
	delayMs: number;
	repeat?: boolean;
}

export interface VoiceEngineV2WatchedStream {
	participantIdentity: string;
	source: VoiceEngineV2TrackSource | string;
	trackSid: string | null;
	quality: VoiceEngineV2RemoteTrackQuality | null;
	enabled: boolean;
}

export interface VoiceEngineV2WatchedStreamKey {
	participantIdentity: string;
	source: VoiceEngineV2TrackSource | string;
}

export type VoiceEngineV2LocalStreamSource = 'camera' | 'screen';

export interface VoiceEngineV2CodecStreamNegotiation {
	source: VoiceEngineV2LocalStreamSource;
	streamIdentity: string;
	preferredCodec: VoiceEngineV2VideoCodec;
	negotiatedCodec: VoiceEngineV2VideoCodec;
	constrainedBy: string | null;
	viewers: Record<string, VoiceEngineV2VideoCodec | null>;
}

export interface VoiceEngineV2CodecNegotiationState {
	overrides: Partial<Record<VoiceEngineV2LocalStreamSource, VoiceEngineV2VideoCodec>>;
	localSupportedVideoCodecs: Array<VoiceEngineV2VideoCodec>;
	remoteSupportedVideoCodecs: Record<string, Array<VoiceEngineV2VideoCodec>>;
	streams: Record<string, VoiceEngineV2CodecStreamNegotiation>;
}

export interface VoiceEngineV2StreamNegotiationProjection {
	source: VoiceEngineV2LocalStreamSource;
	streamIdentity: string;
	negotiatedCodec: VoiceEngineV2VideoCodec;
	preferredCodec: VoiceEngineV2VideoCodec;
	constrainedBy: string | null;
	renegotiating: boolean;
	viewerCount: number;
}

export type VoiceEngineV2CodecGossipMessage =
	| {kind: 'codec.capability'; supportedVideoCodecs: Array<VoiceEngineV2VideoCodec>}
	| {
			kind: 'codec.viewing';
			source: VoiceEngineV2LocalStreamSource;
			watching: boolean;
			supportedVideoCodecs: Array<VoiceEngineV2VideoCodec>;
	  };

export interface VoiceEngineV2InboundVideoTrackSubscription {
	participantSid: string;
	participantIdentity?: string;
	trackSid: string;
	source: VoiceEngineV2TrackSource | string;
	width?: number;
	height?: number;
}

export interface VoiceEngineV2InboundVideoFrame {
	participantSid: string;
	participantIdentity?: string;
	trackSid: string;
	width: number;
	height: number;
	timestampUs: number;
	byteLength?: number;
}

export interface VoiceEngineV2InboundVideoFrameStats {
	participantSid: string;
	participantIdentity?: string;
	trackSid: string;
	width: number;
	height: number;
	frameCount: number;
	lastFrameTimestampUs: number;
	lastFrameByteLength: number | null;
}

export interface VoiceEngineV2InboundVideoTrack {
	participantSid: string;
	participantIdentity?: string;
	trackSid: string;
	source: VoiceEngineV2TrackSource | string;
	width: number;
	height: number;
	frameCount: number;
	lastFrameTimestampUs: number | null;
	lastFrameByteLength: number | null;
}

export interface VoiceEngineV2ConnectionModel {
	status: VoiceEngineV2ConnectionStatus;
	connected: boolean;
	connecting: boolean;
	reconnecting: boolean;
	failed: boolean;
	gateway: {
		selfVoiceState: VoiceEngineV2GatewayVoiceState | null;
		voiceServer: VoiceEngineV2GatewayVoiceServer | null;
	};
	liveKit: VoiceEngineV2LiveKitRoomState;
}

export interface VoiceEngineV2MediaModel {
	microphone: VoiceEngineV2MediaStatus;
	camera: VoiceEngineV2MediaStatus;
	screen: VoiceEngineV2MediaStatus;
	screenAudio: VoiceEngineV2MediaStatus;
	audio: VoiceEngineV2AudioControls;
	effectiveMicrophoneEnabled: boolean;
	localSpeakingOverride: boolean | null;
	e2ee: VoiceEngineV2E2eeState;
	screenCaptureId: string | null;
}

export interface VoiceEngineV2Model {
	connection: VoiceEngineV2ConnectionModel;
	media: VoiceEngineV2MediaModel;
	canPublishMedia: boolean;
	hasActiveLocalMedia: boolean;
	participants: Array<VoiceEngineV2Participant>;
	tracks: Array<VoiceEngineV2Track>;
	watchedStreams: Array<VoiceEngineV2WatchedStream>;
	inboundVideoTracks: Array<VoiceEngineV2InboundVideoTrack>;
	devices: VoiceEngineV2DeviceInventory;
	permissions: Record<string, VoiceEngineV2PermissionResult>;
	stats: VoiceEngineV2Stats | null;
	diagnostics: Array<VoiceEngineV2DiagnosticEntry>;
	tearingDown: boolean;
}
