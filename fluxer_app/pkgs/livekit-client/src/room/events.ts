// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
export enum RoomEvent {
	Connected = 'connected',

	Reconnecting = 'reconnecting',

	SignalReconnecting = 'signalReconnecting',

	Reconnected = 'reconnected',

	Disconnected = 'disconnected',

	ConnectionStateChanged = 'connectionStateChanged',

	Moved = 'moved',

	MediaDevicesChanged = 'mediaDevicesChanged',

	ParticipantConnected = 'participantConnected',

	ParticipantDisconnected = 'participantDisconnected',

	TrackPublished = 'trackPublished',

	TrackSubscribed = 'trackSubscribed',

	TrackSubscriptionFailed = 'trackSubscriptionFailed',

	TrackUnpublished = 'trackUnpublished',

	TrackUnsubscribed = 'trackUnsubscribed',

	TrackMuted = 'trackMuted',

	TrackUnmuted = 'trackUnmuted',

	LocalTrackPublished = 'localTrackPublished',

	LocalTrackUnpublished = 'localTrackUnpublished',

	LocalAudioSilenceDetected = 'localAudioSilenceDetected',

	ActiveSpeakersChanged = 'activeSpeakersChanged',

	ParticipantMetadataChanged = 'participantMetadataChanged',

	ParticipantNameChanged = 'participantNameChanged',

	ParticipantAttributesChanged = 'participantAttributesChanged',

	ParticipantActive = 'participantActive',

	RoomMetadataChanged = 'roomMetadataChanged',

	DataReceived = 'dataReceived',

	SipDTMFReceived = 'sipDTMFReceived',

	TranscriptionReceived = 'transcriptionReceived',

	ConnectionQualityChanged = 'connectionQualityChanged',

	TrackStreamStateChanged = 'trackStreamStateChanged',

	TrackSubscriptionPermissionChanged = 'trackSubscriptionPermissionChanged',

	TrackSubscriptionStatusChanged = 'trackSubscriptionStatusChanged',

	AudioPlaybackStatusChanged = 'audioPlaybackChanged',

	VideoPlaybackStatusChanged = 'videoPlaybackChanged',

	MediaDevicesError = 'mediaDevicesError',

	ParticipantPermissionsChanged = 'participantPermissionsChanged',

	SignalConnected = 'signalConnected',

	RecordingStatusChanged = 'recordingStatusChanged',

	ParticipantEncryptionStatusChanged = 'participantEncryptionStatusChanged',

	EncryptionError = 'encryptionError',
	DCBufferStatusChanged = 'dcBufferStatusChanged',

	ActiveDeviceChanged = 'activeDeviceChanged',

	ChatMessage = 'chatMessage',
	LocalTrackSubscribed = 'localTrackSubscribed',

	MetricsReceived = 'metricsReceived',

	DataTrackPublished = 'dataTrackPublished',

	DataTrackUnpublished = 'dataTrackUnpublished',

	LocalDataTrackPublished = 'localDataTrackPublished',

	LocalDataTrackUnpublished = 'localDataTrackUnpublished',
}

export enum ParticipantEvent {
	TrackPublished = 'trackPublished',

	TrackSubscribed = 'trackSubscribed',

	TrackSubscriptionFailed = 'trackSubscriptionFailed',

	TrackUnpublished = 'trackUnpublished',

	TrackUnsubscribed = 'trackUnsubscribed',

	TrackMuted = 'trackMuted',

	TrackUnmuted = 'trackUnmuted',

	LocalTrackPublished = 'localTrackPublished',

	LocalTrackUnpublished = 'localTrackUnpublished',

	LocalTrackCpuConstrained = 'localTrackCpuConstrained',

	LocalSenderCreated = 'localSenderCreated',

	ParticipantMetadataChanged = 'participantMetadataChanged',

	ParticipantNameChanged = 'participantNameChanged',

	DataReceived = 'dataReceived',

	SipDTMFReceived = 'sipDTMFReceived',

	TranscriptionReceived = 'transcriptionReceived',

	IsSpeakingChanged = 'isSpeakingChanged',

	ConnectionQualityChanged = 'connectionQualityChanged',

	TrackStreamStateChanged = 'trackStreamStateChanged',

	TrackSubscriptionPermissionChanged = 'trackSubscriptionPermissionChanged',

	TrackSubscriptionStatusChanged = 'trackSubscriptionStatusChanged',

	TrackCpuConstrained = 'trackCpuConstrained',

	MediaDevicesError = 'mediaDevicesError',

	AudioStreamAcquired = 'audioStreamAcquired',

	ParticipantPermissionsChanged = 'participantPermissionsChanged',

	PCTrackAdded = 'pcTrackAdded',

	AttributesChanged = 'attributesChanged',

	LocalTrackSubscribed = 'localTrackSubscribed',

	ChatMessage = 'chatMessage',

	Active = 'active',
}

export enum EngineEvent {
	TransportsCreated = 'transportsCreated',
	Connected = 'connected',
	Disconnected = 'disconnected',
	Resuming = 'resuming',
	Resumed = 'resumed',
	Restarting = 'restarting',
	Restarted = 'restarted',
	SignalResumed = 'signalResumed',
	SignalRestarted = 'signalRestarted',
	Closing = 'closing',
	MediaTrackAdded = 'mediaTrackAdded',
	ActiveSpeakersUpdate = 'activeSpeakersUpdate',
	DataPacketReceived = 'dataPacketReceived',
	RTPVideoMapUpdate = 'rtpVideoMapUpdate',
	DCBufferStatusChanged = 'dcBufferStatusChanged',
	ParticipantUpdate = 'participantUpdate',
	RoomUpdate = 'roomUpdate',
	SpeakersChanged = 'speakersChanged',
	StreamStateChanged = 'streamStateChanged',
	ConnectionQualityUpdate = 'connectionQualityUpdate',
	SubscriptionError = 'subscriptionError',
	SubscriptionPermissionUpdate = 'subscriptionPermissionUpdate',
	RemoteMute = 'remoteMute',
	SubscribedQualityUpdate = 'subscribedQualityUpdate',
	LocalTrackUnpublished = 'localTrackUnpublished',
	LocalTrackSubscribed = 'localTrackSubscribed',
	Offline = 'offline',
	SignalRequestResponse = 'signalRequestResponse',
	SignalConnected = 'signalConnected',
	RoomMoved = 'roomMoved',
	PublishDataTrackResponse = 'publishDataTrackResponse',
	UnPublishDataTrackResponse = 'unPublishDataTrackResponse',
	DataTrackSubscriberHandles = 'dataTrackSubscriberHandles',
	DataTrackPacketReceived = 'dataTrackPacketReceived',
	Joined = 'joined',
	TokenRefreshed = 'tokenRefreshed',
	ServerRegionsReported = 'serverRegionsReported',
}

export enum TrackEvent {
	Message = 'message',
	Muted = 'muted',
	Unmuted = 'unmuted',
	Restarted = 'restarted',
	Ended = 'ended',
	Subscribed = 'subscribed',
	Unsubscribed = 'unsubscribed',
	CpuConstrained = 'cpuConstrained',
	UpdateSettings = 'updateSettings',
	UpdateSubscription = 'updateSubscription',
	AudioPlaybackStarted = 'audioPlaybackStarted',
	AudioPlaybackFailed = 'audioPlaybackFailed',
	AudioSilenceDetected = 'audioSilenceDetected',
	VisibilityChanged = 'visibilityChanged',
	VideoDimensionsChanged = 'videoDimensionsChanged',
	VideoPlaybackStarted = 'videoPlaybackStarted',
	VideoPlaybackFailed = 'videoPlaybackFailed',
	ElementAttached = 'elementAttached',
	ElementDetached = 'elementDetached',
	UpstreamPaused = 'upstreamPaused',
	UpstreamResumed = 'upstreamResumed',
	SubscriptionPermissionChanged = 'subscriptionPermissionChanged',
	SubscriptionStatusChanged = 'subscriptionStatusChanged',
	SubscriptionFailed = 'subscriptionFailed',
	TrackProcessorUpdate = 'trackProcessorUpdate',

	AudioTrackFeatureUpdate = 'audioTrackFeatureUpdate',

	TranscriptionReceived = 'transcriptionReceived',

	TimeSyncUpdate = 'timeSyncUpdate',

	PreConnectBufferFlushed = 'preConnectBufferFlushed',
}
