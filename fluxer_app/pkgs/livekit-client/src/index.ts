// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import {Mutex} from '@livekit/mutex';
import {DataPacket_Kind, DisconnectReason, Encryption_Type, SubscriptionError, TrackType} from '@livekit/protocol';
import {getLogger, LoggerNames, LogLevel, setLogExtension, setLogLevel} from './logger.ts';
import * as attributes from './room/attribute-typings.ts';
import DefaultReconnectPolicy from './room/DefaultReconnectPolicy.ts';
import LocalDataTrack from './room/data-track/LocalDataTrack.ts';
import RemoteDataTrack, {type DataTrackSubscribeOptions} from './room/data-track/RemoteDataTrack.ts';
import type {RemoteDataTrackPipelineOptions} from './room/data-track/types.ts';
import LocalParticipant from './room/participant/LocalParticipant.ts';
import Participant, {
	ConnectionQuality,
	type ParticipantEventCallbacks,
	ParticipantKind,
} from './room/participant/Participant.ts';
import type {ParticipantTrackPermission} from './room/participant/ParticipantTrackPermission.ts';
import RemoteParticipant from './room/participant/RemoteParticipant.ts';
import type {ReconnectContext, ReconnectPolicy} from './room/ReconnectPolicy.ts';
import Room, {ConnectionState, type RoomEventCallbacks} from './room/Room.ts';
import type {AudioReceiverStats, AudioSenderStats, VideoReceiverStats, VideoSenderStats} from './room/stats.ts';
import CriticalTimers from './room/timers.ts';
import LocalAudioTrack from './room/track/LocalAudioTrack.ts';
import LocalTrack from './room/track/LocalTrack.ts';
import LocalTrackPublication from './room/track/LocalTrackPublication.ts';
import LocalVideoTrack from './room/track/LocalVideoTrack.ts';
import RemoteAudioTrack from './room/track/RemoteAudioTrack.ts';
import RemoteTrack from './room/track/RemoteTrack.ts';
import RemoteTrackPublication from './room/track/RemoteTrackPublication.ts';
import type {ElementInfo} from './room/track/RemoteVideoTrack.ts';
import RemoteVideoTrack from './room/track/RemoteVideoTrack.ts';
import {type PublicationEventCallbacks, TrackPublication} from './room/track/TrackPublication.ts';
import type {LiveKitReactNativeInfo, TextStreamInfo} from './room/types.ts';
import type {AudioAnalyserOptions} from './room/utils.ts';
import {
	compareVersions,
	createAudioAnalyser,
	getEmptyAudioStreamTrack,
	getEmptyVideoStreamTrack,
	isAudioCodec,
	isAudioTrack,
	isBrowserSupported,
	isLocalParticipant,
	isLocalTrack,
	isRemoteParticipant,
	isRemoteTrack,
	isSVCCodec,
	isVideoCodec,
	isVideoTrack,
	selectPreferredVideoCodec,
	supportsAdaptiveStream,
	supportsAudioOutputSelection,
	supportsAV1,
	supportsDynacast,
	supportsH265,
	supportsVideoCodec,
	supportsVP9,
} from './room/utils.ts';
import {getBrowser} from './utils/browserParser.ts';

export type {BaseE2EEManager} from './e2ee/E2eeManager.ts';
export * from './e2ee/index.ts';
export {
	FrameMetadataManager,
	type FrameMetadataOptions,
	PacketTrailerManager,
	type PacketTrailerOptions,
} from './frameMetadata/FrameMetadataManager.ts';
export type {
	FrameMetadata,
	FrameMetadataPublishOptions,
	PacketTrailerMetadata,
	PacketTrailerPublishOptions,
} from './frameMetadata/types.ts';
export * from './options.ts';
export type * from './room/data-stream/incoming/StreamReader.ts';
export type * from './room/data-stream/outgoing/StreamWriter.ts';
export type {DataTrackFrame} from './room/data-track/frame.ts';
export * from './room/errors.ts';
export * from './room/events.ts';
export type {DataChannelKind} from './room/RTCEngine.ts';
export {type PerformRpcParams, RpcError, type RpcInvocationData} from './room/rpc/index.ts';
export * from './room/token-source/TokenSource.ts';
export * from './room/token-source/types.ts';
export * from './room/track/create.ts';
export {facingModeFromDeviceLabel, facingModeFromLocalTrack} from './room/track/facingMode.ts';
export * from './room/track/options.ts';
export * from './room/track/processor/types.ts';
export * from './room/track/Track.ts';
export * from './room/track/types.ts';
export type {
	ByteStreamInfo,
	ChatMessage,
	DataPublishOptions,
	SendBytesOptions,
	SendTextOptions,
	SimulationScenario,
	TranscriptionSegment,
} from './room/types.ts';
export {
	isSerializer,
	type Serializer,
	type SerializerInput,
	type SerializerOutput,
	serializers,
} from './utils/serializer.ts';
export * from './version.ts';
export type {
	AudioAnalyserOptions,
	AudioReceiverStats,
	AudioSenderStats,
	DataTrackSubscribeOptions,
	ElementInfo,
	LiveKitReactNativeInfo,
	ParticipantEventCallbacks,
	ParticipantTrackPermission,
	PublicationEventCallbacks,
	ReconnectContext,
	ReconnectPolicy,
	RemoteDataTrackPipelineOptions,
	RoomEventCallbacks,
	TextStreamInfo,
	VideoReceiverStats,
	VideoSenderStats,
};
export {
	attributes,
	ConnectionQuality,
	ConnectionState,
	CriticalTimers,
	compareVersions,
	createAudioAnalyser,
	DataPacket_Kind,
	DefaultReconnectPolicy,
	DisconnectReason,
	Encryption_Type,
	getBrowser,
	getEmptyAudioStreamTrack,
	getEmptyVideoStreamTrack,
	getLogger,
	isAudioCodec,
	isAudioTrack,
	isBrowserSupported,
	isLocalParticipant,
	isLocalTrack,
	isRemoteParticipant,
	isRemoteTrack,
	isSVCCodec,
	isVideoCodec,
	isVideoTrack,
	LocalAudioTrack,
	LocalDataTrack,
	LocalParticipant,
	LocalTrack,
	LocalTrackPublication,
	LocalVideoTrack,
	LoggerNames,
	LogLevel,
	Mutex,
	Participant,
	ParticipantKind,
	RemoteAudioTrack,
	RemoteDataTrack,
	RemoteParticipant,
	RemoteTrack,
	RemoteTrackPublication,
	RemoteVideoTrack,
	Room,
	SubscriptionError,
	selectPreferredVideoCodec,
	setLogExtension,
	setLogLevel,
	supportsAdaptiveStream,
	supportsAudioOutputSelection,
	supportsAV1,
	supportsDynacast,
	supportsH265,
	supportsVideoCodec,
	supportsVP9,
	TrackPublication,
	TrackType,
};
