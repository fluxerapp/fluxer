// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {showGenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModalCommands';
import Authentication from '@app/features/auth/state/Authentication';
import type {Channel} from '@app/features/channel/models/Channel';
import Channels from '@app/features/channel/state/Channels';
import GatewayConnection from '@app/features/gateway/transport/GatewayConnection';
import type {GatewayErrorData} from '@app/features/gateway/transport/GatewaySocket';
import type {GuildReadyData} from '@app/features/gateway/types/GatewayGuildTypes';
import type {VoiceState} from '@app/features/gateway/types/GatewayVoiceTypes';
import GuildMatureContentAgree from '@app/features/guild/state/GuildMatureContentAgree';
import Guilds from '@app/features/guild/state/Guilds';
import {SOMETHING_WENT_WRONG_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import Keybind from '@app/features/input/state/InputKeybind';
import GuildMembers from '@app/features/member/state/GuildMembers';
import * as NavigationCommands from '@app/features/navigation/commands/NavigationCommands';
import {SoundType, setSoundOutputDeviceIdResolver} from '@app/features/notification/utils/SoundUtils';
import MediaPermission from '@app/features/permissions/system/state/MediaPermission';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as BackgroundImageDB from '@app/features/theme/utils/BackgroundImageDB';
import * as SoundCommands from '@app/features/ui/commands/SoundCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import Idle from '@app/features/ui/state/Idle';
import Sound from '@app/features/ui/state/Sound';
import {getElectronAPI} from '@app/features/ui/utils/NativeUtils';
import Users from '@app/features/user/state/Users';
import {getStreamKey, parseStreamKey} from '@app/features/voice/components/StreamKeys';
import {voiceStatsDB} from '@app/features/voice/diagnostics/VoiceStatsDB';
import AdaptiveScreenShareEngine from '@app/features/voice/engine/AdaptiveScreenShareEngine';
import {
	createMediaEngineFacadeSnapshot,
	hasPlayedNativeVoiceReadySounds,
	type MediaEngineFacadeEvent,
	type MediaEngineFacadePendingSessionRestore,
	type MediaEngineFacadeSnapshot,
	rememberNativeVoiceReadySounds,
	selectMediaEngineConnectPreflightDecision,
	selectMediaEngineConnectRequestDecision,
	selectMediaEngineGatewayErrorDecision,
	shouldCancelMediaEngineReconnectForServerVoiceStateRemoval,
	shouldImmediatelyDisconnectMediaEngineForServerVoiceStateRemoval,
	shouldNotifyCameraUserLimitRejection,
	shouldRunMediaEngineDeferredDisconnect,
	transitionMediaEngineFacadeSnapshot,
} from '@app/features/voice/engine/MediaEngineFacadeStateMachine';
import {NativeVoiceStatsSession} from '@app/features/voice/engine/media_engine_facade/NativeVoiceStatsSession';
import {
	AFK_CHECK_INTERVAL_MS,
	CLAIM_YOUR_ACCOUNT_TO_JOIN_THIS_VOICE_CHANNEL_DESCRIPTOR,
	CLAIM_YOUR_ACCOUNT_TO_JOIN_VOICE_CHANNELS_YOU_DESCRIPTOR,
	CLAIM_YOUR_ACCOUNT_TO_START_OR_JOIN_1_DESCRIPTOR,
	DEFERRED_DISCONNECT_TIMEOUT_MS,
	RECONNECT_SUCCEEDED_PICK_A_SCREEN_AGAIN_IF_YOU_DESCRIPTOR,
	VOICE_CAMERA_USER_LIMIT_REACHED_DESCRIPTOR,
	VOICE_CHANNEL_NO_LONGER_AVAILABLE_DESCRIPTOR,
	VOICE_CONNECTION_FAILED_DESCRIPTOR,
	VOICE_CONNECTION_LIMIT_REACHED_DESCRIPTOR,
	YOU_CAN_T_JOIN_WHILE_YOU_RE_ON_DESCRIPTOR,
} from '@app/features/voice/engine/media_engine_facade/shared';
import {
	createVoiceSessionRestoreSync,
	saveCurrentVoiceSessionRestoreSnapshot,
	type VoiceSessionRestoreSyncHandle,
} from '@app/features/voice/engine/media_engine_facade/VoiceSessionRestoreSync';
import {
	isNativeVoiceEngineSelected,
	isNativeVoiceEngineSelectionPending,
	requireNativeVoiceEngine,
	shouldUseNativeVoiceEngine,
} from '@app/features/voice/engine/native_voice_engine/getVoiceEngine';
import {
	type NativeAudioDeviceModuleStatus,
	nativeAudioDeviceModuleState,
} from '@app/features/voice/engine/native_voice_engine/NativeAudioDeviceModuleState';
import {NativeCameraPreviewStartGate} from '@app/features/voice/engine/native_voice_engine/NativeCameraPreviewStartGate';
import {
	type NativeCameraPreviewParticipant,
	selectNativeCameraLocalPreviewTrack,
} from '@app/features/voice/engine/native_voice_engine/NativeCameraPreviewTrackSelection';
import {shouldSuppressNativeLocalTrackStateDuringReconnect} from '@app/features/voice/engine/native_voice_engine/NativeLocalMediaReconnectPolicy';
import NativeVideoTileManager, {
	type NativeInboundVideoTrack,
} from '@app/features/voice/engine/native_voice_engine/NativeVideoTileManager';
import {shouldRetryNativeVoiceConnectTimeout} from '@app/features/voice/engine/native_voice_engine/NativeVoiceConnectRetryPolicy';
import {bindNativeVoiceDeviceSync} from '@app/features/voice/engine/native_voice_engine/NativeVoiceDeviceSync';
import NativeVoiceE2EEStore from '@app/features/voice/engine/native_voice_engine/NativeVoiceE2EEStore';
import {
	awaitNativeVoiceEngineReadiness,
	getNativeVoiceEngineCapabilitiesSnapshot,
	refreshNativeVoiceEngineCapabilitiesSnapshot,
} from '@app/features/voice/engine/native_voice_engine/NativeVoiceEngineSelection';
import {NativeVoiceFrameStatsBatcher} from '@app/features/voice/engine/native_voice_engine/NativeVoiceFrameStatsBatcher';
import NativeVoiceStatsStore from '@app/features/voice/engine/native_voice_engine/NativeVoiceStatsStore';
import {
	applyNativeVoiceEngineConnectedRoster,
	collectNativeVoiceEngineConnectedRosterPublishedTracks,
	getNativeVoiceEngineConnectionEventAction,
	ingestNativeVoiceEngineV2BridgeStats,
	isFacadeOwnedConnectionEvent,
	mapNativeVoiceEngineV2BridgeEvent,
	type NativeVoiceEngineLocalTrackParticipant,
	type NativeVoiceEngineLocalTrackPublication,
	type NativeVoiceEngineV2BridgeEventManagers,
} from '@app/features/voice/engine/native_voice_engine/nativeVoiceEngineEventMapper';
import type {VoiceEngine} from '@app/features/voice/engine/native_voice_engine/VoiceEngine';
import {getScreenShareCaptureDiagnosticSnapshot} from '@app/features/voice/engine/ScreenShareCaptureDiagnostics';
import ScreenShareCodecNegotiation, {
	buildLocalCodecAdvertisements,
	getScreenShareCodecPreferenceOrder,
	SCREEN_SHARE_CODEC_NEGOTIATION_TOPIC,
} from '@app/features/voice/engine/ScreenShareCodecNegotiation';
import ScreenSharePublicationMigration, {
	SCREEN_SHARE_PUBLICATION_MIGRATION_TOPIC,
} from '@app/features/voice/engine/ScreenSharePublicationMigration';
import {Store, useStoreVersion} from '@app/features/voice/engine/Store';
import {shouldMoveToAfkOnTick} from '@app/features/voice/engine/VoiceAfkTracking';
import {
	checkChannelLimit,
	checkMultipleConnections,
	sendVoiceStateConnect,
	sendVoiceStateDisconnect,
	syncVoiceStateToServer,
} from '@app/features/voice/engine/VoiceChannelConnector';
import type {VoiceConnectionFailureReason} from '@app/features/voice/engine/VoiceConnectionStateMachine';
import VoiceDevicePermissionState from '@app/features/voice/engine/VoiceDevicePermissionState';
import {getEffectiveAudioState} from '@app/features/voice/engine/VoiceEffectiveAudioState';
import type {NormalizedVoiceState} from '@app/features/voice/engine/VoiceGatewayStateMachine';
import {
	noteLocalVoiceActivity,
	noteLocalVoiceActivityFromSnapshot,
} from '@app/features/voice/engine/VoiceIdleActivityBridge';
import {normalizeVoiceMediaGraphViewerStreamKeys} from '@app/features/voice/engine/VoiceMediaGraph';
import {startVoiceMediaGraphTimerScheduler} from '@app/features/voice/engine/VoiceMediaGraphTimerScheduler';
import {bindOutputDeviceSync} from '@app/features/voice/engine/VoiceOutputDeviceSync';
import type {LivekitParticipantSnapshot} from '@app/features/voice/engine/VoiceParticipantStateMachine';
import {bindRoomEvents, type RoomEventDependencies} from '@app/features/voice/engine/VoiceRoomEventBinder';
import {playSelfJoinChimeOnce} from '@app/features/voice/engine/VoiceSelfJoinChime';
import type {VoiceStateSyncPartial, VoiceStateSyncPayload} from '@app/features/voice/engine/VoiceStateSyncTypes';
import {
	deferStopWatchingStreamKey,
	normalizeStreamGuildId,
	replaceWatchedStreamKeys,
	stopWatchingStreamKey,
} from '@app/features/voice/engine/VoiceStreamWatchState';
import {
	asVoiceTrackSource,
	type VoiceConnectionQuality,
	VoiceTrackSource,
} from '@app/features/voice/engine/VoiceTrackSource';
import {bindVoiceEngineV2AppAudioPreferencesSync} from '@app/features/voice/engine/v2/VoiceEngineV2AppAudioPreferencesSyncBinding';
import {
	createVoiceEngineV2AppAudioSettingsSnapshot,
	hasVoiceEngineV2InputProcessorSettingsChanged,
	hasVoiceEngineV2MicrophoneCaptureSettingsChanged,
} from '@app/features/voice/engine/v2/VoiceEngineV2AppAudioSettingsSync';
import {buildVoiceEngineV2AppCameraPermissionDeniedError} from '@app/features/voice/engine/v2/VoiceEngineV2AppCameraPermissionDeniedError';
import {getCameraCaptureDimensions} from '@app/features/voice/engine/v2/VoiceEngineV2AppCameraResolutionPresets';
import {runCameraTransition} from '@app/features/voice/engine/v2/VoiceEngineV2AppCameraTransition';
import {getLocalDecodableVideoCodecs} from '@app/features/voice/engine/v2/VoiceEngineV2AppCodecCapability';
import {
	computeVoiceEngineV2WatchedStreamGossip,
	ingestVoiceEngineV2CodecGossip,
} from '@app/features/voice/engine/v2/VoiceEngineV2AppCodecGossipAdapter';
import voiceEngineV2AppConnectionHostAdapter, {
	type VoiceServerUpdateData,
} from '@app/features/voice/engine/v2/VoiceEngineV2AppConnectionHostAdapter';
import {
	createVoiceEngineV2AppControllerHost,
	type VoiceEngineV2AppControllerHost,
} from '@app/features/voice/engine/v2/VoiceEngineV2AppControllerHost';
import voiceEngineV2AppDebugLoggingHostAdapter from '@app/features/voice/engine/v2/VoiceEngineV2AppDebugLoggingHostAdapter';
import {createVoiceEngineV2AppEventLogSpillLoggerSink} from '@app/features/voice/engine/v2/VoiceEngineV2AppEventLogSpillLoggerSink';
import {createVoiceEngineV2AppIngestionPort} from '@app/features/voice/engine/v2/VoiceEngineV2AppHostPorts';
import type {VoiceEngineV2AppLifecycleDisposable} from '@app/features/voice/engine/v2/VoiceEngineV2AppLifecycleAdapter';
import voiceEngineV2AppMediaExecutionAdapter from '@app/features/voice/engine/v2/VoiceEngineV2AppMediaExecutionAdapter';
import voiceEngineV2AppMediaStateAdapter from '@app/features/voice/engine/v2/VoiceEngineV2AppMediaStateAdapter';
import {
	resolveVoiceEngineV2NativeCameraDeviceId,
	type VoiceEngineV2NativeCameraDeviceResolution,
} from '@app/features/voice/engine/v2/VoiceEngineV2AppNativeCameraDeviceMapping';
import {VoiceEngineV2AppNativeCaptureExecutionAdapter} from '@app/features/voice/engine/v2/VoiceEngineV2AppNativeCaptureExecutionAdapter';
import {applyVoiceEngineV2NativeScreenShareAudioState} from '@app/features/voice/engine/v2/VoiceEngineV2AppNativeLocalTrackStateSync';
import {
	resolveVoiceEngineV2NativeMicrophoneMaxBitrateBps,
	resolveVoiceEngineV2NativeMicrophonePublishOptions,
	type VoiceEngineV2NativeMicrophonePublishOptions,
} from '@app/features/voice/engine/v2/VoiceEngineV2AppNativeMicrophoneSettings';
import {
	type VoiceEngineV2AppNativeVoiceConnectAttempt,
	VoiceEngineV2AppNativeVoiceConnectionLifecycle,
	type VoiceEngineV2AppNativeVoiceConnectReason,
} from '@app/features/voice/engine/v2/VoiceEngineV2AppNativeVoiceConnectionLifecycle';
import {VoiceEngineV2AppNativeVoiceLiveKitMediaAdapter} from '@app/features/voice/engine/v2/VoiceEngineV2AppNativeVoiceLiveKitMediaAdapter';
import {
	createVoiceEngineV2AppParticipantAdapter,
	type VoiceEngineV2AppParticipantAdapter,
} from '@app/features/voice/engine/v2/VoiceEngineV2AppParticipantAdapter';
import VoiceEngineV2AppPermissionAdapter, {
	type VoiceEngineV2AppNativePermissionEnforcement,
} from '@app/features/voice/engine/v2/VoiceEngineV2AppPermissionAdapter';
import {createVoiceEngineV2AppProductionHostPorts} from '@app/features/voice/engine/v2/VoiceEngineV2AppProductionHostPorts';
import {
	createVoiceEngineV2AppProjectionStore,
	type VoiceEngineV2AppProjectionStore,
} from '@app/features/voice/engine/v2/VoiceEngineV2AppProjectionStore';
import VoiceEngineV2AppRemoteSpeakingAdapter from '@app/features/voice/engine/v2/VoiceEngineV2AppRemoteSpeakingAdapter';
import type {VoiceEngineV2AppScreenShareControllerGateway} from '@app/features/voice/engine/v2/VoiceEngineV2AppScreenShareControllerRouting';
import voiceEngineV2AppScreenShareExecutionAdapter, {
	type DeviceScreenShareCaptureOptions,
	type NativeScreenShareReconnectSnapshot,
	type ScreenShareReconnectSnapshot,
} from '@app/features/voice/engine/v2/VoiceEngineV2AppScreenShareExecutionAdapter';
import {resolveVoiceEngineV2AppSelectedMediaMode} from '@app/features/voice/engine/v2/VoiceEngineV2AppSelectedMediaMode';
import {selectVoiceEngineV2AppIntentSelfMuteForVoiceStatePayload} from '@app/features/voice/engine/v2/VoiceEngineV2AppSelectors';
import {VoiceEngineV2AppSourceLifecycleBridge} from '@app/features/voice/engine/v2/VoiceEngineV2AppSourceLifecycleBridge';
import {VoiceEngineV2AppStatsHostAdapter} from '@app/features/voice/engine/v2/VoiceEngineV2AppStatsHostAdapter';
import VoiceEngineV2AppSubscriptionAdapter from '@app/features/voice/engine/v2/VoiceEngineV2AppSubscriptionAdapter';
import voiceEngineV2AppVoiceStateAdapter from '@app/features/voice/engine/v2/VoiceEngineV2AppVoiceStateAdapter';
import type {NativeScreenShareOptions} from '@app/features/voice/engine/voice_screen_share_manager/DisplayMediaCapture';
import type {VoiceStateAckPayload} from '@app/features/voice/events/VoiceStateAck';
import CallMediaPrefs from '@app/features/voice/state/CallMediaPrefs';
import {type ChannelE2EEStatus, computeChannelE2EEStatus} from '@app/features/voice/state/ChannelE2EEStatus';
import LocalVoiceState from '@app/features/voice/state/LocalVoiceState';
import VoiceCallLayout from '@app/features/voice/state/VoiceCallLayout';
import VoiceRegionTeleport from '@app/features/voice/state/VoiceRegionTeleport';
import VoiceSessionRestore, {type VoiceSessionRestoreSnapshot} from '@app/features/voice/state/VoiceSessionRestore';
import VoiceSettings, {BLUR_BACKGROUND_ID, NONE_BACKGROUND_ID} from '@app/features/voice/state/VoiceSettings';
import {type CodecPreference, getCodecCapabilityReport} from '@app/features/voice/utils/CodecCapabilityDetector';
import {getGpuEncoderReportSync, loadGpuEncoderReport} from '@app/features/voice/utils/GpuEncoderCapabilities';
import {
	getNativeAudioCaptureDiagnosticState,
	setNativeAudioCaptureBridgeLifecycleBridge,
} from '@app/features/voice/utils/NativeAudioCaptureBridge';
import {getOpenH264StatusSync, loadOpenH264Status} from '@app/features/voice/utils/OpenH264Status';
import {areOrderedStringArraysEqual} from '@app/features/voice/utils/StringArrayUtils';
import {VideoBackgroundFramePump} from '@app/features/voice/utils/VideoBackgroundFramePump';
import {
	getVideoDecoderExclusionsSync,
	loadVideoDecoderExclusions,
} from '@app/features/voice/utils/VideoDecoderCapabilities';
import {areVoiceBackgroundsAvailable} from '@app/features/voice/utils/VoiceBackgroundAvailability';
import {voiceDeviceManager} from '@app/features/voice/utils/VoiceDeviceManager';
import {buildVoiceParticipantIdentity} from '@app/features/voice/utils/VoiceParticipantIdentity';
import {
	isVoiceServerMuteActive,
	isVoiceSpeakPermissionDenied,
	resolveVoiceStateSelfMute,
	shouldPrepareMicrophoneForVoiceConnect,
} from '@app/features/voice/utils/VoicePermissionUtils';
import {getActiveVoiceProcessingMode} from '@app/features/voice/utils/VoiceProcessingProfile';
import type {NativeAudioStartOptions} from '@app/types/electron.d';
import {ME} from '@fluxer/constants/src/AppConstants';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {VOICE_CHANNEL_CAMERA_USER_LIMIT} from '@fluxer/constants/src/LimitConstants';
import type {
	VoiceEngineV2AudioMode,
	VoiceEngineV2CameraOptions,
	VoiceEngineV2Command,
	VoiceEngineV2Controller,
	VoiceEngineV2DisconnectReason,
	VoiceEngineV2GatewayVoiceState,
	VoiceEngineV2LatencyDataPoint,
	VoiceEngineV2LocalStreamSource,
	VoiceEngineV2MicrophoneOptions,
	VoiceEngineV2Model,
	VoiceEngineV2PerTrackStats,
	VoiceEngineV2Snapshot,
	VoiceEngineV2Stats,
	VoiceEngineV2StatsSample,
	VoiceEngineV2TransportInfo,
	VoiceEngineV2VideoCodec,
	VoiceEngineV2VoiceStats,
} from '@fluxer/voice_engine_v2';
import {
	encodeVoiceEngineV2CodecGossip,
	shouldApplyGatewayVoiceStateEcho,
	VOICE_ENGINE_V2_CODEC_GOSSIP_TOPIC,
} from '@fluxer/voice_engine_v2';
import {
	translateVoiceEngineV2BridgeEventToEvents,
	type VoiceEngineV2BridgeEvent,
	type VoiceEngineV2BridgeVideoFrame,
} from '@fluxer/voice_engine_v2/bridge';
import {type I18n, i18n as linguiI18n, type MessageDescriptor} from '@lingui/core';
import {
	type ConnectionQuality as LiveKitConnectionQuality,
	type Participant,
	type Room,
	RoomEvent,
	type ScreenShareCaptureOptions,
	Track,
	type TrackPublishOptions,
} from 'livekit-client';
import {makeObservable, observable} from 'mobx';

const logger = new Logger('MediaEngineFacade');
const voiceEngineV2AppNativeCaptureExecutionAdapter = new VoiceEngineV2AppNativeCaptureExecutionAdapter();
const MAX_DIAGNOSTIC_LINUX_AUDIO_TARGETS = 200;
const MAX_DIAGNOSTIC_PIPEWIRE_GRAPH_RECORDS = 500;

class TimeoutError extends Error {
	constructor(message: string) {
		super(message);
		this.name = 'TimeoutError';
	}
}

const NATIVE_VOICE_ENGINE_CONNECT_TIMEOUT_MS = 2_000;
const NATIVE_VOICE_ENGINE_CONNECT_RETRY_BACKOFF_BASE_MS = 25;
const NATIVE_VOICE_ENGINE_CONNECT_RETRY_BACKOFF_CEILING_MS = 100;
const NATIVE_VOICE_ENGINE_CONNECT_MAX_LOCAL_RETRIES = 1;

function getNativeVoiceEngineConnectRetryDelayMs(retryAttempt: number): number {
	const exponentialDelayMs = NATIVE_VOICE_ENGINE_CONNECT_RETRY_BACKOFF_BASE_MS * 2 ** Math.max(0, retryAttempt - 1);
	return Math.min(exponentialDelayMs, NATIVE_VOICE_ENGINE_CONNECT_RETRY_BACKOFF_CEILING_MS);
}

type LinuxAudioRoutingRule = NonNullable<NativeAudioStartOptions['linuxRule']>;

function areVirtmicNodesEqual(left: Record<string, string>, right: Record<string, string>): boolean {
	const leftKeys = Object.keys(left).sort();
	const rightKeys = Object.keys(right).sort();
	if (!areOrderedStringArraysEqual(leftKeys, rightKeys)) return false;
	return leftKeys.every((key) => left[key] === right[key]);
}

function areVirtmicNodeListsEqual(
	left: Array<Record<string, string>> | undefined,
	right: Array<Record<string, string>> | undefined,
): boolean {
	const leftNodes = left ?? [];
	const rightNodes = right ?? [];
	if (leftNodes.length !== rightNodes.length) return false;
	return leftNodes.every((node, index) => areVirtmicNodesEqual(node, rightNodes[index]));
}

function areLinuxAudioRoutingRulesEqual(left: LinuxAudioRoutingRule, right: LinuxAudioRoutingRule): boolean {
	return (
		left.workaround === right.workaround &&
		left.ignoreDevices === right.ignoreDevices &&
		left.ignoreInputMedia === right.ignoreInputMedia &&
		left.ignoreVirtual === right.ignoreVirtual &&
		left.onlySpeakers === right.onlySpeakers &&
		left.onlyDefaultSpeakers === right.onlyDefaultSpeakers &&
		areVirtmicNodeListsEqual(left.include, right.include) &&
		areVirtmicNodeListsEqual(left.exclude, right.exclude)
	);
}
const NATIVE_VOICE_ENGINE_TRANSPORT_RECONNECT_DELAY_MS = 1250;
function encodeVoiceEngineE2EEKey(key: string | null | undefined): ArrayBuffer | null {
	if (!key) return null;
	const encoded = new TextEncoder().encode(key);
	return encoded.buffer.slice(encoded.byteOffset, encoded.byteOffset + encoded.byteLength);
}

function nativeCameraDeviceIdFromResolution(resolution: VoiceEngineV2NativeCameraDeviceResolution): string | undefined {
	assert.ok(resolution !== null && typeof resolution === 'object', 'camera device resolution must be an object');
	switch (resolution.status) {
		case 'default':
			return undefined;
		case 'direct':
			assert.ok(resolution.deviceId, 'direct camera device resolution must carry a native device id');
			logger.debug('Using native camera device id directly', {
				requestedDeviceId: resolution.requestedDeviceId,
				nativeLabel: resolution.nativeLabel,
			});
			return resolution.deviceId;
		case 'mapped':
			assert.ok(resolution.deviceId, 'mapped camera device resolution must carry a native device id');
			logger.info('Mapped browser camera device to native camera device', {
				browserLabel: resolution.browserLabel,
				nativeLabel: resolution.nativeLabel,
				nativeDeviceId: resolution.deviceId,
			});
			return resolution.deviceId;
		case 'ambiguous':
			logger.warn('Native camera device mapping is ambiguous; falling back to default camera', {
				browserLabel: resolution.browserLabel,
				matchCount: resolution.matchCount,
			});
			return undefined;
		case 'unmapped':
			logger.warn('Native camera device mapping failed; falling back to default camera', {
				browserLabel: resolution.browserLabel,
				requestedDeviceId: resolution.requestedDeviceId,
				nativeDeviceCount: resolution.nativeDeviceCount,
			});
			return undefined;
		case 'unavailable':
			logger.warn('Native camera enumeration returned no devices; falling back to default camera', {
				requestedDeviceId: resolution.requestedDeviceId,
			});
			return undefined;
	}
}

async function resolveNativeCameraDeviceId(deviceId?: string): Promise<string | undefined> {
	if (!deviceId || deviceId === 'default') return undefined;
	const trimmedDeviceId = deviceId.trim();
	try {
		const nativeEngine = requireNativeVoiceEngine();
		const [browserDeviceState, nativeDevices] = await Promise.all([
			voiceDeviceManager.ensureDevices({requestPermissions: false, forceRefresh: true}),
			nativeEngine.listCameraDevices(),
		]);
		const resolution = resolveVoiceEngineV2NativeCameraDeviceId({
			requestedDeviceId: trimmedDeviceId,
			browserDevices: browserDeviceState.videoDevices,
			nativeDevices,
		});
		return nativeCameraDeviceIdFromResolution(resolution);
	} catch (error) {
		logger.warn('Failed to map browser camera device to native camera device', {deviceId, error});
		return undefined;
	}
}

function isCameraPermissionDeniedCommandFailure(error: unknown): boolean {
	if (!(error instanceof Error)) return false;
	const code = (error as Error & {code?: unknown}).code;
	if (code === 'permissionDenied') return true;
	return error.message.includes('permissionDenied');
}

function buildCameraCommandOptions(options?: {deviceId?: string; sendUpdate?: boolean}): VoiceEngineV2CameraOptions {
	const commandOptions: VoiceEngineV2CameraOptions = {};
	if (options?.deviceId) {
		commandOptions.deviceId = options.deviceId;
	}
	if (options?.sendUpdate === false) {
		commandOptions.sendUpdate = false;
	}
	return commandOptions;
}

function buildScreenEncodingCaptureOptions(options: {
	width: number;
	height: number;
	frameRate?: number;
}): ScreenShareCaptureOptions {
	const resolution: NonNullable<ScreenShareCaptureOptions['resolution']> = {
		width: options.width,
		height: options.height,
	};
	if (options.frameRate !== undefined) {
		resolution.frameRate = options.frameRate;
	}
	return {resolution};
}

function buildScreenEncodingPublishOptions(options: {
	codec?: '' | 'vp8' | 'vp9' | 'h264' | 'h265' | 'av1';
	frameRate?: number;
	maxBitrateBps?: number;
}): TrackPublishOptions | undefined {
	const publishOptions: TrackPublishOptions = {};
	if (options.codec) {
		publishOptions.videoCodec = options.codec;
	}
	if (options.maxBitrateBps !== undefined) {
		const screenShareEncoding: {maxBitrate: number; maxFramerate?: number} = {
			maxBitrate: options.maxBitrateBps,
		};
		if (options.frameRate !== undefined) {
			screenShareEncoding.maxFramerate = options.frameRate;
		}
		publishOptions.screenShareEncoding = screenShareEncoding;
	}
	return Object.keys(publishOptions).length > 0 ? publishOptions : undefined;
}

function buildRoomEventDependencies(facade: {
	resetStreamTracking: () => void;
	participants: VoiceEngineV2AppParticipantAdapter;
	sourceLifecycleBridge: VoiceEngineV2AppSourceLifecycleBridge | null;
	isUserMovePending: () => boolean;
}): RoomEventDependencies {
	const sourceLifecycleBridge = facade.sourceLifecycleBridge;
	return {
		connection: {
			createGuardedHandler: (guardedAttemptId, handler) =>
				voiceEngineV2AppConnectionHostAdapter.createGuardedHandler(guardedAttemptId, handler),
			isDisconnecting: () => voiceEngineV2AppConnectionHostAdapter.disconnecting,
			isUserMovePending: () => facade.isUserMovePending(),
			markConnected: () => voiceEngineV2AppConnectionHostAdapter.markConnected(),
			markDisconnected: (reason) => voiceEngineV2AppConnectionHostAdapter.markDisconnected(reason),
			markReconnecting: () => voiceEngineV2AppConnectionHostAdapter.markReconnecting(),
			markReconnected: () => voiceEngineV2AppConnectionHostAdapter.markReconnected(),
		},
		media: {
			applyAllLocalAudioPreferences: (activeRoom) =>
				voiceEngineV2AppMediaExecutionAdapter.applyAllLocalAudioPreferences(activeRoom),
			ensureMicrophone: (activeRoom, activeChannelId) =>
				voiceEngineV2AppMediaExecutionAdapter.ensureMicrophone(activeRoom, activeChannelId),
			playEntranceSound: () => voiceEngineV2AppMediaExecutionAdapter.playEntranceSound(),
			resetStreamTracking: () => facade.resetStreamTracking(),
		},
		mediaState: {
			handleLocalTrackStateChange: (source, isPublished) =>
				voiceEngineV2AppMediaStateAdapter.handleLocalTrackStateChange(source, isPublished),
			resetLocalMediaState: (reason) => voiceEngineV2AppMediaStateAdapter.resetLocalMediaState(reason),
		},
		participants: {
			clear: () => facade.participants.clear(),
			hydrateFromRoom: (activeRoom) => facade.participants.hydrateFromRoom(activeRoom),
			removeParticipant: (identity) => facade.participants.removeParticipant(identity),
			updateActiveSpeakers: (speakers) => facade.participants.updateActiveSpeakers(speakers),
			upsertParticipant: (participant) => facade.participants.upsertParticipant(participant),
		},
		permissions: {
			applyDeafen: (activeRoom, deafened) => VoiceEngineV2AppPermissionAdapter.applyDeafen(activeRoom, deafened),
			syncWithPermissionState: (activeGuildId, activeChannelId, activeRoom) =>
				VoiceEngineV2AppPermissionAdapter.syncWithPermissionState(activeGuildId, activeChannelId, activeRoom),
		},
		remoteSpeaking: {
			attachIfApplicable: (participant, publication, track) =>
				VoiceEngineV2AppRemoteSpeakingAdapter.attachIfApplicable(participant, publication, track),
			clear: () => VoiceEngineV2AppRemoteSpeakingAdapter.clear(),
			detachByIdentity: (identity) => VoiceEngineV2AppRemoteSpeakingAdapter.detachByIdentity(identity),
			detachIfTrackMatches: (participant, publication) =>
				VoiceEngineV2AppRemoteSpeakingAdapter.detachIfTrackMatches(participant, publication),
			hydrateFromRoom: (activeRoom) => VoiceEngineV2AppRemoteSpeakingAdapter.hydrateFromRoom(activeRoom),
		},
		screenShare: {
			cleanupLingeringScreenShareTracks: (participant) =>
				voiceEngineV2AppScreenShareExecutionAdapter.cleanupLingeringScreenShareTracks(participant),
			handleLocalScreenShareTrackUnpublished: (activeRoom, didChangeLocalState, publication) =>
				voiceEngineV2AppScreenShareExecutionAdapter.handleLocalScreenShareTrackUnpublished(
					activeRoom,
					didChangeLocalState,
					publication,
				),
			isScreenShareCodecRepublishInFlight: () =>
				voiceEngineV2AppScreenShareExecutionAdapter.isScreenShareCodecRepublishInFlight(),
			renegotiateActiveScreenShareCodec: (activeRoom, selection) =>
				voiceEngineV2AppScreenShareExecutionAdapter.renegotiateActiveScreenShareCodec(
					activeRoom,
					selection.codec,
					selection.reason,
				),
		},
		subscriptions: {
			isScreenShareSubscribed: (participantIdentity) =>
				VoiceEngineV2AppSubscriptionAdapter.isScreenShareSubscribed(participantIdentity),
			reattachScreenShareAfterPublish: (participantIdentity, publication) =>
				VoiceEngineV2AppSubscriptionAdapter.reattachScreenShareAfterPublish(participantIdentity, publication),
			reconcileSubscriptions: () => VoiceEngineV2AppSubscriptionAdapter.reconcileSubscriptions(),
		},
		remoteTrackLifecycle: sourceLifecycleBridge
			? {
					bind: (track, options) => sourceLifecycleBridge.bindRemoteTrackLifecycle(track, options),
				}
			: undefined,
	};
}

function voiceDiagnosticError(error: unknown): Record<string, unknown> {
	if (error instanceof Error) {
		return {
			name: error.name,
			message: error.message,
			stack: error.stack,
		};
	}
	return {message: String(error)};
}

function withTimeout<T>(task: Promise<T>, timeoutMs: number, message: string): Promise<T> {
	let timeoutId: NodeJS.Timeout | null = null;
	const timeout = new Promise<T>((_resolve, reject) => {
		timeoutId = setTimeout(() => reject(new TimeoutError(message)), timeoutMs);
	});
	return Promise.race([task, timeout]).finally(() => {
		if (timeoutId !== null) {
			clearTimeout(timeoutId);
		}
	});
}

function limitLinuxAudioTargets(result: unknown): unknown {
	if (!result || typeof result !== 'object' || !('targets' in result)) return result;
	const record = result as {targets?: unknown};
	if (!Array.isArray(record.targets)) return result;
	return {
		...result,
		targets: record.targets.slice(0, MAX_DIAGNOSTIC_LINUX_AUDIO_TARGETS),
		targetCount: record.targets.length,
		truncated: record.targets.length > MAX_DIAGNOSTIC_LINUX_AUDIO_TARGETS,
	};
}

function limitGraphArray(record: Record<string, unknown>, key: string): Record<string, unknown> {
	const value = record[key];
	if (!Array.isArray(value)) return record;
	return {
		...record,
		[key]: value.slice(0, MAX_DIAGNOSTIC_PIPEWIRE_GRAPH_RECORDS),
		[`${key}Count`]: value.length,
		[`${key}Truncated`]: value.length > MAX_DIAGNOSTIC_PIPEWIRE_GRAPH_RECORDS,
	};
}

function limitPipewireRoutingGraph(graph: unknown): unknown {
	if (!graph || typeof graph !== 'object') return graph;
	let limited = graph as Record<string, unknown>;
	limited = limitGraphArray(limited, 'nodes');
	limited = limitGraphArray(limited, 'ports');
	limited = limitGraphArray(limited, 'ownedLinks');
	return limited;
}

function limitPipewireRoutingGraphResult(result: unknown): unknown {
	if (!result || typeof result !== 'object') return result;
	const record = result as Record<string, unknown>;
	if ('graph' in record) {
		return {...record, graph: limitPipewireRoutingGraph(record.graph)};
	}
	if (Array.isArray(record.graphs)) {
		return {
			...record,
			graphs: record.graphs.map((entry) =>
				entry && typeof entry === 'object'
					? {...entry, graph: limitPipewireRoutingGraph((entry as Record<string, unknown>).graph)}
					: entry,
			),
		};
	}
	return result;
}

function hasPipewireRoutingGraph(result: unknown): boolean {
	if (!result || typeof result !== 'object') return false;
	const record = result as Record<string, unknown>;
	if (record.ok !== true) return false;
	if (record.graph && typeof record.graph === 'object') return true;
	return (
		Array.isArray(record.graphs) &&
		record.graphs.some((entry) => {
			if (!entry || typeof entry !== 'object') return false;
			const graph = (entry as Record<string, unknown>).graph;
			return Boolean(graph && typeof graph === 'object');
		})
	);
}

async function createVoiceAudioDiagnosticsSnapshot(): Promise<Record<string, unknown>> {
	const electron = getElectronAPI();
	const [
		nativeAudioAvailability,
		nativeAudioApplications,
		nativeAudioRoutingGraph,
		virtmicAvailability,
		virtmicTargets,
		virtmicRoutingGraph,
	] = await Promise.all([
		electron?.nativeAudio
			? electron.nativeAudio.getAvailability().catch((error) => ({error: voiceDiagnosticError(error)}))
			: Promise.resolve(null),
		electron?.nativeAudio
			? electron.nativeAudio.listAudibleApplications().catch((error) => ({error: voiceDiagnosticError(error)}))
			: Promise.resolve(null),
		electron?.nativeAudio
			? electron.nativeAudio.getRoutingGraph().catch((error) => ({error: voiceDiagnosticError(error)}))
			: Promise.resolve(null),
		electron?.virtmic
			? electron.virtmic.getAvailability().catch((error) => ({error: voiceDiagnosticError(error)}))
			: Promise.resolve(null),
		electron?.virtmic
			? electron.virtmic.listTargets({granular: true}).catch((error) => ({error: voiceDiagnosticError(error)}))
			: Promise.resolve(null),
		electron?.virtmic
			? electron.virtmic.getRoutingGraph().catch((error) => ({error: voiceDiagnosticError(error)}))
			: Promise.resolve(null),
	]);
	return {
		platform: electron?.platform ?? null,
		nativeAudio: {
			availability: nativeAudioAvailability,
			audibleApplications: nativeAudioApplications,
			capture: getNativeAudioCaptureDiagnosticState(),
			pipewireRoutingGraph: limitPipewireRoutingGraphResult(nativeAudioRoutingGraph),
		},
		linuxRouting: {
			virtmicAvailability,
			pipewireNodeInventory: limitLinuxAudioTargets(virtmicTargets),
			pipewireRoutingGraph: limitPipewireRoutingGraphResult(virtmicRoutingGraph),
			activeLinkGraphExported:
				hasPipewireRoutingGraph(virtmicRoutingGraph) || hasPipewireRoutingGraph(nativeAudioRoutingGraph),
			routingSettings: {
				screenShareAudioSourceMode: VoiceSettings.getScreenShareAudioSourceMode(),
				screenShareAudioIncludeSources: VoiceSettings.getScreenShareAudioIncludeSources(),
				screenShareAudioExcludeSources: VoiceSettings.getScreenShareAudioExcludeSources(),
				shareAppAudio: VoiceSettings.getShareAppAudio(),
				shareDesktopAudio: VoiceSettings.getShareDesktopAudio(),
			},
		},
	};
}

interface ConnectToVoiceChannelOptions {
	skipChannelGate?: boolean;
	deferNavigationUntilConnected?: boolean;
	initialViewerStreamKeys?: Array<string>;
}

interface ConnectDirectlyOptions {
	deferNavigationUntilConnected?: boolean;
	initialViewerStreamKeys?: Array<string>;
}

type TerminalUnloadVoiceDisconnectReason = 'pagehide' | 'beforeunload' | 'unload';

interface NativeLocalMediaReconnectSnapshot {
	connectionId: string | null;
	restoreVideo: boolean;
	restoreStream: boolean;
	screenShare: NativeScreenShareReconnectSnapshot | null;
	screenShareRelease: Promise<void> | null;
}

type VoiceMuteReason = 'guild' | 'permission' | 'voice_push_to_talk' | 'self' | null;

interface ConnectViaNativeEngineOptions {
	forceReconnect?: boolean;
	reason?: VoiceEngineV2AppNativeVoiceConnectReason;
}

export interface VoiceEngineConnectionContext {
	guildId: string | null;
	channelId: string | null;
	connectionId: string | null;
	connected: boolean;
	connecting: boolean;
	reconnecting: boolean;
}

class MediaEngineFacade extends Store {
	private readonly voiceEngineV2Host: VoiceEngineV2AppControllerHost;
	private readonly voiceEngineV2ProjectionStore: VoiceEngineV2AppProjectionStore;
	private readonly voiceEngineV2Participants: VoiceEngineV2AppParticipantAdapter;
	private readonly voiceEngineV2SourceLifecycleBridge: VoiceEngineV2AppSourceLifecycleBridge | null;
	private statsHostAdapter: VoiceEngineV2AppStatsHostAdapter;
	private afkIntervalId: NodeJS.Timeout | null = null;
	private voiceSessionRestoreSync: VoiceSessionRestoreSyncHandle | null = null;
	private outputDeviceSyncDisposer: (() => void) | null = null;
	private audioPreferencesSyncDisposer: (() => void) | null = null;
	private videoCodecDecodeCapResyncDisposer: (() => void) | null = null;
	private videoCodecPublishOverrideSyncDisposer: (() => void) | null = null;
	private localStreamCodecReconcileScheduled = false;
	private previousWatchedStreamCodecGossip = new Map<
		string,
		{identity: string; source: VoiceEngineV2LocalStreamSource}
	>();
	private videoCodecGossipReceiverDisposer: (() => void) | null = null;
	private i18n: I18n | null = linguiI18n;
	private pendingServerDisconnectTimeout: NodeJS.Timeout | null = null;
	private facadeSnapshot: MediaEngineFacadeSnapshot = createMediaEngineFacadeSnapshot();
	private disconnectPromise: Promise<void> | null = null;
	private nativeVoiceEngineV2BridgeEventDisposer: (() => void) | null = null;
	private nativeVoiceDeviceSyncDisposer: (() => void) | null = null;
	private nativeVoiceDataProtocolDisposer: (() => void) | null = null;
	private nativeVoiceConnectRetryTimeoutId: number | null = null;
	private nativeVoiceConnectRetryCounts = new Map<string, number>();
	private readonly nativeVoiceConnectionLifecycle = new VoiceEngineV2AppNativeVoiceConnectionLifecycle();
	private nativeVoiceTransportReconnectTimeoutId: number | null = null;
	private readonly nativeVoiceStatsSession = new NativeVoiceStatsSession({
		ingestStats: (stats, timestampMs) => this.ingestNativeVoiceStats(stats, timestampMs),
	});
	private readonly nativeVoiceFrameStatsBatcher = new NativeVoiceFrameStatsBatcher({
		dispatch: (event) => {
			this.voiceEngineV2Host.dispatch(event);
		},
		onDroppedUpdates: (droppedUpdatesCount) => {
			logger.warn('Native inbound video frame stats dropped updates at the track cap', {droppedUpdatesCount});
			voiceEngineV2AppDebugLoggingHostAdapter.recordNativeVideoDiagnostic('frame_stats.dropped_updates', {
				droppedUpdatesCount,
			});
		},
	});
	private lastNativeVoiceServerUpdate: VoiceServerUpdateData | null = null;
	private nativeVoiceReadySoundConnectionIds = new Set<string>();
	private pendingNativeLocalMediaReconnect: NativeLocalMediaReconnectSnapshot | null = null;
	private terminalUnloadVoiceDisconnectSent = false;
	private nativeCameraPreviewTrackSid: string | null = null;
	private nativeCameraPreviewSessionTrackSid: string | null = null;
	private readonly nativeCameraPreviewStartGate = new NativeCameraPreviewStartGate();
	private videoBackgroundFramePump: VideoBackgroundFramePump | null = null;
	private voiceEngineV2EstablishedConnectionKey: string | null = null;
	private lastVoiceConnectFailed = false;

	constructor() {
		super();
		this.statsHostAdapter = new VoiceEngineV2AppStatsHostAdapter();
		const voiceEngineV2Ingestion = createVoiceEngineV2AppIngestionPort();
		const nativeVoiceMedia = new VoiceEngineV2AppNativeVoiceLiveKitMediaAdapter({
			getEngine: requireNativeVoiceEngine,
			camera: {
				publishCamera: (options) => this.setCameraEnabledViaEngine(true, options),
				updateCameraEncoding: (options) =>
					this.updateActiveCameraCapture(options.deviceId ? {deviceId: options.deviceId} : undefined),
				unpublishCamera: (options) => this.setCameraEnabledViaEngine(false, options),
			},
			screen: {
				publishScreen: (options) =>
					voiceEngineV2AppScreenShareExecutionAdapter.publishControllerScreenViaNativeCapture(options),
				updateScreenEncoding: async (options) => {
					await voiceEngineV2AppScreenShareExecutionAdapter.updateActiveScreenShareSettings(
						null,
						buildScreenEncodingCaptureOptions(options),
						buildScreenEncodingPublishOptions(options),
					);
				},
				unpublishScreen: () => voiceEngineV2AppScreenShareExecutionAdapter.unpublishControllerScreenViaNativeCapture(),
			},
			logger,
		});
		this.voiceEngineV2Host = createVoiceEngineV2AppControllerHost({
			eventLogSpillSink: createVoiceEngineV2AppEventLogSpillLoggerSink({logger}),
			ports: createVoiceEngineV2AppProductionHostPorts({
				gateway: {
					async writeVoiceState(options): Promise<void> {
						const connectionId = voiceEngineV2AppConnectionHostAdapter.connectionId;
						if (!connectionId) return;
						if (!options.channelId) {
							sendVoiceStateDisconnect(options.guildId, connectionId);
							return;
						}
						syncVoiceStateToServer(options.guildId, options.channelId, connectionId, {
							self_mute: options.selfMute,
							self_deaf: options.selfDeaf,
							self_video: options.selfVideo,
							self_stream: options.selfStream,
						});
					},
					async clearVoiceState(guildId): Promise<void> {
						const connectionId = voiceEngineV2AppConnectionHostAdapter.connectionId;
						if (!connectionId) return;
						sendVoiceStateDisconnect(guildId, connectionId);
					},
				},
				connection: {
					startConnection: (guildId, channelId) =>
						voiceEngineV2AppConnectionHostAdapter.startConnection(guildId, channelId),
					disconnectFromVoiceChannel: (reason) =>
						voiceEngineV2AppConnectionHostAdapter.disconnectFromVoiceChannel(reason),
				},
				media: voiceEngineV2AppMediaExecutionAdapter,
				screenShare: voiceEngineV2AppScreenShareExecutionAdapter,
				getRoom: () => this.room,
				getActiveGuildId: () => voiceEngineV2AppConnectionHostAdapter.guildId,
				getActiveChannelId: () => voiceEngineV2AppConnectionHostAdapter.channelId,
				audioOutputStore: {
					async setOutputDevice(deviceId): Promise<void> {
						VoiceSettings.updateSettings({outputDeviceId: deviceId});
					},
				},
				nativeMedia: voiceEngineV2AppNativeCaptureExecutionAdapter,
				nativeVoiceMedia,
				getSelectedMediaMode: resolveVoiceEngineV2AppSelectedMediaMode,
				ingestion: voiceEngineV2Ingestion,
				subscriptions: VoiceEngineV2AppSubscriptionAdapter,
				stats: this.statsHostAdapter,
				lifecycleDisposables: this.createVoiceEngineV2LifecycleDisposables(),
				logger,
			}),
		});
		this.voiceEngineV2ProjectionStore = createVoiceEngineV2AppProjectionStore(this.voiceEngineV2Host);
		this.voiceEngineV2Participants = createVoiceEngineV2AppParticipantAdapter({
			controller: this.voiceEngineV2Controller,
			getModel: () => this.voiceEngineV2Model,
			ingest: (event) => voiceEngineV2Ingestion.ingest(event),
			getCurrentConnectionId: () => voiceEngineV2AppConnectionHostAdapter.connectionId,
		});
		this.voiceEngineV2SourceLifecycleBridge = this.createSourceLifecycleBridge();
		voiceEngineV2AppScreenShareExecutionAdapter.setControllerGateway(this.createScreenShareControllerGateway());
		voiceEngineV2AppScreenShareExecutionAdapter.setSourceLifecycleBridge(this.voiceEngineV2SourceLifecycleBridge);
		voiceEngineV2AppMediaExecutionAdapter.setSourceLifecycleBridge(this.voiceEngineV2SourceLifecycleBridge);
		setNativeAudioCaptureBridgeLifecycleBridge(this.voiceEngineV2SourceLifecycleBridge);
		this.syncVoiceEngineV2AudioControlsFromAppState();
		makeObservable<this, 'facadeSnapshot'>(this, {
			facadeSnapshot: observable.ref,
		});
		this.initializeEngineStoreSync();
		this.initializeVoiceEngineV2ConnectionLifecycleSync();
		this.initializeLocalAudioStateSync();
		this.initializeVoiceSessionRestoreSync();
		this.initializeE2EEStatusSync();
		this.initializeTerminalUnloadVoiceDisconnect();
		Sound.setSelfDeafenedResolver(() => getEffectiveAudioState().effectiveDeaf);
		setSoundOutputDeviceIdResolver(() => VoiceSettings.getOutputDeviceId());
		this.prewarmNativeVoiceEngineInBackground('startup');
		(
			window as typeof window & {
				_mediaEngine?: MediaEngineFacade;
			}
		)._mediaEngine = this;
		logger.debug('MediaEngineFacade initialized');
	}

	setI18n(instance: I18n): void {
		this.i18n = instance;
	}

	private createSourceLifecycleBridge(): VoiceEngineV2AppSourceLifecycleBridge | null {
		const api = getElectronAPI()?.nativeScreenCapture;
		if (!api || typeof api.onLifecycleEvent !== 'function') return null;
		const subscribe = api.onLifecycleEvent.bind(api);
		const host = this.voiceEngineV2Host;
		return new VoiceEngineV2AppSourceLifecycleBridge({
			dispatch: (event) => {
				host.dispatch(event);
			},
			subscribe,
		});
	}

	private createVoiceEngineV2LifecycleDisposables(): ReadonlyArray<VoiceEngineV2AppLifecycleDisposable> {
		return [
			{
				name: 'app-tracking-and-stores',
				dispose: async () => {
					this.stopTracking();
					this.statsHostAdapter.reset();
					VoiceEngineV2AppRemoteSpeakingAdapter.clear();
					VoiceEngineV2AppSubscriptionAdapter.cleanup();
					VoiceEngineV2AppPermissionAdapter.reset();
					this.nativeVoiceFrameStatsBatcher.teardown();
					NativeVideoTileManager.clear();
					this.resetLocalMediaAndScreenShareTracking();
					this.voiceEngineV2Participants.clear();
				},
			},
			{
				name: 'native-voice-runtime-state',
				dispose: async () => {
					this.clearNativeVoiceConnectSession();
					this.clearPendingNativeLocalMediaReconnect();
					this.stopNativeVoiceStatsSession();
					this.lastNativeVoiceServerUpdate = null;
				},
			},
			{
				name: 'native-voice-timers',
				dispose: async () => {
					this.clearNativeVoiceTransportReconnect();
					this.clearNativeVoiceConnectRetry();
				},
			},
			{
				name: 'native-voice-bindings',
				dispose: async () => {
					this.disposeNativeVoiceEngineBindings();
				},
			},
		];
	}

	private transitionFacadeState(event: MediaEngineFacadeEvent): void {
		this.update(() => {
			this.facadeSnapshot = transitionMediaEngineFacadeSnapshot(this.facadeSnapshot, event);
		});
	}

	private prewarmNativeVoiceEngineInBackground(reason: string): void {
		void this.voiceEngineV2Host
			.runAndWait(
				() => {
					this.voiceEngineV2Controller.prewarm();
				},
				{description: `voice engine v2 prewarm (${reason})`},
			)
			.then(() => {
				logger.info('Voice engine v2 prewarm completed', {reason});
			})
			.catch((error) => {
				logger.warn('Voice engine v2 prewarm failed', {reason, error});
			});
	}

	private initializeEngineStoreSync(): void {
		const forwardChange = () => {
			this.scheduleLocalStreamCodecReconcile();
			this.emitChange();
		};
		this.voiceEngineV2ProjectionStore.subscribe(forwardChange);
		voiceEngineV2AppConnectionHostAdapter.subscribe(forwardChange);
		voiceEngineV2AppVoiceStateAdapter.subscribe(forwardChange);
		this.statsHostAdapter.subscribe(forwardChange);
		NativeVoiceStatsStore.subscribe(forwardChange);
		NativeVideoTileManager.subscribe(forwardChange);
		NativeVoiceE2EEStore.subscribe(forwardChange);
		AdaptiveScreenShareEngine.subscribe(forwardChange);
		voiceEngineV2AppMediaExecutionAdapter.subscribe(forwardChange);
		voiceEngineV2AppScreenShareExecutionAdapter.subscribe(forwardChange);
		VoiceEngineV2AppSubscriptionAdapter.subscribe(forwardChange);
		VoiceEngineV2AppPermissionAdapter.subscribe(forwardChange);
		voiceEngineV2AppDebugLoggingHostAdapter.subscribe(forwardChange);
		ScreenSharePublicationMigration.subscribe(forwardChange);
	}

	private initializeVoiceEngineV2ConnectionLifecycleSync(): void {
		assert.ok(this.voiceEngineV2Host != null, 'connection lifecycle sync requires the v2 host');
		assert.equal(
			this.voiceEngineV2EstablishedConnectionKey,
			null,
			'connection lifecycle sync must initialize before any establishment',
		);
		voiceEngineV2AppConnectionHostAdapter.subscribe(() => this.syncVoiceEngineV2ConnectionLifecycle());
		voiceEngineV2AppConnectionHostAdapter.subscribe(() => this.syncVoiceConnectFailureToast());
		this.syncVoiceEngineV2ConnectionLifecycle();
	}

	private syncVoiceConnectFailureToast(): void {
		const failed = voiceEngineV2AppConnectionHostAdapter.connectFailed;
		if (failed === this.lastVoiceConnectFailed) return;
		this.lastVoiceConnectFailed = failed;
		if (!failed) return;
		if (!this.i18n) return;
		ToastCommands.createToast({
			type: 'error',
			children: this.i18n._(VOICE_CONNECTION_FAILED_DESCRIPTOR),
		});
	}

	private syncVoiceEngineV2ConnectionLifecycle(): void {
		const state = voiceEngineV2AppConnectionHostAdapter.connectionState;
		assert.equal(typeof state.connected, 'boolean', 'connection state must expose a boolean connected flag');
		assert.equal(typeof state.reconnecting, 'boolean', 'connection state must expose a boolean reconnecting flag');
		if (state.reconnecting) return;
		const key = state.connected ? `${state.connectionId ?? ''}|${state.voiceServerEndpoint ?? ''}` : null;
		const previousKey = this.voiceEngineV2EstablishedConnectionKey;
		if (key === previousKey) return;
		this.voiceEngineV2EstablishedConnectionKey = key;
		if (key === null) {
			const transition = this.voiceEngineV2Host.dispatch({
				type: 'connection.remoteDisconnected',
				reason: this.resolveVoiceEngineV2RemoteDisconnectReason(),
			});
			assert.notEqual(
				transition.snapshot.connection.status,
				'connected',
				'v2 snapshot must not stay connected after external disconnect',
			);
			return;
		}
		if (previousKey !== null) {
			this.voiceEngineV2Host.dispatch({type: 'connection.remoteDisconnected', reason: 'replaced'});
		}
		const transition = this.voiceEngineV2Host.dispatch({
			type: 'connection.externallyEstablished',
			options: {url: state.voiceServerEndpoint ?? '', token: ''},
		});
		if (transition.snapshot.lifecycle.tearingDown) {
			this.voiceEngineV2EstablishedConnectionKey = null;
			logger.info('Voice engine v2 external establishment observed during lifecycle teardown', {
				connectionStatus: transition.snapshot.connection.status,
			});
			return;
		}
		assert.equal(
			transition.snapshot.connection.status,
			'connected',
			'v2 snapshot must be connected after external establishment',
		);
		this.syncVoiceEngineV2AudioControlsFromAppState();
	}

	private resolveVoiceEngineV2RemoteDisconnectReason(): VoiceEngineV2DisconnectReason {
		const reason = voiceEngineV2AppConnectionHostAdapter.localDisconnectReason;
		assert.ok(reason === null || typeof reason === 'string', 'local disconnect reason must be a string or null');
		switch (reason) {
			case 'error':
			case 'abort':
				return 'network';
			case 'server':
				return 'server';
			case 'channelMove':
			case 'replaced':
				return 'replaced';
			case 'user':
			case null:
				return 'user';
		}
	}

	private get pendingSessionRestore(): MediaEngineFacadePendingSessionRestore | null {
		return this.facadeSnapshot.context.pendingSessionRestore;
	}

	private get pendingUserMove(): {guildId: string | null; channelId: string} | null {
		return this.facadeSnapshot.context.pendingUserMove;
	}

	private get pendingServerDisconnectConnectionId(): string | null {
		return this.facadeSnapshot.context.pendingServerDisconnectConnectionId;
	}

	private get pendingScreenShareReconnect(): ScreenShareReconnectSnapshot | null {
		return this.facadeSnapshot.context.pendingScreenShareReconnect as ScreenShareReconnectSnapshot | null;
	}

	get voiceEngineV2Controller(): VoiceEngineV2Controller {
		return this.voiceEngineV2Host.controller;
	}

	get voiceEngineV2Snapshot(): VoiceEngineV2Snapshot {
		return this.voiceEngineV2ProjectionStore.snapshot;
	}

	get voiceEngineV2Model(): VoiceEngineV2Model {
		return this.voiceEngineV2ProjectionStore.model;
	}

	get room(): Room | null {
		return voiceEngineV2AppConnectionHostAdapter.room;
	}

	get guildId(): string | null {
		return voiceEngineV2AppConnectionHostAdapter.guildId;
	}

	get channelId(): string | null {
		return voiceEngineV2AppConnectionHostAdapter.channelId;
	}

	get connectionId(): string | null {
		return voiceEngineV2AppConnectionHostAdapter.connectionId;
	}

	get connected(): boolean {
		return voiceEngineV2AppConnectionHostAdapter.connected;
	}

	get connecting(): boolean {
		return voiceEngineV2AppConnectionHostAdapter.connecting;
	}

	get reconnecting(): boolean {
		return voiceEngineV2AppConnectionHostAdapter.reconnecting;
	}

	get connectFailed(): boolean {
		return voiceEngineV2AppConnectionHostAdapter.connectFailed;
	}

	get connectFailureReason(): VoiceConnectionFailureReason {
		return voiceEngineV2AppConnectionHostAdapter.connectFailureReason;
	}

	get connectFailedTarget(): {guildId: string | null; channelId: string} | null {
		return voiceEngineV2AppConnectionHostAdapter.connectFailedTarget;
	}

	get voiceServerEndpoint(): string | null {
		return voiceEngineV2AppConnectionHostAdapter.voiceServerEndpoint;
	}

	get voiceDebugLoggingActive(): boolean {
		return voiceEngineV2AppDebugLoggingHostAdapter.active;
	}

	get voiceDebugLoggingToggleInFlight(): boolean {
		return voiceEngineV2AppDebugLoggingHostAdapter.toggleInFlight;
	}

	async setVoiceDebugLoggingEnabled(enabled: boolean): Promise<void> {
		await voiceEngineV2AppDebugLoggingHostAdapter.setEnabled(enabled);
	}

	get connectionContext(): VoiceEngineConnectionContext {
		return {
			guildId: this.guildId,
			channelId: this.channelId,
			connectionId: this.connectionId,
			connected: this.connected,
			connecting: this.connecting,
			reconnecting: this.reconnecting,
		};
	}

	private get nativeVoiceStatsActive(): boolean {
		if (this.room) return false;
		if (!isNativeVoiceEngineSelected()) return false;
		return (
			voiceEngineV2AppConnectionHostAdapter.connected ||
			voiceEngineV2AppConnectionHostAdapter.reconnecting ||
			NativeVoiceStatsStore.stats != null ||
			NativeVoiceStatsStore.currentLatency != null
		);
	}

	private get nativeVoiceParticipantCount(): number {
		const participantCount = Object.keys(this.voiceEngineV2Participants.participants).length;
		return Math.max(participantCount, this.connected ? 1 : 0);
	}

	get participants(): Readonly<Record<string, LivekitParticipantSnapshot>> {
		return this.voiceEngineV2Participants.participants;
	}

	get localConnectionQuality(): VoiceConnectionQuality | LiveKitConnectionQuality {
		if (this.nativeVoiceStatsActive) {
			return this.voiceEngineV2Participants.getLocalParticipant()?.connectionQuality ?? 'unknown';
		}
		return this.room?.localParticipant?.connectionQuality ?? 'unknown';
	}

	get connectionVoiceStates(): Readonly<Record<string, NormalizedVoiceState>> {
		return voiceEngineV2AppVoiceStateAdapter.getConnectionVoiceStates();
	}

	get currentLatency(): number | null {
		if (this.nativeVoiceStatsActive) return NativeVoiceStatsStore.currentLatency;
		return this.statsHostAdapter.currentLatency;
	}

	get averageLatency(): number | null {
		if (this.nativeVoiceStatsActive) return NativeVoiceStatsStore.averageLatency;
		return this.statsHostAdapter.averageLatency;
	}

	get latencyHistory(): Array<VoiceEngineV2LatencyDataPoint> {
		if (this.nativeVoiceStatsActive) return NativeVoiceStatsStore.latencyHistory;
		return this.statsHostAdapter.latencyHistory;
	}

	get voiceStats(): VoiceEngineV2VoiceStats {
		if (this.nativeVoiceStatsActive) {
			return NativeVoiceStatsStore.getVoiceStats({participantCount: this.nativeVoiceParticipantCount});
		}
		return this.statsHostAdapter.voiceStats;
	}

	get perTrackStats(): Array<VoiceEngineV2PerTrackStats> {
		if (this.nativeVoiceStatsActive) return NativeVoiceStatsStore.perTrackStats;
		return this.statsHostAdapter.perTrackStats;
	}

	get statsTimeSeries(): Array<VoiceEngineV2StatsSample> {
		if (this.nativeVoiceStatsActive) return NativeVoiceStatsStore.statsTimeSeries;
		return this.statsHostAdapter.statsTimeSeries;
	}

	get publisherTransport(): VoiceEngineV2TransportInfo | null {
		if (this.nativeVoiceStatsActive) return null;
		return this.statsHostAdapter.publisherTransport;
	}

	get subscriberTransport(): VoiceEngineV2TransportInfo | null {
		if (this.nativeVoiceStatsActive) return null;
		return this.statsHostAdapter.subscriberTransport;
	}

	get reconnectionCount(): number {
		return this.statsHostAdapter.reconnectionCount;
	}

	get estimatedLatency(): number | null {
		return this.statsHostAdapter.estimatedLatency;
	}

	get displayLatency(): number | null {
		const measured = this.currentLatency;
		return measured !== null ? measured : this.estimatedLatency;
	}

	refreshMicrophoneFromSettings(): void {
		void this.refreshMicrophoneFromCurrentEngine().catch((error) => {
			logger.warn('Failed to refresh microphone from settings', {error});
		});
	}

	private async refreshMicrophoneFromCurrentEngine(): Promise<void> {
		if (await shouldUseNativeVoiceEngine()) {
			if (this.voiceEngineV2Snapshot.microphone.published == null) {
				return;
			}
			await this.refreshNativeMicrophoneFromSettings();
			return;
		}
		await voiceEngineV2AppMediaExecutionAdapter.refreshMicrophone(this.room, {forceRepublish: true});
	}

	refreshCameraBackgroundFromSettings(): void {
		void this.refreshCameraBackgroundFromCurrentEngine().catch((error) => {
			logger.warn('Failed to refresh camera background from settings', {error});
		});
	}

	private async refreshCameraBackgroundFromCurrentEngine(): Promise<void> {
		if (await shouldUseNativeVoiceEngine()) {
			if (!this.readNativeCameraActualEnabled()) {
				this.syncVideoBackgroundFramePumpInBackground('camera-background-refresh');
				return;
			}
			try {
				await requireNativeVoiceEngine().updateCameraCapture(await this.getNativeCameraPublishParams());
			} catch (error) {
				logger.warn('Native camera capture hot update failed; falling back to camera republish', {error});
				await this.setCameraEnabledViaEngine(true, {sendUpdate: false});
			}
			this.syncVideoBackgroundFramePumpInBackground('camera-background-refresh');
			return;
		}
		await voiceEngineV2AppMediaExecutionAdapter.refreshCameraBackground();
	}

	refreshCameraCaptureFromSettings(): void {
		void this.refreshCameraCaptureFromCurrentEngine().catch((error) => {
			logger.warn('Failed to refresh camera capture from settings', {error});
		});
	}

	private async refreshCameraCaptureFromCurrentEngine(): Promise<void> {
		if (await shouldUseNativeVoiceEngine()) {
			if (!this.readNativeCameraActualEnabled()) {
				return;
			}
			await this.republishNativeCameraForCaptureChange();
			return;
		}
		await voiceEngineV2AppMediaExecutionAdapter.refreshCameraCapture();
	}

	private async republishNativeCameraForCaptureChange(): Promise<void> {
		this.clearNativeCameraLocalPreview();
		try {
			await requireNativeVoiceEngine().unpublishCamera();
			await this.publishNativeCameraFromSettings();
		} catch (error) {
			logger.warn('Native camera resolution republish failed; attempting to restore camera', {error});
			await this.setCameraEnabledViaEngine(true, {sendUpdate: false});
		}
		this.syncVideoBackgroundFramePumpInBackground('camera-resolution-refresh');
	}

	async refreshActiveScreenShareCodecNegotiation(): Promise<void> {
		const room = this.room;
		const selection = isNativeVoiceEngineSelected()
			? await ScreenShareCodecNegotiation.publishLocalCapabilitiesNative('manual')
			: await ScreenShareCodecNegotiation.publishLocalCapabilities(room, 'manual', {});
		const codec =
			selection?.codec ??
			ScreenShareCodecNegotiation.selectScreenShareCodec(VoiceSettings.getPreferredScreenShareCodec());
		await voiceEngineV2AppScreenShareExecutionAdapter.renegotiateActiveScreenShareCodec(room, codec, 'manual', {
			force: true,
		});
	}

	private reconcileLocalAudioStateInBackground(reason: string): void {
		void this.reconcileLocalAudioState(reason).catch((error) => {
			logger.warn('Local audio reconciliation failed', {reason, error});
		});
	}

	private clearNativeVoiceTransportReconnect(): void {
		if (this.nativeVoiceTransportReconnectTimeoutId !== null) {
			window.clearTimeout(this.nativeVoiceTransportReconnectTimeoutId);
			this.nativeVoiceTransportReconnectTimeoutId = null;
		}
	}

	private clearNativeVoiceConnectRetryTimeout(): void {
		if (this.nativeVoiceConnectRetryTimeoutId !== null) {
			window.clearTimeout(this.nativeVoiceConnectRetryTimeoutId);
			this.nativeVoiceConnectRetryTimeoutId = null;
		}
	}

	private clearNativeVoiceConnectRetry(): void {
		this.clearNativeVoiceConnectRetryTimeout();
		this.nativeVoiceConnectRetryCounts.clear();
	}

	private clearNativeVoiceConnectSession(): void {
		this.nativeVoiceConnectionLifecycle.clearSession();
	}

	private isVoiceConnectionCurrentForNativeAttempt(attempt: VoiceEngineV2AppNativeVoiceConnectAttempt): boolean {
		return this.nativeVoiceConnectionLifecycle.isCurrentAttemptForConnection(
			attempt,
			voiceEngineV2AppConnectionHostAdapter.connectionState,
		);
	}

	private isDuplicateNativeVoiceServerUpdate({
		guildId,
		channelId,
		connectionId,
		endpoint,
		token,
	}: {
		guildId: string | null;
		channelId: string;
		connectionId: string | null;
		endpoint: string;
		token: string;
	}): boolean {
		return this.nativeVoiceConnectionLifecycle.isDuplicateServerUpdate(
			{guildId, channelId, connectionId, endpoint, token},
			voiceEngineV2AppConnectionHostAdapter.connectionState,
			this.lastNativeVoiceServerUpdate?.token ?? null,
		);
	}

	private clearPendingNativeLocalMediaReconnect(): void {
		this.pendingNativeLocalMediaReconnect = null;
	}

	private prepareNativeLocalMediaReconnect(reason: string): void {
		const previous = this.pendingNativeLocalMediaReconnect;
		const restoreVideo = previous?.restoreVideo === true || LocalVoiceState.getSelfVideo();
		const restoreStream = previous?.restoreStream === true || LocalVoiceState.getSelfStream();
		const screenShare =
			previous?.screenShare ??
			(restoreStream ? voiceEngineV2AppScreenShareExecutionAdapter.prepareNativeScreenShareReconnect() : null);
		const screenShareRelease =
			previous?.screenShareRelease ??
			(screenShare
				? voiceEngineV2AppScreenShareExecutionAdapter
						.releaseNativeScreenShareForReconnect(screenShare)
						.catch((error) => {
							logger.warn('Failed to release native screen share before reconnect restore', {reason, error});
						})
				: null);
		if (!restoreVideo && !restoreStream && !screenShare) {
			return;
		}
		this.pendingNativeLocalMediaReconnect = {
			connectionId: voiceEngineV2AppConnectionHostAdapter.connectionId,
			restoreVideo,
			restoreStream,
			screenShare,
			screenShareRelease,
		};
		logger.debug('Prepared native local media reconnect restore', {
			reason,
			connectionId: voiceEngineV2AppConnectionHostAdapter.connectionId,
			restoreVideo,
			restoreStream,
			hasScreenShareSnapshot: screenShare !== null,
		});
	}

	private getPendingNativeLocalTrackRestoreIntent(): {restoreVideo: boolean; restoreStream: boolean} {
		const pending = this.pendingNativeLocalMediaReconnect;
		return {
			restoreVideo: pending?.restoreVideo === true,
			restoreStream: pending?.restoreStream === true,
		};
	}

	private shouldSuppressNativeLocalTrackState(source: VoiceTrackSource, enabled: boolean): boolean {
		const {restoreVideo, restoreStream} = this.getPendingNativeLocalTrackRestoreIntent();
		return shouldSuppressNativeLocalTrackStateDuringReconnect({
			source,
			enabled,
			reconnecting:
				voiceEngineV2AppConnectionHostAdapter.reconnecting || this.pendingNativeLocalMediaReconnect !== null,
			restoreVideo,
			restoreStream,
		});
	}

	private restoreNativeLocalMediaStateInBackground(reason: string): void {
		void this.restoreNativeLocalMediaState(reason).catch((error) => {
			logger.warn('Native local media reconnect restore failed', {reason, error});
		});
	}

	private async restoreNativeLocalMediaState(reason: string): Promise<void> {
		const snapshot = this.pendingNativeLocalMediaReconnect;
		if (!snapshot) return;
		if (snapshot.connectionId && voiceEngineV2AppConnectionHostAdapter.connectionId !== snapshot.connectionId) {
			logger.debug('Dropping stale native local media reconnect restore', {
				reason,
				snapshotConnectionId: snapshot.connectionId,
				currentConnectionId: voiceEngineV2AppConnectionHostAdapter.connectionId,
			});
			this.clearPendingNativeLocalMediaReconnect();
			return;
		}
		this.clearPendingNativeLocalMediaReconnect();
		await snapshot.screenShareRelease;
		const screenShareSnapshot = snapshot.screenShare;
		await this.restoreLocalMedia({
			reason,
			room: this.room,
			restoreCamera: snapshot.restoreVideo && LocalVoiceState.getSelfVideo(),
			restoreStream: snapshot.restoreStream && LocalVoiceState.getSelfStream(),
			restoreStreamFromSnapshot: screenShareSnapshot
				? () => voiceEngineV2AppScreenShareExecutionAdapter.restoreNativeScreenShareReconnect(screenShareSnapshot)
				: null,
			streamSnapshotFallback: true,
			streamPlaySound: false,
			settleStreamStateWhenNotRestored: true,
			toastWhenStreamNotRestored: true,
		});
	}

	private async restoreLocalMedia(args: {
		reason: string;
		room: Room | null;
		restoreCamera: boolean;
		restoreStream: boolean;
		restoreStreamFromSnapshot: (() => Promise<boolean>) | null;
		streamSnapshotFallback: boolean;
		streamPlaySound: boolean;
		settleStreamStateWhenNotRestored: boolean;
		toastWhenStreamNotRestored: boolean;
	}): Promise<void> {
		assert.equal(typeof args.restoreCamera, 'boolean', 'restoreLocalMedia requires a camera flag');
		assert.equal(typeof args.restoreStream, 'boolean', 'restoreLocalMedia requires a stream flag');
		if (args.restoreCamera) {
			try {
				await this.setCameraEnabled(true, {
					deviceId: VoiceSettings.getVideoDeviceId(),
					sendUpdate: false,
				});
			} catch (error) {
				logger.warn('Failed to restore camera during local media restore', {reason: args.reason, error});
			}
		}
		if (!args.restoreStream) return;
		let restored = false;
		if (args.restoreStreamFromSnapshot) {
			restored = await args.restoreStreamFromSnapshot();
			if (!restored && !args.streamSnapshotFallback) {
				voiceEngineV2AppMediaStateAdapter.applyScreenShareState(false, {sendUpdate: false});
				return;
			}
		}
		let streamRestoreFailed = false;
		if (!restored) {
			try {
				await voiceEngineV2AppScreenShareExecutionAdapter.setScreenShareEnabled(args.room, true, {
					sendUpdate: false,
					...(args.streamPlaySound ? {} : {playSound: false}),
				});
				restored = LocalVoiceState.getSelfStream();
			} catch (error) {
				streamRestoreFailed = true;
				logger.warn('Failed to restore screen share during local media restore', {reason: args.reason, error});
			}
		}
		if (restored) return;
		if (streamRestoreFailed || args.settleStreamStateWhenNotRestored) {
			voiceEngineV2AppMediaStateAdapter.applyScreenShareState(false, {sendUpdate: false});
		}
		if (args.toastWhenStreamNotRestored && this.i18n) {
			ToastCommands.createToast({
				type: 'info',
				children: this.i18n._(RECONNECT_SUCCEEDED_PICK_A_SCREEN_AGAIN_IF_YOU_DESCRIPTOR),
			});
		}
	}

	private disposeNativeVoiceEngineBindings(clearStats = true): void {
		this.clearNativeCameraLocalPreview();
		this.nativeVoiceEngineV2BridgeEventDisposer?.();
		this.nativeVoiceEngineV2BridgeEventDisposer = null;
		this.nativeVoiceDeviceSyncDisposer?.();
		this.nativeVoiceDeviceSyncDisposer = null;
		this.nativeVoiceDataProtocolDisposer?.();
		this.nativeVoiceDataProtocolDisposer = null;
		VoiceEngineV2AppSubscriptionAdapter.cleanup();
		this.nativeVoiceFrameStatsBatcher.teardown();
		NativeVideoTileManager.clear();
		NativeVoiceE2EEStore.clear();
		this.stopNativeVoiceStatsSession(clearStats);
		this.clearNativeVoiceConnectSession();
	}

	private isGatewayVoiceStateActiveForConnection(
		connectionId: string,
		guildId: string | null,
		channelId: string,
	): boolean {
		const voiceState = voiceEngineV2AppVoiceStateAdapter.getVoiceStateByConnectionId(connectionId);
		if (!voiceState?.channel_id) return false;
		const expectedGuildId = guildId ?? ME;
		const actualGuildId = voiceState.guild_id ?? ME;
		return voiceState.channel_id === channelId && actualGuildId === expectedGuildId;
	}

	private initializeLocalAudioStateSync(): void {
		let previousState = {
			selfMute: LocalVoiceState.getSelfMute(),
			selfDeaf: LocalVoiceState.getSelfDeaf(),
		};
		LocalVoiceState.subscribe(() => {
			const currentState = {
				selfMute: LocalVoiceState.getSelfMute(),
				selfDeaf: LocalVoiceState.getSelfDeaf(),
			};
			if (currentState.selfMute === previousState.selfMute && currentState.selfDeaf === previousState.selfDeaf) {
				return;
			}
			logger.debug('Local audio state changed, reconciling active voice session', {
				previousState,
				currentState,
			});
			previousState = currentState;
			this.syncVoiceEngineV2AudioControlsFromAppState();
			this.reconcileLocalAudioStateInBackground('local audio state change');
		});
	}

	private initializeE2EEStatusSync(): void {
		let previousRoom: Room | null = null;
		let previousStatus: ChannelE2EEStatus | null = null;
		const syncE2EEStatus = () => {
			const room = voiceEngineV2AppConnectionHostAdapter.room;
			const channelId = voiceEngineV2AppConnectionHostAdapter.channelId;
			const status =
				voiceEngineV2AppConnectionHostAdapter.connected && channelId
					? computeChannelE2EEStatus(voiceEngineV2AppConnectionHostAdapter.guildId, channelId)
					: null;
			if (room === previousRoom && status === previousStatus) {
				return;
			}
			previousRoom = room;
			previousStatus = status;
			if (!room) return;
			if (status === 'encrypted') {
				void room.setE2EEEnabled(true).catch((error) => {
					logger.warn('Failed to enable E2EE on room', error);
				});
			} else if (status === 'broken') {
				void room.setE2EEEnabled(false).catch((error) => {
					logger.warn('Failed to disable E2EE on room after capability mix detected', error);
				});
			}
		};
		voiceEngineV2AppConnectionHostAdapter.subscribe(syncE2EEStatus);
		voiceEngineV2AppVoiceStateAdapter.subscribe(syncE2EEStatus);
	}

	private initializeVoiceSessionRestoreSync(): void {
		this.voiceSessionRestoreSync?.dispose();
		this.voiceSessionRestoreSync = createVoiceSessionRestoreSync();
	}

	private initializeTerminalUnloadVoiceDisconnect(): void {
		const handlePageHide = (event: PageTransitionEvent) => {
			if (event.persisted) return;
			this.disconnectVoiceForTerminalUnload('pagehide');
		};
		const handleBeforeUnload = () => this.disconnectVoiceForTerminalUnload('beforeunload');
		const handleUnload = () => this.disconnectVoiceForTerminalUnload('unload');
		window.addEventListener('pagehide', handlePageHide, {capture: true});
		window.addEventListener('beforeunload', handleBeforeUnload, {capture: true});
		window.addEventListener('unload', handleUnload, {capture: true});
	}

	private disconnectVoiceTransportForTerminalUnload(): void {
		voiceEngineV2AppConnectionHostAdapter.disconnectTransportsForTerminalUnload();
	}

	private discardVoiceConnection(connectionId: string, options: {clearLocalState?: boolean} = {}): void {
		voiceEngineV2AppVoiceStateAdapter.removeVoiceStateConnection(connectionId);
		for (const trackSid of NativeVideoTileManager.unregisterConnection(connectionId)) {
			this.nativeVoiceFrameStatsBatcher.removeTrack(trackSid);
		}
		this.voiceEngineV2Participants.discardConnection(connectionId);
		if (options.clearLocalState) {
			LocalVoiceState.clearConnectionState(connectionId);
		}
	}

	private handleCurrentLocalVoiceStateRemoval(guildId: string | null, voiceState: VoiceState): boolean {
		const connectionId = voiceState.connection_id;
		if (!connectionId) return false;
		const removalInput = {
			voiceStateConnectionId: connectionId,
			voiceStateChannelId: voiceState.channel_id,
			currentConnectionId: voiceEngineV2AppConnectionHostAdapter.connectionId,
			currentChannelId: voiceEngineV2AppConnectionHostAdapter.channelId,
			connected: voiceEngineV2AppConnectionHostAdapter.connected,
			connecting: voiceEngineV2AppConnectionHostAdapter.connecting,
		};
		if (shouldImmediatelyDisconnectMediaEngineForServerVoiceStateRemoval(removalInput)) {
			logger.info('Server removed current voice connection, tearing down local voice transport', {
				guildId,
				connectionId,
			});
			this.cancelPendingServerDisconnect();
			this.applyEngineGatewayEcho(null);
			void this.disconnectFromVoiceChannel('server');
			return true;
		}
		if (!shouldCancelMediaEngineReconnectForServerVoiceStateRemoval(removalInput)) return false;
		logger.info('Server confirmed current voice connection removal, cancelling auto reconnect', {
			guildId,
			connectionId,
		});
		this.cancelPendingServerDisconnect();
		this.applyEngineGatewayEcho(null);
		this.clearViewerStreamKeys();
		VoiceSessionRestore.clearSnapshot();
		this.discardVoiceConnection(connectionId, {clearLocalState: true});
		voiceEngineV2AppConnectionHostAdapter.markDisconnected('server');
		CallMediaPrefs.clearForCall(connectionId);
		return true;
	}

	disconnectVoiceForTerminalUnload(reason: TerminalUnloadVoiceDisconnectReason): void {
		if (this.terminalUnloadVoiceDisconnectSent) return;
		const {guildId, channelId, connectionId, connected, connecting, room} =
			voiceEngineV2AppConnectionHostAdapter.connectionState;
		const hasVoiceTransport = voiceEngineV2AppConnectionHostAdapter.hasTerminalUnloadTransports();
		if (!connected && !connecting && !channelId && !connectionId && !room && !hasVoiceTransport) return;
		this.terminalUnloadVoiceDisconnectSent = true;
		saveCurrentVoiceSessionRestoreSnapshot();
		logger.info('Terminal page unload voice disconnect', {
			reason,
			guildId,
			channelId,
			connectionId,
			connected,
			connecting,
			hasRoom: room != null,
			hasVoiceTransport,
		});
		this.clearNativeVoiceTransportReconnect();
		this.clearNativeVoiceConnectRetry();
		this.clearNativeVoiceConnectSession();
		this.clearPendingNativeLocalMediaReconnect();
		this.stopNativeVoiceStatsSession();
		voiceEngineV2AppScreenShareExecutionAdapter.stopNativeScreenShareForTerminalUnload();
		const hasSpecificConnection = connectionId != null;
		GatewayConnection.sendTerminalVoiceDisconnect(
			{
				guild_id: hasSpecificConnection ? guildId : null,
				channel_id: null,
				self_mute: true,
				self_deaf: true,
				self_video: false,
				self_stream: false,
				viewer_stream_keys: [],
				connection_id: connectionId ?? null,
			},
			`Terminal voice disconnect: ${reason}`,
		);
		this.disconnectVoiceTransportForTerminalUnload();
	}

	private resolveVoiceConnectChannel(guildId: string | null, channelId: string): Channel | null {
		const channel = Channels.getChannel(channelId) ?? null;
		if (!channel) return null;
		if (channel.type === ChannelTypes.GUILD_VOICE) {
			if (guildId && channel.guildId !== guildId) return null;
			return channel;
		}
		if (guildId) return null;
		if (channel.type === ChannelTypes.DM || channel.type === ChannelTypes.GROUP_DM) {
			return channel;
		}
		return null;
	}

	private showVoiceErrorModal(message: MessageDescriptor, dataFlx: string, values?: Record<string, unknown>): void {
		const i18n = this.i18n;
		if (!i18n) return;
		showGenericErrorModal({
			title: () => i18n._(SOMETHING_WENT_WRONG_DESCRIPTOR),
			message: () => i18n._(message, values),
			dataFlx,
		});
	}

	private showVoiceChannelUnavailableErrorModal(): void {
		if (!this.i18n) return;
		this.showVoiceErrorModal(
			VOICE_CHANNEL_NO_LONGER_AVAILABLE_DESCRIPTOR,
			'voice.media-engine-facade.channel-unavailable-error-modal',
		);
	}

	private clearUnavailableVoiceTarget(
		channelId: string,
		reason: 'preflight' | 'gateway-error' | 'channel-delete',
		options: {showToast?: boolean} = {},
	): void {
		logger.info('Clearing unavailable voice target', {channelId, reason});
		this.transitionFacadeState({type: 'unavailableTarget.clear', channelId});
		VoiceSessionRestore.clearSnapshotForChannel(channelId);
		voiceEngineV2AppConnectionHostAdapter.forgetReconnectChannel(channelId);
		if (options.showToast) {
			this.showVoiceChannelUnavailableErrorModal();
		}
	}

	handleChannelDelete(channelId: string): void {
		this.clearUnavailableVoiceTarget(channelId, 'channel-delete');
		if (voiceEngineV2AppConnectionHostAdapter.channelId === channelId) {
			void this.disconnectFromVoiceChannel('server');
		}
	}

	async retryFailedVoiceConnection(): Promise<void> {
		const target = voiceEngineV2AppConnectionHostAdapter.connectFailedTarget;
		if (!target) {
			logger.warn('Retry requested with no failed voice connection target');
			return;
		}
		this.lastVoiceConnectFailed = false;
		voiceEngineV2AppConnectionHostAdapter.resetConnectionState();
		await this.connectToVoiceChannel(target.guildId, target.channelId, {skipChannelGate: true});
	}

	dismissFailedVoiceConnection(): void {
		if (!voiceEngineV2AppConnectionHostAdapter.connectFailed) return;
		this.lastVoiceConnectFailed = false;
		voiceEngineV2AppConnectionHostAdapter.resetConnectionState();
	}

	async connectToVoiceChannel(
		guildId: string | null,
		channelId: string,
		options: ConnectToVoiceChannelOptions = {},
	): Promise<void> {
		this.terminalUnloadVoiceDisconnectSent = false;
		VoiceCallLayout.reset();
		const voiceChannel = this.resolveVoiceConnectChannel(guildId, channelId);
		const resolvedGuildId = voiceChannel ? (guildId ?? voiceChannel.guildId ?? null) : guildId;
		const currentUserId = Authentication.currentUserId;
		const isTimedOut =
			voiceChannel && resolvedGuildId && currentUserId
				? (GuildMembers.getMember(resolvedGuildId, currentUserId)?.isTimedOut() ?? false)
				: false;
		const currentUser = Users.getCurrentUser();
		const isUnclaimed = !(currentUser?.isClaimed() ?? false);
		const guild = resolvedGuildId ? Guilds.getGuild(resolvedGuildId) : null;
		const isOwner = guild?.isOwner(currentUserId) ?? false;
		const channel = voiceChannel ?? Channels.getChannel(channelId);
		const blockedByMatureGate =
			Boolean(voiceChannel) &&
			!options.skipChannelGate &&
			GuildMatureContentAgree.shouldShowGate({
				channelId,
				guildId: resolvedGuildId,
			});
		const preflightDecision = selectMediaEngineConnectPreflightDecision({
			targetAvailable: Boolean(voiceChannel),
			isTimedOut: Boolean(isTimedOut),
			isUnclaimed,
			isGuildOwner: resolvedGuildId ? isOwner : true,
			isDirectMessage: channel?.type === ChannelTypes.DM,
			hasGatewaySocket: Boolean(GatewayConnection.socket),
			blockedByMatureGate,
			channelLimitAllowed: true,
		});
		if (preflightDecision.type === 'cleanup-unavailable-target') {
			this.clearUnavailableVoiceTarget(channelId, 'preflight', {showToast: preflightDecision.showToast});
			return;
		}
		if (preflightDecision.type === 'toast') {
			if (!this.i18n) {
				throw new Error('MediaEngineFacade: i18n not initialized');
			}
			const descriptor =
				preflightDecision.reason === 'timed-out'
					? YOU_CAN_T_JOIN_WHILE_YOU_RE_ON_DESCRIPTOR
					: preflightDecision.reason === 'claim-account-direct'
						? CLAIM_YOUR_ACCOUNT_TO_START_OR_JOIN_1_DESCRIPTOR
						: CLAIM_YOUR_ACCOUNT_TO_JOIN_VOICE_CHANNELS_YOU_DESCRIPTOR;
			this.showVoiceErrorModal(
				descriptor,
				`voice.media-engine-facade.preflight-${preflightDecision.reason}-error-modal`,
			);
			return;
		}
		if (preflightDecision.type === 'abort') {
			if (preflightDecision.reason === 'missing-gateway-socket') {
				logger.warn('No socket');
			}
			return;
		}
		if (preflightDecision.type === 'navigate-channel-gate') {
			NavigationCommands.selectChannel(resolvedGuildId ?? undefined, channelId);
			return;
		}
		const channelLimitAllowed = checkChannelLimit(resolvedGuildId, channelId);
		const channelLimitDecision = selectMediaEngineConnectPreflightDecision({
			targetAvailable: true,
			isTimedOut: false,
			isUnclaimed: false,
			isGuildOwner: true,
			isDirectMessage: false,
			hasGatewaySocket: true,
			blockedByMatureGate: false,
			channelLimitAllowed,
		});
		if (channelLimitDecision.type === 'abort') {
			return;
		}
		const shouldProceed = checkMultipleConnections(
			resolvedGuildId,
			channelId,
			async () =>
				this.connectDirectly(resolvedGuildId, channelId, {
					deferNavigationUntilConnected: options.deferNavigationUntilConnected,
					initialViewerStreamKeys: options.initialViewerStreamKeys,
				}),
			() =>
				this.connectDirectly(resolvedGuildId, channelId, {
					deferNavigationUntilConnected: options.deferNavigationUntilConnected,
					initialViewerStreamKeys: options.initialViewerStreamKeys,
				}),
			() => voiceEngineV2AppConnectionHostAdapter.clearInFlightConnect(),
		);
		if (!shouldProceed) return;
		await this.connectDirectly(resolvedGuildId, channelId, {
			deferNavigationUntilConnected: options.deferNavigationUntilConnected,
			initialViewerStreamKeys: options.initialViewerStreamKeys,
		});
	}

	private async connectDirectly(
		guildId: string | null,
		channelId: string,
		options: ConnectDirectlyOptions = {},
	): Promise<void> {
		VoiceCallLayout.reset();
		if (this.disconnectPromise) {
			logger.debug('Waiting for voice teardown before starting a new connection', {guildId, channelId});
			await this.disconnectPromise;
		}
		const initialDecision = selectMediaEngineConnectRequestDecision(this.facadeSnapshot, {
			guildId,
			channelId,
			connected: voiceEngineV2AppConnectionHostAdapter.connected,
			connecting: voiceEngineV2AppConnectionHostAdapter.connecting,
			currentGuildId: voiceEngineV2AppConnectionHostAdapter.guildId,
			currentChannelId: voiceEngineV2AppConnectionHostAdapter.channelId,
		});
		if (initialDecision.type === 'noop') return;
		if (
			shouldPrepareMicrophoneForVoiceConnect({
				guildId,
				channelId,
				selfMute: LocalVoiceState.getSelfMute(),
				selfDeaf: LocalVoiceState.getSelfDeaf(),
				hasUserSetMute: LocalVoiceState.getHasUserSetMute(),
				mutedByPermission: LocalVoiceState.getMutedByPermission(),
			})
		) {
			void voiceEngineV2AppMediaExecutionAdapter.prepareMicrophonePermissionForConnect().catch((error) => {
				logger.warn('Microphone permission warm-up before voice connect failed; joining listen-only', {error});
			});
		}
		const connectDecision = selectMediaEngineConnectRequestDecision(this.facadeSnapshot, {
			guildId,
			channelId,
			connected: voiceEngineV2AppConnectionHostAdapter.connected,
			connecting: voiceEngineV2AppConnectionHostAdapter.connecting,
			currentGuildId: voiceEngineV2AppConnectionHostAdapter.guildId,
			currentChannelId: voiceEngineV2AppConnectionHostAdapter.channelId,
		});
		if (connectDecision.type === 'noop') return;
		if (connectDecision.type === 'move-user') {
			this.transitionFacadeState({
				type: 'userMove.requested',
				guildId: connectDecision.guildId,
				channelId: connectDecision.channelId,
			});
			await this.disconnectForChannelMove('user');
		}
		this.clearNativeVoiceConnectRetry();
		this.clearNativeVoiceConnectSession();
		if (!voiceEngineV2AppConnectionHostAdapter.startConnection(guildId, channelId)) return;
		const initialViewerStreamKeys = replaceWatchedStreamKeys(options.initialViewerStreamKeys ?? [], {
			sync: false,
		}).keys;
		if (!options.deferNavigationUntilConnected) {
			this.navigateToVoiceChannel(guildId, channelId);
		}
		sendVoiceStateConnect(guildId, channelId, initialViewerStreamKeys);
	}

	async restoreVoiceSession(
		snapshot: VoiceSessionRestoreSnapshot,
		options?: {
			restoreVideo?: boolean;
			restoreStream?: boolean;
		},
	): Promise<void> {
		this.prepareVoiceSessionRestore(snapshot, options);
		await this.connectToVoiceChannel(snapshot.guildId, snapshot.channelId, {deferNavigationUntilConnected: true});
		if (
			!voiceEngineV2AppConnectionHostAdapter.connecting &&
			!(
				voiceEngineV2AppConnectionHostAdapter.connected &&
				voiceEngineV2AppConnectionHostAdapter.guildId === snapshot.guildId &&
				voiceEngineV2AppConnectionHostAdapter.channelId === snapshot.channelId
			)
		) {
			this.clearPreparedVoiceSessionRestore(snapshot);
		}
	}

	prepareVoiceSessionRestore(
		snapshot: VoiceSessionRestoreSnapshot,
		options?: {
			restoreVideo?: boolean;
			restoreStream?: boolean;
		},
	): void {
		this.transitionFacadeState({
			type: 'sessionRestore.prepare',
			guildId: snapshot.guildId,
			channelId: snapshot.channelId,
			restoreVideo: Boolean(options?.restoreVideo && snapshot.selfVideo),
			restoreStream: Boolean(options?.restoreStream && snapshot.selfStream),
		});
	}

	clearPreparedVoiceSessionRestore(snapshot?: Pick<VoiceSessionRestoreSnapshot, 'guildId' | 'channelId'>): void {
		if (!snapshot) {
			this.transitionFacadeState({type: 'sessionRestore.clear'});
			return;
		}
		this.transitionFacadeState({
			type: 'sessionRestore.clearTarget',
			guildId: snapshot.guildId,
			channelId: snapshot.channelId,
		});
	}

	private resetLocalMediaAndScreenShareTracking(): void {
		voiceEngineV2AppScreenShareExecutionAdapter.resetStreamTracking();
		voiceEngineV2AppMediaExecutionAdapter.resetStreamTracking();
	}

	private async stopActiveScreenShareForTeardown(context: 'disconnect' | 'channel_move'): Promise<void> {
		if (
			!LocalVoiceState.getSelfStream() &&
			!voiceEngineV2AppScreenShareExecutionAdapter.hasActiveScreenShareResources()
		)
			return;
		try {
			await voiceEngineV2AppScreenShareExecutionAdapter.setScreenShareEnabled(this.room, false, {
				sendUpdate: false,
				playSound: false,
			});
		} catch (error) {
			logger.warn('Failed to stop screen share during teardown', {context, error});
		}
	}

	async disconnectFromVoiceChannel(reason: 'user' | 'error' | 'server' = 'user'): Promise<void> {
		if (this.disconnectPromise) {
			logger.debug('Voice teardown already in progress', {reason});
			return this.disconnectPromise;
		}
		const disconnectPromise = this.runDisconnectFromVoiceChannel(reason);
		this.disconnectPromise = disconnectPromise;
		try {
			await disconnectPromise;
		} finally {
			if (this.disconnectPromise === disconnectPromise) {
				this.disconnectPromise = null;
			}
		}
	}

	private async runDisconnectFromVoiceChannel(reason: 'user' | 'error' | 'server'): Promise<void> {
		this.clearNativeVoiceTransportReconnect();
		this.clearNativeVoiceConnectRetry();
		this.clearNativeVoiceConnectSession();
		this.clearPendingNativeLocalMediaReconnect();
		this.stopNativeVoiceStatsSession();
		this.lastNativeVoiceServerUpdate = null;
		const {guildId, connectionId, connected, connecting, channelId} =
			voiceEngineV2AppConnectionHostAdapter.connectionState;
		if (!connected && !connecting && !channelId) {
			if (this.nativeVoiceEngineV2BridgeEventDisposer) {
				this.disposeNativeVoiceEngineBindings();
				try {
					await requireNativeVoiceEngine().disconnect();
				} catch (error) {
					logger.warn('Native voice engine disconnect failed', {error, reason});
				}
			}
			return;
		}
		logger.debug('Voice teardown starting', {guildId, channelId, reason});
		this.cancelPendingServerDisconnect();
		this.transitionFacadeState({type: 'disconnect.cleanupStarted', reason});
		await this.stopActiveScreenShareForTeardown('disconnect');
		if (this.nativeVoiceEngineV2BridgeEventDisposer) {
			this.disposeNativeVoiceEngineBindings();
			try {
				await requireNativeVoiceEngine().disconnect();
			} catch (error) {
				logger.warn('Native voice engine disconnect failed', {error, reason});
			}
		}
		this.clearViewerStreamKeys();
		this.stopTracking();
		this.statsHostAdapter.reset();
		if (reason === 'user' || reason === 'server') {
			VoiceSessionRestore.clearSnapshot();
		}
		if (reason !== 'server' && connectionId) {
			sendVoiceStateDisconnect(guildId, connectionId);
		}
		SoundCommands.playSound(SoundType.VoiceDisconnect);
		VoiceCallLayout.reset();
		voiceEngineV2AppMediaStateAdapter.resetLocalMediaState('disconnect');
		this.resetLocalMediaAndScreenShareTracking();
		this.voiceEngineV2Participants.clear();
		if (connectionId) {
			this.discardVoiceConnection(connectionId, {clearLocalState: true});
		} else {
			LocalVoiceState.clearConnectionState(connectionId);
		}
		voiceEngineV2AppConnectionHostAdapter.disconnectFromVoiceChannel(reason);
		if (reason === 'user' || reason === 'server') {
			VoiceSessionRestore.clearSnapshot();
		}
		if (connectionId) CallMediaPrefs.clearForCall(connectionId);
		logger.info('Voice teardown complete', {channelId, reason});
		this.transitionFacadeState({type: 'cleanup.complete'});
	}

	private async disconnectForChannelMove(reason: 'user' | 'server'): Promise<void> {
		this.clearNativeVoiceTransportReconnect();
		this.clearNativeVoiceConnectRetry();
		this.clearNativeVoiceConnectSession();
		this.clearPendingNativeLocalMediaReconnect();
		this.stopNativeVoiceStatsSession();
		this.lastNativeVoiceServerUpdate = null;
		const {guildId, connectionId, connected, connecting, channelId} =
			voiceEngineV2AppConnectionHostAdapter.connectionState;
		if (!connected && !connecting && !channelId) return;
		logger.debug('Voice teardown for channel move', {guildId, channelId});
		this.cancelPendingServerDisconnect();
		this.transitionFacadeState({type: 'channelMove.cleanupStarted', reason});
		await this.stopActiveScreenShareForTeardown('channel_move');
		this.clearViewerStreamKeys();
		this.stopTracking();
		this.statsHostAdapter.reset();
		if (reason === 'user' && connectionId) {
			sendVoiceStateDisconnect(guildId, connectionId);
		}
		VoiceCallLayout.reset();
		voiceEngineV2AppMediaStateAdapter.resetLocalMediaState('disconnect');
		this.resetLocalMediaAndScreenShareTracking();
		this.voiceEngineV2Participants.clear();
		if (reason === 'user' && connectionId) {
			this.discardVoiceConnection(connectionId, {clearLocalState: true});
		} else {
			LocalVoiceState.clearConnectionState(connectionId);
		}
		voiceEngineV2AppConnectionHostAdapter.disconnectForChannelMove();
		if (connectionId) CallMediaPrefs.clearForCall(connectionId);
		SoundCommands.playSound(SoundType.UserMove);
		this.transitionFacadeState({type: 'cleanup.complete'});
	}

	private scheduleDeferredServerDisconnect(connectionId: string): void {
		this.cancelPendingServerDisconnect();
		this.transitionFacadeState({type: 'serverDisconnect.schedule', connectionId});
		this.pendingServerDisconnectTimeout = setTimeout(() => {
			const currentVoiceState = voiceEngineV2AppVoiceStateAdapter.getVoiceStateByConnectionId(connectionId);
			const currentVoiceStateChannelId = currentVoiceState?.channel_id ?? null;
			if (
				shouldRunMediaEngineDeferredDisconnect(this.facadeSnapshot, {
					connectionId,
					currentConnectionId: voiceEngineV2AppConnectionHostAdapter.connectionId,
					connected: voiceEngineV2AppConnectionHostAdapter.connected,
					currentVoiceStateChannelId,
				})
			) {
				logger.info('Deferred disconnect executing - no VOICE_SERVER_UPDATE received', {connectionId});
				void this.disconnectFromVoiceChannel('server');
			} else if (currentVoiceStateChannelId) {
				logger.debug('Deferred disconnect ignored because voice state is active again', {
					connectionId,
					channelId: currentVoiceStateChannelId,
				});
			}
			this.pendingServerDisconnectTimeout = null;
			this.transitionFacadeState({type: 'serverDisconnect.timeoutElapsed', connectionId});
		}, DEFERRED_DISCONNECT_TIMEOUT_MS);
		logger.debug('Scheduled deferred disconnect', {connectionId, timeoutMs: DEFERRED_DISCONNECT_TIMEOUT_MS});
	}

	private isPushToTalkActive(): boolean {
		if (getActiveVoiceProcessingMode(VoiceSettings) === 'studio') return false;
		return Keybind.isPushToTalkEffective();
	}

	private isPushToMuteActive(): boolean {
		if (getActiveVoiceProcessingMode(VoiceSettings) === 'studio') return false;
		return Keybind.isPushToMuteEffective();
	}

	private getVoiceEngineV2AudioMode(): VoiceEngineV2AudioMode {
		if (this.isPushToTalkActive()) return 'pushToTalk';
		if (this.isPushToMuteActive()) return 'pushToMute';
		return 'voiceActivity';
	}

	private syncVoiceEngineV2AudioControlsFromAppState(): void {
		this.voiceEngineV2Controller.setAudioControls({
			mode: this.getVoiceEngineV2AudioMode(),
			locallyMuted: LocalVoiceState.getSelfMute(),
			preferredLocallyMuted: LocalVoiceState.getSelfMute(),
			locallyDeafened: LocalVoiceState.getSelfDeaf(),
			mutedByPermission: LocalVoiceState.getMutedByPermission(),
			hasUserSetMute: LocalVoiceState.getHasUserSetMute(),
			hasUserSetDeaf: LocalVoiceState.getHasUserSetDeaf(),
			shouldUnmuteOnUndeafen: LocalVoiceState.shouldUnmuteOnUndeafen,
			pushToTalkActive: this.isPushToTalkActive() && Keybind.pushToTalkHeld,
			pushToMuteActive: this.isPushToMuteActive() && Keybind.pushToMuteHeld,
			inputVolume: VoiceSettings.inputVolume,
			outputVolume: VoiceSettings.outputVolume,
		});
	}

	private getEffectiveSelfMuteForVoiceStatePayload(): boolean {
		this.syncVoiceEngineV2AudioControlsFromAppState();
		return selectVoiceEngineV2AppIntentSelfMuteForVoiceStatePayload(this.voiceEngineV2Snapshot);
	}

	private cancelPendingServerDisconnect(): void {
		if (this.pendingServerDisconnectTimeout) {
			clearTimeout(this.pendingServerDisconnectTimeout);
			this.pendingServerDisconnectTimeout = null;
			logger.debug('Cancelled pending server disconnect', {
				connectionId: this.pendingServerDisconnectConnectionId,
			});
		}
		this.transitionFacadeState({type: 'serverDisconnect.cancel'});
	}

	private seedLocalVoiceStateFromServerUpdate(raw: VoiceServerUpdateData): void {
		const previousConnectionId = voiceEngineV2AppConnectionHostAdapter.connectionId;
		const currentVoiceState = raw.connection_id
			? voiceEngineV2AppVoiceStateAdapter.getVoiceStateByConnectionId(raw.connection_id)
			: null;
		const resolvedGuildId = raw.guild_id ?? voiceEngineV2AppConnectionHostAdapter.guildId ?? null;
		const resolvedChannelId = raw.channel_id ?? voiceEngineV2AppConnectionHostAdapter.channelId ?? null;
		if (!raw.connection_id) return;
		const viewerStreamKeys = currentVoiceState?.viewer_stream_keys ?? LocalVoiceState.getViewerStreamKeys();
		const seededViewerStreamKeys = this.replaceViewerStreamConnectionId(
			viewerStreamKeys,
			resolvedGuildId,
			resolvedChannelId,
			previousConnectionId,
			raw.connection_id,
		);
		LocalVoiceState.seedConnectionState(raw.connection_id, {
			selfMute: currentVoiceState?.self_mute ?? LocalVoiceState.getSelfMute(),
			selfDeaf: currentVoiceState?.self_deaf ?? LocalVoiceState.getSelfDeaf(),
			selfVideo: currentVoiceState?.self_video ?? LocalVoiceState.getSelfVideo(),
			selfStream: currentVoiceState?.self_stream ?? LocalVoiceState.getSelfStream(),
			viewerStreamKeys: seededViewerStreamKeys,
		});
		replaceWatchedStreamKeys(seededViewerStreamKeys, {sync: false});
		const currentUserId = Users.getCurrentUser()?.id;
		if (currentUserId) {
			this.migratePinnedScreenShareIdentity(currentUserId, previousConnectionId, raw.connection_id);
		}
	}

	private finalizeNativeVoiceConnection(
		guildId: string | null,
		channelId: string,
		connectionId: string | null,
		source: 'connect-promise' | 'native-event',
		attempt: VoiceEngineV2AppNativeVoiceConnectAttempt,
	): void {
		if (!this.nativeVoiceConnectionLifecycle.isActiveAttempt(attempt)) {
			logger.debug('Ignoring stale native voice engine connection finalization from replaced attempt', {
				source,
				connectionId,
				attemptId: attempt.id,
				activeAttemptId: this.nativeVoiceConnectionLifecycle.activeAttemptId,
			});
			return;
		}
		if (connectionId && voiceEngineV2AppConnectionHostAdapter.connectionId !== connectionId) {
			logger.warn('Ignoring stale native voice engine connection finalization', {
				source,
				connectionId,
				currentConnectionId: voiceEngineV2AppConnectionHostAdapter.connectionId,
			});
			return;
		}
		const alreadyReady =
			this.nativeVoiceConnectionLifecycle.readyConnectionId === connectionId &&
			voiceEngineV2AppConnectionHostAdapter.connected &&
			!voiceEngineV2AppConnectionHostAdapter.connecting;
		const suppressSelfJoinSound = this.pendingUserMove !== null;
		if (
			!voiceEngineV2AppConnectionHostAdapter.connected ||
			voiceEngineV2AppConnectionHostAdapter.connecting ||
			voiceEngineV2AppConnectionHostAdapter.reconnecting
		) {
			voiceEngineV2AppConnectionHostAdapter.markConnected();
		}
		this.transitionFacadeState({type: 'connection.connected', guildId, channelId});
		if (alreadyReady) return;
		this.nativeVoiceConnectionLifecycle.setReadyConnectionId(connectionId);
		this.clearNativeVoiceTransportReconnect();
		this.clearNativeVoiceConnectRetry();
		startVoiceMediaGraphTimerScheduler();
		this.startNativeVoiceStatsSession();
		void voiceEngineV2AppDebugLoggingHostAdapter.start({
			guildId,
			channelId,
			connectionId,
			room: null,
			collectSnapshot: () => this.createVoiceDiagnosticsSnapshot(),
		});
		this.audioPreferencesSyncDisposer?.();
		this.audioPreferencesSyncDisposer = this.bindNativeAudioPreferencesSync();
		this.videoCodecDecodeCapResyncDisposer?.();
		this.videoCodecDecodeCapResyncDisposer = this.bindVideoDecodeCapResync();
		this.videoCodecPublishOverrideSyncDisposer?.();
		this.videoCodecPublishOverrideSyncDisposer = this.bindVideoCodecPublishOverrideSync();
		VoiceEngineV2AppPermissionAdapter.syncWithNativePermissionState(
			guildId,
			channelId,
			this.createNativePermissionEnforcement(),
		);
		this.seedNativeParticipantsFromChannelVoiceStates(guildId, channelId);
		this.navigateToVoiceChannel(guildId, channelId);
		logger.info('Native voice engine connection ready', {guildId, channelId, connectionId, source});
		if (!hasPlayedNativeVoiceReadySounds(this.nativeVoiceReadySoundConnectionIds, connectionId)) {
			rememberNativeVoiceReadySounds(this.nativeVoiceReadySoundConnectionIds, connectionId);
			if (!suppressSelfJoinSound) {
				playSelfJoinChimeOnce(connectionId, 'native-ready');
			}
			void voiceEngineV2AppMediaExecutionAdapter.playEntranceSound();
		}
		void ScreenShareCodecNegotiation.publishLocalCapabilitiesNative('connected');
		this.restoreNativeLocalMediaStateInBackground('native voice engine connected');
		this.reconcileLocalAudioStateInBackground('native voice engine connected');
	}

	private getNativeVoiceConnectRetryKey(guildId: string | null, channelId: string, connectionId: string): string {
		return `${guildId ?? 'dm'}:${channelId}:${connectionId}`;
	}

	private async retryNativeVoiceEngineConnectAfterTimeout(
		error: unknown,
		raw: VoiceServerUpdateData,
		guildId: string | null,
		channelId: string,
		connectionId: string | null,
		attempt: VoiceEngineV2AppNativeVoiceConnectAttempt,
	): Promise<boolean> {
		if (!(error instanceof TimeoutError) || !connectionId) return false;
		if (!this.isVoiceConnectionCurrentForNativeAttempt(attempt)) {
			logger.debug('Skipping native voice engine connect timeout from stale attempt', {
				guildId,
				channelId,
				connectionId,
				attemptId: attempt.id,
				activeAttemptId: this.nativeVoiceConnectionLifecycle.activeAttemptId,
				currentConnectionId: voiceEngineV2AppConnectionHostAdapter.connectionId,
				currentChannelId: voiceEngineV2AppConnectionHostAdapter.channelId,
			});
			return true;
		}
		const voiceState = voiceEngineV2AppVoiceStateAdapter.getVoiceStateByConnectionId(connectionId);
		if (
			!shouldRetryNativeVoiceConnectTimeout({
				connectionId,
				guildId,
				channelId,
				voiceState,
				connectionState: voiceEngineV2AppConnectionHostAdapter.connectionState,
			})
		) {
			logger.warn('Native voice engine connect timeout not retried because voice connection is inactive', {
				guildId,
				channelId,
				connectionId,
				hasGatewayVoiceState: voiceState != null,
				currentConnectionId: voiceEngineV2AppConnectionHostAdapter.connectionId,
				currentChannelId: voiceEngineV2AppConnectionHostAdapter.channelId,
				connecting: voiceEngineV2AppConnectionHostAdapter.connecting,
				connected: voiceEngineV2AppConnectionHostAdapter.connected,
				reconnecting: voiceEngineV2AppConnectionHostAdapter.reconnecting,
				error,
			});
			return false;
		}
		const retryKey = this.getNativeVoiceConnectRetryKey(guildId, channelId, connectionId);
		const retryAttempt = (this.nativeVoiceConnectRetryCounts.get(retryKey) ?? 0) + 1;
		if (retryAttempt > NATIVE_VOICE_ENGINE_CONNECT_MAX_LOCAL_RETRIES) {
			logger.warn('Native voice engine connect timeout retry budget exhausted', {
				guildId,
				channelId,
				connectionId,
				retryAttempt: retryAttempt - 1,
				maxRetries: NATIVE_VOICE_ENGINE_CONNECT_MAX_LOCAL_RETRIES,
				error,
			});
			return false;
		}
		this.nativeVoiceConnectRetryCounts.set(retryKey, retryAttempt);
		const retryDelayMs = getNativeVoiceEngineConnectRetryDelayMs(retryAttempt);
		logger.warn('Native voice engine connect timed out; retrying local connect', {
			guildId,
			channelId,
			connectionId,
			retryAttempt,
			maxRetries: NATIVE_VOICE_ENGINE_CONNECT_MAX_LOCAL_RETRIES,
			retryDelayMs,
			timeoutMs: NATIVE_VOICE_ENGINE_CONNECT_TIMEOUT_MS,
			error,
		});
		this.clearNativeVoiceTransportReconnect();
		this.disposeNativeVoiceEngineBindings(false);
		try {
			await requireNativeVoiceEngine().disconnect();
		} catch (disconnectError) {
			logger.warn('Native voice engine disconnect before connect retry failed; retrying anyway', {
				guildId,
				channelId,
				connectionId,
				retryAttempt,
				disconnectError,
			});
		}
		if (voiceEngineV2AppConnectionHostAdapter.connectionId !== connectionId) {
			logger.debug('Skipping native voice engine connect retry after connection changed', {
				guildId,
				channelId,
				connectionId,
				currentConnectionId: voiceEngineV2AppConnectionHostAdapter.connectionId,
			});
			return true;
		}
		this.clearNativeVoiceConnectRetryTimeout();
		this.nativeVoiceConnectRetryTimeoutId = window.setTimeout(() => {
			this.nativeVoiceConnectRetryTimeoutId = null;
			if (voiceEngineV2AppConnectionHostAdapter.connectionId !== connectionId) {
				logger.debug('Skipping stale native voice engine connect retry', {
					guildId,
					channelId,
					connectionId,
					currentConnectionId: voiceEngineV2AppConnectionHostAdapter.connectionId,
				});
				return;
			}
			this.connectViaNativeEngine(raw, {forceReconnect: true, reason: 'connect-timeout-retry'});
		}, retryDelayMs);
		return true;
	}

	private async handleNativeVoiceEngineConnectFailure(
		error: unknown,
		guildId: string | null,
		channelId: string,
		connectionId: string | null,
		raw: VoiceServerUpdateData,
		attempt: VoiceEngineV2AppNativeVoiceConnectAttempt,
	): Promise<void> {
		if (!this.nativeVoiceConnectionLifecycle.isActiveAttempt(attempt)) {
			logger.warn('Ignoring native voice engine connect failure from replaced attempt', {
				guildId,
				channelId,
				connectionId,
				attemptId: attempt.id,
				activeAttemptId: this.nativeVoiceConnectionLifecycle.activeAttemptId,
				error,
			});
			return;
		}
		if (
			connectionId &&
			this.nativeVoiceConnectionLifecycle.readyConnectionId === connectionId &&
			voiceEngineV2AppConnectionHostAdapter.connected &&
			!voiceEngineV2AppConnectionHostAdapter.connecting
		) {
			logger.warn('Ignoring native voice engine connect failure after connection was already finalized', {
				guildId,
				channelId,
				connectionId,
				error,
			});
			return;
		}
		if (connectionId && voiceEngineV2AppConnectionHostAdapter.connectionId !== connectionId) {
			logger.warn('Ignoring stale native voice engine connect failure', {
				guildId,
				channelId,
				connectionId,
				currentConnectionId: voiceEngineV2AppConnectionHostAdapter.connectionId,
				error,
			});
			return;
		}
		if (await this.retryNativeVoiceEngineConnectAfterTimeout(error, raw, guildId, channelId, connectionId, attempt)) {
			return;
		}
		logger.error('Native voice engine connect failed', {guildId, channelId, connectionId, error});
		this.clearNativeVoiceTransportReconnect();
		this.clearNativeVoiceConnectRetry();
		this.clearNativeVoiceConnectSession();
		this.clearPendingNativeLocalMediaReconnect();
		this.disposeNativeVoiceEngineBindings();
		this.stopTracking();
		this.statsHostAdapter.reset();
		this.voiceEngineV2Participants.clear();
		if (connectionId) {
			this.discardVoiceConnection(connectionId, {clearLocalState: true});
			sendVoiceStateDisconnect(guildId, connectionId);
		}
		try {
			await requireNativeVoiceEngine().disconnect();
		} catch (disconnectError) {
			logger.warn('Native voice engine disconnect after connect failure failed', {disconnectError});
		}
		voiceEngineV2AppConnectionHostAdapter.markDisconnected('error');
	}

	private scheduleNativeVoiceTransportReconnect(
		raw: VoiceServerUpdateData,
		connectionId: string,
		attempt: VoiceEngineV2AppNativeVoiceConnectAttempt,
	): void {
		this.clearNativeVoiceTransportReconnect();
		this.nativeVoiceTransportReconnectTimeoutId = window.setTimeout(() => {
			this.nativeVoiceTransportReconnectTimeoutId = null;
			if (
				!this.nativeVoiceConnectionLifecycle.isActiveAttempt(attempt) ||
				voiceEngineV2AppConnectionHostAdapter.connectionId !== connectionId ||
				!voiceEngineV2AppConnectionHostAdapter.reconnecting ||
				!this.isGatewayVoiceStateActiveForConnection(
					connectionId,
					raw.guild_id ?? null,
					raw.channel_id ?? voiceEngineV2AppConnectionHostAdapter.channelId ?? '',
				)
			) {
				logger.debug('Skipping stale native voice engine transport reconnect', {
					connectionId,
					attemptId: attempt.id,
					activeAttemptId: this.nativeVoiceConnectionLifecycle.activeAttemptId,
					currentConnectionId: voiceEngineV2AppConnectionHostAdapter.connectionId,
					reconnecting: voiceEngineV2AppConnectionHostAdapter.reconnecting,
				});
				return;
			}
			this.connectViaNativeEngine(raw, {forceReconnect: true, reason: 'transport-reconnect'});
		}, NATIVE_VOICE_ENGINE_TRANSPORT_RECONNECT_DELAY_MS);
	}

	private handleNativeVoiceEngineDisconnected(
		guildId: string | null,
		channelId: string,
		attempt: VoiceEngineV2AppNativeVoiceConnectAttempt,
	): void {
		if (!this.nativeVoiceConnectionLifecycle.isActiveAttempt(attempt)) {
			logger.debug('Ignoring native voice engine disconnect from replaced attempt', {
				guildId,
				channelId,
				attemptId: attempt.id,
				activeAttemptId: this.nativeVoiceConnectionLifecycle.activeAttemptId,
			});
			return;
		}
		const connectionId = voiceEngineV2AppConnectionHostAdapter.connectionId;
		const lastServerUpdate = this.lastNativeVoiceServerUpdate;
		if (
			connectionId &&
			lastServerUpdate?.connection_id === connectionId &&
			this.isGatewayVoiceStateActiveForConnection(connectionId, guildId, channelId)
		) {
			logger.warn('Native voice engine transport disconnected while gateway voice state is still active', {
				guildId,
				channelId,
				connectionId,
			});
			this.prepareNativeLocalMediaReconnect('native voice engine transport disconnected');
			AdaptiveScreenShareEngine.stop();
			voiceEngineV2AppConnectionHostAdapter.markReconnecting();
			this.scheduleNativeVoiceTransportReconnect(lastServerUpdate, connectionId, attempt);
			return;
		}
		void this.disconnectFromVoiceChannel('server');
	}

	private connectViaNativeEngine(raw: VoiceServerUpdateData, options: ConnectViaNativeEngineOptions = {}): void {
		const reason = options.reason ?? 'server-update';
		const guildId = raw.guild_id ?? null;
		const channelId = raw.channel_id ?? null;
		const endpoint = raw.endpoint ?? null;
		const token = raw.token ?? null;
		const connectionId = raw.connection_id ?? null;
		if (!endpoint || !token || !channelId) {
			logger.warn('Native voice engine: ignoring VOICE_SERVER_UPDATE missing endpoint/token/channel', {
				hasEndpoint: !!endpoint,
				hasToken: !!token,
				hasChannel: !!channelId,
			});
			return;
		}
		if (this.voiceEngineV2Participants.isConnectionDiscarded(connectionId)) {
			logger.warn('Native voice engine: ignoring VOICE_SERVER_UPDATE for discarded connection', {
				guildId,
				channelId,
				connectionId,
			});
			return;
		}
		if (
			!options.forceReconnect &&
			this.isDuplicateNativeVoiceServerUpdate({guildId, channelId, connectionId, endpoint, token})
		) {
			logger.debug('Native voice engine: ignoring duplicate VOICE_SERVER_UPDATE for active connection', {
				guildId,
				channelId,
				connectionId,
				endpoint,
				connecting: voiceEngineV2AppConnectionHostAdapter.connecting,
				connected: voiceEngineV2AppConnectionHostAdapter.connected,
				reconnecting: voiceEngineV2AppConnectionHostAdapter.reconnecting,
				activeAttemptId: this.nativeVoiceConnectionLifecycle.activeAttemptId,
			});
			return;
		}
		this.seedLocalVoiceStateFromServerUpdate(raw);
		if (!voiceEngineV2AppConnectionHostAdapter.acceptNativeVoiceServerUpdate(raw)) {
			return;
		}
		this.lastNativeVoiceServerUpdate = raw;
		this.clearNativeVoiceTransportReconnect();
		const attempt = this.nativeVoiceConnectionLifecycle.createAttempt({
			guildId,
			channelId,
			connectionId,
			endpoint,
			reason,
		});
		try {
			const currentVoiceState = raw.connection_id
				? voiceEngineV2AppVoiceStateAdapter.getVoiceStateByConnectionId(raw.connection_id)
				: null;
			if (currentVoiceState) {
				this.seedNativeParticipantFromVoiceState(guildId, currentVoiceState);
			}
			const engine = requireNativeVoiceEngine();
			VoiceEngineV2AppSubscriptionAdapter.cleanup();
			VoiceEngineV2AppSubscriptionAdapter.setNativeEngine(engine);
			this.nativeVoiceEngineV2BridgeEventDisposer?.();
			this.nativeVoiceEngineV2BridgeEventDisposer = engine.onEvent((event) => {
				this.handleNativeVoiceEngineV2BridgeEvent(event, guildId, channelId, connectionId, attempt);
			});
			this.bindNativeVoiceDataProtocols(engine);
			this.startNativeVideoTileFrameSubscription('native-connect');
			this.nativeVoiceDeviceSyncDisposer?.();
			this.nativeVoiceDeviceSyncDisposer = bindNativeVoiceDeviceSync({
				engine,
				getParticipants: () => this.voiceEngineV2Participants.participants,
				subscribeParticipants: (listener) => this.voiceEngineV2ProjectionStore.subscribe(listener),
			});
			logger.info('Native voice engine connect issuing', {
				guildId,
				channelId,
				connectionId,
				attemptId: attempt.id,
				reason,
				hasE2EE: !!raw.e2ee_key,
				timeoutMs: NATIVE_VOICE_ENGINE_CONNECT_TIMEOUT_MS,
			});
			void (async (): Promise<void> => {
				const readiness = await awaitNativeVoiceEngineReadiness();
				if (!readiness.ready) {
					logger.warn('Native voice engine readiness was not confirmed before connect; dialing anyway', {
						guildId,
						channelId,
						connectionId,
						attemptId: attempt.id,
						readinessReason: readiness.reason ?? null,
					});
				}
				const audioDeviceModuleStatus = await nativeAudioDeviceModuleState.ensureStatus();
				this.dispatchNativeAudioDeviceModuleStatus(audioDeviceModuleStatus);
				await withTimeout(
					engine.connect({url: endpoint, token, e2eeKey: encodeVoiceEngineE2EEKey(raw.e2ee_key)}),
					NATIVE_VOICE_ENGINE_CONNECT_TIMEOUT_MS,
					`Native voice engine connect timed out after ${NATIVE_VOICE_ENGINE_CONNECT_TIMEOUT_MS}ms`,
				);
			})()
				.then(() => {
					this.finalizeNativeVoiceConnection(guildId, channelId, connectionId, 'connect-promise', attempt);
				})
				.catch((error) => {
					void this.handleNativeVoiceEngineConnectFailure(error, guildId, channelId, connectionId, raw, attempt);
				});
		} catch (error) {
			void this.handleNativeVoiceEngineConnectFailure(error, guildId, channelId, connectionId, raw, attempt);
		}
	}

	private applyNativeLocalTrackState(
		source: VoiceTrackSource,
		enabled: boolean,
		eventName: string,
		trackSid?: string,
		participant?: NativeVoiceEngineLocalTrackParticipant,
		publication?: NativeVoiceEngineLocalTrackPublication,
	): void {
		const suppressed = this.shouldSuppressNativeLocalTrackState(source, enabled);
		if (source === VoiceTrackSource.Camera) {
			this.recordNativeVideoDiagnostic('local_track_state.received', {
				source,
				enabled,
				eventName,
				trackSid: trackSid ?? null,
				participantSid: participant?.participantSid ?? null,
				participantIdentity: participant?.participantIdentity ?? null,
				publicationTrackSid: publication?.trackSid ?? null,
				suppressed,
			});
		}
		if (suppressed) {
			if (source === VoiceTrackSource.Camera) {
				this.recordNativeVideoDiagnostic('local_track_state.suppressed', {
					source,
					enabled,
					eventName,
					trackSid: trackSid ?? null,
				});
			}
			logger.debug('Suppressing native local track state during reconnect restore', {
				source,
				enabled,
				eventName,
			});
			return;
		}
		if (source === VoiceTrackSource.ScreenShare) {
			voiceEngineV2AppScreenShareExecutionAdapter.syncNativeEngineScreenSharePublishedTrackSidInternal(
				enabled,
				trackSid,
				publication,
			);
			voiceEngineV2AppMediaStateAdapter.applyScreenShareState(enabled, {sendUpdate: false});
		} else if (source === VoiceTrackSource.Camera) {
			const cameraTrackSid = trackSid ?? publication?.trackSid;
			if (enabled) {
				this.registerNativeCameraLocalPreviewTrack(cameraTrackSid, participant);
			} else {
				this.clearNativeCameraLocalPreview(cameraTrackSid);
			}
			voiceEngineV2AppMediaStateAdapter.applyCameraState(enabled, {sendUpdate: false});
			this.syncVideoBackgroundFramePumpInBackground('native-camera-track-state');
		} else if (source === VoiceTrackSource.ScreenShareAudio) {
			applyVoiceEngineV2NativeScreenShareAudioState(enabled);
		} else {
			logger.debug(`Native engine ${eventName}`, {source});
		}
	}

	private readNativeCameraActualEnabled(): boolean {
		try {
			const enabled = requireNativeVoiceEngine().isPublishingCamera();
			assert.equal(typeof enabled, 'boolean', 'native camera publication state must be a boolean');
			return enabled;
		} catch (error) {
			logger.warn('Failed to read native camera publication state; falling back to local voice state', {error});
			return LocalVoiceState.getSelfVideo();
		}
	}

	private get nativeVoiceEngineV2BridgeEventManagers(): NativeVoiceEngineV2BridgeEventManagers {
		return {
			participants: {
				upsertParticipantFromNative: (fields) => this.voiceEngineV2Participants.upsertParticipantFromNative(fields),
				patchParticipantTrackFlags: (identity, flags) =>
					this.voiceEngineV2Participants.patchParticipantTrackFlags(identity, flags),
				setConnectionQualityForNative: (sid, quality) =>
					this.voiceEngineV2Participants.setConnectionQualityForNative(sid, quality),
				updateActiveSpeakersBySid: (sids) => this.voiceEngineV2Participants.updateActiveSpeakersBySid(sids),
				applyNativeSpeakingSample: (sample, nowMs) =>
					this.voiceEngineV2Participants.applyNativeSpeakingSample(sample, nowMs),
				sweepNativeSpeakingHeartbeats: (nowMs) => this.voiceEngineV2Participants.sweepNativeSpeakingHeartbeats(nowMs),
				removeParticipant: (identity) => this.voiceEngineV2Participants.removeParticipant(identity),
				removeParticipantBySid: (sid) => this.voiceEngineV2Participants.removeParticipantBySid(sid),
			},
			inboundVideo: {
				registerTrack: (participantSid, trackSid, source, participantIdentity) => {
					voiceEngineV2AppDebugLoggingHostAdapter.recordNativeVideoDiagnostic('track.register_requested', {
						participantSid,
						participantIdentity,
						trackSid,
						source,
						reason: 'native-track-subscribed',
					});
					NativeVideoTileManager.registerTrack(participantSid, trackSid, source, participantIdentity);
					const registered = NativeVideoTileManager.tracks[trackSid] ?? null;
					voiceEngineV2AppDebugLoggingHostAdapter.recordNativeVideoDiagnostic(
						registered ? 'track.registered' : 'track.register_refused',
						{
							participantSid,
							participantIdentity,
							trackSid,
							source,
							reason: 'native-track-subscribed',
							registeredWidth: registered?.width ?? null,
							registeredHeight: registered?.height ?? null,
						},
					);
				},
				unregisterTrack: (trackSid) => {
					voiceEngineV2AppDebugLoggingHostAdapter.recordNativeVideoDiagnostic('track.unregister_requested', {
						trackSid,
						reason: 'native-track-unsubscribed',
					});
					this.nativeVoiceFrameStatsBatcher.removeTrack(trackSid);
					NativeVideoTileManager.unregisterTrack(trackSid);
				},
				unregisterParticipant: (participantSid) => {
					voiceEngineV2AppDebugLoggingHostAdapter.recordNativeVideoDiagnostic('participant.unregister_requested', {
						participantSid,
					});
					for (const track of NativeVideoTileManager.getTracksForParticipant(participantSid)) {
						this.nativeVoiceFrameStatsBatcher.removeTrack(track.trackSid);
					}
					NativeVideoTileManager.unregisterParticipant(participantSid);
				},
			},
			localMedia: {
				onLocalTrackPublished: (source, trackSid, participant, publication) =>
					this.applyNativeLocalTrackState(source, true, 'localTrackPublished', trackSid, participant, publication),
				onLocalTrackUnpublished: (source, trackSid, participant, publication) =>
					this.applyNativeLocalTrackState(source, false, 'localTrackUnpublished', trackSid, participant, publication),
				onLocalTrackRepublished: (source, trackSid, participant, publication) =>
					this.applyNativeLocalTrackState(source, true, 'localTrackRepublished', trackSid, participant, publication),
			},
			e2ee: {
				setState: (sid, raw) => NativeVoiceE2EEStore.setState(sid, raw),
				remove: (sid) => NativeVoiceE2EEStore.remove(sid),
			},
			stats: {
				setStats: (stats, timestampMs = Date.now()) =>
					NativeVoiceStatsStore.setStats(stats, timestampMs, {mergeSparseTrackStats: true}),
			},
		};
	}

	private getNativeRemoteParticipantIdentities(): Array<string> {
		const identities: Array<string> = [];
		const participants = this.voiceEngineV2Participants.participants;
		for (const participantIdentity in participants) {
			const participant = participants[participantIdentity];
			if (!participant || participant.isLocal) continue;
			identities.push(participant.identity);
		}
		return identities;
	}

	private bindNativeVoiceDataProtocols(engine: VoiceEngine): void {
		this.nativeVoiceDataProtocolDisposer?.();
		const codecDisposer = ScreenShareCodecNegotiation.bindNative(
			{
				publishData: (params) => engine.publishData(params),
				getRemoteParticipantIdentities: () => this.getNativeRemoteParticipantIdentities(),
			},
			{
				onSelectedCodecChanged: (selection) => {
					void voiceEngineV2AppScreenShareExecutionAdapter
						.renegotiateActiveScreenShareCodec(null, selection.codec, selection.reason)
						.catch((error) => {
							logger.warn('Failed to apply negotiated native screen share codec', {
								error,
								codec: selection.codec,
								reason: selection.reason,
							});
						});
				},
			},
		);
		this.nativeVoiceDataProtocolDisposer = () => {
			codecDisposer();
			ScreenShareCodecNegotiation.dispose();
			ScreenSharePublicationMigration.dispose();
		};
	}

	private stopNativeVoiceStatsSession(clearStats: boolean = true): void {
		this.nativeVoiceStatsSession.stop(clearStats);
	}

	private startNativeVoiceStatsSession(): void {
		this.nativeVoiceStatsSession.start();
	}

	private ingestNativeVoiceStats(stats: VoiceEngineV2Stats, timestampMs: number): void {
		ingestNativeVoiceEngineV2BridgeStats(stats, this.nativeVoiceEngineV2BridgeEventManagers, timestampMs);
	}

	private recordNativeVideoDiagnostic(type: string, data?: Record<string, unknown>): void {
		voiceEngineV2AppDebugLoggingHostAdapter.recordNativeVideoDiagnostic(type, data);
	}

	private recordNativeVideoFrame(frame: VoiceEngineV2BridgeVideoFrame): void {
		voiceEngineV2AppDebugLoggingHostAdapter.recordNativeVideoFrame(frame);
		this.nativeVoiceFrameStatsBatcher.recordFrame(frame);
	}

	private startNativeVideoTileFrameSubscription(reason: string): boolean {
		assert.ok(reason.length > 0, 'native video frame subscription reason must be non-empty');
		if (NativeVideoTileManager.isFrameSubscriptionActive) {
			voiceEngineV2AppDebugLoggingHostAdapter.recordNativeVideoDiagnostic('frame_subscription.already_active', {
				reason,
			});
			return true;
		}
		const bridge = window.electron?.voiceEngine;
		if (!bridge) {
			voiceEngineV2AppDebugLoggingHostAdapter.recordNativeVideoDiagnostic('frame_subscription.bridge_missing', {
				reason,
			});
			logger.warn('Cannot subscribe to native video frames; bridge unavailable', {reason});
			return false;
		}
		NativeVideoTileManager.start(bridge, {
			onFrame: (frame) => this.recordNativeVideoFrame(frame),
		});
		voiceEngineV2AppDebugLoggingHostAdapter.recordNativeVideoDiagnostic('frame_subscription.started', {reason});
		return true;
	}

	private seedNativeParticipantsFromChannelVoiceStates(guildId: string | null, channelId: string): void {
		if (!isNativeVoiceEngineSelected()) return;
		const channelVoiceStates = voiceEngineV2AppVoiceStateAdapter.getAllVoiceStatesInChannel(guildId ?? ME, channelId);
		for (const connectionId in channelVoiceStates) {
			const voiceState = channelVoiceStates[connectionId];
			if (!voiceState) continue;
			this.seedNativeParticipantFromVoiceState(guildId, {
				...voiceState,
				viewer_stream_keys: [...voiceState.viewer_stream_keys],
			});
		}
	}

	private seedNativeParticipantFromVoiceState(guildId: string | null, voiceState: VoiceState): void {
		if (!isNativeVoiceEngineSelected()) return;
		if (!voiceState.connection_id) return;
		if (this.voiceEngineV2Participants.isConnectionDiscarded(voiceState.connection_id)) return;
		const identity = buildVoiceParticipantIdentity(voiceState.user_id, voiceState.connection_id);
		const rawIncomingGuildId = guildId ?? voiceState.guild_id ?? null;
		const incomingGuildId = rawIncomingGuildId === ME ? null : rawIncomingGuildId;
		const isCurrentVoiceContext =
			voiceState.channel_id === voiceEngineV2AppConnectionHostAdapter.channelId &&
			incomingGuildId === (voiceEngineV2AppConnectionHostAdapter.guildId ?? null);
		if (!voiceState.channel_id || !isCurrentVoiceContext) {
			if (this.voiceEngineV2Participants.getParticipant(identity)) {
				this.voiceEngineV2Participants.removeParticipant(identity);
			}
			return;
		}
		const isLocal =
			voiceState.user_id === Users.getCurrentUser()?.id &&
			voiceState.connection_id === voiceEngineV2AppConnectionHostAdapter.connectionId;
		this.voiceEngineV2Participants.upsertParticipantFromNative({
			identity,
			sid: this.voiceEngineV2Participants.getParticipant(identity)?.sid ?? '',
			isLocal,
			isMicrophoneEnabled: !voiceState.self_mute && !voiceState.mute,
			isCameraEnabled: voiceState.self_video,
			isScreenShareEnabled: voiceState.self_stream,
		});
	}

	private bytesFromNativeDataPayload(payload: Record<string, unknown>): Uint8Array | null {
		const bytes = payload.payloadBytes;
		if (!Array.isArray(bytes)) return null;
		const output = new Uint8Array(bytes.length);
		for (let i = 0; i < bytes.length; i++) {
			const value = bytes[i];
			if (typeof value !== 'number' || !Number.isInteger(value) || value < 0 || value > 255) return null;
			output[i] = value;
		}
		return output;
	}

	private getNativeVoiceEventConnectionId(event: VoiceEngineV2BridgeEvent): string | null {
		const payload = event.payload as Record<string, unknown>;
		const identity =
			typeof payload.identity === 'string'
				? payload.identity
				: typeof payload.participantIdentity === 'string'
					? payload.participantIdentity
					: null;
		return identity ? this.voiceEngineV2Participants.extractConnectionId(identity) : null;
	}

	private shouldIgnoreNativeVoiceEventForDiscardedConnection(event: VoiceEngineV2BridgeEvent): boolean {
		const connectionId = this.getNativeVoiceEventConnectionId(event);
		if (!this.voiceEngineV2Participants.isConnectionDiscarded(connectionId)) return false;
		logger.debug('Ignoring native voice engine event for discarded connection', {type: event.type, connectionId});
		return true;
	}

	private handleNativeDataReceivedEvent(event: VoiceEngineV2BridgeEvent): boolean {
		if (event.type !== 'dataReceived') return false;
		const payload = event.payload as Record<string, unknown>;
		const participantIdentity = typeof payload.identity === 'string' ? payload.identity : null;
		const topic = typeof payload.topic === 'string' ? payload.topic : null;
		const bytes = this.bytesFromNativeDataPayload(payload);
		if (!participantIdentity || !topic || !bytes) return true;
		if (topic === SCREEN_SHARE_CODEC_NEGOTIATION_TOPIC) {
			ScreenShareCodecNegotiation.handleNativeDataMessage(participantIdentity, bytes);
		} else if (topic === SCREEN_SHARE_PUBLICATION_MIGRATION_TOPIC) {
			ScreenSharePublicationMigration.handleNativeDataMessage(participantIdentity, bytes);
		} else if (topic === VOICE_ENGINE_V2_CODEC_GOSSIP_TOPIC) {
			ingestVoiceEngineV2CodecGossip(this.voiceEngineV2Controller, participantIdentity, bytes);
		}
		return true;
	}

	private handleNativeParticipantProtocolEvent(event: VoiceEngineV2BridgeEvent): void {
		const payload = event.payload as Record<string, unknown>;
		const identity = typeof payload.identity === 'string' ? payload.identity : null;
		if (event.type === 'participantJoined') {
			ScreenShareCodecNegotiation.handleNativeParticipantConnected();
		} else if (event.type === 'participantLeft' && identity) {
			ScreenShareCodecNegotiation.handleNativeParticipantDisconnected(identity);
		}
	}

	private handleNativeRemoteTrackPublicationEvent(event: VoiceEngineV2BridgeEvent): void {
		if (event.type !== 'trackPublished') return;
		const payload = event.payload as Record<string, unknown>;
		const identity = typeof payload.identity === 'string' ? payload.identity : null;
		if (!identity) return;
		const source = asVoiceTrackSource(payload.source);
		if (source === VoiceTrackSource.Camera) {
			VoiceEngineV2AppSubscriptionAdapter.reattachVideoAfterPublish(identity);
		} else if (source === VoiceTrackSource.ScreenShare || source === VoiceTrackSource.ScreenShareAudio) {
			VoiceEngineV2AppSubscriptionAdapter.reattachScreenShareAfterPublish(identity);
		}
	}

	private reattachNativeRemoteTrackSubscriptionsFromConnectedRoster(payload: Record<string, unknown>): void {
		for (const track of collectNativeVoiceEngineConnectedRosterPublishedTracks(payload)) {
			if (track.source === VoiceTrackSource.Camera) {
				VoiceEngineV2AppSubscriptionAdapter.reattachVideoAfterPublish(track.identity);
			} else if (track.source === VoiceTrackSource.ScreenShare || track.source === VoiceTrackSource.ScreenShareAudio) {
				VoiceEngineV2AppSubscriptionAdapter.reattachScreenShareAfterPublish(track.identity);
			}
		}
	}

	private handleNativeVoiceEngineV2BridgeEvent(
		event: VoiceEngineV2BridgeEvent,
		guildId: string | null,
		channelId: string,
		connectionId: string | null,
		attempt: VoiceEngineV2AppNativeVoiceConnectAttempt,
	): void {
		if (!this.nativeVoiceConnectionLifecycle.isActiveAttempt(attempt)) {
			logger.debug('Ignoring native voice engine event from replaced attempt', {
				type: event.type,
				connectionId,
				attemptId: attempt.id,
				activeAttemptId: this.nativeVoiceConnectionLifecycle.activeAttemptId,
			});
			return;
		}
		voiceEngineV2AppDebugLoggingHostAdapter.recordNativeEngineEvent(event);
		if (isFacadeOwnedConnectionEvent(event.type)) {
			if (connectionId && voiceEngineV2AppConnectionHostAdapter.connectionId !== connectionId) {
				logger.debug('Ignoring stale native voice engine connection event', {
					type: event.type,
					connectionId,
					currentConnectionId: voiceEngineV2AppConnectionHostAdapter.connectionId,
				});
				return;
			}
			const action = getNativeVoiceEngineConnectionEventAction(event);
			switch (action) {
				case 'connected':
					applyNativeVoiceEngineConnectedRoster(
						event.payload as Record<string, unknown>,
						this.nativeVoiceEngineV2BridgeEventManagers,
					);
					this.reattachNativeRemoteTrackSubscriptionsFromConnectedRoster(event.payload as Record<string, unknown>);
					this.finalizeNativeVoiceConnection(guildId, channelId, connectionId, 'native-event', attempt);
					break;
				case 'disconnected':
					this.handleNativeVoiceEngineDisconnected(guildId, channelId, attempt);
					break;
				case 'reconnecting':
					this.prepareNativeLocalMediaReconnect('native voice engine reconnecting');
					AdaptiveScreenShareEngine.stop();
					this.statsHostAdapter.stopLatencyTracking();
					this.statsHostAdapter.stopStatsTracking();
					voiceEngineV2AppConnectionHostAdapter.markReconnecting();
					break;
				case 'reconnected':
					this.clearNativeVoiceTransportReconnect();
					this.clearNativeVoiceConnectRetry();
					this.nativeVoiceConnectionLifecycle.setReadyConnectionId(connectionId);
					voiceEngineV2AppConnectionHostAdapter.markReconnected();
					this.statsHostAdapter.incrementReconnectionCount();
					this.statsHostAdapter.startLatencyTracking();
					this.statsHostAdapter.startStatsTracking();
					void ScreenShareCodecNegotiation.publishLocalCapabilitiesNative('reconnected');
					this.restoreNativeLocalMediaStateInBackground('native voice engine reconnected');
					this.reconcileLocalAudioStateInBackground('native voice engine reconnected');
					break;
				default:
					logger.debug('Native voice engine connection event ignored', {
						type: event.type,
						state: (event.payload as Record<string, unknown>).state,
					});
					break;
			}
			return;
		}
		if (this.shouldIgnoreNativeVoiceEventForDiscardedConnection(event)) return;
		if (this.handleNativeDataReceivedEvent(event)) return;
		this.dispatchNativeVoiceEngineV2BridgeEvent(event);
		mapNativeVoiceEngineV2BridgeEvent(event, this.nativeVoiceEngineV2BridgeEventManagers);
		this.handleNativeParticipantProtocolEvent(event);
		this.handleNativeRemoteTrackPublicationEvent(event);
	}

	private dispatchNativeVoiceEngineV2BridgeEvent(event: VoiceEngineV2BridgeEvent): void {
		const translatedEvents = translateVoiceEngineV2BridgeEventToEvents(event);
		const eventCount = translatedEvents.length;
		for (let eventIndex = 0; eventIndex < eventCount; eventIndex += 1) {
			const translatedEvent = translatedEvents[eventIndex];
			if (!translatedEvent) continue;
			this.voiceEngineV2Host.dispatch(translatedEvent);
		}
	}

	private dispatchNativeAudioDeviceModuleStatus(status: NativeAudioDeviceModuleStatus): void {
		assert.equal(typeof status, 'string', 'native audio device module status must be a string');
		this.voiceEngineV2Host.dispatch({
			type: 'nativeAudioDeviceModule.statusChanged',
			status,
		});
	}

	handleVoiceServerUpdate(raw: VoiceServerUpdateData): void {
		if (raw.connection_id && raw.connection_id === this.pendingServerDisconnectConnectionId) {
			this.cancelPendingServerDisconnect();
		}
		if (isNativeVoiceEngineSelected()) {
			this.connectViaNativeEngine(raw);
			return;
		}
		if (isNativeVoiceEngineSelectionPending()) {
			void this.handleVoiceServerUpdateAfterNativeProbe(raw);
			return;
		}
		this.handleVoiceServerUpdateViaJs(raw);
	}

	private async handleVoiceServerUpdateAfterNativeProbe(raw: VoiceServerUpdateData): Promise<void> {
		try {
			if (await shouldUseNativeVoiceEngine()) {
				this.connectViaNativeEngine(raw);
				return;
			}
		} catch (error) {
			logger.warn('Native voice engine probe failed while handling VOICE_SERVER_UPDATE', {error});
			voiceEngineV2AppConnectionHostAdapter.resetConnectionState();
			return;
		}
		this.handleVoiceServerUpdateViaJs(raw);
	}

	private handleVoiceServerUpdateViaJs(raw: VoiceServerUpdateData): void {
		const expectedChannelId = voiceEngineV2AppConnectionHostAdapter.channelId;
		const currentVoiceState = raw.connection_id
			? voiceEngineV2AppVoiceStateAdapter.getVoiceStateByConnectionId(raw.connection_id)
			: null;
		if (
			voiceEngineV2AppConnectionHostAdapter.connected &&
			raw.channel_id &&
			currentVoiceState?.channel_id &&
			currentVoiceState.channel_id !== raw.channel_id
		) {
			logger.warn('Ignoring VOICE_SERVER_UPDATE that conflicts with known voice state', {
				expectedChannelId,
				incomingChannelId: raw.channel_id,
				connectionId: raw.connection_id,
				voiceStateChannelId: currentVoiceState.channel_id,
			});
			return;
		}
		if (
			raw.channel_id &&
			expectedChannelId &&
			raw.channel_id !== expectedChannelId &&
			this.pendingUserMove?.channelId === expectedChannelId &&
			voiceEngineV2AppConnectionHostAdapter.connecting
		) {
			logger.warn('Ignoring VOICE_SERVER_UPDATE during user-initiated move', {
				expectedChannelId,
				incomingChannelId: raw.channel_id,
				connectionId: raw.connection_id,
			});
			return;
		}
		const resolvedGuildId = raw.guild_id ?? voiceEngineV2AppConnectionHostAdapter.guildId ?? null;
		const resolvedChannelId = raw.channel_id ?? voiceEngineV2AppConnectionHostAdapter.channelId ?? null;
		this.seedLocalVoiceStateFromServerUpdate(raw);
		const shouldPreserveLocalMedia =
			Boolean(this.facadeSnapshot.context.activeConnection) &&
			this.facadeSnapshot.context.activeConnection?.guildId === resolvedGuildId &&
			this.facadeSnapshot.context.activeConnection?.channelId === resolvedChannelId;
		const shouldApplyPendingSessionRestore =
			Boolean(this.pendingSessionRestore) &&
			this.pendingSessionRestore?.guildId === resolvedGuildId &&
			this.pendingSessionRestore?.channelId === resolvedChannelId;
		voiceEngineV2AppConnectionHostAdapter.handleVoiceServerUpdate(
			raw,
			(room, attemptId, guildId, channelId) => {
				const roomEventDependencies = buildRoomEventDependencies({
					resetStreamTracking: () => this.resetLocalMediaAndScreenShareTracking(),
					participants: this.voiceEngineV2Participants,
					sourceLifecycleBridge: this.voiceEngineV2SourceLifecycleBridge,
					isUserMovePending: () => this.pendingUserMove !== null,
				});
				bindRoomEvents(
					room,
					attemptId,
					guildId,
					channelId,
					{
						onConnected: async () => {
							this.transitionFacadeState({type: 'connection.connected', guildId, channelId});
							this.navigateToVoiceChannel(guildId, channelId);
							this.startTracking(room);
							if (shouldPreserveLocalMedia) {
								await this.restoreLocalMediaState(room);
							}
							if (shouldApplyPendingSessionRestore) {
								await this.restorePendingSessionMedia();
							}
							await this.reconcileLocalAudioState('voice room connected');
						},
						onDisconnected: () => {
							AdaptiveScreenShareEngine.stop();
							this.stopTracking();
							this.statsHostAdapter.reset();
						},
						onReconnecting: () => {
							AdaptiveScreenShareEngine.stop();
							this.statsHostAdapter.stopLatencyTracking();
							this.statsHostAdapter.stopStatsTracking();
						},
						onReconnected: () => {
							const room = voiceEngineV2AppConnectionHostAdapter.room;
							if (room?.localParticipant?.getTrackPublication(Track.Source.ScreenShare)?.videoTrack) {
								AdaptiveScreenShareEngine.start(room);
							}
							this.statsHostAdapter.incrementReconnectionCount();
							this.statsHostAdapter.startLatencyTracking();
							this.statsHostAdapter.startStatsTracking();
							this.reconcileLocalAudioStateInBackground('voice room reconnected');
						},
					},
					roomEventDependencies,
				);
			},
			(isChannelMove, previousRoom) => {
				this.cancelPendingServerDisconnect();
				this.stopTracking();
				if (isChannelMove) {
					this.statsHostAdapter.reset();
				}
				this.voiceEngineV2Participants.clear();
				let shouldKeepPreviousRoomTracks = false;
				if (!isChannelMove && shouldPreserveLocalMedia && LocalVoiceState.getSelfStream()) {
					const pendingScreenShareReconnect =
						voiceEngineV2AppScreenShareExecutionAdapter.prepareScreenShareReconnect(previousRoom);
					if (pendingScreenShareReconnect) {
						this.transitionFacadeState({
							type: 'screenShareReconnect.prepare',
							snapshot: pendingScreenShareReconnect,
						});
						this.syncLocalVoiceStateWithServer({self_stream: true});
						shouldKeepPreviousRoomTracks = true;
					} else {
						this.transitionFacadeState({type: 'screenShareReconnect.clear'});
					}
				} else {
					this.transitionFacadeState({type: 'screenShareReconnect.clear'});
				}
				if (!shouldPreserveLocalMedia && this.facadeSnapshot.context.activeConnection) {
					voiceEngineV2AppMediaStateAdapter.resetLocalMediaState('disconnect');
					this.resetLocalMediaAndScreenShareTracking();
				}
				return shouldKeepPreviousRoomTracks;
			},
			(_room, _attemptId) => {},
			(newRoom, attemptId, guildId, channelId) => {
				logger.info('Region hot-swap complete, rebinding tracking', {guildId, channelId, attemptId});
				this.stopTracking();
				this.startTracking(newRoom);
				this.voiceEngineV2Participants.hydrateFromRoom(newRoom);
				VoiceEngineV2AppRemoteSpeakingAdapter.hydrateFromRoom(newRoom);
				VoiceEngineV2AppSubscriptionAdapter.reconcileSubscriptions();
				VoiceEngineV2AppPermissionAdapter.applyDeafen(newRoom, getEffectiveAudioState().effectiveDeaf);
				voiceEngineV2AppMediaExecutionAdapter.applyAllLocalAudioPreferences(newRoom);
				if (newRoom.localParticipant?.getTrackPublication(Track.Source.ScreenShare)?.videoTrack) {
					AdaptiveScreenShareEngine.start(newRoom);
				} else {
					AdaptiveScreenShareEngine.stop();
				}
				if (guildId && channelId) {
					VoiceEngineV2AppPermissionAdapter.syncWithPermissionState(guildId, channelId, newRoom);
				}
				this.reconcileLocalAudioStateInBackground('region hot-swap complete');
			},
			(_guildId, _channelId, _connectionId, _attemptId, error) => {
				logger.warn('LiveKit connect failed, clearing gateway voice state', {error});
				void this.disconnectFromVoiceChannel('error');
			},
		);
	}

	handleConnectionOpen(guilds: Array<GuildReadyData>): void {
		voiceEngineV2AppVoiceStateAdapter.handleConnectionOpen(guilds);
	}

	handleGuildCreate(guild: GuildReadyData): void {
		voiceEngineV2AppVoiceStateAdapter.handleGuildCreate(guild);
	}

	handleGuildDelete(guildId: string): void {
		voiceEngineV2AppVoiceStateAdapter.handleGuildDelete(guildId);
		if (voiceEngineV2AppConnectionHostAdapter.connected && voiceEngineV2AppConnectionHostAdapter.guildId === guildId) {
			void this.disconnectFromVoiceChannel('server');
		}
	}

	handlePassiveVoiceStates(guildId: string, voiceStates: Array<VoiceState>): void {
		for (const voiceState of voiceStates) {
			const stateWithGuild = {
				...voiceState,
				guild_id: voiceState.guild_id ?? guildId,
			};
			voiceEngineV2AppVoiceStateAdapter.handleGatewayVoiceStateUpdate(guildId, stateWithGuild);
			this.seedNativeParticipantFromVoiceState(guildId, stateWithGuild);
		}
	}

	handleGatewayVoiceStateUpdate(guildId: string | null, voiceState: VoiceState): void {
		const user = Users.getCurrentUser();
		const isLocalConnection =
			user &&
			voiceState.user_id === user.id &&
			voiceState.connection_id === voiceEngineV2AppConnectionHostAdapter.connectionId;
		if (voiceEngineV2AppVoiceStateAdapter.isConnectionIgnored(voiceState.connection_id)) {
			voiceEngineV2AppVoiceStateAdapter.handleGatewayVoiceStateUpdate(guildId, voiceState);
			if (isLocalConnection) {
				this.handleCurrentLocalVoiceStateRemoval(guildId, voiceState);
			}
			return;
		}
		const previousConnectionState = voiceState.connection_id
			? voiceEngineV2AppVoiceStateAdapter.getVoiceStateByConnectionId(voiceState.connection_id)
			: null;
		const incomingViewerStreamKeys = normalizeVoiceMediaGraphViewerStreamKeys(voiceState.viewer_stream_keys);
		const previousLocalMediaState = isLocalConnection
			? {
					selfVideo: LocalVoiceState.getSelfVideo(),
					selfStream: LocalVoiceState.getSelfStream(),
				}
			: null;
		const previousLocalAudioState =
			isLocalConnection && previousConnectionState
				? {
						serverMute: isVoiceServerMuteActive(
							previousConnectionState,
							previousConnectionState.guild_id,
							previousConnectionState.channel_id,
						),
						serverDeaf: previousConnectionState.deaf,
						selfMute: previousConnectionState.self_mute,
						selfDeaf: previousConnectionState.self_deaf,
					}
				: null;
		const previousViewerStreamKeys: ReadonlyArray<string> = previousConnectionState?.viewer_stream_keys ?? [];
		if (!isLocalConnection && voiceState.connection_id) {
			this.migrateWatchedRemoteStreamConnection(guildId, voiceState);
		}
		const localServerPayload: VoiceStateSyncPayload | null =
			isLocalConnection && voiceState.channel_id && voiceState.connection_id
				? {
						guild_id: guildId,
						channel_id: voiceState.channel_id,
						connection_id: voiceState.connection_id,
						self_mute: voiceState.self_mute,
						self_deaf: voiceState.self_deaf,
						self_video: voiceState.self_video,
						self_stream: voiceState.self_stream,
						viewer_stream_keys: incomingViewerStreamKeys,
					}
				: null;
		const shouldApplyIncomingLocalState =
			localServerPayload === null
				? true
				: shouldApplyGatewayVoiceStateEcho(
						this.voiceEngineV2Snapshot,
						this.toEngineGatewayVoiceState(voiceState, guildId),
					);
		const projectedVoiceState =
			isLocalConnection && !shouldApplyIncomingLocalState
				? {
						...voiceState,
						self_mute: LocalVoiceState.getSelfMute(),
						self_deaf: LocalVoiceState.getSelfDeaf(),
						self_video: LocalVoiceState.getSelfVideo(),
						self_stream: LocalVoiceState.getSelfStream(),
						viewer_stream_keys: LocalVoiceState.getViewerStreamKeys(),
					}
				: voiceState;
		voiceEngineV2AppVoiceStateAdapter.handleGatewayVoiceStateUpdate(guildId, projectedVoiceState);
		this.seedNativeParticipantFromVoiceState(guildId, projectedVoiceState);
		if (!isLocalConnection && voiceEngineV2AppConnectionHostAdapter.connected && voiceState.channel_id) {
			if (!areOrderedStringArraysEqual(previousViewerStreamKeys, incomingViewerStreamKeys)) {
				this.playSpectatorSounds(previousViewerStreamKeys, incomingViewerStreamKeys);
			}
		}
		if (!isLocalConnection && voiceState.connection_id) {
			const previousGuildId = normalizeStreamGuildId(previousConnectionState?.guild_id);
			const nextGuildId = normalizeStreamGuildId(guildId ?? voiceState.guild_id);
			const previousChannelId = previousConnectionState?.channel_id ?? null;
			const nextChannelId = voiceState.channel_id ?? null;
			const leftPreviousStreamChannel =
				previousChannelId != null && (previousChannelId !== nextChannelId || previousGuildId !== nextGuildId);
			const streamEnded = voiceState.self_stream === false;
			const streamChannelId = leftPreviousStreamChannel ? previousChannelId : nextChannelId;
			const streamGuildId = leftPreviousStreamChannel ? previousGuildId : nextGuildId;
			if (streamChannelId && (streamEnded || leftPreviousStreamChannel)) {
				const streamKey = getStreamKey(streamGuildId, streamChannelId, voiceState.connection_id);
				if (streamEnded) {
					deferStopWatchingStreamKey(streamKey, {
						guildId: streamGuildId,
						channelId: streamChannelId,
					});
				} else {
					stopWatchingStreamKey(streamKey, {
						guildId: streamGuildId,
						channelId: streamChannelId,
					});
				}
			}
		}
		if (isLocalConnection) {
			const currentConnectionId = voiceEngineV2AppConnectionHostAdapter.connectionId;
			const isCurrentConnection = voiceState.connection_id === currentConnectionId;
			let shouldApplyCurrentServerState = true;
			const serverPayload = localServerPayload;
			if (this.handleCurrentLocalVoiceStateRemoval(guildId, voiceState)) {
				return;
			}
			if (voiceState.channel_id && voiceState.connection_id && isCurrentConnection) {
				const ptmActive = this.isPushToMuteActive();
				const pttActive = this.isPushToTalkActive();
				const applyServerState = shouldApplyIncomingLocalState;
				shouldApplyCurrentServerState = applyServerState;
				LocalVoiceState.syncConnectionState(voiceState.connection_id, {
					selfMute: ptmActive || pttActive || !applyServerState ? undefined : voiceState.self_mute,
					selfDeaf: applyServerState ? voiceState.self_deaf : undefined,
					selfVideo: applyServerState ? voiceState.self_video : undefined,
					selfStream: applyServerState ? voiceState.self_stream : undefined,
					viewerStreamKeys: applyServerState ? incomingViewerStreamKeys : undefined,
				});
				if (applyServerState) {
					replaceWatchedStreamKeys([...incomingViewerStreamKeys], {sync: false});
				}
			}
			this.applyEngineGatewayEcho(serverPayload ? this.toEngineGatewayVoiceState(voiceState, guildId) : null);
			if (voiceState.channel_id && voiceState.connection_id) {
				if (isCurrentConnection && this.pendingServerDisconnectConnectionId === voiceState.connection_id) {
					this.cancelPendingServerDisconnect();
				}
				if (
					isCurrentConnection &&
					voiceEngineV2AppConnectionHostAdapter.connected &&
					!voiceEngineV2AppConnectionHostAdapter.connecting &&
					voiceState.channel_id !== voiceEngineV2AppConnectionHostAdapter.channelId
				) {
					const updatedGuildId = guildId ?? voiceState.guild_id ?? null;
					void this.disconnectForChannelMove('server').finally(() => {
						voiceEngineV2AppConnectionHostAdapter.recoverConnectionExpectation(updatedGuildId, voiceState.channel_id!);
					});
					return;
				}
				const shouldRecoverConnectionExpectation =
					!voiceEngineV2AppConnectionHostAdapter.connected &&
					!voiceEngineV2AppConnectionHostAdapter.connecting &&
					voiceEngineV2AppConnectionHostAdapter.guildId === null &&
					voiceEngineV2AppConnectionHostAdapter.channelId === null;
				if (shouldRecoverConnectionExpectation) {
					const recoveredGuildId = guildId ?? voiceState.guild_id ?? null;
					voiceEngineV2AppConnectionHostAdapter.recoverConnectionExpectation(recoveredGuildId, voiceState.channel_id);
					logger.info('Recovered connection expectation from voice state update', {
						guildId: recoveredGuildId,
						channelId: voiceState.channel_id,
						connectionId: voiceState.connection_id,
					});
				}
				const nextServerMute = isVoiceServerMuteActive(
					voiceState,
					guildId ?? voiceState.guild_id,
					voiceState.channel_id,
				);
				const audioStateChanged =
					previousLocalAudioState &&
					(previousLocalAudioState.serverMute !== nextServerMute ||
						previousLocalAudioState.serverDeaf !== voiceState.deaf ||
						previousLocalAudioState.selfMute !== voiceState.self_mute ||
						previousLocalAudioState.selfDeaf !== voiceState.self_deaf);
				if (isCurrentConnection && audioStateChanged) {
					this.reconcileLocalAudioStateInBackground('voice state update');
				}
			}
			if (previousLocalMediaState) {
				const videoDisabled =
					shouldApplyCurrentServerState && previousLocalMediaState.selfVideo && voiceState.self_video === false;
				const streamDisabled =
					shouldApplyCurrentServerState && previousLocalMediaState.selfStream && voiceState.self_stream === false;
				if (videoDisabled) {
					logger.info('Server disabled camera for local connection, unpublishing video track');
					void this.setCameraEnabled(false, {sendUpdate: false});
				}
				if (streamDisabled) {
					logger.info('Server disabled screen share for local connection, unpublishing stream track');
					void this.setScreenShareEnabled(false, {sendUpdate: false});
				}
			}
			if (
				voiceState.channel_id === null &&
				voiceEngineV2AppConnectionHostAdapter.connected &&
				voiceState.connection_id &&
				isCurrentConnection
			) {
				this.scheduleDeferredServerDisconnect(voiceState.connection_id);
			}
		}
	}

	handleGatewayVoiceStateDelete(guildId: string, userId: string): void {
		voiceEngineV2AppVoiceStateAdapter.handleGatewayVoiceStateDelete(guildId, userId);
	}

	getCurrentUserVoiceState(guildId?: string | null): NormalizedVoiceState | null {
		return voiceEngineV2AppVoiceStateAdapter.getCurrentUserVoiceState(
			guildId,
			Users.getCurrentUser()?.id,
			voiceEngineV2AppConnectionHostAdapter.connectionId,
		);
	}

	getVoiceState(guildId: string | null, userId?: string): NormalizedVoiceState | null {
		const currentUserId = Users.getCurrentUser()?.id;
		const connectionId =
			!userId || userId === currentUserId ? voiceEngineV2AppConnectionHostAdapter.connectionId : null;
		return voiceEngineV2AppVoiceStateAdapter.getVoiceState(guildId, userId, currentUserId, connectionId);
	}

	getVoiceStateByConnectionId(connectionId: string): NormalizedVoiceState | null {
		return voiceEngineV2AppVoiceStateAdapter.getVoiceStateByConnectionId(connectionId);
	}

	isVoiceConnectionIgnored(connectionId: string | null | undefined): boolean {
		return voiceEngineV2AppVoiceStateAdapter.isConnectionIgnored(connectionId);
	}

	getAllVoiceStatesInChannel(guildId: string, channelId: string): Readonly<Record<string, NormalizedVoiceState>> {
		return voiceEngineV2AppVoiceStateAdapter.getAllVoiceStatesInChannel(guildId, channelId);
	}

	getAllVoiceStatesInGuild(
		guildId: string,
	): Readonly<Record<string, Readonly<Record<string, NormalizedVoiceState>>>> | undefined {
		return voiceEngineV2AppVoiceStateAdapter.getAllVoiceStatesInGuild(guildId);
	}

	getAllVoiceStates(): Readonly<
		Record<string, Readonly<Record<string, Readonly<Record<string, NormalizedVoiceState>>>>>
	> {
		return voiceEngineV2AppVoiceStateAdapter.getAllVoiceStates();
	}

	handleGatewayVoiceStateAck(data: VoiceStateAckPayload): void {
		if (shouldNotifyCameraUserLimitRejection({status: data.status, errorCode: data.error_code})) {
			this.handleCameraUserLimitRejection();
		}
		const canonicalState = data.canonical_state;
		if (!canonicalState?.channel_id || !canonicalState.connection_id) {
			logger.debug('Ignoring voice state ack without canonical connection state', {
				mutationId: data.mutation_id,
				status: data.status,
				guildId: data.guild_id,
				channelId: data.channel_id,
				connectionId: data.connection_id,
			});
			return;
		}
		this.applyEngineGatewayEcho(
			this.toEngineGatewayVoiceState(canonicalState, canonicalState.guild_id ?? data.guild_id ?? null),
		);
	}

	private handleCameraUserLimitRejection(): void {
		logger.warn('Camera enable rejected by the gateway camera user limit', {
			limit: VOICE_CHANNEL_CAMERA_USER_LIMIT,
		});
		void this.setCameraEnabled(false, {sendUpdate: false}).catch((error) => {
			logger.warn('Failed to turn the camera back off after a camera user limit rejection', {error});
		});
		if (!this.i18n) return;
		this.showVoiceErrorModal(
			VOICE_CAMERA_USER_LIMIT_REACHED_DESCRIPTOR,
			'voice.media-engine-facade.camera-user-limit-error-modal',
			{voiceChannelCameraUserLimit: VOICE_CHANNEL_CAMERA_USER_LIMIT},
		);
	}

	private toEngineGatewayVoiceState(
		source: {
			guild_id?: string | null;
			channel_id: string | null;
			user_id: string;
			session_id?: string;
			self_mute: boolean;
			self_deaf: boolean;
			self_video: boolean;
			self_stream: boolean;
			suppress?: boolean;
		},
		guildId: string | null,
	): VoiceEngineV2GatewayVoiceState {
		return {
			guildId: guildId ?? source.guild_id ?? null,
			channelId: source.channel_id,
			userId: source.user_id,
			sessionId: source.session_id ?? null,
			selfMute: source.self_mute,
			selfDeaf: source.self_deaf,
			selfVideo: source.self_video,
			selfStream: source.self_stream,
			suppress: source.suppress ?? false,
			requestToSpeakTimestamp: null,
		};
	}

	private applyEngineGatewayEcho(voiceState: VoiceEngineV2GatewayVoiceState | null): void {
		this.voiceEngineV2Controller.dispatch({type: 'gateway.voiceStateUpdated', voiceState});
		if (voiceState) {
			this.voiceEngineV2Controller.reconcileGatewayVoiceState();
		}
	}

	syncLocalVoiceStateWithServer(partial?: VoiceStateSyncPartial): void {
		LocalVoiceState.ensurePermissionMute();
		const {guildId, channelId, connectionId} = voiceEngineV2AppConnectionHostAdapter.connectionState;
		if (!channelId || !connectionId) return;
		const devicePermission = VoiceDevicePermissionState.getState().permissionStatus;
		const micGranted = MediaPermission.isMicrophoneGranted() || devicePermission === 'granted';
		const selfMute = resolveVoiceStateSelfMute({
			guildId,
			channelId,
			microphoneGranted: micGranted,
			requestedSelfMute: partial?.self_mute,
			effectiveSelfMute: this.getEffectiveSelfMuteForVoiceStatePayload(),
		});
		const payload: VoiceStateSyncPayload = {
			guild_id: guildId,
			channel_id: channelId,
			connection_id: connectionId,
			self_mute: selfMute,
			self_deaf: partial?.self_deaf ?? LocalVoiceState.getSelfDeaf(),
			self_video: partial?.self_video ?? LocalVoiceState.getSelfVideo(),
			self_stream: partial?.self_stream ?? LocalVoiceState.getSelfStream(),
			viewer_stream_keys: partial?.viewer_stream_keys ?? LocalVoiceState.getViewerStreamKeys(),
		};
		if (!micGranted && !LocalVoiceState.getSelfMute()) {
			LocalVoiceState.updateSelfMute(true);
		}
		this.voiceEngineV2Controller.setDesiredGatewayVoiceState({
			guildId,
			channelId,
			selfMute: payload.self_mute,
			selfDeaf: payload.self_deaf,
			selfVideo: payload.self_video,
			selfStream: payload.self_stream,
		});
		if (partial?.viewer_stream_keys !== undefined) {
			syncVoiceStateToServer(guildId, channelId, connectionId, {viewer_stream_keys: payload.viewer_stream_keys});
		}
	}

	private async reconcileLocalAudioState(reason: string): Promise<void> {
		const {channelId, connectionId} = voiceEngineV2AppConnectionHostAdapter.connectionState;
		if (!channelId || !connectionId) {
			logger.debug('Skipping local audio reconciliation without an active voice connection', {
				reason,
				channelId,
				connectionId,
			});
			return;
		}
		const serverVoiceState = voiceEngineV2AppVoiceStateAdapter.getVoiceStateByConnectionId(connectionId);
		const serverMute = isVoiceServerMuteActive(
			serverVoiceState,
			voiceEngineV2AppConnectionHostAdapter.guildId,
			channelId,
		);
		const serverDeaf = serverVoiceState?.deaf ?? false;
		if (await shouldUseNativeVoiceEngine()) {
			await this.reconcileNativeEngineMicState({channelId, serverMute, serverDeaf});
			this.syncLocalVoiceStateWithServer();
			return;
		}
		await voiceEngineV2AppMediaExecutionAdapter.reconcileEffectiveAudioState(
			voiceEngineV2AppConnectionHostAdapter.room,
			{
				channelId,
				serverMute,
				serverDeaf,
			},
		);
		this.syncLocalVoiceStateWithServer();
	}

	private createNativePermissionEnforcement(): VoiceEngineV2AppNativePermissionEnforcement {
		return {
			revokeMicrophone: () => this.reconcileLocalAudioState('speak permission revoked'),
			revokeCamera: () => this.setCameraEnabled(false, {sendUpdate: true}),
			revokeScreenShare: () => this.setScreenShareEnabled(false, {sendUpdate: true, playSound: false}),
		};
	}

	private async reconcileNativeEngineMicState(params: {
		channelId: string;
		serverMute: boolean;
		serverDeaf: boolean;
	}): Promise<void> {
		const permissionMuted = isVoiceSpeakPermissionDenied(
			voiceEngineV2AppConnectionHostAdapter.guildId,
			params.channelId,
		);
		const audioState = getEffectiveAudioState({
			serverMute: params.serverMute || permissionMuted,
			serverDeaf: params.serverDeaf,
		});
		const pttActive = this.isPushToTalkActive();
		const ptmActive = this.isPushToMuteActive();
		const effectiveMute =
			audioState.effectiveDeaf ||
			audioState.serverMute ||
			audioState.selfMute ||
			(pttActive && !Keybind.pushToTalkHeld) ||
			(ptmActive && Keybind.pushToMuteHeld);
		await this.setMicEnabledViaEngine(!effectiveMute);
	}

	getParticipantByUserIdAndConnectionId(
		userId: string,
		connectionId: string | null,
	): LivekitParticipantSnapshot | undefined {
		return this.voiceEngineV2Participants.getParticipantByUserIdAndConnectionId(userId, connectionId);
	}

	upsertParticipant(participant: Participant): void {
		this.voiceEngineV2Participants.upsertParticipant(participant);
	}

	async setCameraEnabled(
		enabled: boolean,
		options?: {
			deviceId?: string;
			sendUpdate?: boolean;
		},
	): Promise<void> {
		const selectedOptions = enabled
			? {
					deviceId: options?.deviceId ?? VoiceSettings.getVideoDeviceId(),
					...(options?.sendUpdate === false ? {sendUpdate: false} : {}),
				}
			: options;
		const commandOptions = buildCameraCommandOptions(selectedOptions);
		try {
			await this.voiceEngineV2Host.runAndWait(
				() => {
					if (enabled) {
						this.voiceEngineV2Controller.publishCamera(commandOptions);
					} else {
						this.voiceEngineV2Controller.unpublishCamera(commandOptions);
					}
				},
				{description: enabled ? 'publish camera' : 'unpublish camera'},
			);
		} catch (error) {
			if (isCameraPermissionDeniedCommandFailure(error)) {
				logger.info('Camera enable denied by permission; denial already surfaced to the user', {enabled});
				return;
			}
			throw error;
		}
	}

	async enableMicrophone(
		room: Room,
		channelId: string | null,
		options: VoiceEngineV2MicrophoneOptions = {},
	): Promise<void> {
		await voiceEngineV2AppMediaExecutionAdapter.enableMicrophone(room, channelId, options);
	}

	async refreshMicrophonePublishSettingsForChannel(channelId: string): Promise<void> {
		assert.ok(channelId.length > 0, 'microphone publish settings refresh requires a channelId');
		if (channelId !== voiceEngineV2AppConnectionHostAdapter.channelId) return;
		if (await shouldUseNativeVoiceEngine()) {
			await this.refreshNativeMicrophonePublishSettingsForChannel(channelId);
			return;
		}
		await voiceEngineV2AppMediaExecutionAdapter.refreshMicrophonePublishSettings(
			voiceEngineV2AppConnectionHostAdapter.room,
			channelId,
		);
	}

	private async refreshNativeMicrophonePublishSettingsForChannel(channelId: string): Promise<void> {
		assert.ok(channelId.length > 0, 'native microphone publish settings refresh requires a channelId');
		assert.equal(
			channelId,
			voiceEngineV2AppConnectionHostAdapter.channelId,
			'native microphone publish settings refresh requires the active voice channel',
		);
		const microphone = this.voiceEngineV2Snapshot.microphone;
		if (microphone.status !== 'published') {
			logger.debug('Skipping native microphone bitrate refresh because the microphone is not published', {
				channelId,
				status: microphone.status,
			});
			return;
		}
		const publishedOptions = microphone.published as VoiceEngineV2NativeMicrophonePublishOptions | null;
		const publishedMaxBitrateBps = publishedOptions?.maxBitrateBps;
		const nextMaxBitrateBps = resolveVoiceEngineV2NativeMicrophoneMaxBitrateBps(
			Channels.getChannel(channelId)?.bitrate ?? null,
		);
		if (nextMaxBitrateBps === publishedMaxBitrateBps) {
			logger.debug('Skipping native microphone bitrate refresh because the effective bitrate is unchanged', {
				channelId,
				maxBitrateBps: publishedMaxBitrateBps ?? null,
			});
			return;
		}
		logger.info('Refreshing native microphone publish settings for channel bitrate change', {
			channelId,
			previousMaxBitrateBps: publishedMaxBitrateBps ?? null,
			nextMaxBitrateBps: nextMaxBitrateBps ?? null,
		});
		await this.refreshNativeMicrophoneFromSettings();
	}

	private async getNativeCameraPublishParams(options?: {deviceId?: string}): Promise<{
		deviceId?: string;
		width: number;
		height: number;
		frameRate: number;
		mirror?: boolean;
		backgroundMode?: 'none' | 'blur' | 'custom';
		backgroundCustomMediaPath?: string;
		backgroundCustomMediaKind?: 'static' | 'animated' | 'video';
		backgroundBlurStrength: number;
	}> {
		const preset = getCameraCaptureDimensions(VoiceSettings.getCameraResolution());
		const deviceId = options?.deviceId ?? VoiceSettings.getVideoDeviceId();
		const nativeDeviceId = await resolveNativeCameraDeviceId(deviceId);
		const backgroundImageId = areVoiceBackgroundsAvailable()
			? VoiceSettings.getBackgroundImageId()
			: NONE_BACKGROUND_ID;
		const backgroundMode =
			backgroundImageId === BLUR_BACKGROUND_ID ? 'blur' : backgroundImageId === NONE_BACKGROUND_ID ? 'none' : 'custom';
		const customMedia =
			backgroundMode === 'custom' ? await BackgroundImageDB.getNativeBackgroundMediaSource(backgroundImageId) : null;
		if (backgroundMode === 'custom' && !customMedia) {
			throw new Error(`Selected native camera background media is missing: ${backgroundImageId}`);
		}
		return {
			...preset,
			mirror: VoiceSettings.getMirrorCamera(),
			backgroundMode: customMedia || backgroundMode !== 'custom' ? backgroundMode : 'none',
			...(customMedia
				? {
						backgroundCustomMediaPath: customMedia.path,
						backgroundCustomMediaKind: customMedia.mediaKind,
					}
				: {}),
			...(nativeDeviceId ? {deviceId: nativeDeviceId} : {}),
			backgroundBlurStrength: VoiceSettings.getBackgroundBlurStrength(),
		};
	}

	private async getNativeDeviceScreenShareCaptureOptions(
		options?: DeviceScreenShareCaptureOptions,
	): Promise<DeviceScreenShareCaptureOptions> {
		const nativeVideoDeviceId = await resolveNativeCameraDeviceId(options?.videoDeviceId);
		const {videoDeviceId: _videoDeviceId, ...rest} = options ?? {};
		return {
			...rest,
			...(options?.videoDeviceId ? {previewVideoDeviceId: options.videoDeviceId} : {}),
			...(nativeVideoDeviceId ? {videoDeviceId: nativeVideoDeviceId} : {}),
		};
	}

	private clearNativeCameraLocalPreviewTrack(trackSid: string): void {
		assert.ok(trackSid.length > 0, 'native camera preview trackSid must be non-empty before clearing');
		this.recordNativeVideoDiagnostic('camera_preview.local_cleared', {
			trackSid,
		});
		this.nativeVoiceFrameStatsBatcher.removeTrack(trackSid);
		NativeVideoTileManager.unregisterTrack(trackSid);
	}

	private clearNativeCameraLocalPreview(trackSidHint?: string): void {
		const trackSid = this.nativeCameraPreviewTrackSid;
		assert.ok(trackSid === null || trackSid.length > 0, 'native camera preview trackSid must be null or non-empty');
		this.nativeCameraPreviewTrackSid = null;
		if (trackSid) {
			this.clearNativeCameraLocalPreviewTrack(trackSid);
		}
		if (trackSidHint && trackSidHint !== trackSid && trackSidHint !== this.nativeCameraPreviewSessionTrackSid) {
			this.clearNativeCameraLocalPreviewTrack(trackSidHint);
		}
		assert.equal(this.nativeCameraPreviewTrackSid, null, 'native camera preview must be cleared');
	}

	private shouldAcceptNativeCameraPreviewParticipant(identity: string | undefined): boolean {
		if (!identity) return true;
		const connectionId = this.voiceEngineV2Participants.extractConnectionId(identity);
		if (this.voiceEngineV2Participants.isConnectionDiscarded(connectionId)) return false;
		const currentConnectionId = voiceEngineV2AppConnectionHostAdapter.connectionId;
		if (!connectionId || !currentConnectionId) return true;
		return connectionId === currentConnectionId;
	}

	private resolveNativeCameraPreviewParticipant(
		participant?: NativeVoiceEngineLocalTrackParticipant,
	): NativeCameraPreviewParticipant | null {
		if (!this.shouldAcceptNativeCameraPreviewParticipant(participant?.participantIdentity)) {
			logger.debug('Ignoring native camera preview participant from stale connection', {
				participantIdentity: participant?.participantIdentity,
				currentConnectionId: voiceEngineV2AppConnectionHostAdapter.connectionId,
			});
			return null;
		}
		if (participant?.participantSid) {
			return {
				...(participant.participantIdentity ? {identity: participant.participantIdentity} : {}),
				sid: participant.participantSid,
			};
		}
		if (participant?.participantIdentity) {
			return {
				identity: participant.participantIdentity,
				sid: participant.participantIdentity,
			};
		}
		const localParticipant = this.voiceEngineV2Participants.getLocalParticipant();
		if (localParticipant?.identity) {
			return {
				identity: localParticipant.identity,
				sid: localParticipant.sid || localParticipant.identity,
			};
		}
		const currentUserId = Users.getCurrentUser()?.id;
		const connectionId = voiceEngineV2AppConnectionHostAdapter.connectionId;
		if (!currentUserId || !connectionId) return null;
		const identity = buildVoiceParticipantIdentity(currentUserId, connectionId);
		return {
			identity,
			sid: identity,
		};
	}

	private recordNativeCameraPreviewLocalRegisterRequest(
		trackSid: string | undefined,
		participant?: NativeVoiceEngineLocalTrackParticipant,
	): void {
		this.recordNativeVideoDiagnostic('camera_preview.local_register_requested', {
			trackSid: trackSid ?? null,
			participantSid: participant?.participantSid ?? null,
			participantIdentity: participant?.participantIdentity ?? null,
		});
	}

	private recordNativeCameraPreviewLocalRegisterRefused(
		reason: string,
		trackSid: string | null,
		details: Record<string, unknown> = {},
	): void {
		this.recordNativeVideoDiagnostic('camera_preview.local_register_refused', {
			trackSid,
			reason,
			...details,
		});
	}

	private isNativeCameraPreviewLocalTrackCurrent(
		trackSid: string,
		localParticipant: NativeCameraPreviewParticipant,
		registered: NativeInboundVideoTrack | undefined,
	): registered is NativeInboundVideoTrack {
		return (
			this.nativeCameraPreviewTrackSid === trackSid &&
			registered != null &&
			registered.participantSid === localParticipant.sid &&
			registered.participantIdentity === localParticipant.identity
		);
	}

	private isNativeCameraPreviewTrackOwner(
		registered: NativeInboundVideoTrack,
		localParticipant: NativeCameraPreviewParticipant,
	): boolean {
		return (
			registered.participantSid === localParticipant.sid && registered.participantIdentity === localParticipant.identity
		);
	}

	private registerNativeCameraPreviewTrackWithTileManager(
		trackSid: string,
		localParticipant: NativeCameraPreviewParticipant,
	): NativeInboundVideoTrack | null {
		this.clearNativeCameraLocalPreview();
		NativeVideoTileManager.registerTrack(
			localParticipant.sid,
			trackSid,
			VoiceTrackSource.Camera,
			localParticipant.identity,
		);
		return NativeVideoTileManager.tracks[trackSid] ?? null;
	}

	private recordNativeCameraPreviewLocalRegistered(
		trackSid: string,
		localParticipant: NativeCameraPreviewParticipant,
		registered: NativeInboundVideoTrack,
	): void {
		this.recordNativeVideoDiagnostic('camera_preview.local_registered', {
			trackSid,
			participantSid: localParticipant.sid,
			participantIdentity: localParticipant.identity,
			width: registered.width,
			height: registered.height,
		});
	}

	private registerNativeCameraLocalPreviewTrack(
		trackSid: string | undefined,
		participant?: NativeVoiceEngineLocalTrackParticipant,
	): void {
		this.recordNativeCameraPreviewLocalRegisterRequest(trackSid, participant);
		if (!trackSid) {
			this.recordNativeCameraPreviewLocalRegisterRefused('missing-track-sid', null);
			logger.warn('Cannot attach native camera preview without native track SID');
			return;
		}
		assert.ok(trackSid.length > 0, 'native camera preview trackSid must be non-empty');
		const localParticipant = this.resolveNativeCameraPreviewParticipant(participant);
		if (!localParticipant) {
			this.recordNativeCameraPreviewLocalRegisterRefused('missing-local-participant', trackSid);
			logger.warn('Cannot attach native camera preview without local participant');
			return;
		}
		assert.ok(localParticipant.sid.length > 0, 'native camera preview participant sid must be non-empty');
		const registered = NativeVideoTileManager.tracks[trackSid];
		if (this.isNativeCameraPreviewLocalTrackCurrent(trackSid, localParticipant, registered)) {
			this.recordNativeVideoDiagnostic('camera_preview.local_already_registered', {
				trackSid,
				participantSid: localParticipant.sid,
				participantIdentity: localParticipant.identity,
				width: registered.width,
				height: registered.height,
			});
			return;
		}
		const confirmed = this.registerNativeCameraPreviewTrackWithTileManager(trackSid, localParticipant);
		if (confirmed == null) {
			this.recordNativeCameraPreviewLocalRegisterRefused('tile-manager-refused', trackSid, {
				participantSid: localParticipant.sid,
				participantIdentity: localParticipant.identity,
			});
			logger.warn('Native camera preview registration was refused', {trackSid});
			return;
		}
		if (!this.isNativeCameraPreviewTrackOwner(confirmed, localParticipant)) {
			this.recordNativeCameraPreviewLocalRegisterRefused('owner-conflict', trackSid, {
				participantSid: localParticipant.sid,
				participantIdentity: localParticipant.identity,
				registeredParticipantSid: confirmed.participantSid,
				registeredParticipantIdentity: confirmed.participantIdentity,
			});
			logger.warn('Native camera preview registration conflicted with an existing track owner', {
				trackSid,
				registeredParticipantSid: confirmed.participantSid,
				registeredParticipantIdentity: confirmed.participantIdentity,
				expectedParticipantSid: localParticipant.sid,
				expectedParticipantIdentity: localParticipant.identity,
			});
			return;
		}
		this.nativeCameraPreviewTrackSid = trackSid;
		this.recordNativeCameraPreviewLocalRegistered(trackSid, localParticipant, confirmed);
		assert.equal(this.nativeCameraPreviewTrackSid, trackSid, 'native camera preview must record the registered track');
	}

	private async publishNativeCameraFromSettings(options?: {deviceId?: string}): Promise<void> {
		await requireNativeVoiceEngine().publishCamera(await this.getNativeCameraPublishParams(options));
	}

	async updateActiveCameraCapture(options?: {deviceId?: string}): Promise<void> {
		if (!(await shouldUseNativeVoiceEngine()) || !this.readNativeCameraActualEnabled()) {
			await this.setCameraEnabled(true, options);
			return;
		}
		try {
			await requireNativeVoiceEngine().updateCameraCapture(await this.getNativeCameraPublishParams(options));
		} catch (error) {
			logger.warn('Native camera device hot update failed; falling back to camera republish', {error});
			await this.setCameraEnabled(true, options);
		}
		this.syncVideoBackgroundFramePumpInBackground('camera-capture-update');
	}

	private getVideoBackgroundFramePump(): VideoBackgroundFramePump | null {
		if (this.videoBackgroundFramePump) return this.videoBackgroundFramePump;
		const bridge = window.electron?.voiceEngine;
		if (!bridge) return null;
		this.videoBackgroundFramePump = new VideoBackgroundFramePump({
			bridge,
			getCaptureDimensions: () => getCameraCaptureDimensions(VoiceSettings.getCameraResolution()),
		});
		return this.videoBackgroundFramePump;
	}

	private getSelectedVideoBackgroundId(): string | null {
		if (!areVoiceBackgroundsAvailable()) return null;
		const backgroundImageId = VoiceSettings.getBackgroundImageId();
		if (backgroundImageId === NONE_BACKGROUND_ID || backgroundImageId === BLUR_BACKGROUND_ID) return null;
		const image = VoiceSettings.getBackgroundImages().find((entry) => entry.id === backgroundImageId);
		return image?.mediaKind === 'video' ? backgroundImageId : null;
	}

	private shouldRunVideoBackgroundFramePump(): boolean {
		if (!isNativeVoiceEngineSelected()) return false;
		const cameraActive = this.readNativeCameraActualEnabled() || this.nativeCameraPreviewSessionTrackSid !== null;
		if (!cameraActive) return false;
		return this.getSelectedVideoBackgroundId() !== null;
	}

	private async syncVideoBackgroundFramePump(reason: string): Promise<void> {
		const pump = this.getVideoBackgroundFramePump();
		if (!pump) return;
		const backgroundId = this.shouldRunVideoBackgroundFramePump() ? this.getSelectedVideoBackgroundId() : null;
		if (backgroundId) {
			const started = await pump.start(backgroundId);
			if (!started) {
				logger.warn('Video background frame pump could not start', {reason, backgroundId});
			}
			return;
		}
		if (pump.isRunning()) {
			await pump.stop();
		}
	}

	private syncVideoBackgroundFramePumpInBackground(reason: string): void {
		void this.syncVideoBackgroundFramePump(reason).catch((error) => {
			logger.warn('Failed to sync video background frame pump', {reason, error});
		});
	}

	isNativeCameraPreviewSessionAvailable(): boolean {
		if (!isNativeVoiceEngineSelected()) return false;
		const capabilities = getNativeVoiceEngineCapabilitiesSnapshot();
		if (capabilities == null) return true;
		return capabilities.cameraCapture === true;
	}

	private async resolveNativeCameraPreviewCapability(): Promise<boolean> {
		if (!isNativeVoiceEngineSelected()) return false;
		const cached = getNativeVoiceEngineCapabilitiesSnapshot();
		if (cached != null) return cached.cameraCapture === true;
		try {
			const live = await refreshNativeVoiceEngineCapabilitiesSnapshot();
			return live?.cameraCapture === true;
		} catch (error) {
			logger.warn('Failed to query native voice engine capabilities for camera preview', {error});
			return false;
		}
	}

	private ensureNativeVideoTileFrameSubscription(): void {
		this.startNativeVideoTileFrameSubscription('camera-preview');
	}

	private registerNativeCameraPreviewSessionTrack(trackSid: string): boolean {
		assert.ok(trackSid.length > 0, 'native camera preview session trackSid must be non-empty');
		this.recordNativeVideoDiagnostic('camera_preview.session_register_requested', {trackSid});
		const previousTrackSid = this.nativeCameraPreviewSessionTrackSid;
		if (previousTrackSid && previousTrackSid !== trackSid) {
			this.recordNativeVideoDiagnostic('camera_preview.session_previous_cleared', {
				trackSid: previousTrackSid,
				nextTrackSid: trackSid,
			});
			this.nativeVoiceFrameStatsBatcher.removeTrack(previousTrackSid);
			NativeVideoTileManager.unregisterTrack(previousTrackSid);
		}
		NativeVideoTileManager.registerTrack(trackSid, trackSid, VoiceTrackSource.Camera);
		const registered = NativeVideoTileManager.tracks[trackSid];
		if (!registered) {
			this.recordNativeVideoDiagnostic('camera_preview.session_register_refused', {
				trackSid,
				reason: 'tile-manager-refused',
			});
			logger.warn('Native camera preview session registration was refused', {trackSid});
			return false;
		}
		if (registered.participantSid !== trackSid || registered.source !== VoiceTrackSource.Camera) {
			this.recordNativeVideoDiagnostic('camera_preview.session_register_refused', {
				trackSid,
				reason: 'owner-conflict',
				registeredParticipantSid: registered.participantSid,
				registeredSource: registered.source,
			});
			logger.warn('Native camera preview session registration resolved to a conflicting track', {
				trackSid,
				registeredParticipantSid: registered.participantSid,
				registeredSource: registered.source,
			});
			return false;
		}
		this.nativeCameraPreviewSessionTrackSid = trackSid;
		assert.equal(
			this.nativeCameraPreviewSessionTrackSid,
			trackSid,
			'native camera preview session must record the registered track',
		);
		this.recordNativeVideoDiagnostic('camera_preview.session_registered', {
			trackSid,
			width: registered.width,
			height: registered.height,
		});
		return true;
	}

	private async startNativeCameraPreviewSessionNow(
		options: {deviceId?: string} | undefined,
		generation: number,
	): Promise<string | null> {
		assert.ok(
			this.nativeCameraPreviewStartGate.isCurrent(generation),
			'native camera preview start must begin on the current generation',
		);
		if (!(await this.resolveNativeCameraPreviewCapability())) {
			return null;
		}
		const engine = requireNativeVoiceEngine();
		const params = await this.getNativeCameraPublishParams(options);
		if (!this.nativeCameraPreviewStartGate.isCurrent(generation)) {
			return null;
		}
		const info = await engine.startCameraPreview(params);
		assert.ok(info.trackSid.length > 0, 'native camera preview session trackSid must be non-empty');
		if (!this.nativeCameraPreviewStartGate.isCurrent(generation)) {
			await engine.stopCameraPreview().catch((error) => {
				logger.warn('Failed to stop superseded native camera preview session', {error});
			});
			return null;
		}
		this.ensureNativeVideoTileFrameSubscription();
		if (!this.registerNativeCameraPreviewSessionTrack(info.trackSid)) {
			await engine.stopCameraPreview().catch((error) => {
				logger.warn('Failed to stop unregistered native camera preview session', {error});
			});
			return null;
		}
		this.syncVideoBackgroundFramePumpInBackground('camera-preview-session-start');
		return info.trackSid;
	}

	async startNativeCameraPreviewSession(options?: {deviceId?: string}): Promise<string | null> {
		const generation = this.nativeCameraPreviewStartGate.nextGeneration();
		return this.nativeCameraPreviewStartGate.runLatest(generation, () =>
			this.startNativeCameraPreviewSessionNow(options, generation),
		);
	}

	async stopNativeCameraPreviewSession(): Promise<void> {
		this.nativeCameraPreviewStartGate.invalidate();
		const trackSid = this.nativeCameraPreviewSessionTrackSid;
		this.nativeCameraPreviewSessionTrackSid = null;
		this.syncVideoBackgroundFramePumpInBackground('camera-preview-session-stop');
		if (trackSid) {
			this.recordNativeVideoDiagnostic('camera_preview.session_cleared', {trackSid});
		}
		if (isNativeVoiceEngineSelected()) {
			try {
				await requireNativeVoiceEngine().stopCameraPreview();
			} catch (error) {
				logger.warn('Failed to stop native camera preview session', {error});
			}
		}
		if (trackSid) {
			this.nativeVoiceFrameStatsBatcher.removeTrack(trackSid);
			NativeVideoTileManager.unregisterTrack(trackSid);
		}
	}

	getNativeCameraPreviewSessionStream(): MediaStream | null {
		const trackSid = this.nativeCameraPreviewSessionTrackSid;
		if (!trackSid) return null;
		return NativeVideoTileManager.tracks[trackSid]?.stream ?? null;
	}

	private getNativeCameraLocalPreviewTrack(
		localParticipant?: NativeCameraPreviewParticipant | null,
	): NativeInboundVideoTrack | null {
		return selectNativeCameraLocalPreviewTrack({
			currentTrackSid: this.nativeCameraPreviewTrackSid,
			localParticipant: localParticipant ?? this.resolveNativeCameraPreviewParticipant(),
			sessionTrackSid: this.nativeCameraPreviewSessionTrackSid,
			tracks: NativeVideoTileManager.tracks,
		});
	}

	getNativeCameraLocalPreviewStream(localParticipant?: NativeCameraPreviewParticipant | null): MediaStream | null {
		return this.getNativeCameraLocalPreviewTrack(localParticipant)?.stream ?? null;
	}

	isNativeCameraPublished(): boolean {
		if (!isNativeVoiceEngineSelected()) return false;
		return this.readNativeCameraActualEnabled();
	}

	private async setCameraEnabledViaEngine(
		enabled: boolean,
		options?: {
			deviceId?: string;
			sendUpdate?: boolean;
		},
	): Promise<void> {
		const sendUpdate = options?.sendUpdate ?? true;
		const outcome = await runCameraTransition({
			enabled,
			sendUpdate,
			publish: async () => {
				if (enabled) {
					await this.publishNativeCameraFromSettings(options);
				} else {
					this.clearNativeCameraLocalPreview();
					await requireNativeVoiceEngine().unpublishCamera();
				}
			},
			readActualEnabled: () => this.readNativeCameraActualEnabled(),
			onPermissionDenied: () => {
				voiceEngineV2AppMediaStateAdapter.applyCameraState(false, {sendUpdate});
			},
			onSuccessSettled: null,
			onFailure: (_actual, error) => {
				logger.warn('Native voice engine camera state update failed', {enabled, error});
				this.clearNativeCameraLocalPreview();
			},
			rethrowOnFailure: true,
		});
		this.syncVideoBackgroundFramePumpInBackground('camera-state-transition');
		if (outcome === 'denied') {
			logger.warn('Native voice engine camera state update denied by permission', {enabled});
			throw buildVoiceEngineV2AppCameraPermissionDeniedError();
		}
		if (outcome === 'applied') {
			logger.info('Native voice engine camera state updated', {enabled});
		}
	}

	async setScreenShareEnabled(
		enabled: boolean,
		options?: ScreenShareCaptureOptions & {
			sendUpdate?: boolean;
			playSound?: boolean;
			restartIfEnabled?: boolean;
		},
		publishOptions?: TrackPublishOptions,
	): Promise<void> {
		await voiceEngineV2AppScreenShareExecutionAdapter.setScreenShareEnabled(
			this.room,
			enabled,
			options,
			publishOptions,
		);
	}

	async startDeviceScreenShare(
		options?: DeviceScreenShareCaptureOptions,
		publishOptions?: TrackPublishOptions,
	): Promise<void> {
		if (!isNativeVoiceEngineSelected()) {
			await voiceEngineV2AppScreenShareExecutionAdapter.startDeviceScreenShare(this.room, options, publishOptions);
			return;
		}
		await voiceEngineV2AppScreenShareExecutionAdapter.startNativeDeviceScreenShare(
			await this.getNativeDeviceScreenShareCaptureOptions(options),
			{
				sendUpdate: options?.sendUpdate,
				playSound: options?.playSound,
			},
			publishOptions,
		);
	}

	private createScreenShareControllerGateway(): VoiceEngineV2AppScreenShareControllerGateway {
		const plannedScreenOperationIds = (commands: ReadonlyArray<VoiceEngineV2Command>): Array<number> => {
			const operationIds: Array<number> = [];
			for (const command of commands) {
				if (command.type === 'screen.publish') operationIds.push(command.operationId);
				if (command.type === 'screen.updateEncoding') operationIds.push(command.operationId);
				if (command.type === 'screen.unpublish') operationIds.push(command.operationId);
			}
			return operationIds;
		};
		return {
			isScreenCommandRoutable: () => this.voiceEngineV2Snapshot.connection.status === 'connected',
			hasScreenPublication: () => {
				const screen = this.voiceEngineV2Snapshot.screen;
				if (screen.published != null) return true;
				return screen.status === 'publishing' || screen.status === 'unpublishing';
			},
			hasScreenDesired: () => this.voiceEngineV2Snapshot.screen.desired != null,
			clearScreenDesired: () => {
				if (this.voiceEngineV2Snapshot.screen.desired == null) return;
				this.voiceEngineV2Controller.unpublishScreen();
			},
			executingScreenOperationId: () => this.voiceEngineV2Host.executingCommand('screen')?.operationId ?? null,
			isScreenOperationPending: (operationId) => this.voiceEngineV2Host.isOperationPending(operationId),
			publishScreen: (options, onPlanned) =>
				this.voiceEngineV2Host.runAndWait(
					() => {
						this.voiceEngineV2Controller.publishScreen(options);
					},
					{
						description: 'publish screen share',
						staleCompletion: 'resolve',
						onCommandsPlanned: (commands) => onPlanned(plannedScreenOperationIds(commands)),
					},
				),
			unpublishScreen: (onPlanned) =>
				this.voiceEngineV2Host.runAndWait(
					() => {
						this.voiceEngineV2Controller.unpublishScreen();
					},
					{
						description: 'unpublish screen share',
						staleCompletion: 'resolve',
						onCommandsPlanned: (commands) => onPlanned(plannedScreenOperationIds(commands)),
					},
				),
		};
	}

	private getNativeMicrophonePublishOptions(
		options: VoiceEngineV2NativeMicrophonePublishOptions = {},
	): VoiceEngineV2NativeMicrophonePublishOptions {
		const channelId = voiceEngineV2AppConnectionHostAdapter.channelId;
		const channelBitrateBps = channelId ? (Channels.getChannel(channelId)?.bitrate ?? null) : null;
		return resolveVoiceEngineV2NativeMicrophonePublishOptions(VoiceSettings, options, channelBitrateBps);
	}

	private async refreshNativeMicrophoneFromSettings(): Promise<void> {
		await this.voiceEngineV2Host.runAndWait(
			() => {
				this.voiceEngineV2Controller.publishMicrophone(this.getNativeMicrophonePublishOptions());
			},
			{description: 'refresh microphone settings'},
		);
	}

	async setMicEnabledViaEngine(enabled: boolean): Promise<void> {
		assert.equal(typeof enabled, 'boolean', 'setMicEnabledViaEngine enabled must be a boolean');
		if (enabled) {
			await this.voiceEngineV2Host.runAndWait(
				() => {
					this.voiceEngineV2Controller.publishMicrophone(this.getNativeMicrophonePublishOptions());
				},
				{description: 'publish microphone', staleCompletion: 'resolve'},
			);
		}
		await this.voiceEngineV2Host.runAndWait(
			() => {
				this.voiceEngineV2Controller.setMicrophoneEnabled(enabled);
			},
			{description: 'set microphone enabled', staleCompletion: 'resolve'},
		);
	}

	async replaceActiveDisplayScreenShare(
		options?: ScreenShareCaptureOptions,
		publishOptions?: TrackPublishOptions,
	): Promise<boolean> {
		if (!isNativeVoiceEngineSelected()) {
			return voiceEngineV2AppScreenShareExecutionAdapter.replaceActiveDisplayScreenShare(
				this.room,
				options,
				publishOptions,
			);
		}
		return voiceEngineV2AppScreenShareExecutionAdapter.replaceActiveNativeDisplayScreenShareFromActiveSource(
			options,
			publishOptions,
		);
	}

	async replaceActiveDeviceScreenShare(
		options?: DeviceScreenShareCaptureOptions,
		publishOptions?: TrackPublishOptions,
	): Promise<boolean> {
		if (!isNativeVoiceEngineSelected()) {
			return voiceEngineV2AppScreenShareExecutionAdapter.replaceActiveDeviceScreenShare(
				this.room,
				options,
				publishOptions,
			);
		}
		return voiceEngineV2AppScreenShareExecutionAdapter.replaceActiveNativeDeviceScreenShare(
			await this.getNativeDeviceScreenShareCaptureOptions(options),
			publishOptions,
		);
	}

	async startNativeDisplayScreenShare(
		nativeOptions: NativeScreenShareOptions,
		options?: {sendUpdate?: boolean; playSound?: boolean},
		publishOptions?: TrackPublishOptions,
	): Promise<void> {
		await voiceEngineV2AppScreenShareExecutionAdapter.startNativeDisplayScreenShare(
			this.room,
			nativeOptions,
			options,
			publishOptions,
		);
	}

	async replaceActiveNativeDisplayScreenShare(
		nativeOptions: NativeScreenShareOptions,
		options?: ScreenShareCaptureOptions,
		publishOptions?: TrackPublishOptions,
	): Promise<boolean> {
		return voiceEngineV2AppScreenShareExecutionAdapter.replaceActiveNativeDisplayScreenShare(
			this.room,
			nativeOptions,
			options,
			publishOptions,
		);
	}

	async ensureLinuxScreenShareAudioPublication(
		linuxRule?: NonNullable<NativeAudioStartOptions['linuxRule']>,
		options?: {includeSelfWindowAudio?: boolean; replaceExisting?: boolean},
	): Promise<boolean> {
		if (isNativeVoiceEngineSelected()) {
			return this.ensureNativeLinuxScreenShareAudioPublication(linuxRule);
		}
		return voiceEngineV2AppScreenShareExecutionAdapter.ensureLinuxScreenShareAudioPublication(
			this.room,
			linuxRule,
			options,
		);
	}

	private async ensureNativeLinuxScreenShareAudioPublication(
		linuxRule?: NonNullable<NativeAudioStartOptions['linuxRule']>,
	): Promise<boolean> {
		assert.ok(isNativeVoiceEngineSelected(), 'native Linux screen-share audio link requires the native engine');
		if (!linuxRule) return false;
		const adapter = voiceEngineV2AppScreenShareExecutionAdapter;
		const currentOptions = adapter.captureCoordinator.activeCaptureOptions;
		if (!adapter.captureCoordinator.activeCaptureId || !currentOptions) {
			logger.debug('Skipping native Linux screen-share audio link without an active native screen share');
			return false;
		}
		if (adapter.isScreenSharePending) {
			logger.warn('Skipping native Linux screen-share audio link while a screen-share operation is pending');
			return false;
		}
		const activeRule = currentOptions.nativeAudioLinuxRule ?? null;
		const ruleUnchanged = activeRule != null && areLinuxAudioRoutingRulesEqual(activeRule, linuxRule);
		if (adapter.nativeEngineScreenShareAudioPump && ruleUnchanged) {
			logger.debug('Native Linux screen-share audio link already matches the requested routing rule');
			return true;
		}
		const {audioTrack: _audioTrack, ...restOptions} = currentOptions;
		const nextOptions: NativeScreenShareOptions = {...restOptions, nativeAudioLinuxRule: linuxRule};
		const published = await adapter.audioPump.startAudio(nextOptions);
		if (!published) {
			logger.warn('Native Linux screen-share audio link failed to adopt the requested routing rule');
			return false;
		}
		adapter.adoptNativeEngineScreenShareOptionsInternal(nextOptions);
		LocalVoiceState.updateSelfStreamAudio(true);
		assert.ok(adapter.nativeEngineScreenShareAudioPump != null, 'native screen-share audio pump must be active');
		logger.info('Native Linux screen-share audio link adopted new routing rule');
		return true;
	}

	setScreenShareAudioMuted(muted: boolean): void {
		voiceEngineV2AppScreenShareExecutionAdapter.setScreenShareAudioMuted(this.room, muted);
	}

	async updateActiveScreenShareSettings(
		options?: ScreenShareCaptureOptions,
		publishOptions?: TrackPublishOptions,
	): Promise<boolean> {
		return voiceEngineV2AppScreenShareExecutionAdapter.updateActiveScreenShareSettings(
			this.room,
			options,
			publishOptions,
		);
	}

	applyLocalAudioPreferencesForUser(userId: string): void {
		voiceEngineV2AppMediaExecutionAdapter.applyLocalAudioPreferencesForUser(userId, this.room);
	}

	applyAllLocalAudioPreferences(): void {
		voiceEngineV2AppMediaExecutionAdapter.applyAllLocalAudioPreferences(this.room);
	}

	applyLocalInputVolume(): void {
		voiceEngineV2AppMediaExecutionAdapter.applyLocalInputVolume(this.room);
	}

	setLocalVideoDisabled(identity: string, disabled: boolean): void {
		voiceEngineV2AppMediaExecutionAdapter.setLocalVideoDisabled(identity, disabled, this.room, this.connectionId);
	}

	applyPushToTalkHold(held: boolean): void {
		voiceEngineV2AppMediaExecutionAdapter.applyPushToTalkHold(held, this.room, () => this.getCurrentUserVoiceState());
		this.syncVoiceEngineV2AudioControlsFromAppState();
	}

	applyPushToMuteHold(held: boolean): void {
		voiceEngineV2AppMediaExecutionAdapter.applyPushToMuteHold(held, this.room, () => this.getCurrentUserVoiceState());
		this.syncVoiceEngineV2AudioControlsFromAppState();
	}

	handlePushToTalkModeChange(): void {
		voiceEngineV2AppMediaExecutionAdapter.handlePushToTalkModeChange(this.room, () => this.getCurrentUserVoiceState());
		this.syncVoiceEngineV2AudioControlsFromAppState();
	}

	getMuteReason(voiceState: VoiceState | null): VoiceMuteReason {
		return voiceEngineV2AppMediaExecutionAdapter.getMuteReason(voiceState, this.guildId, this.channelId);
	}

	async toggleCameraFromKeybind(): Promise<void> {
		const current = LocalVoiceState.getSelfVideo();
		await this.setCameraEnabled(!current, {deviceId: VoiceSettings.getVideoDeviceId()});
	}

	async toggleScreenShareFromKeybind(): Promise<void> {
		await voiceEngineV2AppScreenShareExecutionAdapter.toggleScreenShareFromKeybind(this.room);
	}

	private startTracking(roomOverride?: Room | null): void {
		const room = roomOverride ?? voiceEngineV2AppConnectionHostAdapter.room;
		if (!room) {
			logger.warn('No room available');
			return;
		}
		startVoiceMediaGraphTimerScheduler();
		this.statsHostAdapter.setRoom(room);
		this.statsHostAdapter.startLatencyTracking();
		this.statsHostAdapter.startStatsTracking();
		VoiceEngineV2AppSubscriptionAdapter.setRoom(room);
		VoiceEngineV2AppPermissionAdapter.initializeSubscriptions(room);
		this.outputDeviceSyncDisposer?.();
		this.outputDeviceSyncDisposer = bindOutputDeviceSync(room);
		this.audioPreferencesSyncDisposer?.();
		this.audioPreferencesSyncDisposer = this.bindAudioPreferencesSync(room);
		this.videoCodecDecodeCapResyncDisposer?.();
		this.videoCodecDecodeCapResyncDisposer = this.bindVideoDecodeCapResync();
		this.videoCodecPublishOverrideSyncDisposer?.();
		this.videoCodecPublishOverrideSyncDisposer = this.bindVideoCodecPublishOverrideSync();
		this.videoCodecGossipReceiverDisposer?.();
		this.videoCodecGossipReceiverDisposer = this.bindVideoCodecGossipReceiver(room);
		this.startAfkTracking();
		const channelId = voiceEngineV2AppConnectionHostAdapter.channelId;
		if (channelId) {
			void voiceEngineV2AppDebugLoggingHostAdapter.start({
				guildId: voiceEngineV2AppConnectionHostAdapter.guildId,
				channelId,
				connectionId: voiceEngineV2AppConnectionHostAdapter.connectionId,
				room,
				collectSnapshot: () => this.createVoiceDiagnosticsSnapshot(),
			});
		}
		logger.info('All tracking started');
	}

	private stopTracking(): void {
		void voiceEngineV2AppDebugLoggingHostAdapter.stop('tracking-stopped');
		this.statsHostAdapter.stopLatencyTracking();
		this.statsHostAdapter.stopStatsTracking();
		VoiceEngineV2AppRemoteSpeakingAdapter.clear();
		VoiceEngineV2AppSubscriptionAdapter.cleanup();
		VoiceEngineV2AppPermissionAdapter.reset();
		this.outputDeviceSyncDisposer?.();
		this.outputDeviceSyncDisposer = null;
		this.audioPreferencesSyncDisposer?.();
		this.audioPreferencesSyncDisposer = null;
		this.videoCodecDecodeCapResyncDisposer?.();
		this.videoCodecDecodeCapResyncDisposer = null;
		this.videoCodecPublishOverrideSyncDisposer?.();
		this.videoCodecPublishOverrideSyncDisposer = null;
		this.videoCodecGossipReceiverDisposer?.();
		this.videoCodecGossipReceiverDisposer = null;
		this.previousWatchedStreamCodecGossip = new Map();
		this.stopAfkTracking();
		logger.info('All tracking stopped');
	}

	private async createVoiceDiagnosticsSnapshot(): Promise<Record<string, unknown>> {
		await Promise.allSettled([loadGpuEncoderReport(), loadOpenH264Status(), loadVideoDecoderExclusions()]);
		return {
			connection: {
				guildId: voiceEngineV2AppConnectionHostAdapter.guildId,
				channelId: voiceEngineV2AppConnectionHostAdapter.channelId,
				connectionId: voiceEngineV2AppConnectionHostAdapter.connectionId,
				connected: voiceEngineV2AppConnectionHostAdapter.connected,
				connecting: voiceEngineV2AppConnectionHostAdapter.connecting,
				reconnecting: voiceEngineV2AppConnectionHostAdapter.reconnecting,
				disconnecting: voiceEngineV2AppConnectionHostAdapter.disconnecting,
				voiceServerEndpoint: voiceEngineV2AppConnectionHostAdapter.voiceServerEndpoint,
				regionHotSwapInProgress: voiceEngineV2AppConnectionHostAdapter.regionHotSwapInProgress,
				reconnectAttempts: voiceEngineV2AppConnectionHostAdapter.reconnectAttempts,
				nativeVoiceEngineSelected: isNativeVoiceEngineSelected(),
			},
			localVoiceState: {
				selfMute: LocalVoiceState.getSelfMute(),
				selfDeaf: LocalVoiceState.getSelfDeaf(),
				selfVideo: LocalVoiceState.getSelfVideo(),
				selfStream: LocalVoiceState.getSelfStream(),
				selfStreamAudio: LocalVoiceState.getSelfStreamAudio(),
				selfStreamAudioMute: LocalVoiceState.getSelfStreamAudioMute(),
				viewerStreamKeys: LocalVoiceState.getViewerStreamKeys(),
				hasUserSetMute: LocalVoiceState.getHasUserSetMute(),
				hasUserSetDeaf: LocalVoiceState.getHasUserSetDeaf(),
			},
			effectiveAudioState: getEffectiveAudioState(),
			connectionVoiceStates: voiceEngineV2AppVoiceStateAdapter.getConnectionVoiceStates(),
			participants: Object.values(this.voiceEngineV2Participants.participants),
			stats: {
				currentLatency: this.currentLatency,
				averageLatency: this.averageLatency,
				displayLatency: this.displayLatency,
				estimatedLatency: this.estimatedLatency,
				reconnectionCount: this.reconnectionCount,
				voiceStats: this.voiceStats,
				perTrackStats: this.perTrackStats,
				statsTimeSeriesTail: this.statsTimeSeries.slice(-30),
				latencyHistoryTail: this.latencyHistory.slice(-30),
				publisherTransport: this.publisherTransport,
				subscriberTransport: this.subscriberTransport,
			},
			screenShare: {
				selectedCodec: ScreenShareCodecNegotiation.getSelectedCodec(),
				preferenceOrder: getScreenShareCodecPreferenceOrder(),
				capture: await getScreenShareCaptureDiagnosticSnapshot(),
				audioCapture: await createVoiceAudioDiagnosticsSnapshot(),
				codecCapabilities: {
					localAdvertisements: buildLocalCodecAdvertisements(),
					report: getCodecCapabilityReport(),
					gpuEncoderReport: getGpuEncoderReportSync(),
					openH264Status: getOpenH264StatusSync(),
					decoderExclusions: getVideoDecoderExclusionsSync(),
				},
				adaptiveQuality: AdaptiveScreenShareEngine.qualitySnapshot,
				settings: {
					preferredScreenShareCodec: VoiceSettings.getPreferredScreenShareCodec(),
					screenShareEncoderMode: VoiceSettings.getScreenShareEncoderMode(),
					screenShareSoftwareQuality: VoiceSettings.getScreenShareSoftwareQuality(),
					screenShareScalabilityMode: VoiceSettings.getScreenShareScalabilityMode(),
					screenShareBackupCodecMode: VoiceSettings.getScreenShareBackupCodecMode(),
					screenShareMaxBitrateMbps: VoiceSettings.getScreenShareMaxBitrateMbps(),
					adaptiveScreenShareQuality: VoiceSettings.getAdaptiveScreenShareQuality(),
					screenshareResolution: VoiceSettings.getScreenshareResolution(),
					videoFrameRate: VoiceSettings.getVideoFrameRate(),
					streamingMode: VoiceSettings.getStreamingMode(),
					shareAppAudio: VoiceSettings.getShareAppAudio(),
					shareDesktopAudio: VoiceSettings.getShareDesktopAudio(),
					shareDeviceAudio: VoiceSettings.getShareDeviceAudio(),
					screenShareAudioSourceMode: VoiceSettings.getScreenShareAudioSourceMode(),
					screenShareAudioIncludeSources: VoiceSettings.getScreenShareAudioIncludeSources(),
					screenShareAudioExcludeSources: VoiceSettings.getScreenShareAudioExcludeSources(),
				},
			},
		};
	}

	private abortVoiceConnection(): void {
		this.transitionFacadeState({type: 'screenShareReconnect.clear'});
		voiceEngineV2AppConnectionHostAdapter.abortConnection();
	}

	private bindVideoCodecPublishOverrideSync(): () => void {
		assert.ok(this.voiceEngineV2Host != null, 'video codec publish override sync requires the v2 host');
		const apply = (source: VoiceEngineV2LocalStreamSource, preference: CodecPreference): void => {
			this.voiceEngineV2Controller.setVideoCodecOverride(source, preference === 'auto' ? null : preference);
		};
		let previousScreen: CodecPreference = VoiceSettings.getPreferredScreenShareCodec();
		let previousCamera: CodecPreference = VoiceSettings.getPreferredVideoCodec();
		apply('screen', previousScreen);
		apply('camera', previousCamera);
		return VoiceSettings.subscribe(() => {
			const screen = VoiceSettings.getPreferredScreenShareCodec();
			if (screen !== previousScreen) {
				previousScreen = screen;
				apply('screen', screen);
			}
			const camera = VoiceSettings.getPreferredVideoCodec();
			if (camera !== previousCamera) {
				previousCamera = camera;
				apply('camera', camera);
			}
		});
	}

	private bindVideoDecodeCapResync(): () => void {
		assert.ok(this.voiceEngineV2Host != null, 'video decode cap resync requires the v2 host');
		let previous: CodecPreference = VoiceSettings.getEmulatedDecodeVideoCodecCap();
		return VoiceSettings.subscribe(() => {
			const current = VoiceSettings.getEmulatedDecodeVideoCodecCap();
			if (current === previous) return;
			previous = current;
			this.previousWatchedStreamCodecGossip = new Map();
			this.syncWatchedStreamCodecGossip();
		});
	}

	private scheduleLocalStreamCodecReconcile(): void {
		if (this.localStreamCodecReconcileScheduled) return;
		this.localStreamCodecReconcileScheduled = true;
		queueMicrotask(() => {
			this.localStreamCodecReconcileScheduled = false;
			if (this.voiceEngineV2Host == null) return;
			this.reconcileLocalStreamCodec('camera', this.voiceEngineV2Snapshot.camera);
			this.reconcileLocalStreamCodec('screen', this.voiceEngineV2Snapshot.screen);
			this.syncWatchedStreamCodecGossip();
		});
	}

	private syncWatchedStreamCodecGossip(): void {
		const result = computeVoiceEngineV2WatchedStreamGossip(
			this.previousWatchedStreamCodecGossip,
			this.voiceEngineV2Snapshot.watchedStreams,
			getLocalDecodableVideoCodecs(VoiceSettings.getEmulatedDecodeVideoCodecCap()),
		);
		this.previousWatchedStreamCodecGossip = result.next;
		for (const {destinationIdentity, message} of result.messages) {
			this.voiceEngineV2Controller.publishData({
				payload: encodeVoiceEngineV2CodecGossip(message),
				reliable: true,
				topic: VOICE_ENGINE_V2_CODEC_GOSSIP_TOPIC,
				destinationIdentities: [destinationIdentity],
			});
		}
	}

	private bindVideoCodecGossipReceiver(room: Room): () => void {
		const handler = (payload: Uint8Array, participant?: Participant, _kind?: unknown, topic?: string): void => {
			if (topic !== VOICE_ENGINE_V2_CODEC_GOSSIP_TOPIC) return;
			const identity = participant?.identity;
			if (!identity) return;
			ingestVoiceEngineV2CodecGossip(this.voiceEngineV2Controller, identity, payload);
		};
		room.on(RoomEvent.DataReceived, handler);
		return () => {
			room.off(RoomEvent.DataReceived, handler);
		};
	}

	private reconcileLocalStreamCodec(
		source: VoiceEngineV2LocalStreamSource,
		media: {
			status: string;
			published: {codec?: VoiceEngineV2VideoCodec} | null;
			desired: {codec?: VoiceEngineV2VideoCodec} | null;
		},
	): void {
		const registered = this.voiceEngineV2Snapshot.codecNegotiation.streams[source] != null;
		const active = media.published != null || media.status === 'publishing';
		if (!active) {
			if (registered) this.voiceEngineV2Controller.unregisterLocalStreamCodec(source);
			return;
		}
		if (registered) return;
		const codec: VoiceEngineV2VideoCodec = media.published?.codec ?? media.desired?.codec ?? '';
		const streamIdentity = `${source}:${crypto.randomUUID()}`;
		this.voiceEngineV2Controller.registerLocalStreamCodec(source, streamIdentity, codec);
	}

	private bindNativeAudioPreferencesSync(): () => void {
		assert.ok(this.voiceEngineV2Host != null, 'native audio preferences sync requires the v2 host');
		let previous = createVoiceEngineV2AppAudioSettingsSnapshot();
		const sync = (): void => {
			const current = createVoiceEngineV2AppAudioSettingsSnapshot();
			const captureChanged = hasVoiceEngineV2MicrophoneCaptureSettingsChanged(previous, current);
			const processorChanged = hasVoiceEngineV2InputProcessorSettingsChanged(previous, current);
			previous = current;
			if (!captureChanged && !processorChanged) return;
			if (this.voiceEngineV2Snapshot.microphone.published == null) return;
			void this.refreshNativeMicrophoneFromSettings().catch((error) => {
				logger.warn('Failed to refresh native microphone after audio settings change', {error});
			});
		};
		const disposers = [VoiceSettings.subscribe(sync), VoiceDevicePermissionState.subscribe(sync)];
		assert.equal(disposers.length, 2, 'native audio preferences sync must subscribe to both settings stores');
		return () => {
			for (const dispose of disposers) {
				dispose();
			}
		};
	}

	private bindAudioPreferencesSync(room: Room): () => void {
		return bindVoiceEngineV2AppAudioPreferencesSync(
			room,
			{
				refreshMicrophone: async () => this.refreshMicrophoneFromCurrentEngine(),
				refreshLocalVoiceInputProcessor: async () => {
					if (await shouldUseNativeVoiceEngine()) {
						await this.refreshNativeMicrophoneFromSettings();
						return;
					}
					await voiceEngineV2AppMediaExecutionAdapter.refreshLocalVoiceInputProcessor(room);
				},
				applyLocalInputVolume: () => voiceEngineV2AppMediaExecutionAdapter.applyLocalInputVolume(room),
				applyAllLocalAudioPreferences: () => voiceEngineV2AppMediaExecutionAdapter.applyAllLocalAudioPreferences(room),
			},
			logger,
		);
	}

	private startAfkTracking(): void {
		this.stopAfkTracking();
		this.afkIntervalId = setInterval(() => {
			if (
				!voiceEngineV2AppConnectionHostAdapter.connected ||
				!voiceEngineV2AppConnectionHostAdapter.guildId ||
				!voiceEngineV2AppConnectionHostAdapter.channelId
			)
				return;
			const guild = Guilds.getGuild(voiceEngineV2AppConnectionHostAdapter.guildId);
			if (
				shouldMoveToAfkOnTick({
					hasRecentVoiceActivity: this.noteCurrentLocalVoiceActivity(),
					channelId: voiceEngineV2AppConnectionHostAdapter.channelId,
					afkChannelId: guild?.afkChannelId,
					afkTimeoutSeconds: guild?.afkTimeout,
					inactiveDurationMs: Idle.getInactiveDurationMs(),
				})
			) {
				void this.moveToAfkChannel();
			}
		}, AFK_CHECK_INTERVAL_MS);
	}

	private noteCurrentLocalVoiceActivity(): boolean {
		const livekitParticipant = voiceEngineV2AppConnectionHostAdapter.room?.localParticipant;
		if (noteLocalVoiceActivity(livekitParticipant)) return true;
		return noteLocalVoiceActivityFromSnapshot(this.voiceEngineV2Participants.getLocalParticipant());
	}

	private stopAfkTracking(): void {
		if (this.afkIntervalId !== null) {
			clearInterval(this.afkIntervalId);
			this.afkIntervalId = null;
		}
	}

	private playSpectatorSounds(oldStreamKeys: ReadonlyArray<string>, newStreamKeys: ReadonlyArray<string>): void {
		if (VoiceRegionTeleport.shouldSuppressRejoinSounds()) return;
		const myConnectionId = voiceEngineV2AppConnectionHostAdapter.connectionId;
		if (!myConnectionId) return;
		const myViewerStreamKeys = LocalVoiceState.getViewerStreamKeys();
		const isRelevantStream = (key: string): boolean => {
			const parsed = parseStreamKey(key);
			if (!parsed) return false;
			if (parsed.connectionId === myConnectionId) return true;
			if (myViewerStreamKeys.includes(key)) return true;
			return false;
		};
		const isEndedLocalStream = (key: string): boolean => {
			const parsed = parseStreamKey(key);
			return parsed?.connectionId === myConnectionId && !LocalVoiceState.getSelfStream();
		};
		const oldRelevant = new Set(oldStreamKeys.filter(isRelevantStream));
		const newRelevant = new Set(newStreamKeys.filter(isRelevantStream));
		const joined = newStreamKeys.filter((k) => isRelevantStream(k) && !oldRelevant.has(k));
		const left = oldStreamKeys.filter((k) => isRelevantStream(k) && !newRelevant.has(k) && !isEndedLocalStream(k));
		if (joined.length > 0) {
			SoundCommands.playSound(SoundType.ViewerJoin);
		} else if (left.length > 0) {
			SoundCommands.playSound(SoundType.ViewerLeave);
		}
	}

	private replaceViewerStreamConnectionId(
		keys: ReadonlyArray<string>,
		guildId: string | null | undefined,
		channelId: string | null | undefined,
		previousConnectionId: string | null | undefined,
		nextConnectionId: string | null | undefined,
	): Array<string> {
		if (!channelId || !previousConnectionId || !nextConnectionId || previousConnectionId === nextConnectionId) {
			return [...keys];
		}
		const normalizedGuildId = normalizeStreamGuildId(guildId);
		const previousKey = getStreamKey(normalizedGuildId, channelId, previousConnectionId);
		const nextKey = getStreamKey(normalizedGuildId, channelId, nextConnectionId);
		let changed = false;
		const updated: Array<string> = [];
		for (const key of keys) {
			const migratedKey = key === previousKey ? nextKey : key;
			changed ||= migratedKey !== key;
			if (!updated.includes(migratedKey)) {
				updated.push(migratedKey);
			}
		}
		return changed ? updated : [...keys];
	}

	private migratePinnedScreenShareIdentity(
		userId: string,
		previousConnectionId: string | null | undefined,
		nextConnectionId: string | null | undefined,
	): void {
		if (!previousConnectionId || !nextConnectionId || previousConnectionId === nextConnectionId) return;
		if (VoiceCallLayout.pinnedParticipantSource !== VoiceTrackSource.ScreenShare) return;
		if (VoiceCallLayout.pinnedParticipantIdentity !== buildVoiceParticipantIdentity(userId, previousConnectionId))
			return;
		VoiceCallLayout.setPinnedParticipant(
			buildVoiceParticipantIdentity(userId, nextConnectionId),
			VoiceTrackSource.ScreenShare,
		);
	}

	private migrateWatchedRemoteStreamConnection(guildId: string | null, voiceState: VoiceState): void {
		const channelId = voiceState.channel_id;
		const nextConnectionId = voiceState.connection_id;
		if (!channelId || !nextConnectionId || !voiceState.self_stream) return;
		const normalizedGuildId = normalizeStreamGuildId(guildId ?? voiceState.guild_id);
		const previousStates = voiceEngineV2AppVoiceStateAdapter.getAllVoiceStatesInChannel(
			normalizedGuildId ?? ME,
			channelId,
		);
		const currentKeys = LocalVoiceState.getViewerStreamKeys();
		let updatedKeys: Array<string> = [...currentKeys];
		let migrated = false;
		for (const previousConnectionKey in previousStates) {
			const previousState = previousStates[previousConnectionKey];
			if (!previousState) continue;
			const previousConnectionId = previousState.connection_id;
			if (
				previousState.user_id !== voiceState.user_id ||
				!previousConnectionId ||
				previousConnectionId === nextConnectionId ||
				!previousState.self_stream
			) {
				continue;
			}
			const nextKeys = this.replaceViewerStreamConnectionId(
				updatedKeys,
				normalizedGuildId,
				channelId,
				previousConnectionId,
				nextConnectionId,
			);
			if (!areOrderedStringArraysEqual(nextKeys, updatedKeys)) {
				this.migratePinnedScreenShareIdentity(voiceState.user_id, previousConnectionId, nextConnectionId);
				updatedKeys = nextKeys;
				migrated = true;
			}
		}
		const result = replaceWatchedStreamKeys(updatedKeys, {sync: false});
		if (!result.changed) return;
		this.syncLocalVoiceStateWithServer({viewer_stream_keys: result.keys});
		if (migrated) {
			logger.info('Migrated watched stream connection id', {
				userId: voiceState.user_id,
				channelId,
				nextConnectionId,
			});
		}
	}

	disconnectRemoteDevice(guildId: string, connectionId: string): void {
		if (connectionId === voiceEngineV2AppConnectionHostAdapter.connectionId) {
			void this.disconnectFromVoiceChannel('user');
			return;
		}
		this.discardVoiceConnection(connectionId);
		sendVoiceStateDisconnect(guildId, connectionId);
	}

	disconnectAllRemoteDevices(
		devices: ReadonlyArray<{
			guildId: string;
			connectionId: string;
		}>,
	): void {
		let shouldDisconnectCurrentDevice = false;
		const currentConnectionId = voiceEngineV2AppConnectionHostAdapter.connectionId;
		for (const device of devices) {
			if (device.connectionId === currentConnectionId) {
				shouldDisconnectCurrentDevice = true;
				continue;
			}
			this.discardVoiceConnection(device.connectionId);
			sendVoiceStateDisconnect(device.guildId, device.connectionId);
		}
		if (shouldDisconnectCurrentDevice) {
			void this.disconnectFromVoiceChannel('user');
		}
	}

	async moveToAfkChannel(): Promise<void> {
		const {guildId, channelId, connected} = voiceEngineV2AppConnectionHostAdapter.connectionState;
		if (!connected || !guildId || !channelId) return;
		const guild = Guilds.getGuild(guildId);
		if (!guild?.afkChannelId || channelId === guild.afkChannelId) return;
		await this.connectToVoiceChannel(guildId, guild.afkChannelId);
	}

	getLastConnectedChannel(): {
		guildId: string;
		channelId: string;
	} | null {
		return voiceEngineV2AppConnectionHostAdapter.lastConnectedChannel;
	}

	getShouldReconnect(): boolean {
		return voiceEngineV2AppConnectionHostAdapter.shouldAutoReconnect;
	}

	markReconnectionAttempted(): void {
		voiceEngineV2AppConnectionHostAdapter.markReconnectionAttempted();
	}

	async handleLogout(): Promise<void> {
		this.terminalUnloadVoiceDisconnectSent = false;
		this.cancelPendingServerDisconnect();
		this.clearNativeVoiceTransportReconnect();
		this.clearNativeVoiceConnectRetry();
		this.clearNativeVoiceConnectSession();
		this.clearPendingNativeLocalMediaReconnect();
		this.stopNativeVoiceStatsSession();
		this.lastNativeVoiceServerUpdate = null;
		this.transitionFacadeState({type: 'cleanup.logoutStarted'});
		this.clearViewerStreamKeys();
		this.stopTracking();
		await this.stopActiveScreenShareForTeardown('disconnect');
		VoiceSessionRestore.clearSnapshot();
		voiceEngineV2AppConnectionHostAdapter.cleanup();
		voiceEngineV2AppVoiceStateAdapter.clearAllVoiceStates();
		this.voiceEngineV2Participants.clear();
		VoiceEngineV2AppPermissionAdapter.reset();
		this.resetLocalMediaAndScreenShareTracking();
		voiceEngineV2AppMediaStateAdapter.resetLocalMediaState('logout');
		try {
			await voiceStatsDB.clear();
		} catch (error) {
			logger.error('Failed to clear voice stats DB during logout', error);
			throw error;
		} finally {
			logger.info('Cleanup complete');
			this.transitionFacadeState({type: 'cleanup.complete'});
		}
	}

	handleGatewayError(error: GatewayErrorData): void {
		const decision = selectMediaEngineGatewayErrorDecision(this.facadeSnapshot, {
			code: error.code,
			connecting: voiceEngineV2AppConnectionHostAdapter.connecting,
			connected: voiceEngineV2AppConnectionHostAdapter.connected,
			channelId: voiceEngineV2AppConnectionHostAdapter.channelId,
		});
		if (decision.type === 'ignore') return;
		logger.warn(`Voice-related gateway error: [${error.code}] ${error.message}`);
		if (decision.clearPendingSessionRestore) {
			this.transitionFacadeState({type: 'sessionRestore.clear'});
		}
		if (decision.unavailableChannelId) {
			this.clearUnavailableVoiceTarget(decision.unavailableChannelId, 'gateway-error', {
				showToast: decision.showUnavailableToast,
			});
		}
		if (decision.clearViewerStreamKeys) {
			this.clearViewerStreamKeys();
		}
		if (decision.abortConnection) {
			logger.info('Gateway voice error while connecting, aborting', {code: error.code});
			this.abortVoiceConnection();
		}
		if (decision.disconnectReason) {
			void this.disconnectFromVoiceChannel(decision.disconnectReason);
		}
		if (decision.toast) {
			if (!this.i18n) {
				throw new Error('MediaEngineFacade: i18n not initialized');
			}
			const descriptor =
				decision.toast === 'timed-out'
					? YOU_CAN_T_JOIN_WHILE_YOU_RE_ON_DESCRIPTOR
					: decision.toast === 'connection-limit'
						? VOICE_CONNECTION_LIMIT_REACHED_DESCRIPTOR
						: CLAIM_YOUR_ACCOUNT_TO_JOIN_THIS_VOICE_CHANNEL_DESCRIPTOR;
			this.showVoiceErrorModal(descriptor, `voice.media-engine-facade.gateway-${decision.toast}-error-modal`);
		}
	}

	cleanup(): void {
		this.terminalUnloadVoiceDisconnectSent = false;
		this.cancelPendingServerDisconnect();
		this.syncVideoBackgroundFramePumpInBackground('facade-cleanup');
		this.clearNativeVoiceTransportReconnect();
		this.clearNativeVoiceConnectRetry();
		this.clearNativeVoiceConnectSession();
		this.clearPendingNativeLocalMediaReconnect();
		this.stopNativeVoiceStatsSession();
		this.lastNativeVoiceServerUpdate = null;
		this.transitionFacadeState({type: 'cleanup.cleanupStarted'});
		this.clearViewerStreamKeys();
		this.stopTracking();
		this.statsHostAdapter.cleanup();
		VoiceEngineV2AppSubscriptionAdapter.cleanup();
		VoiceEngineV2AppPermissionAdapter.reset();
		this.resetLocalMediaAndScreenShareTracking();
		voiceEngineV2AppConnectionHostAdapter.cleanup();
		voiceEngineV2AppVoiceStateAdapter.clearAllVoiceStates();
		this.voiceEngineV2Participants.clear();
		voiceEngineV2AppMediaStateAdapter.resetLocalMediaState('cleanup');
		this.transitionFacadeState({type: 'cleanup.complete'});
	}

	reset(): void {
		this.terminalUnloadVoiceDisconnectSent = false;
		this.cancelPendingServerDisconnect();
		this.clearNativeVoiceTransportReconnect();
		this.clearNativeVoiceConnectRetry();
		this.clearNativeVoiceConnectSession();
		this.clearPendingNativeLocalMediaReconnect();
		this.stopNativeVoiceStatsSession();
		this.lastNativeVoiceServerUpdate = null;
		this.clearViewerStreamKeys();
		this.statsHostAdapter.reset();
		this.transitionFacadeState({type: 'cleanup.reset'});
		voiceEngineV2AppConnectionHostAdapter.resetConnectionState();
		voiceEngineV2AppConnectionHostAdapter.resetReconnectState();
		VoiceEngineV2AppRemoteSpeakingAdapter.clear();
		VoiceEngineV2AppPermissionAdapter.reset();
		this.resetLocalMediaAndScreenShareTracking();
		this.voiceEngineV2Participants.clear();
		voiceEngineV2AppMediaStateAdapter.resetLocalMediaState('cleanup');
	}

	private async restoreLocalMediaState(roomOverride?: Room | null): Promise<void> {
		const room = roomOverride ?? voiceEngineV2AppConnectionHostAdapter.room;
		const participant = room?.localParticipant ?? null;
		if (!participant) return;
		const pendingScreenShareReconnect = this.pendingScreenShareReconnect;
		this.transitionFacadeState({type: 'screenShareReconnect.consume'});
		const shouldRestoreScreenShare =
			(LocalVoiceState.getSelfStream() || pendingScreenShareReconnect !== null) && !participant.isScreenShareEnabled;
		await this.restoreLocalMedia({
			reason: 'voice room connected',
			room,
			restoreCamera: LocalVoiceState.getSelfVideo() && !participant.isCameraEnabled,
			restoreStream: shouldRestoreScreenShare,
			restoreStreamFromSnapshot: pendingScreenShareReconnect
				? () =>
						voiceEngineV2AppScreenShareExecutionAdapter.restoreScreenShareReconnect(room, pendingScreenShareReconnect)
				: null,
			streamSnapshotFallback: false,
			streamPlaySound: true,
			settleStreamStateWhenNotRestored: false,
			toastWhenStreamNotRestored: false,
		});
	}

	private async restorePendingSessionMedia(): Promise<void> {
		const pendingRestore = this.pendingSessionRestore;
		this.transitionFacadeState({type: 'sessionRestore.consume'});
		if (!pendingRestore) {
			return;
		}
		await this.restoreLocalMedia({
			reason: 'startup voice restore',
			room: this.room,
			restoreCamera: pendingRestore.restoreVideo,
			restoreStream: pendingRestore.restoreStream,
			restoreStreamFromSnapshot: null,
			streamSnapshotFallback: false,
			streamPlaySound: true,
			settleStreamStateWhenNotRestored: false,
			toastWhenStreamNotRestored: true,
		});
	}

	private navigateToVoiceChannel(guildId: string | null, channelId: string): void {
		const targetGuildId = guildId ?? ME;
		NavigationCommands.selectChannel(targetGuildId, channelId);
	}

	private clearViewerStreamKeys(): void {
		if (LocalVoiceState.getViewerStreamKeys().length === 0) {
			return;
		}
		replaceWatchedStreamKeys([], {sync: false});
	}
}

const instance = new MediaEngineFacade();

export function useMediaEngineVersion(): number {
	return useStoreVersion(instance);
}

export function getVoiceEngineV2Model(): VoiceEngineV2Model {
	return instance.voiceEngineV2Model;
}

export function getVoiceEngineV2Snapshot(): VoiceEngineV2Snapshot {
	return instance.voiceEngineV2Snapshot;
}

export function useVoiceEngineV2Model(): VoiceEngineV2Model {
	useMediaEngineVersion();
	return instance.voiceEngineV2Model;
}

export function useVoiceEngineV2Snapshot(): VoiceEngineV2Snapshot {
	useMediaEngineVersion();
	return instance.voiceEngineV2Snapshot;
}

(
	window as typeof window & {
		_mediaEngineFacade?: MediaEngineFacade;
	}
)._mediaEngineFacade = instance;

export default instance;
