// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {Endpoints} from '@app/features/app/constants/Endpoints';
import {LimitResolver} from '@app/features/app/utils/LimitResolverAdapter';
import {isLimitToggleEnabled} from '@app/features/app/utils/LimitUtils';
import Channels from '@app/features/channel/state/Channels';
import type {VoiceState} from '@app/features/gateway/types/GatewayVoiceTypes';
import Keybind from '@app/features/input/state/InputKeybind';
import {getVoiceContextEntranceSoundScope} from '@app/features/notification/utils/EntranceSoundScopes';
import {handleMediaPermissionBlocked} from '@app/features/permissions/system/commands/MacPermissionsModalCommands';
import MediaPermission from '@app/features/permissions/system/state/MediaPermission';
import {ensureNativePermission} from '@app/features/permissions/system/utils/NativePermissions';
import {http} from '@app/features/platform/transport/RestTransport';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {isDesktop} from '@app/features/ui/utils/NativeUtils';
import {Store} from '@app/features/voice/engine/Store';
import VoiceDevicePermissionState from '@app/features/voice/engine/VoiceDevicePermissionState';
import type {EffectiveAudioState} from '@app/features/voice/engine/VoiceEffectiveAudioState';
import {getEffectiveAudioState} from '@app/features/voice/engine/VoiceEffectiveAudioState';
import {
	createVoiceMicrophoneFailureLatchSnapshot,
	isVoiceMicrophoneFailureLatchActive,
	selectVoiceLocalAudioEffectiveSelfMute,
	selectVoiceMicrophoneFailureCount,
	transitionVoiceMicrophoneFailureLatchSnapshot,
	type VoiceMicrophoneFailureLatchEvent,
} from '@app/features/voice/engine/VoiceLocalAudioReconcilePolicy';
import {resolveLocalSpeakingOverrideState} from '@app/features/voice/engine/VoiceLocalSpeakingGate';
import {
	getRoomFromMediaEngine,
	getVoiceConnectionContextFromMediaEngine,
	getVoiceEngineV2SnapshotFromMediaEngine,
	setVoiceEngineV2ParticipantAudioLevelSpeaking,
	syncLocalVoiceStateWithServer,
	updateLocalParticipantFromRoom,
} from '@app/features/voice/engine/VoiceMediaEngineBridge';
import {
	createVoiceMediaSnapshot,
	transitionVoiceMediaSnapshot,
	type VoiceMediaEvent,
	type VoiceMediaSnapshot,
} from '@app/features/voice/engine/VoiceMediaStateMachine';
import {
	getLocalSpeakingThresholdRms,
	SPEAKING_LOCAL_RELEASE_MS,
} from '@app/features/voice/engine/VoiceSpeakingThreshold';
import type {VoiceStateSyncPartial} from '@app/features/voice/engine/VoiceStateSyncTypes';
import {
	enforceLocalMediaPublicationCap,
	getLocalMicrophonePublications,
	getPrimaryLocalMicrophonePublication,
} from '@app/features/voice/engine/VoiceTrackPublicationUtils';
import {
	asVoiceTrackSource,
	isScreenShareAudioPublicationLike,
	VoiceTrackSource,
} from '@app/features/voice/engine/VoiceTrackSource';
import {
	assertBoolean,
	assertNonEmptyString,
	assertNullableObjectLike,
	assertObjectLike,
	assertOptionalNonEmptyString,
	isMutedOrDeafened,
	isPermissionDeniedError,
} from '@app/features/voice/engine/v2/VoiceEngineV2AppAdapterAssertions';
import {getCameraVideoPreset} from '@app/features/voice/engine/v2/VoiceEngineV2AppCameraResolutionPresets';
import {
	runCameraTransition,
	type VoiceEngineV2AppCameraTransitionOutcome,
} from '@app/features/voice/engine/v2/VoiceEngineV2AppCameraTransition';
import {
	chooseMicrophoneRefreshStrategy,
	computeSpeakingDetectorRms,
	createInitialMicrophoneEnableState,
	createInitialMicrophoneRefreshState,
	type MicrophoneEnableContext,
	type MicrophoneEnableState,
	type MicrophoneRefreshContext,
	type MicrophoneRefreshState,
	readSpeakingDetectorThresholdRms,
	type SpeakingDetectorGraph,
	type SpeakingDetectorTickOptions,
} from '@app/features/voice/engine/v2/VoiceEngineV2AppMicrophoneTransaction';
import {
	selectVoiceEngineV2AppEffectiveSelfMuteFromAudioControls,
	selectVoiceEngineV2AppMuteReason,
	selectVoiceEngineV2AppParticipant,
	type VoiceEngineV2AppVoiceMuteReason,
} from '@app/features/voice/engine/v2/VoiceEngineV2AppSelectors';
import type {VoiceEngineV2AppSourceLifecycleBridge} from '@app/features/voice/engine/v2/VoiceEngineV2AppSourceLifecycleBridge';
import {ensureNativeMediaPermission} from '@app/features/voice/engine/voice_screen_share_manager/NativePermissionGate';
import CallMediaPrefs from '@app/features/voice/state/CallMediaPrefs';
import EntranceSoundLibrary from '@app/features/voice/state/EntranceSoundLibrary';
import LocalVoiceState from '@app/features/voice/state/LocalVoiceState';
import ParticipantVolume from '@app/features/voice/state/ParticipantVolume';
import VoiceSettings from '@app/features/voice/state/VoiceSettings';
import {buildMicrophonePublishOptions} from '@app/features/voice/utils/AudioPublishOptions';
import {applyBackgroundProcessor} from '@app/features/voice/utils/VideoBackgroundProcessor';
import {
	removeVoiceInputProcessor,
	syncVoiceInputProcessor,
	updateVoiceInputGain,
} from '@app/features/voice/utils/VoiceInputProcessor';
import {
	isVoicePermissionMuteActive,
	isVoiceSpeakPermissionDenied,
} from '@app/features/voice/utils/VoicePermissionUtils';
import {
	applyContentHintToTrack,
	getActiveVoiceProcessingMode,
	resolveVoiceProcessingFromStateForDeviceLabel,
} from '@app/features/voice/utils/VoiceProcessingProfile';
import type {
	VoiceEngineV2AudioControls,
	VoiceEngineV2AudioMode,
	VoiceEngineV2CameraEncodingOptions,
	VoiceEngineV2MicrophoneOptions,
} from '@fluxer/voice_engine_v2';
import type {
	AudioCaptureOptions,
	LocalAudioTrack,
	LocalTrackPublication,
	LocalVideoTrack,
	Room,
	TrackPublishOptions,
} from 'livekit-client';
import {Track} from 'livekit-client';

const logger = new Logger('VoiceEngineV2AppMediaExecutionAdapter');
const LOCAL_SPEAKING_ANALYSER_INTERVAL_MS = 50;
export const REPUBLISH_MICROPHONE_GUARD_MS = 150;
type VoiceMuteReason = VoiceEngineV2AppVoiceMuteReason;

export interface SetCameraEnabledOptions {
	deviceId?: string;
	sendUpdate?: boolean;
}

export interface RefreshMicrophoneOptions {
	forceRepublish?: boolean;
}

function extractUserIdFromVoiceIdentity(identity: string): string | null {
	const match = identity.match(/^user_(\d+)(?:_(.+))?$/);
	return match ? match[1] : null;
}

function isPushToTalkActiveFromAppState(): boolean {
	if (getActiveVoiceProcessingMode(VoiceSettings) === 'studio') return false;
	return Keybind.isPushToTalkEffective();
}

function isPushToMuteActiveFromAppState(): boolean {
	if (getActiveVoiceProcessingMode(VoiceSettings) === 'studio') return false;
	return Keybind.isPushToMuteEffective();
}

function getVoiceEngineV2AudioModeFromAppState(): VoiceEngineV2AudioMode {
	if (isPushToTalkActiveFromAppState()) return 'pushToTalk';
	if (isPushToMuteActiveFromAppState()) return 'pushToMute';
	return 'voiceActivity';
}

let microphoneFailureLatchSnapshot = createVoiceMicrophoneFailureLatchSnapshot();

function applyMicrophoneFailureLatchEvent(event: VoiceMicrophoneFailureLatchEvent): boolean {
	const wasLatched = isVoiceMicrophoneFailureLatchActive(microphoneFailureLatchSnapshot);
	microphoneFailureLatchSnapshot = transitionVoiceMicrophoneFailureLatchSnapshot(microphoneFailureLatchSnapshot, event);
	const isLatched = isVoiceMicrophoneFailureLatchActive(microphoneFailureLatchSnapshot);
	if (wasLatched === isLatched) return false;
	logger.info('Microphone failure self-mute latch changed', {
		event: event.type,
		latched: isLatched,
		failureCount: selectVoiceMicrophoneFailureCount(microphoneFailureLatchSnapshot),
	});
	return true;
}

export function isMicrophoneEnableFailureLatched(): boolean {
	return isVoiceMicrophoneFailureLatchActive(microphoneFailureLatchSnapshot);
}

function getLatchedLocalSelfMute(): boolean {
	return selectVoiceLocalAudioEffectiveSelfMute({
		localSelfMute: LocalVoiceState.getSelfMute(),
		microphoneFailureLatched: isMicrophoneEnableFailureLatched(),
	});
}

function getVoiceEngineV2AudioControlsFromAppState(): VoiceEngineV2AudioControls {
	return {
		mode: getVoiceEngineV2AudioModeFromAppState(),
		locallyMuted: getLatchedLocalSelfMute(),
		preferredLocallyMuted: LocalVoiceState.getSelfMute(),
		locallyDeafened: LocalVoiceState.getSelfDeaf(),
		mutedByPermission: LocalVoiceState.getMutedByPermission(),
		hasUserSetMute: LocalVoiceState.getHasUserSetMute(),
		hasUserSetDeaf: LocalVoiceState.getHasUserSetDeaf(),
		shouldUnmuteOnUndeafen: LocalVoiceState.shouldUnmuteOnUndeafen,
		pushToTalkActive: isPushToTalkActiveFromAppState() && Keybind.pushToTalkHeld,
		pushToMuteActive: isPushToMuteActiveFromAppState() && Keybind.pushToMuteHeld,
		inputVolume: VoiceSettings.inputVolume,
		outputVolume: VoiceSettings.outputVolume,
	};
}

function getEffectiveSelfMuteForVoiceStatePayloadFromV2AudioControls(): boolean {
	return selectVoiceEngineV2AppEffectiveSelfMuteFromAudioControls(getVoiceEngineV2AudioControlsFromAppState());
}

export class VoiceEngineV2AppMediaExecutionAdapter extends Store {
	private speakingAudioContext: AudioContext | null = null;
	private speakingSourceNode: MediaStreamAudioSourceNode | null = null;
	private speakingAnalyserNode: AnalyserNode | null = null;
	private speakingTimerId: number | null = null;
	private speakingSilenceStartedAt: number | null = null;
	private speakingTrackEndedCleanup: (() => void) | null = null;
	private mediaStateSnapshot: VoiceMediaSnapshot = createVoiceMediaSnapshot();
	private microphoneEnablePromise: Promise<void> | null = null;
	private microphoneRefreshQueue: Promise<void> = Promise.resolve();
	private cameraBackgroundRefreshQueue: Promise<void> = Promise.resolve();
	private cameraCaptureRefreshQueue: Promise<void> = Promise.resolve();
	private sourceLifecycleBridge: VoiceEngineV2AppSourceLifecycleBridge | null = null;
	private microphoneLifecycleBinding: {captureId: string; trackId: string; cleanup: () => void} | null = null;
	private cameraLifecycleBinding: {captureId: string; trackId: string; cleanup: () => void} | null = null;

	setSourceLifecycleBridge(bridge: VoiceEngineV2AppSourceLifecycleBridge | null): void {
		this.unbindMicrophoneLifecycle();
		this.unbindCameraLifecycle();
		this.sourceLifecycleBridge = bridge;
	}

	private bindMediaTrackLifecycle(
		captureId: string,
		sourceId: string,
		track: MediaStreamTrack,
	): {captureId: string; trackId: string; cleanup: () => void} | null {
		const bridge = this.sourceLifecycleBridge;
		if (!bridge) return null;
		assertNonEmptyString(captureId, 'bindMediaTrackLifecycle.captureId');
		assertNonEmptyString(sourceId, 'bindMediaTrackLifecycle.sourceId');
		if (!bridge.bind({captureId, sourceId})) return null;
		let cleanedUp = false;
		const onEnded = (): void => {
			if (cleanedUp) return;
			bridge.reportLifecycle({captureId, kind: 'error', message: 'track-ended'});
		};
		track.addEventListener('ended', onEnded);
		const cleanup = (): void => {
			if (cleanedUp) return;
			cleanedUp = true;
			track.removeEventListener('ended', onEnded);
			bridge.unbind(captureId);
		};
		assert.equal(typeof cleanup, 'function', 'cleanup must be a function');
		return {captureId, trackId: track.id, cleanup};
	}

	private bindMicrophoneLifecycle(track: MediaStreamTrack | null | undefined): void {
		this.unbindMicrophoneLifecycle();
		if (!track) return;
		const captureId = `voice-mic:${track.id}`;
		const sourceId = `voice-mic:${track.id}`;
		this.microphoneLifecycleBinding = this.bindMediaTrackLifecycle(captureId, sourceId, track);
	}

	private unbindMicrophoneLifecycle(): void {
		const binding = this.microphoneLifecycleBinding;
		if (!binding) return;
		binding.cleanup();
		this.microphoneLifecycleBinding = null;
	}

	private bindCameraLifecycle(track: MediaStreamTrack | null | undefined): void {
		this.unbindCameraLifecycle();
		if (!track) return;
		const captureId = `voice-cam:${track.id}`;
		const sourceId = `voice-cam:${track.id}`;
		this.cameraLifecycleBinding = this.bindMediaTrackLifecycle(captureId, sourceId, track);
	}

	private unbindCameraLifecycle(): void {
		const binding = this.cameraLifecycleBinding;
		if (!binding) return;
		binding.cleanup();
		this.cameraLifecycleBinding = null;
	}

	isMicrophoneFailureLatched(): boolean {
		return isMicrophoneEnableFailureLatched();
	}

	resetMicrophoneFailureLatch(): void {
		this.transitionMicrophoneFailureLatch({type: 'latch.reset'});
	}

	noteUserMuteIntentChanged(): void {
		this.transitionMicrophoneFailureLatch({type: 'mute.userIntentChanged'});
	}

	private transitionMicrophoneFailureLatch(event: VoiceMicrophoneFailureLatchEvent): void {
		if (!applyMicrophoneFailureLatchEvent(event)) return;
		this.emitChange();
	}

	private observeMicrophoneFailureLatchScope(channelId: string | null): void {
		this.transitionMicrophoneFailureLatch({
			type: 'scope.observed',
			channelId,
			inputDeviceId: VoiceSettings.getInputDeviceId(),
		});
	}

	private isSpeakPermissionDenied(channelId: string | null): boolean {
		const connection = getVoiceConnectionContextFromMediaEngine();
		return isVoiceSpeakPermissionDenied(connection?.guildId ?? null, channelId);
	}

	private transitionMediaState(event: VoiceMediaEvent): void {
		this.update(() => {
			const nextSnapshot = transitionVoiceMediaSnapshot(this.mediaStateSnapshot, event);
			this.mediaStateSnapshot =
				nextSnapshot.context.commands.length > 0
					? transitionVoiceMediaSnapshot(nextSnapshot, {type: 'commands.clear'})
					: nextSnapshot;
		});
	}

	private getActiveRoom(): Room | null {
		return getRoomFromMediaEngine();
	}

	private getActiveChannelId(): string | null {
		return getVoiceConnectionContextFromMediaEngine()?.channelId ?? null;
	}

	private updateMediaAudioControls(): void {
		this.transitionMediaState({
			type: 'audio.controls.update',
			controls: {
				pushToTalkActive: isPushToTalkActiveFromAppState(),
				pushToTalkHeld: Keybind.pushToTalkHeld,
				pushToMuteActive: isPushToMuteActiveFromAppState(),
				pushToMuteHeld: Keybind.pushToMuteHeld,
			},
		});
	}

	private async enforceSpeakPermissionMute(room: Room): Promise<void> {
		await this.disableMicrophone(room);
		this.syncVoiceState({self_mute: true});
		this.syncLocalSpeakingOverride(room);
	}

	async prepareMicrophonePermissionForConnect(): Promise<boolean> {
		assert.ok(
			this.mediaStateSnapshot !== null,
			'prepareMicrophonePermissionForConnect pre-condition: snapshot present',
		);
		assert.equal(
			typeof MediaPermission.isMicrophoneGranted,
			'function',
			'prepareMicrophonePermissionForConnect pre-condition: permission helper present',
		);
		this.transitionMediaState({type: 'permission.warmup.start'});
		const devicePermission = VoiceDevicePermissionState.getState().permissionStatus;
		if (MediaPermission.isMicrophoneGranted() || devicePermission === 'granted') {
			this.transitionMediaState({type: 'permission.warmup.granted'});
			return true;
		}
		if (MediaPermission.isMicrophoneExplicitlyDenied() || devicePermission === 'denied') {
			this.transitionMediaState({type: 'permission.warmup.denied'});
			this.handleMicrophonePermissionDenied();
			return false;
		}
		if (isDesktop()) {
			try {
				const nativeResult = await ensureNativePermission('microphone');
				if (nativeResult === 'denied') {
					logger.warn('Native microphone permission denied before voice connect');
					this.transitionMediaState({type: 'permission.warmup.denied'});
					this.handleMicrophonePermissionDenied();
					return false;
				}
				if (nativeResult === 'granted') {
					MediaPermission.updateMicrophonePermissionGranted();
					this.transitionMediaState({type: 'permission.warmup.granted'});
					return true;
				}
			} catch (error) {
				logger.warn('Native microphone permission check failed before voice connect; continuing', {error});
			}
		}
		if (!navigator.mediaDevices?.getUserMedia) {
			this.transitionMediaState({type: 'permission.warmup.unavailable'});
			return true;
		}
		try {
			const stream = await navigator.mediaDevices.getUserMedia({audio: true});
			stream.getTracks().forEach((track) => track.stop());
			MediaPermission.updateMicrophonePermissionGranted();
			this.transitionMediaState({type: 'permission.warmup.granted'});
			return true;
		} catch (error) {
			if (isPermissionDeniedError(error)) {
				logger.warn('Microphone permission denied before voice connect', {error});
				this.transitionMediaState({type: 'permission.warmup.denied'});
				this.handleMicrophonePermissionDenied();
				return false;
			}
			logger.warn('Microphone permission warm-up failed before voice connect; continuing', {error});
			this.transitionMediaState({type: 'permission.warmup.failedContinuing'});
			return true;
		}
	}

	async ensureMicrophone(room: Room, channelId: string): Promise<void> {
		assertObjectLike<Room>(room, 'ensureMicrophone.room');
		assertNonEmptyString(channelId, 'ensureMicrophone.channelId');
		this.observeMicrophoneFailureLatchScope(channelId);
		if (this.isSpeakPermissionDenied(channelId)) {
			logger.debug('Skipping microphone: speak permission denied');
			await this.enforceSpeakPermissionMute(room);
			return;
		}
		const audioState = getEffectiveAudioState();
		const denied = MediaPermission.isMicrophoneExplicitlyDenied();
		const isPttMutedOnly =
			isPushToTalkActiveFromAppState() &&
			!Keybind.pushToTalkHeld &&
			!audioState.selfMute &&
			!audioState.serverMute &&
			!audioState.effectiveDeaf;
		if (audioState.effectiveMute && !isPttMutedOnly) {
			logger.debug('Skipping: effective audio state is muted or deafened', audioState);
			if (audioState.selfMute) {
				this.syncVoiceState({self_mute: true});
			}
			return;
		}
		if (denied) {
			logger.debug('Microphone explicitly denied');
			if (!LocalVoiceState.getSelfMute()) {
				LocalVoiceState.updateSelfMute(true);
			}
			this.syncVoiceState({self_mute: true});
			return;
		}
		if (!room.localParticipant) {
			logger.warn('No local participant');
			return;
		}
		try {
			await this.enableMicrophone(room, channelId);
			MediaPermission.updateMicrophonePermissionGranted();
			if (isPttMutedOnly) {
				getLocalMicrophonePublications(room.localParticipant).forEach((publication) => {
					publication.mute().catch((error) => logger.error('Failed to mute publication for PTT', {error}));
				});
				this.syncVoiceState({self_mute: true});
			} else {
				this.syncVoiceState({self_mute: audioState.selfMute});
			}
			this.syncLocalSpeakingOverride(room);
		} catch (e: unknown) {
			if (isPermissionDeniedError(e)) {
				this.handleMicrophonePermissionDenied();
				this.syncVoiceState({self_mute: true});
			}
		}
	}

	async reconcileEffectiveAudioState(
		room: Room | null,
		params: {
			channelId: string;
			serverMute: boolean;
			serverDeaf: boolean;
		},
	): Promise<void> {
		assertNullableObjectLike<Room>(room, 'reconcileEffectiveAudioState.room');
		assertObjectLike<typeof params>(params, 'reconcileEffectiveAudioState.params');
		assertNonEmptyString(params.channelId, 'reconcileEffectiveAudioState.params.channelId');
		assertBoolean(params.serverMute, 'reconcileEffectiveAudioState.params.serverMute');
		assertBoolean(params.serverDeaf, 'reconcileEffectiveAudioState.params.serverDeaf');
		if (!room?.localParticipant) {
			logger.debug('Skipping audio-state reconciliation: no local participant');
			return;
		}
		this.observeMicrophoneFailureLatchScope(params.channelId);
		const permissionMuted = this.isSpeakPermissionDenied(params.channelId);
		const audioState = this.getEffectiveAudioState({
			serverMute: params.serverMute || permissionMuted,
			serverDeaf: params.serverDeaf,
			selfMute: getLatchedLocalSelfMute(),
		});
		logger.info('Reconciling local media after voice state update', {
			channelId: params.channelId,
			...audioState,
		});
		this.transitionMediaState({
			type: 'audio.reconcile',
			audioState,
			controls: {
				pushToTalkActive: isPushToTalkActiveFromAppState(),
				pushToTalkHeld: Keybind.pushToTalkHeld,
				pushToMuteActive: isPushToMuteActiveFromAppState(),
				pushToMuteHeld: Keybind.pushToMuteHeld,
			},
			permissionMuted,
			hasLiveMicrophonePublication: this.hasLiveMicrophonePublication(room),
		});
		this.applyDeafen(room, audioState.effectiveDeaf);
		this.applyAllLocalAudioPreferences(room);
		if (permissionMuted) {
			await this.enforceSpeakPermissionMute(room);
			return;
		}
		const pttActive = isPushToTalkActiveFromAppState();
		const ptmActive = isPushToMuteActiveFromAppState();
		const effectiveMute =
			audioState.effectiveDeaf ||
			audioState.serverMute ||
			audioState.selfMute ||
			(pttActive && !Keybind.pushToTalkHeld) ||
			(ptmActive && Keybind.pushToMuteHeld);
		if (effectiveMute) {
			this.setAudioPublicationsMuted(room, true, 'voice state update');
			this.syncLocalSpeakingOverride(room);
			return;
		}
		if (!this.hasLiveMicrophonePublication(room)) {
			try {
				await this.enableMicrophone(room, params.channelId);
			} catch (error) {
				logger.warn('Failed to restore microphone while reconciling audio state', {
					error,
					channelId: params.channelId,
					audioState,
				});
				if (!LocalVoiceState.getSelfMute()) {
					LocalVoiceState.updateSelfMute(true);
				}
				this.syncVoiceState({self_mute: true});
				return;
			}
		}
		this.setAudioPublicationsMuted(room, false, 'voice state update');
		this.syncLocalSpeakingOverride(room);
	}

	private hasMicrophonePublication(room: Room): boolean {
		return this.getMicrophonePublication(room) !== null;
	}

	private hasLiveMicrophonePublication(room: Room): boolean {
		const publication = this.getMicrophonePublication(room);
		if (!publication) return false;
		const track = publication.track;
		if (!track) return false;
		const mediaStreamTrack = track.mediaStreamTrack;
		if (!mediaStreamTrack) return false;
		return mediaStreamTrack.readyState !== 'ended';
	}

	private getMicrophonePublication(room: Room): LocalTrackPublication | null {
		if (!room.localParticipant) return null;
		return getPrimaryLocalMicrophonePublication(room.localParticipant);
	}

	private resolveInputDeviceId(): string {
		const inputDeviceId = VoiceSettings.getInputDeviceId();
		const {inputDevices} = VoiceDevicePermissionState.getState();
		if (inputDevices.length === 0) return inputDeviceId;
		if (inputDevices.some((device) => device.deviceId === inputDeviceId)) {
			return inputDeviceId;
		}
		return inputDevices[0].deviceId;
	}

	private resolveActiveInputDeviceLabel(): string | null {
		const deviceId = this.resolveInputDeviceId();
		const {inputDevices} = VoiceDevicePermissionState.getState();
		const device = inputDevices.find((d) => d.deviceId === deviceId);
		return device?.label || null;
	}

	private getMicrophoneCaptureOptions(options: VoiceEngineV2MicrophoneOptions = {}): AudioCaptureOptions {
		const profile = resolveVoiceProcessingFromStateForDeviceLabel(VoiceSettings, this.resolveActiveInputDeviceLabel());
		return {
			deviceId: options.deviceId ?? this.resolveInputDeviceId(),
			echoCancellation: options.echoCancellation ?? profile.echoCancellation,
			noiseSuppression: options.noiseSuppression ?? profile.browserNoiseSuppression,
			autoGainControl: options.autoGainControl ?? profile.autoGainControl,
			voiceIsolation: false,
		};
	}

	private getMicrophonePublishOptions(channelId: string | null): TrackPublishOptions | undefined {
		const channelBitrate = channelId ? Channels.getChannel(channelId)?.bitrate : null;
		const profile = resolveVoiceProcessingFromStateForDeviceLabel(VoiceSettings, this.resolveActiveInputDeviceLabel());
		return buildMicrophonePublishOptions(channelBitrate, profile.mode);
	}

	async refreshMicrophonePublishSettings(room: Room | null, channelId: string | null): Promise<void> {
		assertNullableObjectLike<Room>(room, 'refreshMicrophonePublishSettings.room');
		assertOptionalNonEmptyString(channelId, 'refreshMicrophonePublishSettings.channelId');
		if (!room?.localParticipant || !this.hasMicrophonePublication(room)) {
			return;
		}
		const audioTrack = this.getLocalAudioTrack(room);
		const sender = audioTrack?.sender;
		if (!sender) {
			logger.warn('No sender found for active microphone track');
			return;
		}
		const maxBitrate = this.getMicrophonePublishOptions(channelId)?.audioPreset?.maxBitrate;
		const senderParameters = sender.getParameters();
		const encodings = senderParameters.encodings?.length ? senderParameters.encodings : [{}];
		const isAlreadyApplied = encodings.every((encoding) => encoding.maxBitrate === maxBitrate);
		if (isAlreadyApplied) {
			return;
		}
		senderParameters.encodings = encodings.map((encoding) => {
			const nextEncoding = {...encoding};
			if (maxBitrate !== undefined) {
				nextEncoding.maxBitrate = maxBitrate;
			} else {
				delete nextEncoding.maxBitrate;
			}
			return nextEncoding;
		});
		try {
			await sender.setParameters(senderParameters);
			logger.debug('Updated microphone publish settings', {channelId, maxBitrate});
		} catch (error) {
			logger.warn('Failed to update microphone publish settings', {error, channelId, maxBitrate});
		}
	}

	async refreshMicrophone(room: Room | null, options: RefreshMicrophoneOptions = {}): Promise<void> {
		assertNullableObjectLike<Room>(room, 'refreshMicrophone.room');
		assertObjectLike<RefreshMicrophoneOptions>(options, 'refreshMicrophone.options');
		this.observeMicrophoneFailureLatchScope(this.getActiveChannelId());
		const refresh = async () => this.refreshMicrophoneNow(room, options);
		const pendingRefresh = this.microphoneRefreshQueue.then(refresh, refresh);
		this.microphoneRefreshQueue = pendingRefresh.catch(() => {});
		return pendingRefresh;
	}

	private async refreshMicrophoneNow(room: Room | null, options: RefreshMicrophoneOptions = {}): Promise<void> {
		if (!room?.localParticipant || !this.hasMicrophonePublication(room)) {
			return;
		}
		this.transitionMediaState({
			type: 'refresh.request',
			hasPublication: true,
			forceRepublish: options.forceRepublish,
		});
		const audioTrack = this.getLocalAudioTrack(room);
		if (!audioTrack) {
			return;
		}
		const captureOptions = this.getMicrophoneCaptureOptions();
		const strategy = chooseMicrophoneRefreshStrategy(options);
		if (strategy === 'force-republish') {
			await this.attemptMicrophoneForceRepublish(room, captureOptions);
			return;
		}
		const ctx: MicrophoneRefreshContext = {room, options, audioTrack, captureOptions};
		const state: MicrophoneRefreshState = createInitialMicrophoneRefreshState();
		await this.prepareMicrophoneRefreshHold(ctx, state);
		try {
			await this.restartActiveMicrophoneTrack(ctx, state);
		} catch (error) {
			this.transitionMediaState({type: 'refresh.restart.failure'});
			logger.warn('Failed to restart microphone capture; re-publishing mic from scratch', {
				error,
				captureOptions,
			});
		}
		if (state.restartSucceeded) {
			await this.releaseMicrophoneRefreshHold(state);
			return;
		}
		await this.republishMicrophoneAfterRestartFailure(room);
	}

	private async attemptMicrophoneForceRepublish(room: Room, captureOptions: AudioCaptureOptions): Promise<void> {
		assertObjectLike<Room>(room, 'attemptMicrophoneForceRepublish.room');
		assertObjectLike<AudioCaptureOptions>(captureOptions, 'attemptMicrophoneForceRepublish.captureOptions');
		try {
			await this.republishMicrophone(room);
			this.transitionMediaState({type: 'refresh.republish.success'});
			logger.debug('Re-published microphone capture settings', {captureOptions});
		} catch (recoveryError) {
			this.transitionMediaState({type: 'refresh.republish.failure'});
			logger.error('Failed to re-publish microphone after capture settings change', recoveryError);
		}
	}

	private async prepareMicrophoneRefreshHold(
		ctx: MicrophoneRefreshContext,
		state: MicrophoneRefreshState,
	): Promise<void> {
		assertObjectLike<MicrophoneRefreshContext>(ctx, 'prepareMicrophoneRefreshHold.ctx');
		assertObjectLike<MicrophoneRefreshState>(state, 'prepareMicrophoneRefreshHold.state');
		const primaryMicPublication =
			Array.from(ctx.room.localParticipant.audioTrackPublications.values()).find(
				(pub) => pub.track === ctx.audioTrack,
			) ?? null;
		state.primaryMicPublication = primaryMicPublication;
		state.shouldUnmuteAfter = primaryMicPublication ? !primaryMicPublication.isMuted : false;
		if (state.shouldUnmuteAfter) {
			await primaryMicPublication?.mute().catch((muteError) => {
				logger.warn('Failed to hold mic muted while refreshing voice input processor', {muteError});
			});
		}
	}

	private async restartActiveMicrophoneTrack(
		ctx: MicrophoneRefreshContext,
		state: MicrophoneRefreshState,
	): Promise<void> {
		assertObjectLike<MicrophoneRefreshContext>(ctx, 'restartActiveMicrophoneTrack.ctx');
		assertObjectLike<MicrophoneRefreshState>(state, 'restartActiveMicrophoneTrack.state');
		await ctx.audioTrack.restartTrack(ctx.captureOptions);
		await this.refreshMicrophonePublishSettings(ctx.room, this.getActiveChannelId());
		await syncVoiceInputProcessor(ctx.audioTrack);
		this.applyMicrophoneRefreshSnapshot(ctx);
		state.restartSucceeded = true;
		this.transitionMediaState({type: 'refresh.restart.success'});
		logger.debug('Refreshed microphone capture settings');
	}

	private applyMicrophoneRefreshSnapshot(ctx: MicrophoneRefreshContext): void {
		assertObjectLike<MicrophoneRefreshContext>(ctx, 'applyMicrophoneRefreshSnapshot.ctx');
		assertObjectLike<LocalAudioTrack>(ctx.audioTrack, 'applyMicrophoneRefreshSnapshot.audioTrack');
		const profile = resolveVoiceProcessingFromStateForDeviceLabel(VoiceSettings, this.resolveActiveInputDeviceLabel());
		if (ctx.audioTrack.mediaStreamTrack) {
			applyContentHintToTrack(ctx.audioTrack.mediaStreamTrack, profile.contentHint);
		}
		this.startLocalSpeakingDetector(ctx.room, ctx.audioTrack.mediaStreamTrack ?? null);
	}

	private async releaseMicrophoneRefreshHold(state: MicrophoneRefreshState): Promise<void> {
		assertObjectLike<MicrophoneRefreshState>(state, 'releaseMicrophoneRefreshHold.state');
		if (!state.shouldUnmuteAfter) return;
		const selfMute = getEffectiveSelfMuteForVoiceStatePayloadFromV2AudioControls();
		const selfDeaf = LocalVoiceState.getSelfDeaf();
		if (selfMute || selfDeaf) return;
		await state.primaryMicPublication?.unmute().catch((unmuteError) => {
			logger.warn('Failed to resume mic after refreshing voice input processor', {unmuteError});
		});
	}

	private async republishMicrophoneAfterRestartFailure(room: Room): Promise<void> {
		assertObjectLike<Room>(room, 'republishMicrophoneAfterRestartFailure.room');
		try {
			await this.republishMicrophone(room);
			this.transitionMediaState({type: 'refresh.republish.success'});
		} catch (recoveryError) {
			this.transitionMediaState({type: 'refresh.republish.failure'});
			logger.error('Failed to recover microphone after restart failure', recoveryError);
		}
	}

	private async republishMicrophone(room: Room): Promise<void> {
		assertObjectLike<Room>(room, 'republishMicrophone.room');
		await this.disableMicrophone(room);
		await this.waitForRepublishGuardDeadline();
		await this.enableMicrophone(room, this.getActiveChannelId());
	}

	private waitForRepublishGuardDeadline(): Promise<void> {
		const deadlineMs = REPUBLISH_MICROPHONE_GUARD_MS;
		return new Promise<void>((resolve) => {
			let resolved = false;
			const handle = setTimeout(() => {
				if (resolved) return;
				resolved = true;
				resolve();
			}, deadlineMs);
			if (handle && typeof (handle as {unref?: () => void}).unref === 'function') {
				(handle as {unref?: () => void}).unref!();
			}
		});
	}

	async enableMicrophone(
		room: Room,
		channelId: string | null,
		options: VoiceEngineV2MicrophoneOptions = {},
	): Promise<void> {
		assertObjectLike<Room>(room, 'enableMicrophone.room');
		assertOptionalNonEmptyString(channelId, 'enableMicrophone.channelId');
		assertObjectLike<VoiceEngineV2MicrophoneOptions>(options, 'enableMicrophone.options');
		if (this.microphoneEnablePromise) {
			await this.microphoneEnablePromise;
			return;
		}
		this.microphoneEnablePromise = this.enableMicrophoneNow(room, channelId, options);
		try {
			await this.microphoneEnablePromise;
		} finally {
			this.microphoneEnablePromise = null;
		}
	}

	private async enableMicrophoneNow(
		room: Room,
		channelId: string | null,
		options: VoiceEngineV2MicrophoneOptions,
	): Promise<void> {
		this.transitionMediaState({
			type: 'microphone.enable.request',
			hasPublication: this.hasMicrophonePublication(room),
			hasLivePublication: this.hasLiveMicrophonePublication(room),
			speakPermissionDenied: this.isSpeakPermissionDenied(channelId),
			permissionDenied: MediaPermission.isMicrophoneExplicitlyDenied(),
		});
		if (this.isSpeakPermissionDenied(channelId)) {
			logger.debug('Skipping microphone enable: speak permission denied');
			await this.enforceSpeakPermissionMute(room);
			return;
		}
		if (this.hasMicrophonePublication(room)) {
			if (this.hasLiveMicrophonePublication(room)) {
				logger.debug('Microphone track already published, skipping duplicate publish');
				this.transitionMicrophoneFailureLatch({type: 'microphone.enableSucceeded'});
				return;
			}
			logger.warn('Existing microphone publication has an ended track; unpublishing before reacquire');
			await this.disableMicrophone(room);
		}
		const ctx: MicrophoneEnableContext = {room, channelId, options};
		const state: MicrophoneEnableState = createInitialMicrophoneEnableState();
		try {
			await this.acquireMicrophoneDevice(ctx, state);
			if (!ctx.room.localParticipant) {
				logger.warn('No local participant');
				this.transitionMediaState({type: 'microphone.enable.failure'});
				return;
			}
			await this.publishMicrophoneAudioTrack(ctx, state);
			await this.installVoiceInputProcessor(ctx, state);
			this.attachLocalSpeakingDetectorForPublish(ctx, state);
			MediaPermission.updateMicrophonePermissionGranted();
			this.transitionMediaState({type: 'microphone.enable.success'});
			this.transitionMicrophoneFailureLatch({type: 'microphone.enableSucceeded'});
			logger.info('Successfully enabled microphone');
		} catch (e: unknown) {
			this.transitionMicrophoneFailureLatch({
				type: 'microphone.enableFailed',
				channelId: ctx.channelId ?? this.getActiveChannelId(),
				inputDeviceId: VoiceSettings.getInputDeviceId(),
			});
			await this.rollbackMicrophoneEnable(ctx, state, e);
			throw e;
		}
	}

	private async acquireMicrophoneDevice(ctx: MicrophoneEnableContext, state: MicrophoneEnableState): Promise<void> {
		assertObjectLike<MicrophoneEnableContext>(ctx, 'acquireMicrophoneDevice.ctx');
		assertObjectLike<MicrophoneEnableState>(state, 'acquireMicrophoneDevice.state');
		if (isDesktop()) {
			await ensureNativeMediaPermission({kind: 'microphone', onDenied: 'throw'});
		}
		await VoiceDevicePermissionState.ensureDevices({requestPermissions: false});
	}

	private async publishMicrophoneAudioTrack(ctx: MicrophoneEnableContext, state: MicrophoneEnableState): Promise<void> {
		assertObjectLike<MicrophoneEnableContext>(ctx, 'publishMicrophoneAudioTrack.ctx');
		assertObjectLike<MicrophoneEnableState>(state, 'publishMicrophoneAudioTrack.state');
		await ctx.room.localParticipant.setMicrophoneEnabled(
			true,
			this.getMicrophoneCaptureOptions(ctx.options),
			this.getMicrophonePublishOptions(ctx.channelId),
		);
		state.microphoneWasPublished = true;
		await this.refreshMicrophonePublishSettings(ctx.room, ctx.channelId);
		const audioTrack = this.getLocalAudioTrack(ctx.room);
		state.audioTrack = audioTrack;
		state.primaryMicPublication = audioTrack
			? (Array.from(ctx.room.localParticipant.audioTrackPublications.values()).find(
					(pub) => pub.track === audioTrack,
				) ?? null)
			: null;
		state.shouldUnmuteAfter = state.primaryMicPublication ? !state.primaryMicPublication.isMuted : false;
		this.bindMicrophoneLifecycle(state.audioTrack?.mediaStreamTrack);
	}

	private async installVoiceInputProcessor(ctx: MicrophoneEnableContext, state: MicrophoneEnableState): Promise<void> {
		assertObjectLike<MicrophoneEnableContext>(ctx, 'installVoiceInputProcessor.ctx');
		assertObjectLike<MicrophoneEnableState>(state, 'installVoiceInputProcessor.state');
		if (state.primaryMicPublication && state.shouldUnmuteAfter) {
			await state.primaryMicPublication.mute().catch((error) => {
				logger.warn('Failed to hold mic muted while installing voice input processor', {error});
			});
		}
		await syncVoiceInputProcessor(state.audioTrack);
		if (state.primaryMicPublication && state.shouldUnmuteAfter) {
			const selfMute = getEffectiveSelfMuteForVoiceStatePayloadFromV2AudioControls();
			const selfDeaf = LocalVoiceState.getSelfDeaf();
			if (!selfMute && !selfDeaf) {
				await state.primaryMicPublication.unmute().catch((error) => {
					logger.warn('Failed to resume mic after installing voice input processor', {error});
				});
			}
		}
	}

	private attachLocalSpeakingDetectorForPublish(ctx: MicrophoneEnableContext, state: MicrophoneEnableState): void {
		assertObjectLike<MicrophoneEnableContext>(ctx, 'attachLocalSpeakingDetectorForPublish.ctx');
		assertObjectLike<MicrophoneEnableState>(state, 'attachLocalSpeakingDetectorForPublish.state');
		const profile = resolveVoiceProcessingFromStateForDeviceLabel(VoiceSettings, this.resolveActiveInputDeviceLabel());
		if (state.audioTrack?.mediaStreamTrack) {
			applyContentHintToTrack(state.audioTrack.mediaStreamTrack, profile.contentHint);
		}
		this.startLocalSpeakingDetector(ctx.room, state.audioTrack?.mediaStreamTrack ?? null);
	}

	private async rollbackMicrophoneEnable(
		ctx: MicrophoneEnableContext,
		state: MicrophoneEnableState,
		error: unknown,
	): Promise<void> {
		assertObjectLike<MicrophoneEnableContext>(ctx, 'rollbackMicrophoneEnable.ctx');
		assertObjectLike<MicrophoneEnableState>(state, 'rollbackMicrophoneEnable.state');
		if (state.microphoneWasPublished) {
			await this.disableMicrophone(ctx.room);
		}
		if (isPermissionDeniedError(error)) {
			logger.error('Permission denied');
			this.transitionMediaState({
				type: 'microphone.enable.failure',
				permissionDenied: true,
				publicationCreated: state.microphoneWasPublished,
			});
			this.handleMicrophonePermissionDenied();
			return;
		}
		logger.error('Failed', error);
		this.transitionMediaState({
			type: 'microphone.enable.failure',
			publicationCreated: state.microphoneWasPublished,
		});
	}

	async disableMicrophone(room: Room): Promise<void> {
		assertObjectLike<Room>(room, 'disableMicrophone.room');
		if (!room.localParticipant) {
			logger.warn('No local participant');
			return;
		}
		this.transitionMediaState({
			type: 'microphone.disable.request',
			hasPublication: this.hasMicrophonePublication(room),
		});
		try {
			const participant = room.localParticipant;
			const microphonePublications = getLocalMicrophonePublications(participant);
			if (microphonePublications.length > 0) {
				const tracks = microphonePublications
					.map((pub) => pub.track)
					.filter((track): track is LocalAudioTrack => Boolean(track));
				await removeVoiceInputProcessor();
				this.stopLocalSpeakingDetector(room);
				await Promise.allSettled(tracks.map((track) => participant.unpublishTrack(track)));
				logger.info('Successfully disabled microphone', {tracksUnpublished: tracks.length});
			}
			this.unbindMicrophoneLifecycle();
			this.transitionMediaState({type: 'microphone.disable.success'});
		} catch (e) {
			this.transitionMediaState({type: 'microphone.disable.failure'});
			logger.error('Failed', e);
		}
	}

	async setMicrophoneEnabled(enabled: boolean, room: Room, channelId: string | null): Promise<void> {
		assertBoolean(enabled, 'setMicrophoneEnabled.enabled');
		assertObjectLike<Room>(room, 'setMicrophoneEnabled.room');
		assertOptionalNonEmptyString(channelId, 'setMicrophoneEnabled.channelId');
		if (enabled) {
			await this.enableMicrophone(room, channelId);
		} else {
			await this.disableMicrophone(room);
		}
	}

	private handleMicrophonePermissionDenied(): void {
		MediaPermission.markMicrophoneExplicitlyDenied();
		handleMediaPermissionBlocked('microphone');
		if (!LocalVoiceState.getSelfMute()) {
			LocalVoiceState.updateSelfMute(true);
		}
	}

	async updateCameraEncoding(options: VoiceEngineV2CameraEncodingOptions): Promise<void> {
		assertObjectLike<VoiceEngineV2CameraEncodingOptions>(options, 'updateCameraEncoding.options');
		const room = this.getActiveRoom();
		const participant = room?.localParticipant;
		if (!participant) {
			logger.warn('updateCameraEncoding skipped: no local participant');
			return;
		}
		const publication = Array.from(participant.videoTrackPublications.values()).find(
			(pub) => pub.source === Track.Source.Camera,
		);
		const track = publication?.track as LocalVideoTrack | undefined;
		if (!track) {
			logger.warn('updateCameraEncoding skipped: no active camera track');
			return;
		}
		await this.applyCameraEncodingConstraints(track, options);
		await this.applyCameraEncodingBitrate(track, options);
		await this.applyCameraEncodingEffects(track, options);
		updateLocalParticipantFromRoom(room);
		logger.info('Applied in-place camera encoding update', {
			width: options.width,
			height: options.height,
			frameRate: options.frameRate,
			mirror: options.mirror,
			backgroundMode: options.backgroundMode,
		});
	}

	private async applyCameraEncodingConstraints(
		track: LocalVideoTrack,
		options: VoiceEngineV2CameraEncodingOptions,
	): Promise<void> {
		const constraints: MediaTrackConstraints = {};
		if (typeof options.width === 'number') constraints.width = {ideal: options.width};
		if (typeof options.height === 'number') constraints.height = {ideal: options.height};
		if (typeof options.frameRate === 'number') constraints.frameRate = {ideal: options.frameRate};
		if (Object.keys(constraints).length === 0) return;
		try {
			await track.mediaStreamTrack.applyConstraints(constraints);
		} catch (error) {
			logger.warn('Failed to apply in-place camera constraints', {error, constraints});
		}
	}

	private async applyCameraEncodingBitrate(
		track: LocalVideoTrack,
		options: VoiceEngineV2CameraEncodingOptions,
	): Promise<void> {
		if (typeof options.maxBitrateBps !== 'number') return;
		const sender = track.sender;
		if (!sender) {
			logger.warn('Failed to apply in-place camera bitrate: no sender');
			return;
		}
		const senderParameters = sender.getParameters();
		const encodings = senderParameters.encodings?.length ? senderParameters.encodings : [{}];
		const alreadyApplied = encodings.every((encoding) => encoding.maxBitrate === options.maxBitrateBps);
		if (alreadyApplied) return;
		senderParameters.encodings = encodings.map((encoding) => ({...encoding, maxBitrate: options.maxBitrateBps}));
		try {
			await sender.setParameters(senderParameters);
		} catch (error) {
			logger.warn('Failed to apply in-place camera bitrate', {error, maxBitrateBps: options.maxBitrateBps});
		}
	}

	private async applyCameraEncodingEffects(
		track: LocalVideoTrack,
		options: VoiceEngineV2CameraEncodingOptions,
	): Promise<void> {
		const touchesEffects =
			options.mirror !== undefined ||
			options.backgroundMode !== undefined ||
			options.backgroundBlurStrength !== undefined ||
			options.backgroundCustomMediaPath !== undefined;
		if (!touchesEffects) return;
		try {
			await applyBackgroundProcessor(track, options.mirror === undefined ? undefined : {mirrorCamera: options.mirror});
		} catch (error) {
			logger.warn('Failed to apply in-place camera effects', {error});
		}
	}

	async setCameraEnabled(
		enabled: boolean,
		options?: SetCameraEnabledOptions,
	): Promise<VoiceEngineV2AppCameraTransitionOutcome> {
		assertBoolean(enabled, 'setCameraEnabled.enabled');
		if (options !== undefined) {
			assertObjectLike<SetCameraEnabledOptions>(options, 'setCameraEnabled.options');
		}
		const room = this.getActiveRoom();
		const {sendUpdate = true, ...restOptions} = options || {};
		if (!room?.localParticipant) {
			logger.warn('No room or local participant');
			return 'failed';
		}
		this.transitionMediaState({
			type: 'camera.setEnabled.request',
			enabled,
			currentlyEnabled: room.localParticipant.isCameraEnabled ?? false,
		});
		const outcome = await runCameraTransition({
			enabled,
			sendUpdate,
			publish: () => this.publishCameraTransition(room, enabled, restOptions),
			readActualEnabled: () => room.localParticipant?.isCameraEnabled ?? false,
			onPermissionDenied: () => {
				this.transitionMediaState({type: 'camera.failure', actualEnabled: false, permissionDenied: true});
			},
			onSuccessSettled: () => updateLocalParticipantFromRoom(room),
			onFailure: (actual, error) => {
				logger.error('Failed', {enabled, error});
				this.transitionMediaState({type: 'camera.failure', actualEnabled: actual});
				updateLocalParticipantFromRoom(room);
			},
			rethrowOnFailure: false,
		});
		if (outcome === 'applied') {
			this.transitionMediaState({type: 'camera.success', enabled});
			logger.info('Success', {enabled});
		}
		return outcome;
	}

	private async publishCameraTransition(
		room: Room,
		enabled: boolean,
		restOptions: Omit<SetCameraEnabledOptions, 'sendUpdate'>,
	): Promise<void> {
		const participant = room.localParticipant;
		assert.ok(participant, 'camera transition requires a local participant');
		assertBoolean(enabled, 'publishCameraTransition.enabled');
		await this.enforceCameraPublicationCap(participant, enabled ? 'before camera enable' : 'before camera disable');
		const videoResolution = getCameraVideoPreset(VoiceSettings.getCameraResolution());
		await participant.setCameraEnabled(enabled, {resolution: videoResolution, ...restOptions});
		await this.enforceCameraPublicationCap(participant, enabled ? 'after camera enable' : 'after camera disable');
		if (enabled) {
			await this.applyBackgroundToCamera(participant);
			const cameraPublication = Array.from(participant.videoTrackPublications.values()).find(
				(pub) => pub.source === Track.Source.Camera,
			);
			const cameraTrack = cameraPublication?.track?.mediaStreamTrack;
			this.bindCameraLifecycle(cameraTrack);
		} else {
			this.unbindCameraLifecycle();
		}
	}

	private async enforceCameraPublicationCap(participant: Room['localParticipant'], reason: string): Promise<void> {
		const result = await enforceLocalMediaPublicationCap(participant, 'camera');
		for (const failure of result.failedPublications) {
			logger.warn('Failed to enforce local camera publication cap', {
				error: failure.error,
				reason,
				trackSid: failure.publication.trackSid,
			});
		}
	}

	private async applyBackgroundToCamera(
		participant: Room['localParticipant'],
		options: {warnIfMissing?: boolean} = {},
	): Promise<boolean> {
		const videoPublication = Array.from(participant.videoTrackPublications.values()).find(
			(pub) => pub.source === Track.Source.Camera,
		);
		const track = videoPublication?.track as LocalVideoTrack | undefined;
		if (!track) {
			if (options.warnIfMissing === false) {
				logger.debug('Skipping background refresh: no camera track');
			} else {
				logger.warn('No camera track found to apply background');
			}
			return false;
		}
		await applyBackgroundProcessor(track);
		return true;
	}

	async refreshCameraBackground(room?: Room | null): Promise<void> {
		if (room !== undefined) {
			assertNullableObjectLike<Room>(room, 'refreshCameraBackground.room');
		}
		assert.ok(this.mediaStateSnapshot !== null, 'refreshCameraBackground pre-condition: snapshot present');
		const refresh = async () => {
			const activeRoom = room === undefined ? this.getActiveRoom() : room;
			const participant = activeRoom?.localParticipant;
			if (!participant) {
				logger.debug('Skipping background refresh: no local participant');
				return;
			}
			const didApply = await this.applyBackgroundToCamera(participant, {warnIfMissing: false});
			if (didApply) {
				updateLocalParticipantFromRoom(activeRoom);
			}
		};
		const pendingRefresh = this.cameraBackgroundRefreshQueue.then(refresh, refresh);
		this.cameraBackgroundRefreshQueue = pendingRefresh.catch(() => {});
		return pendingRefresh;
	}

	async refreshCameraCapture(room?: Room | null): Promise<void> {
		if (room !== undefined) {
			assertNullableObjectLike<Room>(room, 'refreshCameraCapture.room');
		}
		assert.ok(this.mediaStateSnapshot !== null, 'refreshCameraCapture pre-condition: snapshot present');
		const refresh = async () => this.refreshCameraCaptureNow(room);
		const pendingRefresh = this.cameraCaptureRefreshQueue.then(refresh, refresh);
		this.cameraCaptureRefreshQueue = pendingRefresh.catch(() => {});
		return pendingRefresh;
	}

	private async refreshCameraCaptureNow(room?: Room | null): Promise<void> {
		const activeRoom = room === undefined ? this.getActiveRoom() : room;
		if (!activeRoom?.localParticipant) {
			return;
		}
		const participant = activeRoom.localParticipant;
		if (!participant.isCameraEnabled) {
			return;
		}
		const cameraPublication = Array.from(participant.videoTrackPublications.values()).find(
			(pub) => pub.source === Track.Source.Camera,
		);
		const cameraTrack = cameraPublication?.track;
		if (!cameraTrack) {
			return;
		}
		await participant.unpublishTrack(cameraTrack);
		await this.publishCameraTransition(activeRoom, true, {deviceId: VoiceSettings.getVideoDeviceId()});
		updateLocalParticipantFromRoom(activeRoom);
	}

	async toggleCameraFromKeybind(): Promise<void> {
		assert.equal(
			typeof LocalVoiceState.getSelfVideo,
			'function',
			'toggleCameraFromKeybind pre-condition: LocalVoiceState present',
		);
		assert.equal(
			typeof VoiceSettings.getVideoDeviceId,
			'function',
			'toggleCameraFromKeybind pre-condition: VoiceSettings present',
		);
		const current = LocalVoiceState.getSelfVideo();
		await this.setCameraEnabled(!current, {deviceId: VoiceSettings.getVideoDeviceId()});
	}

	async playEntranceSound(): Promise<void> {
		assert.ok(this.mediaStateSnapshot !== null, 'playEntranceSound pre-condition: snapshot present');
		const connection = getVoiceConnectionContextFromMediaEngine();
		const channelId = connection?.channelId ?? null;
		if (!channelId) return;
		const hasEntranceSounds = isLimitToggleEnabled(
			{feature_voice_entrance_sounds: LimitResolver.resolve({key: 'feature_voice_entrance_sounds', fallback: 0})},
			'feature_voice_entrance_sounds',
		);
		if (!hasEntranceSounds) return;
		if (!EntranceSoundLibrary.loaded) {
			await EntranceSoundLibrary.load().catch((error) => {
				logger.debug('Entrance sound library load rejected, continuing without sound', {error});
			});
		}
		const scope = getVoiceContextEntranceSoundScope(connection?.guildId ?? null);
		const resolved = EntranceSoundLibrary.resolveForScope(scope);
		if (!resolved) return;
		try {
			await http.post(Endpoints.VOICE_CHANNEL_ENTRANCE_SOUND(channelId), {body: {sound_id: resolved.sound.id}});
			logger.debug('Entrance sound play requested', {soundId: resolved.sound.id, scopeKind: resolved.scope.kind});
		} catch (error) {
			logger.warn('Failed to request entrance sound play', {error});
		}
	}

	resetStreamTracking(): void {
		assert.ok(this.mediaStateSnapshot !== null, 'resetStreamTracking pre-condition: snapshot present');
		void removeVoiceInputProcessor();
		this.stopLocalSpeakingDetector();
		this.transitionMediaState({type: 'media.reset'});
	}

	syncVoiceState(partial: VoiceStateSyncPartial): void {
		assertObjectLike<VoiceStateSyncPartial>(partial, 'syncVoiceState.partial');
		syncLocalVoiceStateWithServer(partial);
	}

	applyLocalAudioPreferencesForUser(userId: string, room: Room | null): void {
		assertNonEmptyString(userId, 'applyLocalAudioPreferencesForUser.userId');
		assertNullableObjectLike<Room>(room, 'applyLocalAudioPreferencesForUser.room');
		if (!room) {
			const connection = getVoiceConnectionContextFromMediaEngine();
			logger.warn(`Skipped audio preferences for user ${userId} because no room was attached`, {
				userId,
				guildId: connection?.guildId ?? null,
				channelId: connection?.channelId ?? null,
				connectionId: connection?.connectionId ?? null,
				connecting: connection?.connecting ?? false,
				reconnecting: connection?.reconnecting ?? false,
			});
			return;
		}
		room.remoteParticipants.forEach((participant) => {
			if (extractUserIdFromVoiceIdentity(participant.identity) !== userId) return;
			try {
				ParticipantVolume.applySettingsToParticipant(participant);
			} catch (error) {
				logger.warn(`Failed to apply audio preferences for user ${userId}`, {error});
			}
		});
	}

	applyAllLocalAudioPreferences(room: Room | null): void {
		assertNullableObjectLike<Room>(room, 'applyAllLocalAudioPreferences.room');
		if (!room) {
			return;
		}
		ParticipantVolume.applySettingsToRoom(room);
	}

	applyLocalInputVolume(room: Room | null): void {
		assertNullableObjectLike<Room>(room, 'applyLocalInputVolume.room');
		if (!room?.localParticipant) {
			return;
		}
		const audioTrack = this.getLocalAudioTrack(room);
		updateVoiceInputGain(audioTrack);
	}

	async refreshLocalVoiceInputProcessor(room: Room | null): Promise<void> {
		assertNullableObjectLike<Room>(room, 'refreshLocalVoiceInputProcessor.room');
		if (!room?.localParticipant) {
			return;
		}
		const audioTrack = this.getLocalAudioTrack(room);
		await syncVoiceInputProcessor(audioTrack);
		if (!audioTrack?.mediaStreamTrack) {
			return;
		}
		const profile = resolveVoiceProcessingFromStateForDeviceLabel(VoiceSettings, this.resolveActiveInputDeviceLabel());
		applyContentHintToTrack(audioTrack.mediaStreamTrack, profile.contentHint);
		this.startLocalSpeakingDetector(room, audioTrack.mediaStreamTrack);
	}

	setLocalVideoDisabled(identity: string, disabled: boolean, room: Room | null, connectionId: string | null): void {
		assertNonEmptyString(identity, 'setLocalVideoDisabled.identity');
		assertBoolean(disabled, 'setLocalVideoDisabled.disabled');
		assertNullableObjectLike<Room>(room, 'setLocalVideoDisabled.room');
		assertOptionalNonEmptyString(connectionId, 'setLocalVideoDisabled.connectionId');
		if (!connectionId) {
			logger.warn('No connection ID');
			return;
		}
		CallMediaPrefs.setVideoDisabled(connectionId, identity, disabled);
		if (!room) return;
		const p = room.remoteParticipants.get(identity);
		if (!p) return;
		p.videoTrackPublications.forEach((pub) => {
			if (pub.source !== Track.Source.Camera) {
				logger.debug('Ignoring non-camera publication in local video subscription update', {
					identity,
					source: pub.source,
					trackSid: pub.trackSid,
				});
				return;
			}
			try {
				if (disabled) {
					pub.setSubscribed(false);
					logger.debug('Unsubscribed from track', {
						identity,
						source: pub.source,
						trackSid: pub.trackSid,
					});
					return;
				}
				pub.setSubscribed(true);
				logger.debug('Re-subscribed to local video track', {
					identity,
					trackSid: pub.trackSid,
					trackType: 'camera',
				});
			} catch (err) {
				logger.error('Failed to update subscription', {
					error: err,
					identity,
					source: pub.source,
					disabled,
				});
			}
		});
	}

	applyPushToTalkHold(held: boolean, room: Room | null, getCurrentUserVoiceState: () => VoiceState | null): void {
		assertBoolean(held, 'applyPushToTalkHold.held');
		assertNullableObjectLike<Room>(room, 'applyPushToTalkHold.room');
		assert.equal(
			typeof getCurrentUserVoiceState,
			'function',
			'applyPushToTalkHold.getCurrentUserVoiceState must be a function',
		);
		Keybind.setPushToTalkHeld(held);
		if (!isPushToTalkActiveFromAppState()) return;
		const serverVoiceState = getCurrentUserVoiceState();
		const audioState = this.getEffectiveAudioState({
			serverMute: serverVoiceState?.mute ?? false,
			serverDeaf: serverVoiceState?.deaf,
		});
		if (isMutedOrDeafened(audioState)) return;
		const targetMute = audioState.selfMute || !held;
		const ok = this.setMicrophonePublicationsMuted(room, targetMute, 'push to talk');
		if (!ok) {
			this.syncVoiceState({self_mute: true});
			return;
		}
		this.syncVoiceState({self_mute: targetMute});
		this.updateMediaAudioControls();
		this.syncLocalSpeakingOverride(room);
	}

	applyPushToMuteHold(held: boolean, room: Room | null, getCurrentUserVoiceState: () => VoiceState | null): void {
		assertBoolean(held, 'applyPushToMuteHold.held');
		assertNullableObjectLike<Room>(room, 'applyPushToMuteHold.room');
		assert.equal(
			typeof getCurrentUserVoiceState,
			'function',
			'applyPushToMuteHold.getCurrentUserVoiceState must be a function',
		);
		Keybind.setPushToMuteHeld(held);
		if (!isPushToMuteActiveFromAppState()) return;
		const serverVoiceState = getCurrentUserVoiceState();
		const audioState = this.getEffectiveAudioState({
			serverMute: serverVoiceState?.mute ?? false,
			serverDeaf: serverVoiceState?.deaf,
		});
		if (isMutedOrDeafened(audioState)) return;
		const baselineMute = LocalVoiceState.getSelfMute();
		const targetMute = held || baselineMute;
		const ok = this.setMicrophonePublicationsMuted(room, targetMute, 'push to mute');
		if (!ok) {
			this.syncVoiceState({self_mute: true});
			return;
		}
		this.syncVoiceState({self_mute: targetMute});
		this.updateMediaAudioControls();
		this.syncLocalSpeakingOverride(room);
	}

	handlePushToTalkModeChange(room: Room | null, getCurrentUserVoiceState: () => VoiceState | null): void {
		assertNullableObjectLike<Room>(room, 'handlePushToTalkModeChange.room');
		assert.equal(
			typeof getCurrentUserVoiceState,
			'function',
			'handlePushToTalkModeChange.getCurrentUserVoiceState must be a function',
		);
		Keybind.resetPushToTalkState();
		Keybind.resetPushToMuteState();
		if (isPushToTalkActiveFromAppState() && LocalVoiceState.getSelfMute()) {
			LocalVoiceState.updateSelfMute(false);
		}
		const serverVoiceState = getCurrentUserVoiceState();
		const audioState = this.getEffectiveAudioState({
			serverMute: serverVoiceState?.mute ?? false,
			serverDeaf: serverVoiceState?.deaf,
		});
		if (isMutedOrDeafened(audioState)) return;
		const targetMute = getEffectiveSelfMuteForVoiceStatePayloadFromV2AudioControls();
		const ok = this.setMicrophonePublicationsMuted(room, targetMute, 'push to talk mode change');
		if (!ok) {
			this.syncVoiceState({self_mute: true});
			return;
		}
		this.syncVoiceState({self_mute: targetMute});
		this.updateMediaAudioControls();
		this.syncLocalSpeakingOverride(room);
	}

	getMuteReason(voiceState: VoiceState | null, guildId?: string | null, channelId?: string | null): VoiceMuteReason {
		if (voiceState !== null) {
			assertObjectLike<VoiceState>(voiceState, 'getMuteReason.voiceState');
		}
		assertOptionalNonEmptyString(guildId, 'getMuteReason.guildId');
		assertOptionalNonEmptyString(channelId, 'getMuteReason.channelId');
		return selectVoiceEngineV2AppMuteReason({
			voiceState,
			permissionMuted: isVoicePermissionMuteActive(voiceState, guildId, channelId),
			audio: getVoiceEngineV2AudioControlsFromAppState(),
		});
	}

	private getLocalAudioTrack(room: Room): LocalAudioTrack | null {
		const micPublication = this.getMicrophonePublication(room);
		return (micPublication?.track as LocalAudioTrack) ?? null;
	}

	private getEffectiveAudioState(override?: {
		serverMute?: boolean;
		serverDeaf?: boolean;
		selfMute?: boolean;
	}): EffectiveAudioState {
		return getEffectiveAudioState(override);
	}

	private applyDeafen(room: Room, deafened: boolean): void {
		logger.debug('Applying deaf state', {
			deafened,
			participantCount: room.remoteParticipants.size,
		});
		room.remoteParticipants.forEach((participant) => {
			participant.audioTrackPublications.forEach((publication) => {
				const isScreenShareAudio = isScreenShareAudioPublicationLike(publication);
				const isMicrophone = asVoiceTrackSource(publication.source) === VoiceTrackSource.Microphone;
				if (!isScreenShareAudio && !isMicrophone) return;
				try {
					if (isScreenShareAudio) {
						if (deafened && publication.isDesired) {
							publication.setEnabled(false);
						}
						return;
					}
					if (deafened) {
						if (publication.isDesired) {
							publication.setEnabled(false);
						}
						publication.setSubscribed(false);
						return;
					}
					publication.setSubscribed(true);
					publication.setEnabled(true);
				} catch (error) {
					logger.error('Failed to apply deaf state to remote audio publication', {
						error,
						deafened,
						identity: participant.identity,
						isScreenShareAudio,
						trackSid: publication.trackSid,
					});
				}
			});
		});
	}

	private setAudioPublicationsMuted(room: Room, micMuted: boolean, reason: string): void {
		const muteStreamAudio = VoiceSettings.getMuteStreamAudio();
		for (const publication of room.localParticipant.audioTrackPublications.values()) {
			if (!publication) continue;
			let target: boolean;
			if (publication.source === Track.Source.Microphone) {
				target = micMuted;
			} else if (publication.source === Track.Source.ScreenShareAudio) {
				target = muteStreamAudio;
			} else {
				continue;
			}
			const operation = target ? publication.mute() : publication.unmute();
			operation.catch((error) =>
				logger.error(target ? 'Failed to mute publication' : 'Failed to unmute publication', {
					error,
					reason,
					source: publication.source,
				}),
			);
		}
	}

	private setMicrophonePublicationsMuted(room: Room | null, muted: boolean, reason: string): boolean {
		if (!room?.localParticipant) return true;
		const microphonePublications = getLocalMicrophonePublications(room.localParticipant);
		if (!muted && microphonePublications.length === 0) {
			logger.debug('No microphone publication exists while unmuting; syncing voice state without a local track.');
			return true;
		}
		microphonePublications.forEach((publication) => {
			const operation = muted ? publication.mute() : publication.unmute();
			operation.catch((error) =>
				logger.error(muted ? 'Failed to mute publication' : 'Failed to unmute publication', {
					error,
					reason,
					source: publication.source,
				}),
			);
		});
		return true;
	}

	private getLocalSpeakingOverrideState(room: Room): boolean | null {
		if (!room.localParticipant) return false;
		const publication = this.getMicrophonePublication(room);
		return resolveLocalSpeakingOverrideState({
			pushToTalkActive: isPushToTalkActiveFromAppState(),
			pushToMuteActive: isPushToMuteActiveFromAppState(),
			pushToMuteHeld: Keybind.pushToMuteHeld,
			selfDeaf: LocalVoiceState.getSelfDeaf(),
			effectiveSelfMute: getEffectiveSelfMuteForVoiceStatePayloadFromV2AudioControls(),
			hasMicrophonePublication: Boolean(publication),
			microphonePublicationMuted: publication?.isMuted ?? true,
		});
	}

	private setLocalParticipantAudioLevelSpeaking(room: Room, speaking: boolean): void {
		const localParticipant = room.localParticipant;
		if (!localParticipant) {
			return;
		}
		if (this.getLocalParticipantAudioLevelSpeaking(localParticipant.identity) === speaking) return;
		if (!setVoiceEngineV2ParticipantAudioLevelSpeaking(localParticipant.identity, speaking)) {
			updateLocalParticipantFromRoom(room);
			void setVoiceEngineV2ParticipantAudioLevelSpeaking(localParticipant.identity, speaking);
		}
	}

	private getLocalParticipantAudioLevelSpeaking(identity: string): boolean | null {
		const snapshot = getVoiceEngineV2SnapshotFromMediaEngine();
		if (!snapshot) return null;
		return selectVoiceEngineV2AppParticipant(snapshot, identity)?.isAudioLevelSpeaking ?? null;
	}

	private syncLocalSpeakingOverride(room: Room | null): void {
		if (!room?.localParticipant) return;
		const speaking = this.getLocalSpeakingOverrideState(room);
		if (speaking === null) return;
		this.speakingSilenceStartedAt = null;
		this.setLocalParticipantAudioLevelSpeaking(room, speaking);
	}

	private startLocalSpeakingDetector(room: Room, track: MediaStreamTrack | null): void {
		this.stopLocalSpeakingDetector(room);
		if (!track || track.readyState === 'ended' || !room.localParticipant) {
			return;
		}
		const AudioContextCtor =
			window.AudioContext || (window as typeof window & {webkitAudioContext?: typeof AudioContext}).webkitAudioContext;
		if (!AudioContextCtor) {
			return;
		}
		try {
			const graph = this.buildSpeakingDetectorAudioGraph(AudioContextCtor, track);
			this.commitSpeakingDetectorGraph(graph);
			const tick = this.createSpeakingDetectorTick({
				room,
				track,
				graph,
				getThresholdRms: () => getLocalSpeakingThresholdRms(VoiceSettings.getVadThreshold()),
				releaseDelayMs: SPEAKING_LOCAL_RELEASE_MS,
				localParticipantIdentity: room.localParticipant.identity,
			});
			this.bindSpeakingDetectorTrackEndedListener(room, track);
			this.speakingTimerId = window.setTimeout(tick, LOCAL_SPEAKING_ANALYSER_INTERVAL_MS);
			this.transitionMediaState({type: 'speakingDetector.attach'});
		} catch (error) {
			logger.warn('Failed to start local speaking detector', {error});
			this.stopLocalSpeakingDetector(room);
		}
	}

	private buildSpeakingDetectorAudioGraph(
		AudioContextCtor: typeof AudioContext,
		track: MediaStreamTrack,
	): SpeakingDetectorGraph {
		assert.equal(
			typeof AudioContextCtor,
			'function',
			'buildSpeakingDetectorAudioGraph.AudioContextCtor must be constructor',
		);
		assertObjectLike<MediaStreamTrack>(track, 'buildSpeakingDetectorAudioGraph.track');
		const audioContext = new AudioContextCtor({latencyHint: 'interactive'});
		const sourceNode = audioContext.createMediaStreamSource(new MediaStream([track]));
		const analyserNode = audioContext.createAnalyser();
		analyserNode.fftSize = 256;
		analyserNode.smoothingTimeConstant = 0.15;
		sourceNode.connect(analyserNode);
		if (audioContext.state === 'suspended') {
			void audioContext.resume().catch((error) => {
				logger.debug('Local speaking AudioContext resume rejected', {error});
			});
		}
		const samples = new Uint8Array(analyserNode.fftSize);
		return {audioContext, sourceNode, analyserNode, samples};
	}

	private commitSpeakingDetectorGraph(graph: SpeakingDetectorGraph): void {
		assertObjectLike<SpeakingDetectorGraph>(graph, 'commitSpeakingDetectorGraph.graph');
		this.speakingAudioContext = graph.audioContext;
		this.speakingSourceNode = graph.sourceNode;
		this.speakingAnalyserNode = graph.analyserNode;
		this.speakingSilenceStartedAt = null;
	}

	private bindSpeakingDetectorTrackEndedListener(room: Room, track: MediaStreamTrack): void {
		assertObjectLike<Room>(room, 'bindSpeakingDetectorTrackEndedListener.room');
		assertObjectLike<MediaStreamTrack>(track, 'bindSpeakingDetectorTrackEndedListener.track');
		const onTrackEnded = (): void => {
			this.transitionMediaState({type: 'microphone.publication.ended'});
			this.stopLocalSpeakingDetector(room);
		};
		track.addEventListener('ended', onTrackEnded, {once: true});
		this.speakingTrackEndedCleanup = () => {
			track.removeEventListener('ended', onTrackEnded);
		};
	}

	private createSpeakingDetectorTick(options: SpeakingDetectorTickOptions): () => void {
		assertObjectLike<SpeakingDetectorTickOptions>(options, 'createSpeakingDetectorTick.options');
		assertNonEmptyString(
			options.localParticipantIdentity,
			'createSpeakingDetectorTick.options.localParticipantIdentity',
		);
		const setSpeaking = (speaking: boolean): void => {
			this.setLocalParticipantAudioLevelSpeaking(options.room, speaking);
		};
		const tick = (): void => {
			this.speakingTimerId = null;
			if (this.speakingAnalyserNode !== options.graph.analyserNode) return;
			if (options.track.readyState === 'ended') {
				this.stopLocalSpeakingDetector(options.room);
				return;
			}
			const localSpeakingOverride = this.getLocalSpeakingOverrideState(options.room);
			if (localSpeakingOverride !== null) {
				this.applySpeakingDetectorOverride(options, localSpeakingOverride, setSpeaking);
				this.speakingTimerId = window.setTimeout(tick, LOCAL_SPEAKING_ANALYSER_INTERVAL_MS);
				return;
			}
			this.applySpeakingDetectorSample(options, setSpeaking);
			this.speakingTimerId = window.setTimeout(tick, LOCAL_SPEAKING_ANALYSER_INTERVAL_MS);
		};
		return tick;
	}

	private applySpeakingDetectorOverride(
		options: SpeakingDetectorTickOptions,
		localSpeakingOverride: boolean,
		setSpeaking: (speaking: boolean) => void,
	): void {
		assertObjectLike<SpeakingDetectorTickOptions>(options, 'applySpeakingDetectorOverride.options');
		assertBoolean(localSpeakingOverride, 'applySpeakingDetectorOverride.localSpeakingOverride');
		this.speakingSilenceStartedAt = null;
		if (this.getLocalParticipantAudioLevelSpeaking(options.localParticipantIdentity) !== localSpeakingOverride) {
			setSpeaking(localSpeakingOverride);
		}
	}

	private applySpeakingDetectorSample(
		options: SpeakingDetectorTickOptions,
		setSpeaking: (speaking: boolean) => void,
	): void {
		assertObjectLike<SpeakingDetectorTickOptions>(options, 'applySpeakingDetectorSample.options');
		assert.ok(options.releaseDelayMs >= 0, 'applySpeakingDetectorSample.options.releaseDelayMs must be non-negative');
		const {graph, releaseDelayMs, localParticipantIdentity} = options;
		const threshold = readSpeakingDetectorThresholdRms(options.getThresholdRms);
		graph.analyserNode.getByteTimeDomainData(graph.samples);
		const rms = computeSpeakingDetectorRms(graph.samples);
		const now = performance.now();
		if (rms >= threshold) {
			this.speakingSilenceStartedAt = null;
			if (!this.getLocalParticipantAudioLevelSpeaking(localParticipantIdentity)) {
				setSpeaking(true);
			}
			return;
		}
		this.speakingSilenceStartedAt ??= now;
		if (
			this.getLocalParticipantAudioLevelSpeaking(localParticipantIdentity) &&
			now - this.speakingSilenceStartedAt >= releaseDelayMs
		) {
			setSpeaking(false);
		}
	}

	private stopLocalSpeakingDetector(room?: Room | null): void {
		this.transitionMediaState({type: 'speakingDetector.detach'});
		if (this.speakingTimerId !== null) {
			window.clearTimeout(this.speakingTimerId);
			this.speakingTimerId = null;
		}
		this.speakingTrackEndedCleanup?.();
		this.speakingTrackEndedCleanup = null;
		try {
			this.speakingSourceNode?.disconnect();
		} catch (error) {
			logger.debug('Failed to disconnect local speaking source node', {error});
		}
		try {
			this.speakingAnalyserNode?.disconnect();
		} catch (error) {
			logger.debug('Failed to disconnect local speaking analyser node', {error});
		}
		void this.speakingAudioContext?.close().catch((error) => {
			logger.debug('Failed to close local speaking AudioContext', {error});
		});
		this.speakingAudioContext = null;
		this.speakingSourceNode = null;
		this.speakingAnalyserNode = null;
		this.speakingSilenceStartedAt = null;
		if (room) {
			this.setLocalParticipantAudioLevelSpeaking(room, false);
		}
	}
}

export default new VoiceEngineV2AppMediaExecutionAdapter();
