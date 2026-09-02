// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {isDesktop, isNativeMacOS} from '@app/features/ui/utils/NativeUtils';
import AdaptiveScreenShareEngine from '@app/features/voice/engine/AdaptiveScreenShareEngine';
import {updateLocalParticipantFromRoom} from '@app/features/voice/engine/VoiceMediaEngineBridge';
import {
	enforceLocalMediaPublicationCap,
	getLocalPublicationMediaStreamTrack,
	getLocalScreenShareAudioPublications,
	getLocalScreenSharePublications,
	getLocalScreenShareVideoPublications,
} from '@app/features/voice/engine/VoiceTrackPublicationUtils';
import {VoiceTrackSource} from '@app/features/voice/engine/VoiceTrackSource';
import type {
	ScreenShareReconnectSnapshot,
	VoiceEngineV2AppScreenShareExecutionAdapter,
} from '@app/features/voice/engine/v2/VoiceEngineV2AppScreenShareExecutionAdapter';
import {
	guardScreenShareEntry,
	SCREEN_SHARE_SOURCE_SWITCH_UNSUPPORTED_PLATFORM_WARNING,
	SCREEN_SHARE_UNSUPPORTED_PLATFORM_WARNING,
} from '@app/features/voice/engine/v2/VoiceEngineV2AppScreenShareGuards';
import {
	applyScreenShareState,
	buildScreenShareFailureTransition,
	runScreenShareActivationRitual,
	settleScreenShareFailure,
} from '@app/features/voice/engine/v2/VoiceEngineV2AppScreenShareRituals';
import {createDeviceReplacementTracks} from '@app/features/voice/engine/voice_screen_share_manager/DeviceMediaCapture';
import {createDisplayScreenShareTracks} from '@app/features/voice/engine/voice_screen_share_manager/DisplayMediaCapture';
import {
	ensureNativeCameraPermissionForDeviceShare,
	ensureNativeMediaPermission,
	ensureNativeMicrophonePermissionForDeviceShare,
} from '@app/features/voice/engine/voice_screen_share_manager/NativePermissionGate';
import {
	type CapturedScreenShareTracks,
	type DeviceScreenShareCaptureOptions,
	type DisplayScreenShareCaptureContext,
	getReplacementScreenShareSettingsOptions,
	logger,
	type ScreenShareCaptureCleanupSnapshot,
	type SimulcastTrackInfoLike,
	stopMediaTrack,
} from '@app/features/voice/engine/voice_screen_share_manager/shared';
import ActiveScreenShareSource, {
	type PublishedScreenShareSource,
} from '@app/features/voice/state/ActiveScreenShareSource';
import type LocalVoiceState from '@app/features/voice/state/LocalVoiceState';
import SoftwareEncoderWarning from '@app/features/voice/state/SoftwareEncoderWarning';
import VoiceSettings from '@app/features/voice/state/VoiceSettings';
import {
	prepareHighFidelityScreenShareAudioTrack,
	SCREEN_SHARE_AUDIO_PUBLISH_OPTIONS,
} from '@app/features/voice/utils/AudioPublishOptions';
import type {ScreenShareContentSource} from '@app/features/voice/utils/CodecCapabilityDetector';
import {commitNativeAudioBridgeReplacement} from '@app/features/voice/utils/NativeAudioCaptureBridge';
import {ScreenShareAudioCaptureError} from '@app/features/voice/utils/ScreenShareAudioCaptureError';
import {ScreenShareRollbackIncompleteError} from '@app/features/voice/utils/ScreenShareRollbackIncompleteError';
import {applyCameraMirrorProcessor} from '@app/features/voice/utils/VideoBackgroundProcessor';
import {
	createLocalAudioTrack,
	createLocalVideoTrack,
	LocalAudioTrack,
	type LocalParticipant,
	type LocalTrackPublication,
	type LocalVideoTrack,
	type Room,
	type ScreenShareCaptureOptions,
	Track,
	type TrackPublishOptions,
} from 'livekit-client';

function isUserCancelledScreenShareError(error: unknown): boolean {
	if (!(error instanceof Error)) return false;
	if (error.name === 'AbortError') return true;
	if (error.name === 'NotAllowedError') return true;
	return false;
}

function isUserCancelledOrPermissionDeniedError(error: unknown): boolean {
	if (!(error instanceof Error)) return false;
	if (error.name === 'AbortError') return true;
	if (error.name === 'NotAllowedError') return true;
	if (error.name === 'PermissionDeniedError') return true;
	return false;
}

const COMMITTED_PUBLICATION_INVARIANT_ATTEMPTS = 2;

interface ScreenShareReplacementSnapshot {
	videoTrack: MediaStreamTrack;
	videoHadProcessor: boolean;
	audioPublication?: LocalTrackPublication;
	audioTrack?: LocalAudioTrack;
	audioMediaStreamTrack?: MediaStreamTrack;
	audioMuted: boolean;
	contentSource: ScreenShareContentSource;
	publishedSource: PublishedScreenShareSource | null;
	sourceId: string | null;
	isOwnWindow: boolean;
	publishOptions: TrackPublishOptions;
}

type ScreenShareAudioReplacementStage =
	| {kind: 'none'}
	| {
			kind: 'candidate';
			publication: LocalTrackPublication;
			track: LocalAudioTrack;
			mediaStreamTrack: MediaStreamTrack;
	  };

interface ScreenShareSimulcastReplacementStageEntry {
	info: SimulcastTrackInfoLike;
	previousTrack: MediaStreamTrack;
	nextTrack: MediaStreamTrack;
	sender?: RTCRtpSender;
}

type ScreenShareSimulcastReplacementStage = Array<ScreenShareSimulcastReplacementStageEntry>;

function isCurrentScreenShareAudioReplacementStage(
	participant: LocalParticipant,
	stage: Extract<ScreenShareAudioReplacementStage, {kind: 'candidate'}>,
): boolean {
	const publication = Array.from(participant.audioTrackPublications.values()).find(
		(candidate) => candidate === stage.publication,
	);
	const localTrack = publication?.audioTrack ?? publication?.track;
	const mediaStreamTrack = localTrack?.mediaStream?.getAudioTracks()[0] ?? localTrack?.mediaStreamTrack;
	return (
		publication === stage.publication &&
		localTrack === stage.track &&
		mediaStreamTrack === stage.mediaStreamTrack &&
		stage.mediaStreamTrack.readyState === 'live'
	);
}

interface ScreenShareReconciliationStep {
	name: string;
	run: () => void;
}

export class VoiceEngineV2AppScreenShareLiveKitFlows {
	private readonly adapter: VoiceEngineV2AppScreenShareExecutionAdapter;

	constructor(adapter: VoiceEngineV2AppScreenShareExecutionAdapter) {
		this.adapter = adapter;
	}

	private async ensureMacScreenRecordingPermission(): Promise<void> {
		if (!(isDesktop() && isNativeMacOS())) return;
		await ensureNativeMediaPermission({kind: 'screen', onDenied: 'throw'});
	}

	private async restartScreenShareViaSetEnabled(
		room: Room | null,
		restOptions: ScreenShareCaptureOptions,
		sendUpdate: boolean,
		playSound: boolean,
		publishOptions: TrackPublishOptions | undefined,
	): Promise<void> {
		assert.equal(typeof sendUpdate, 'boolean');
		assert.equal(typeof playSound, 'boolean');
		await this.setEnabled(room, false, {sendUpdate: false, playSound: false});
		await this.setEnabled(room, true, {...restOptions, sendUpdate, playSound}, publishOptions);
	}

	private preparePreflightForSetEnabled(
		participant: LocalParticipant,
		enabled: boolean,
		applyState: (value: boolean) => void,
	): ScreenShareCaptureCleanupSnapshot | null {
		assert.ok(participant);
		assert.equal(typeof enabled, 'boolean');
		if (!enabled) applyState(false);
		if (!enabled) {
			this.adapter.setStreamingPriorityInternal(false);
			this.adapter.cleanupActiveScreenShareEndListenerInternal();
			this.adapter.cancelEncoderVerificationInternal();
			AdaptiveScreenShareEngine.stop();
		}
		SoftwareEncoderWarning.reset();
		const stopCleanupSnapshot = enabled ? null : this.adapter.getScreenShareCaptureCleanupSnapshotInternal(participant);
		return stopCleanupSnapshot;
	}

	private async finalizeSetEnabledSuccess(
		room: Room | null,
		participant: LocalParticipant,
		enabled: boolean,
		effectivePublishOptions: TrackPublishOptions | undefined,
		stopCleanupSnapshot: ScreenShareCaptureCleanupSnapshot | null,
		applyState: (value: boolean) => void,
		playSound: boolean,
	): Promise<void> {
		assert.ok(participant);
		assert.equal(typeof enabled, 'boolean');
		await runScreenShareActivationRitual({
			adapter: this.adapter,
			room,
			participant,
			active: enabled,
			steps: {
				acquireStreamingPriority: false,
				enforcePublicationCap: enabled,
				applyState: enabled ? applyState : null,
				applyStatePosition: 'before-pipeline',
				publishPipeline: enabled ? {contentSource: undefined, effectivePublishOptions} : null,
				deactivateCleanup: enabled
					? null
					: async () => {
							await this.adapter.cleanupLingeringScreenShareTracks(participant, stopCleanupSnapshot ?? undefined);
						},
				updateLocalParticipant: true,
				audioSync: {kind: 'participant-after-watch'},
				syncPersistedAudioPreferenceWhenActive: true,
				playSound,
				buildResolveTransition: () => ({
					type: 'share.resolve',
					active: enabled,
					sourceType: enabled ? 'display' : null,
					encoderVerificationScheduled: this.adapter.encoderVerificationTimer != null,
					streamingPriorityHeld: this.adapter.streamingPriorityHeld,
				}),
			},
		});
		logger.info('Success', {enabled});
	}

	private async handleSetEnabledFailure(
		room: Room | null,
		participant: LocalParticipant,
		enabled: boolean,
		stopCleanupSnapshot: ScreenShareCaptureCleanupSnapshot | null,
		applyState: (value: boolean) => void,
		playSound: boolean,
		error: unknown,
	): Promise<void> {
		assert.ok(participant);
		const cancelled = isUserCancelledScreenShareError(error);
		if (cancelled) {
			logger.debug('User cancelled or permission denied', {name: (error as Error).name});
		} else {
			logger.error('Failed', {enabled, error});
		}
		const actual = participant.isScreenShareEnabled;
		if (enabled && !actual) {
			this.adapter.setStreamingPriorityInternal(false);
			this.adapter.clearScreenShareKeepAliveSinkInternal();
		}
		if (!actual) {
			await this.adapter.cleanupLingeringScreenShareTracks(participant, stopCleanupSnapshot ?? undefined);
		}
		settleScreenShareFailure({
			adapter: this.adapter,
			room,
			participant,
			actual,
			applyState,
			onInactiveAfterSync: null,
			monitorEndOnActive: true,
			playSound: !cancelled && playSound,
			buildTransition: (actualNow) =>
				buildScreenShareFailureTransition({
					cancelled,
					active: actualNow,
					sourceType: actualNow ? this.adapter.getActiveScreenShareSourceTypeInternal() : null,
				}),
		});
		if (!cancelled) throw error;
	}

	private shouldRestartScreenShare(
		enabled: boolean,
		restartIfEnabled: boolean,
		existingPublicationCount: number,
	): boolean {
		if (!enabled) return false;
		if (!restartIfEnabled) return false;
		return existingPublicationCount > 0;
	}

	async setEnabled(
		room: Room | null,
		enabled: boolean,
		options?: ScreenShareCaptureOptions & {
			sendUpdate?: boolean;
			playSound?: boolean;
			restartIfEnabled?: boolean;
		},
		publishOptions?: TrackPublishOptions,
	): Promise<void> {
		assert.equal(typeof enabled, 'boolean');
		if (guardScreenShareEntry({platformUnsupportedWarning: SCREEN_SHARE_UNSUPPORTED_PLATFORM_WARNING}) !== 'proceed') {
			return;
		}
		const {sendUpdate = true, playSound = true, restartIfEnabled = false, ...restOptions} = options || {};
		const participant = room?.localParticipant;
		if (!participant) {
			logger.warn('No participant');
			return;
		}
		const pendingVerdict = guardScreenShareEntry({
			pending: {
				active: this.adapter.isScreenSharePending,
				debugMessage: 'Already pending, ignoring request',
				onBlocked: () => {
					if (!enabled) this.adapter.queuePendingStopRequestInternal(options);
				},
			},
		});
		if (pendingVerdict === 'share-pending') {
			return;
		}
		const existingScreenSharePublications = getLocalScreenSharePublications(participant);
		if (this.shouldRestartScreenShare(enabled, restartIfEnabled, existingScreenSharePublications.length)) {
			await this.restartScreenShareViaSetEnabled(room, restOptions, sendUpdate, playSound, publishOptions);
			return;
		}
		if (enabled) {
			await enforceLocalMediaPublicationCap(participant, VoiceTrackSource.ScreenShare);
			await this.ensureMacScreenRecordingPermission();
		}
		const applyState = (value: boolean) => {
			applyScreenShareState(this.adapter, value, sendUpdate, sendUpdate);
		};
		const stopCleanupSnapshot = this.preparePreflightForSetEnabled(participant, enabled, applyState);
		this.adapter.transitionScreenShareLifecycleInternal(
			enabled ? {type: 'share.start', sourceType: 'display'} : {type: 'share.stop', request: {sendUpdate, playSound}},
		);
		if (enabled) this.adapter.setStreamingPriorityInternal(true);
		try {
			const effectivePublishOptions = await this.adapter.getEffectivePublishOptionsInternal(enabled, publishOptions);
			await participant.setScreenShareEnabled(enabled, restOptions, effectivePublishOptions);
			await this.finalizeSetEnabledSuccess(
				room,
				participant,
				enabled,
				effectivePublishOptions,
				stopCleanupSnapshot,
				applyState,
				playSound,
			);
		} catch (error) {
			await this.handleSetEnabledFailure(room, participant, enabled, stopCleanupSnapshot, applyState, playSound, error);
		}
	}

	private async createDeviceTracksForShare(
		options: DeviceScreenShareCaptureOptions | undefined,
		createdTracks: Array<LocalAudioTrack | LocalVideoTrack>,
	): Promise<{videoTrack: LocalVideoTrack; audioTrack: LocalAudioTrack | undefined}> {
		assert.ok(createdTracks);
		const {videoDeviceId, audioDeviceId, resolution} = options || {};
		await ensureNativeCameraPermissionForDeviceShare('start');
		if (audioDeviceId !== undefined) {
			await ensureNativeMicrophonePermissionForDeviceShare('start');
		}
		const videoTrack = await createLocalVideoTrack({
			deviceId: videoDeviceId && videoDeviceId !== 'default' ? videoDeviceId : undefined,
			resolution: resolution
				? {width: resolution.width, height: resolution.height, frameRate: resolution.frameRate}
				: undefined,
		});
		createdTracks.push(videoTrack);
		await applyCameraMirrorProcessor(videoTrack);
		let audioTrack: LocalAudioTrack | undefined;
		if (audioDeviceId !== undefined) {
			audioTrack = await createLocalAudioTrack({
				deviceId: audioDeviceId || undefined,
				echoCancellation: false,
				noiseSuppression: false,
				autoGainControl: false,
				voiceIsolation: false,
				channelCount: 2,
				sampleRate: 48000,
			});
			createdTracks.push(audioTrack);
		}
		return {videoTrack, audioTrack};
	}

	private async publishDeviceTracks(
		participant: LocalParticipant,
		videoTrack: LocalVideoTrack,
		audioTrack: LocalAudioTrack | undefined,
		effectivePublishOptions: TrackPublishOptions | undefined,
		publishedTracks: Array<LocalAudioTrack | LocalVideoTrack>,
	): Promise<void> {
		assert.ok(participant);
		assert.ok(videoTrack);
		await participant.publishTrack(videoTrack, {
			...effectivePublishOptions,
			source: Track.Source.ScreenShare,
			stream: VoiceTrackSource.ScreenShare,
		});
		publishedTracks.push(videoTrack);
		if (audioTrack) {
			prepareHighFidelityScreenShareAudioTrack(audioTrack.mediaStreamTrack);
			await participant.publishTrack(audioTrack, SCREEN_SHARE_AUDIO_PUBLISH_OPTIONS);
			publishedTracks.push(audioTrack);
		}
		await enforceLocalMediaPublicationCap(participant, VoiceTrackSource.ScreenShare);
	}

	private async finalizeDeviceShareSuccess(
		room: Room | null,
		participant: LocalParticipant,
		options: DeviceScreenShareCaptureOptions | undefined,
		audioTrack: LocalAudioTrack | undefined,
		effectivePublishOptions: TrackPublishOptions | undefined,
		applyState: (value: boolean) => void,
		playSound: boolean,
	): Promise<void> {
		assert.ok(participant);
		const {videoDeviceId} = options || {};
		await runScreenShareActivationRitual({
			adapter: this.adapter,
			room,
			participant,
			active: true,
			steps: {
				acquireStreamingPriority: true,
				enforcePublicationCap: false,
				applyState,
				applyStatePosition: 'before-pipeline',
				publishPipeline: {contentSource: 'device', effectivePublishOptions},
				deactivateCleanup: null,
				updateLocalParticipant: true,
				audioSync: {kind: 'participant-after-watch'},
				syncPersistedAudioPreferenceWhenActive: false,
				playSound,
				buildResolveTransition: () => ({
					type: 'share.resolve',
					active: true,
					sourceType: 'device',
					encoderVerificationScheduled: this.adapter.encoderVerificationTimer != null,
					streamingPriorityHeld: this.adapter.streamingPriorityHeld,
				}),
			},
		});
		logger.info('Started device screen share', {videoDeviceId, audioIncluded: audioTrack != null});
	}

	private async handleDeviceShareFailure(
		room: Room | null,
		participant: LocalParticipant,
		options: DeviceScreenShareCaptureOptions | undefined,
		applyState: (value: boolean) => void,
		createdTracks: Array<LocalAudioTrack | LocalVideoTrack>,
		publishedTracks: Array<LocalAudioTrack | LocalVideoTrack>,
		error: unknown,
	): Promise<void> {
		assert.ok(participant);
		if (publishedTracks.length > 0) {
			await Promise.allSettled(publishedTracks.map((track) => participant.unpublishTrack(track)));
		}
		createdTracks.forEach((track) => {
			track.stop();
		});
		const cancelled = isUserCancelledOrPermissionDeniedError(error);
		settleScreenShareFailure({
			adapter: this.adapter,
			room,
			participant,
			actual: participant.isScreenShareEnabled,
			applyState,
			onInactiveAfterSync: null,
			monitorEndOnActive: false,
			playSound: false,
			buildTransition: (actualNow) =>
				buildScreenShareFailureTransition({
					cancelled,
					active: actualNow,
					sourceType: actualNow ? this.adapter.getActiveScreenShareSourceTypeInternal() : null,
				}),
		});
		if (!cancelled) {
			logger.error('Failed to start device screen share', {
				error,
				videoDeviceId: options?.videoDeviceId,
				audioIncluded: options?.audioDeviceId != null,
			});
		}
	}

	async startDeviceScreenShare(
		room: Room | null,
		options?: DeviceScreenShareCaptureOptions,
		publishOptions?: TrackPublishOptions,
	): Promise<void> {
		if (guardScreenShareEntry({platformUnsupportedWarning: SCREEN_SHARE_UNSUPPORTED_PLATFORM_WARNING}) !== 'proceed') {
			return;
		}
		const {sendUpdate = true, playSound = true} = options || {};
		const participant = room?.localParticipant;
		if (!participant) {
			logger.warn('No participant');
			return;
		}
		const pendingVerdict = guardScreenShareEntry({
			pending: {
				active: this.adapter.isScreenSharePending,
				debugMessage: 'Already pending, ignoring device share request',
			},
		});
		if (pendingVerdict === 'share-pending') {
			return;
		}
		if (getLocalScreenSharePublications(participant).length > 0) {
			await this.setEnabled(room, false, {sendUpdate: false, playSound: false});
		}
		const applyState = (value: boolean) => {
			applyScreenShareState(this.adapter, value, sendUpdate, sendUpdate);
		};
		this.adapter.transitionScreenShareLifecycleInternal({type: 'share.start', sourceType: 'device'});
		const createdTracks: Array<LocalAudioTrack | LocalVideoTrack> = [];
		const publishedTracks: Array<LocalAudioTrack | LocalVideoTrack> = [];
		try {
			const effectivePublishOptions = await this.adapter.getEffectivePublishOptionsInternal(true, publishOptions);
			const {videoTrack, audioTrack} = await this.createDeviceTracksForShare(options, createdTracks);
			await this.publishDeviceTracks(participant, videoTrack, audioTrack, effectivePublishOptions, publishedTracks);
			await this.finalizeDeviceShareSuccess(
				room,
				participant,
				options,
				audioTrack,
				effectivePublishOptions,
				applyState,
				playSound,
			);
		} catch (error) {
			await this.handleDeviceShareFailure(
				room,
				participant,
				options,
				applyState,
				createdTracks,
				publishedTracks,
				error,
			);
		}
		await this.adapter.applyPendingScreenShareRequestsInternal(room, participant);
	}

	private emitReplaceShareResult(
		participant: LocalParticipant,
		sourceType: 'display' | 'device',
		didReplace: boolean,
	): void {
		assert.ok(participant);
		if (didReplace) {
			this.adapter.transitionScreenShareLifecycleInternal({
				type: 'share.resolve',
				active: true,
				sourceType,
				encoderVerificationScheduled: this.adapter.encoderVerificationTimer != null,
				streamingPriorityHeld: this.adapter.streamingPriorityHeld,
			});
			return;
		}
		this.adapter.transitionScreenShareLifecycleInternal({
			type: 'share.reject',
			active: participant.isScreenShareEnabled,
			sourceType: participant.isScreenShareEnabled ? this.adapter.getActiveScreenShareSourceTypeInternal() : null,
		});
	}

	private handleReplaceShareFailure(
		participant: LocalParticipant,
		failureContext: {kind: 'display'} | {kind: 'device'; options?: DeviceScreenShareCaptureOptions},
		error: unknown,
	): boolean {
		assert.ok(participant);
		const cancelled = isUserCancelledOrPermissionDeniedError(error);
		if (cancelled) {
			const label = failureContext.kind === 'display' ? 'screen share' : 'device share';
			logger.debug(`User cancelled or denied ${label} source switch`, {name: (error as Error).name});
		} else if (failureContext.kind === 'display') {
			logger.error('Failed to replace active display screen share source', {error});
		} else {
			logger.error('Failed to replace active device screen share source', {
				error,
				videoDeviceId: failureContext.options?.videoDeviceId,
				audioIncluded: failureContext.options?.audioDeviceId != null,
			});
		}
		const active = participant.isScreenShareEnabled;
		const sourceType = active ? this.adapter.getActiveScreenShareSourceTypeInternal() : null;
		this.adapter.transitionScreenShareLifecycleInternal(
			buildScreenShareFailureTransition({cancelled, active, sourceType}),
		);
		return cancelled;
	}

	async replaceActiveDisplayShare(
		room: Room | null,
		options?: ScreenShareCaptureOptions,
		publishOptions?: TrackPublishOptions,
		captureContext?: DisplayScreenShareCaptureContext,
	): Promise<boolean> {
		const platformVerdict = guardScreenShareEntry({
			platformUnsupportedWarning: SCREEN_SHARE_SOURCE_SWITCH_UNSUPPORTED_PLATFORM_WARNING,
		});
		if (platformVerdict !== 'proceed') {
			return false;
		}
		const participant = room?.localParticipant;
		if (!participant || !participant.isScreenShareEnabled) {
			logger.warn('No active screen share to replace');
			return false;
		}
		const pendingVerdict = guardScreenShareEntry({
			pending: {
				active: this.adapter.isScreenSharePending,
				debugMessage: 'Already pending, ignoring screen share source switch',
			},
		});
		if (pendingVerdict === 'share-pending') {
			return false;
		}
		let didReplace = false;
		this.adapter.transitionScreenShareLifecycleInternal({
			type: 'share.replace',
			sourceType: 'display',
			codecRepublishInFlight: true,
		});
		try {
			const tracks = await createDisplayScreenShareTracks(options, captureContext);
			didReplace = await this.replaceActiveTracks(room, participant, tracks, options, publishOptions);
			this.emitReplaceShareResult(participant, 'display', didReplace);
		} catch (error) {
			const cancelled = this.handleReplaceShareFailure(participant, {kind: 'display'}, error);
			if (!cancelled) throw error;
		} finally {
			await this.adapter.applyPendingScreenShareRequestsInternal(room, participant);
		}
		return didReplace;
	}

	async replaceActiveDeviceShare(
		room: Room | null,
		options?: DeviceScreenShareCaptureOptions,
		publishOptions?: TrackPublishOptions,
	): Promise<boolean> {
		const platformVerdict = guardScreenShareEntry({
			platformUnsupportedWarning: SCREEN_SHARE_SOURCE_SWITCH_UNSUPPORTED_PLATFORM_WARNING,
		});
		if (platformVerdict !== 'proceed') {
			return false;
		}
		const participant = room?.localParticipant;
		if (!participant || !participant.isScreenShareEnabled) {
			logger.warn('No active screen share to replace');
			return false;
		}
		const pendingVerdict = guardScreenShareEntry({
			pending: {
				active: this.adapter.isScreenSharePending,
				debugMessage: 'Already pending, ignoring device share source switch',
			},
		});
		if (pendingVerdict === 'share-pending') {
			return false;
		}
		let didReplace = false;
		this.adapter.transitionScreenShareLifecycleInternal({type: 'share.replace', sourceType: 'device'});
		try {
			await ensureNativeCameraPermissionForDeviceShare('replace');
			if (options?.audioDeviceId !== undefined) {
				await ensureNativeMicrophonePermissionForDeviceShare('replace');
			}
			const tracks = await createDeviceReplacementTracks(options);
			didReplace = await this.replaceActiveTracks(room, participant, tracks, undefined, publishOptions, 'device');
			this.emitReplaceShareResult(participant, 'device', didReplace);
		} catch (error) {
			this.handleReplaceShareFailure(participant, {kind: 'device', options}, error);
		}
		await this.adapter.applyPendingScreenShareRequestsInternal(room, participant);
		return didReplace;
	}

	private captureScreenShareReplacementSnapshot(
		participant: LocalParticipant,
		screenShareTrack: LocalVideoTrack,
		publishOptions: TrackPublishOptions,
	): ScreenShareReplacementSnapshot {
		const videoHadProcessor = screenShareTrack.getProcessor() != null;
		const sourceVideoTrack =
			screenShareTrack.mediaStream?.getVideoTracks()[0] ??
			(videoHadProcessor ? undefined : screenShareTrack.mediaStreamTrack);
		if (!sourceVideoTrack || sourceVideoTrack.readyState !== 'live') {
			throw new Error('Active screen share has no live source video track to preserve');
		}
		const audioPublications = getLocalScreenShareAudioPublications(participant);
		if (audioPublications.length > 1) {
			throw new Error('Active screen share has multiple audio publications before source replacement');
		}
		const audioPublication = audioPublications[0];
		const localAudioTrack = audioPublication?.audioTrack ?? (audioPublication?.track as LocalAudioTrack | undefined);
		const sourceAudioTrack = localAudioTrack?.mediaStream?.getAudioTracks()[0] ?? localAudioTrack?.mediaStreamTrack;
		if (audioPublication && (!localAudioTrack || !sourceAudioTrack || sourceAudioTrack.readyState !== 'live')) {
			throw new Error('Active screen share audio publication has no live source track to preserve');
		}
		return {
			videoTrack: sourceVideoTrack,
			videoHadProcessor,
			...(audioPublication ? {audioPublication} : {}),
			...(localAudioTrack && sourceAudioTrack
				? {audioTrack: localAudioTrack, audioMediaStreamTrack: sourceAudioTrack}
				: {}),
			audioMuted: audioPublication?.isMuted ?? false,
			contentSource: this.adapter.getActiveScreenShareContentSourceInternal(),
			publishedSource: ActiveScreenShareSource.getPublishedSource(),
			sourceId: ActiveScreenShareSource.getSourceId(),
			isOwnWindow: ActiveScreenShareSource.isOwnWindow(),
			publishOptions,
		};
	}

	private restorePublishedScreenShareSource(snapshot: ScreenShareReplacementSnapshot): void {
		if (snapshot.publishedSource === null) {
			ActiveScreenShareSource.clear();
			return;
		}
		ActiveScreenShareSource.setPublishedSource(snapshot.publishedSource, snapshot.sourceId, {
			isOwnWindow: snapshot.isOwnWindow,
		});
	}

	private async restoreScreenShareReplacement(
		room: Room,
		participant: LocalParticipant,
		screenShareTrack: LocalVideoTrack,
		snapshot: ScreenShareReplacementSnapshot,
		audioStage: ScreenShareAudioReplacementStage,
	): Promise<void> {
		this.adapter.cleanupActiveScreenShareEndListenerInternal();
		if (snapshot.videoTrack.readyState !== 'live') {
			throw new Error('Screen share replacement rollback source ended before it could be restored');
		}
		if (audioStage.kind === 'candidate') {
			const stagedPublication = Array.from(participant.audioTrackPublications.values()).find(
				(publication) => publication === audioStage.publication,
			);
			const stagedTrack = stagedPublication?.audioTrack ?? stagedPublication?.track;
			if (stagedPublication === audioStage.publication && stagedTrack === audioStage.track) {
				await participant.unpublishTrack(audioStage.track, false).catch((error) => {
					logger.warn('Failed to unpublish screen share audio candidate during rollback', {error});
				});
			}
			stopMediaTrack(audioStage.mediaStreamTrack);
		}
		const activeVideoTrack = screenShareTrack.mediaStream?.getVideoTracks()[0] ?? screenShareTrack.mediaStreamTrack;
		const sourceNeedsReplacement = activeVideoTrack !== snapshot.videoTrack;
		if (sourceNeedsReplacement) {
			await screenShareTrack.stageTrackReplacement(snapshot.videoTrack);
		}
		if (snapshot.videoHadProcessor) {
			const restoredProcessor = await applyCameraMirrorProcessor(screenShareTrack, true);
			if (!restoredProcessor) {
				throw new Error('Screen share rollback could not restore the previous video processor');
			}
		} else if (screenShareTrack.getProcessor()) {
			await screenShareTrack.stopProcessor(false);
		}
		if (snapshot.audioPublication && snapshot.audioTrack && snapshot.audioMediaStreamTrack) {
			const restoredAudioPublication = Array.from(participant.audioTrackPublications.values()).find(
				(publication) => publication === snapshot.audioPublication,
			);
			const restoredAudioTrack = restoredAudioPublication?.audioTrack ?? restoredAudioPublication?.track;
			const restoredAudioMediaStreamTrack =
				restoredAudioTrack?.mediaStream?.getAudioTracks()[0] ?? restoredAudioTrack?.mediaStreamTrack;
			if (
				!restoredAudioPublication ||
				restoredAudioTrack !== snapshot.audioTrack ||
				restoredAudioMediaStreamTrack !== snapshot.audioMediaStreamTrack ||
				restoredAudioMediaStreamTrack.readyState !== 'live'
			) {
				throw new Error('Screen share audio rollback did not restore its publication');
			}
			if (snapshot.audioMuted) {
				await restoredAudioPublication.mute();
			} else {
				await restoredAudioPublication.unmute();
			}
		}
		this.restorePublishedScreenShareSource(snapshot);
		await runScreenShareActivationRitual({
			adapter: this.adapter,
			room,
			participant,
			active: true,
			steps: {
				acquireStreamingPriority: false,
				enforcePublicationCap: true,
				applyState: () => applyScreenShareState(this.adapter, true, true, true),
				applyStatePosition: 'after-pipeline',
				publishPipeline: {
					contentSource: snapshot.contentSource,
					effectivePublishOptions: snapshot.publishOptions,
				},
				deactivateCleanup: null,
				updateLocalParticipant: true,
				audioSync: {kind: 'participant-after-watch'},
				syncPersistedAudioPreferenceWhenActive: true,
				playSound: false,
				buildResolveTransition: null,
			},
		});
		if (snapshot.videoTrack.readyState !== 'live') {
			throw new Error('Screen share replacement rollback source ended while runtime state was restored');
		}
		if (sourceNeedsReplacement) {
			await screenShareTrack.commitStagedTrackReplacement(snapshot.videoTrack, false);
		}
	}

	private async failClosedScreenShareReplacement(
		participant: LocalParticipant,
		snapshot: ScreenShareReplacementSnapshot,
		cleanupSnapshot?: ScreenShareCaptureCleanupSnapshot,
	): Promise<void> {
		let cleanupError: unknown;
		try {
			await this.adapter.cleanupLingeringScreenShareTracks(participant, cleanupSnapshot);
		} catch (error) {
			cleanupError = error;
			logger.error('Failed to clean up screen share after replacement rollback failed', {error});
		}
		stopMediaTrack(snapshot.videoTrack);
		stopMediaTrack(snapshot.audioMediaStreamTrack);
		ActiveScreenShareSource.clear();
		applyScreenShareState(this.adapter, false, true, true);
		this.adapter.syncLocalStreamWatchStateInternal(false);
		this.adapter.syncLocalScreenShareAudioStateInternal(participant, false);
		AdaptiveScreenShareEngine.stop();
		if (cleanupError !== undefined) {
			throw new ScreenShareRollbackIncompleteError([cleanupError]);
		}
	}

	private async stageScreenShareAudioReplacement(
		participant: LocalParticipant,
		tracks: CapturedScreenShareTracks,
	): Promise<ScreenShareAudioReplacementStage> {
		assert.ok(participant);
		assert.ok(tracks);
		if (!tracks.audioTrack) {
			return {kind: 'none'};
		}
		prepareHighFidelityScreenShareAudioTrack(tracks.audioTrack);
		let publication: LocalTrackPublication | null = null;
		const candidateTrack = new LocalAudioTrack(tracks.audioTrack);
		try {
			await candidateTrack.mute();
			publication = await participant.publishTrack(candidateTrack, SCREEN_SHARE_AUDIO_PUBLISH_OPTIONS);
			const track = publication.audioTrack ?? (publication.track as LocalAudioTrack | undefined);
			const mediaStreamTrack = track?.mediaStream?.getAudioTracks()[0] ?? track?.mediaStreamTrack;
			if (!track || mediaStreamTrack !== tracks.audioTrack || mediaStreamTrack.readyState !== 'live') {
				throw new Error('Replacement screen share audio publication has no live local track');
			}
			if (!publication.isMuted) {
				await publication.mute();
			}
			const stage: Extract<ScreenShareAudioReplacementStage, {kind: 'candidate'}> = {
				kind: 'candidate',
				publication,
				track,
				mediaStreamTrack,
			};
			if (!isCurrentScreenShareAudioReplacementStage(participant, stage)) {
				throw new Error('Replacement screen share audio publication changed during staging');
			}
			return stage;
		} catch (error) {
			let cleanupError: unknown;
			const track = publication?.audioTrack ?? (publication?.track as LocalAudioTrack | undefined);
			const currentPublication = Array.from(participant.audioTrackPublications.values()).find(
				(candidate) => candidate === publication,
			);
			if (
				publication &&
				track &&
				currentPublication === publication &&
				(publication.audioTrack ?? publication.track) === track
			) {
				try {
					await participant.unpublishTrack(track, false);
				} catch (caughtCleanupError) {
					cleanupError = caughtCleanupError;
				}
			}
			stopMediaTrack(tracks.audioTrack);
			if (cleanupError !== undefined) {
				throw new ScreenShareRollbackIncompleteError([error, cleanupError]);
			}
			throw error;
		}
	}

	private async commitScreenShareAudioReplacement(
		participant: LocalParticipant,
		videoPublication: LocalTrackPublication,
		videoTrack: LocalVideoTrack,
		videoMediaStreamTrack: MediaStreamTrack,
		snapshot: ScreenShareReplacementSnapshot,
		audioStage: ScreenShareAudioReplacementStage,
	): Promise<void> {
		const cleanupErrors: Array<unknown> = [];
		if (audioStage.kind === 'candidate' && !isCurrentScreenShareAudioReplacementStage(participant, audioStage)) {
			throw new Error('Replacement screen share audio source changed before replacement commit');
		}
		const previousAudioTrack = snapshot.audioTrack;
		if (previousAudioTrack) {
			const previousAudioPublicationPresent = Array.from(participant.audioTrackPublications.values()).some(
				(publication) => publication === snapshot.audioPublication,
			);
			const previousAudioMediaStreamTrack =
				previousAudioTrack.mediaStream?.getAudioTracks()[0] ?? previousAudioTrack.mediaStreamTrack;
			if (
				!previousAudioPublicationPresent ||
				previousAudioMediaStreamTrack !== snapshot.audioMediaStreamTrack ||
				previousAudioMediaStreamTrack.readyState !== 'live'
			) {
				throw new Error('Previous screen share audio source changed before replacement commit');
			}
			try {
				await previousAudioTrack.mute();
			} catch (error) {
				cleanupErrors.push(error);
				stopMediaTrack(snapshot.audioMediaStreamTrack);
			}
			try {
				const currentPreviousAudioPublication = Array.from(participant.audioTrackPublications.values()).find(
					(publication) => publication === snapshot.audioPublication,
				);
				const currentPreviousAudioTrack =
					currentPreviousAudioPublication?.audioTrack ?? currentPreviousAudioPublication?.track;
				const currentPreviousAudioMediaStreamTrack =
					currentPreviousAudioTrack?.mediaStream?.getAudioTracks()[0] ?? currentPreviousAudioTrack?.mediaStreamTrack;
				if (
					currentPreviousAudioPublication !== snapshot.audioPublication ||
					currentPreviousAudioTrack !== previousAudioTrack ||
					currentPreviousAudioMediaStreamTrack !== snapshot.audioMediaStreamTrack
				) {
					throw new Error('Previous screen share audio source changed while replacement commit was in progress');
				}
				if (audioStage.kind === 'candidate') {
					await participant.unpublishTrack(previousAudioTrack, false);
					stopMediaTrack(snapshot.audioMediaStreamTrack);
				} else {
					await this.adapter.replaceActiveScreenShareAudioTrackInternal(participant, undefined);
				}
			} catch (error) {
				cleanupErrors.push(error);
				stopMediaTrack(snapshot.audioMediaStreamTrack);
			}
		}
		if (audioStage.kind === 'candidate') {
			try {
				await audioStage.publication.unmute();
			} catch (error) {
				cleanupErrors.push(error);
				stopMediaTrack(audioStage.mediaStreamTrack);
			}
		}
		const hasCommittedPublicationInvariant = (): boolean => {
			const videoPublications = getLocalScreenShareVideoPublications(participant);
			const currentVideoTrack = videoPublication.videoTrack ?? videoPublication.track;
			const currentVideoMediaStreamTrack =
				currentVideoTrack?.mediaStream?.getVideoTracks()[0] ?? currentVideoTrack?.mediaStreamTrack;
			if (
				videoPublications.length !== 1 ||
				videoPublications[0] !== videoPublication ||
				currentVideoTrack !== videoTrack ||
				currentVideoMediaStreamTrack !== videoMediaStreamTrack ||
				videoMediaStreamTrack.readyState !== 'live'
			) {
				return false;
			}
			const audioPublications = getLocalScreenShareAudioPublications(participant);
			if (audioStage.kind === 'none') {
				return audioPublications.length === 0;
			}
			if (audioPublications.length !== 1 || audioPublications[0] !== audioStage.publication) {
				return false;
			}
			const publishedAudioTrack = audioStage.publication.audioTrack ?? audioStage.publication.track;
			return (
				publishedAudioTrack === audioStage.track && isCurrentScreenShareAudioReplacementStage(participant, audioStage)
			);
		};
		let publicationInvariantSatisfied = false;
		for (let attempt = 0; attempt < COMMITTED_PUBLICATION_INVARIANT_ATTEMPTS; attempt++) {
			const capResult = await enforceLocalMediaPublicationCap(participant, VoiceTrackSource.ScreenShare, {
				preferredPublication: audioStage.kind === 'candidate' ? audioStage.publication : undefined,
				stopOnUnpublish: true,
			});
			for (const failure of capResult.failedPublications) {
				cleanupErrors.push(failure.error);
				stopMediaTrack(getLocalPublicationMediaStreamTrack(failure.publication) ?? undefined);
			}
			publicationInvariantSatisfied = hasCommittedPublicationInvariant();
			if (publicationInvariantSatisfied) break;
		}
		if (!publicationInvariantSatisfied) {
			throw new AggregateError(
				[...cleanupErrors, new Error('Committed screen share publications do not match the replacement')],
				'Failed to establish committed screen share publication state',
			);
		}
		if (cleanupErrors.length > 0) {
			logger.warn('Recovered from screen share publication cleanup failures after source replacement', {
				errors: cleanupErrors,
			});
		}
		if (audioStage.kind === 'candidate') {
			commitNativeAudioBridgeReplacement();
		}
		if (!hasCommittedPublicationInvariant()) {
			throw new Error('Committed screen share publication ownership changed during native audio finalization');
		}
		this.adapter.syncLocalScreenShareAudioStateInternal(participant, true);
		this.adapter.syncPersistedScreenShareAudioPreferenceInternal(participant);
	}

	private async finalizeReplaceActiveTracks(
		room: Room,
		participant: LocalParticipant,
		tracks: CapturedScreenShareTracks,
		nextContentSource: ScreenShareContentSource,
		options: ScreenShareCaptureOptions | undefined,
		effectivePublishOptions: TrackPublishOptions | undefined,
	): Promise<void> {
		assert.ok(participant);
		assert.ok(tracks);
		const replacementSettings = getReplacementScreenShareSettingsOptions(options, tracks.audioTrack != null);
		await this.adapter.updateActiveScreenShareSettings(
			room,
			replacementSettings ? {...replacementSettings, audio: undefined} : undefined,
			effectivePublishOptions,
		);
		await runScreenShareActivationRitual({
			adapter: this.adapter,
			room,
			participant,
			active: true,
			steps: {
				acquireStreamingPriority: false,
				enforcePublicationCap: false,
				applyState: () => applyScreenShareState(this.adapter, true, true, true),
				applyStatePosition: 'after-pipeline',
				publishPipeline: {contentSource: nextContentSource, effectivePublishOptions},
				deactivateCleanup: null,
				updateLocalParticipant: true,
				audioSync: {kind: 'participant-after-watch'},
				syncPersistedAudioPreferenceWhenActive: true,
				playSound: false,
				buildResolveTransition: null,
			},
		});
	}

	private reconcileCommittedScreenShareReplacement(
		room: Room,
		participant: LocalParticipant,
		nextContentSource: ScreenShareContentSource,
		effectivePublishOptions: TrackPublishOptions | undefined,
	): void {
		const steps: Array<ScreenShareReconciliationStep> = [
			{
				name: 'content hint',
				run: () => this.adapter.applyScreenShareContentHintInternal(participant, nextContentSource),
			},
			{name: 'keep-alive sink', run: () => this.adapter.ensureScreenShareKeepAliveSinkInternal(participant)},
			{name: 'audio content hint', run: () => this.adapter.applyScreenShareAudioContentHintInternal(participant)},
			{name: 'end monitor', run: () => this.adapter.monitorActiveScreenShareEndInternal(room, participant)},
			{
				name: 'encoder verification',
				run: () =>
					this.adapter.startEncoderVerificationInternal(room, participant, effectivePublishOptions?.videoCodec),
			},
			{name: 'adaptive engine', run: () => AdaptiveScreenShareEngine.start(room)},
			{name: 'local stream state', run: () => applyScreenShareState(this.adapter, true, true, true)},
			{name: 'participant snapshot', run: () => updateLocalParticipantFromRoom(room)},
			{name: 'watch state', run: () => this.adapter.syncLocalStreamWatchStateInternal(true)},
			{name: 'audio state', run: () => this.adapter.syncLocalScreenShareAudioStateInternal(participant, true)},
			{
				name: 'persisted audio preference',
				run: () => this.adapter.syncPersistedScreenShareAudioPreferenceInternal(participant),
			},
		];
		for (const step of steps) {
			try {
				step.run();
			} catch (error) {
				logger.error('Failed to reconcile committed screen share runtime state', {
					error,
					nextContentSource,
					step: step.name,
				});
			}
		}
	}

	private async replaceActiveTracks(
		room: Room,
		participant: LocalParticipant,
		tracks: CapturedScreenShareTracks,
		options?: ScreenShareCaptureOptions,
		publishOptions?: TrackPublishOptions,
		contentSource?: ScreenShareContentSource,
	): Promise<boolean> {
		const screenSharePublication = participant.getTrackPublication(Track.Source.ScreenShare);
		const screenShareTrack = screenSharePublication?.videoTrack;
		if (!screenShareTrack) {
			stopMediaTrack(tracks.videoTrack);
			stopMediaTrack(tracks.audioTrack);
			logger.warn('No active screen share video track to replace');
			return false;
		}
		const nextContentSource = contentSource ?? 'display';
		const previousPublishOptions = {
			...(((screenSharePublication as {options?: TrackPublishOptions}).options ?? {}) as TrackPublishOptions),
		};
		let snapshot: ScreenShareReplacementSnapshot | null = null;
		let effectivePublishOptions: TrackPublishOptions | undefined;
		try {
			snapshot = this.captureScreenShareReplacementSnapshot(participant, screenShareTrack, previousPublishOptions);
			effectivePublishOptions = await this.adapter.getEffectivePublishOptionsInternal(true, publishOptions);
			await enforceLocalMediaPublicationCap(participant, VoiceTrackSource.ScreenShare);
		} catch (error) {
			stopMediaTrack(tracks.videoTrack);
			stopMediaTrack(tracks.audioTrack);
			throw error;
		}
		assert.ok(snapshot, 'screen share replacement snapshot must exist after preflight');
		if (tracks.videoTrack === snapshot.videoTrack) {
			stopMediaTrack(tracks.audioTrack);
			throw new Error('Screen share replacement returned the active source track as its candidate');
		}
		let audioStage: ScreenShareAudioReplacementStage = {kind: 'none'};
		let simulcastStage: ScreenShareSimulcastReplacementStage = [];
		let requestedAudioStageInProgress = false;
		this.adapter.cleanupActiveScreenShareEndListenerInternal();
		try {
			await screenShareTrack.stageTrackReplacement(tracks.videoTrack);
			if (nextContentSource === 'device') {
				const mirrorCamera = VoiceSettings.getMirrorCamera();
				const processor = await applyCameraMirrorProcessor(screenShareTrack, mirrorCamera);
				if (mirrorCamera && !processor) {
					throw new Error('Replacement device share could not apply its required mirror processor');
				}
				if (!mirrorCamera && screenShareTrack.getProcessor()) {
					throw new Error('Replacement device share could not clear its previous video processor');
				}
			} else if (screenShareTrack.getProcessor()) {
				await screenShareTrack.stopProcessor(false);
			}
			const activeVideoTrack = screenShareTrack.mediaStream?.getVideoTracks()[0] ?? screenShareTrack.mediaStreamTrack;
			if (activeVideoTrack !== tracks.videoTrack) {
				throw new Error('Replacement screen share video track did not take ownership of its publication');
			}
			if (activeVideoTrack.readyState !== 'live') {
				throw new Error('Replacement screen share video track ended before commit');
			}
			requestedAudioStageInProgress = tracks.displayCapture?.requireAudio === true;
			audioStage = await this.stageScreenShareAudioReplacement(participant, tracks);
			requestedAudioStageInProgress = false;
			if (audioStage.kind === 'candidate' && !isCurrentScreenShareAudioReplacementStage(participant, audioStage)) {
				throw new Error('Replacement screen share audio track ended before commit');
			}
			simulcastStage = await this.stageScreenShareSimulcastReplacement(screenShareTrack, tracks.videoTrack);
			await this.finalizeReplaceActiveTracks(
				room,
				participant,
				tracks,
				nextContentSource,
				options,
				effectivePublishOptions,
			);
			await screenShareTrack.commitStagedTrackReplacement(tracks.videoTrack, false);
		} catch (error) {
			try {
				await this.rollbackScreenShareSimulcastReplacement(simulcastStage);
				await this.restoreScreenShareReplacement(room, participant, screenShareTrack, snapshot, audioStage);
			} catch (rollbackError) {
				try {
					await this.failClosedScreenShareReplacement(participant, snapshot);
				} catch (cleanupError) {
					stopMediaTrack(tracks.videoTrack);
					stopMediaTrack(tracks.audioTrack);
					throw new ScreenShareRollbackIncompleteError([error, rollbackError, cleanupError]);
				}
				stopMediaTrack(tracks.videoTrack);
				stopMediaTrack(tracks.audioTrack);
				throw new AggregateError(
					[error, rollbackError],
					'Screen share replacement and rollback both failed; the share was stopped',
				);
			}
			stopMediaTrack(tracks.videoTrack);
			stopMediaTrack(tracks.audioTrack);
			if (
				requestedAudioStageInProgress &&
				!(error instanceof ScreenShareAudioCaptureError) &&
				!(error instanceof ScreenShareRollbackIncompleteError)
			) {
				throw new ScreenShareAudioCaptureError({
					sourceId: tracks.displayCapture?.sourceId,
					sourceKind: tracks.displayCapture?.displayShareEnvironment,
					reason: 'requested-audio-publication-failed',
					detail: error instanceof Error ? `${error.name}: ${error.message}` : 'Unknown audio replacement failure',
				});
			}
			throw error;
		}
		const committedCleanupSnapshot = this.adapter.getScreenShareCaptureCleanupSnapshotInternal(participant);
		let requestedAudioCommitInProgress = false;
		try {
			requestedAudioCommitInProgress = tracks.displayCapture?.requireAudio === true;
			await this.commitScreenShareAudioReplacement(
				participant,
				screenSharePublication,
				screenShareTrack,
				tracks.videoTrack,
				snapshot,
				audioStage,
			);
			requestedAudioCommitInProgress = false;
			this.commitScreenShareSimulcastReplacement(simulcastStage);
		} catch (error) {
			try {
				await this.failClosedScreenShareReplacement(participant, snapshot, committedCleanupSnapshot);
			} catch (cleanupError) {
				for (const entry of simulcastStage) stopMediaTrack(entry.nextTrack);
				stopMediaTrack(tracks.videoTrack);
				stopMediaTrack(tracks.audioTrack);
				throw new ScreenShareRollbackIncompleteError([error, cleanupError]);
			}
			for (const entry of simulcastStage) stopMediaTrack(entry.nextTrack);
			stopMediaTrack(tracks.videoTrack);
			stopMediaTrack(tracks.audioTrack);
			if (
				requestedAudioCommitInProgress &&
				!(error instanceof ScreenShareAudioCaptureError) &&
				!(error instanceof ScreenShareRollbackIncompleteError)
			) {
				throw new ScreenShareAudioCaptureError({
					sourceId: tracks.displayCapture?.sourceId,
					sourceKind: tracks.displayCapture?.displayShareEnvironment,
					reason: 'requested-audio-publication-failed',
					detail: error instanceof Error ? `${error.name}: ${error.message}` : 'Unknown audio replacement failure',
				});
			}
			if (error instanceof ScreenShareRollbackIncompleteError) throw error;
			throw new AggregateError([error], 'Committed screen share audio replacement failed; the share was stopped');
		}
		stopMediaTrack(snapshot.videoTrack);
		this.reconcileCommittedScreenShareReplacement(room, participant, nextContentSource, effectivePublishOptions);
		logger.info('Replaced active screen share source', {audioIncluded: tracks.audioTrack != null});
		return true;
	}

	private async stageScreenShareSimulcastReplacement(
		screenShareTrack: LocalVideoTrack,
		candidateTrack: MediaStreamTrack,
	): Promise<ScreenShareSimulcastReplacementStage> {
		const simulcastCodecs = (
			screenShareTrack as LocalVideoTrack & {
				simulcastCodecs?: Map<unknown, SimulcastTrackInfoLike>;
			}
		).simulcastCodecs;
		if (!simulcastCodecs?.size) {
			return [];
		}
		const stage: ScreenShareSimulcastReplacementStage = [];
		for (const simulcastTrackInfo of simulcastCodecs.values()) {
			const previousTrack = simulcastTrackInfo.mediaStreamTrack;
			const sender = simulcastTrackInfo.sender;
			const nextTrack = candidateTrack.clone();
			const entry = {info: simulcastTrackInfo, previousTrack, nextTrack, sender};
			stage.push(entry);
			try {
				if (sender && sender.track !== previousTrack) {
					throw new Error('Screen share simulcast sender does not own its recorded source track');
				}
				await sender?.replaceTrack(nextTrack);
			} catch (error) {
				try {
					await this.rollbackScreenShareSimulcastReplacement(stage);
				} catch (rollbackError) {
					throw new AggregateError(
						[error, rollbackError],
						'Screen share simulcast replacement and rollback both failed',
					);
				}
				throw error;
			}
		}
		return stage;
	}

	private async rollbackScreenShareSimulcastReplacement(stage: ScreenShareSimulcastReplacementStage): Promise<void> {
		const rollbackErrors: Array<unknown> = [];
		for (let index = stage.length - 1; index >= 0; index -= 1) {
			const entry = stage[index];
			if (!entry) continue;
			try {
				if (entry.sender?.track === entry.nextTrack) {
					await entry.sender.replaceTrack(entry.previousTrack);
				}
			} catch (error) {
				rollbackErrors.push(error);
			}
			stopMediaTrack(entry.nextTrack);
		}
		stage.length = 0;
		if (rollbackErrors.length > 0) {
			throw new AggregateError(rollbackErrors, 'Screen share simulcast rollback was incomplete');
		}
	}

	private commitScreenShareSimulcastReplacement(stage: ScreenShareSimulcastReplacementStage): void {
		for (const entry of stage) {
			if (entry.sender && entry.sender.track !== entry.nextTrack) {
				throw new Error('Screen share simulcast sender changed before replacement commit');
			}
		}
		for (const entry of stage) {
			entry.info.mediaStreamTrack = entry.nextTrack;
			stopMediaTrack(entry.previousTrack);
		}
		stage.length = 0;
	}

	private finalizeReconnectAlreadyEnabled(room: Room | null, participant: LocalParticipant): boolean {
		assert.ok(participant);
		assert.equal(participant.isScreenShareEnabled, true);
		this.adapter.ensureScreenShareKeepAliveSinkInternal(participant);
		applyScreenShareState(this.adapter, true, false);
		updateLocalParticipantFromRoom(room);
		this.adapter.transitionScreenShareLifecycleInternal({
			type: 'share.resolve',
			active: true,
			sourceType: this.adapter.getActiveScreenShareSourceTypeInternal(),
			encoderVerificationScheduled: this.adapter.encoderVerificationTimer != null,
			streamingPriorityHeld: this.adapter.streamingPriorityHeld,
		});
		return true;
	}

	private async restoreReconnectAudio(
		participant: LocalParticipant,
		snapshot: ScreenShareReconnectSnapshot,
	): Promise<boolean> {
		assert.ok(participant);
		assert.ok(snapshot);
		const audioTrack = snapshot.audioTrack;
		if (!audioTrack || audioTrack.readyState === 'ended') return false;
		try {
			prepareHighFidelityScreenShareAudioTrack(audioTrack);
			await participant.publishTrack(audioTrack, SCREEN_SHARE_AUDIO_PUBLISH_OPTIONS);
			const audioPublication = participant.getTrackPublication(Track.Source.ScreenShareAudio);
			if (snapshot.audioMuted) {
				await audioPublication?.mute();
			} else {
				await this.adapter.unmuteScreenShareAudioPublicationInternal(
					participant,
					'restore screen-share audio after reconnect',
				);
			}
			return true;
		} catch (error) {
			logger.warn('Failed to restore screen-share audio after reconnect; continuing video-only', {error});
			return false;
		}
	}

	private async finalizeRestoreReconnectSuccess(
		room: Room | null,
		participant: LocalParticipant,
		snapshot: ScreenShareReconnectSnapshot,
		effectivePublishOptions: TrackPublishOptions | undefined,
		audioPublished: boolean,
	): Promise<void> {
		assert.ok(participant);
		await runScreenShareActivationRitual({
			adapter: this.adapter,
			room,
			participant,
			active: true,
			steps: {
				acquireStreamingPriority: true,
				enforcePublicationCap: true,
				applyState: () => applyScreenShareState(this.adapter, true, false),
				applyStatePosition: 'before-pipeline',
				publishPipeline: {contentSource: snapshot.contentSource, effectivePublishOptions},
				deactivateCleanup: null,
				updateLocalParticipant: true,
				audioSync: {kind: 'participant-after-watch'},
				syncPersistedAudioPreferenceWhenActive: true,
				playSound: false,
				buildResolveTransition: () => ({
					type: 'share.resolve',
					active: true,
					sourceType: this.adapter.getScreenShareSourceTypeForContentSourceInternal(snapshot.contentSource),
					encoderVerificationScheduled: this.adapter.encoderVerificationTimer != null,
					streamingPriorityHeld: this.adapter.streamingPriorityHeld,
				}),
			},
		});
		logger.info('Restored screen share after voice reconnect', {audioPublished});
	}

	private handleRestoreReconnectFailure(
		room: Room | null,
		participant: LocalParticipant,
		snapshot: ScreenShareReconnectSnapshot,
		videoPublished: boolean,
		error: unknown,
	): boolean {
		assert.ok(participant);
		logger.warn('Failed to restore screen share after voice reconnect', {error, videoPublished});
		return settleScreenShareFailure({
			adapter: this.adapter,
			room,
			participant,
			actual: participant.isScreenShareEnabled,
			applyState: (actualNow) => applyScreenShareState(this.adapter, actualNow, false),
			onInactiveAfterSync: () => {
				stopMediaTrack(snapshot.videoTrack);
				stopMediaTrack(snapshot.audioTrack);
			},
			monitorEndOnActive: false,
			playSound: false,
			buildTransition: (actualNow) => ({
				type: 'share.reject',
				active: actualNow,
				sourceType: actualNow
					? this.adapter.getScreenShareSourceTypeForContentSourceInternal(snapshot.contentSource)
					: null,
			}),
		});
	}

	async restoreReconnect(
		room: Room | null,
		snapshot: ScreenShareReconnectSnapshot,
		publishOptions?: TrackPublishOptions,
	): Promise<boolean> {
		if (guardScreenShareEntry({platformUnsupportedWarning: SCREEN_SHARE_UNSUPPORTED_PLATFORM_WARNING}) !== 'proceed') {
			return false;
		}
		const participant = room?.localParticipant;
		if (!participant) {
			logger.warn('No participant');
			return false;
		}
		if (participant.isScreenShareEnabled) {
			await enforceLocalMediaPublicationCap(participant, VoiceTrackSource.ScreenShare);
			return this.finalizeReconnectAlreadyEnabled(room, participant);
		}
		const pendingVerdict = guardScreenShareEntry({
			pending: {
				active: this.adapter.isScreenSharePending,
				debugMessage: 'Already pending, ignoring screen share reconnect restore',
			},
		});
		if (pendingVerdict === 'share-pending') {
			return false;
		}
		if (snapshot.videoTrack.readyState === 'ended') {
			logger.warn('Cannot restore screen share reconnect from ended video track');
			return false;
		}
		this.adapter.transitionScreenShareLifecycleInternal({
			type: 'share.restore',
			sourceType: this.adapter.getScreenShareSourceTypeForContentSourceInternal(snapshot.contentSource),
		});
		let videoPublished = false;
		try {
			if (getLocalScreenSharePublications(participant).length > 0) {
				await this.adapter.cleanupLingeringScreenShareTracks(participant);
			}
			const effectivePublishOptions = await this.adapter.getEffectivePublishOptionsInternal(true, publishOptions);
			await participant.publishTrack(snapshot.videoTrack, {
				...effectivePublishOptions,
				source: Track.Source.ScreenShare,
				stream: VoiceTrackSource.ScreenShare,
			});
			videoPublished = true;
			const audioPublished = await this.restoreReconnectAudio(participant, snapshot);
			await this.finalizeRestoreReconnectSuccess(room, participant, snapshot, effectivePublishOptions, audioPublished);
			return true;
		} catch (error) {
			return this.handleRestoreReconnectFailure(room, participant, snapshot, videoPublished, error);
		}
	}
}

export type {LocalVoiceState};
