// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {isDesktop, isNativeMacOS} from '@app/features/ui/utils/NativeUtils';
import AdaptiveScreenShareEngine from '@app/features/voice/engine/AdaptiveScreenShareEngine';
import {updateLocalParticipantFromRoom} from '@app/features/voice/engine/VoiceMediaEngineBridge';
import {
	enforceLocalMediaPublicationCap,
	getLocalScreenSharePublications,
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
	getReplacementScreenShareSettingsOptions,
	logger,
	type ScreenShareCaptureCleanupSnapshot,
	type SimulcastTrackInfoLike,
	stopMediaTrack,
} from '@app/features/voice/engine/voice_screen_share_manager/shared';
import type LocalVoiceState from '@app/features/voice/state/LocalVoiceState';
import SoftwareEncoderWarning from '@app/features/voice/state/SoftwareEncoderWarning';
import {
	prepareHighFidelityScreenShareAudioTrack,
	SCREEN_SHARE_AUDIO_PUBLISH_OPTIONS,
} from '@app/features/voice/utils/AudioPublishOptions';
import type {ScreenShareContentSource} from '@app/features/voice/utils/CodecCapabilityDetector';
import {commitNativeAudioBridgeReplacement} from '@app/features/voice/utils/NativeAudioCaptureBridge';
import {applyCameraMirrorProcessor} from '@app/features/voice/utils/VideoBackgroundProcessor';
import {
	createLocalAudioTrack,
	createLocalVideoTrack,
	type LocalAudioTrack,
	type LocalParticipant,
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
	): void {
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
	}

	async replaceActiveDisplayShare(
		room: Room | null,
		options?: ScreenShareCaptureOptions,
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
			const tracks = await createDisplayScreenShareTracks(options);
			didReplace = await this.replaceActiveTracks(room, participant, tracks, options, publishOptions);
			this.emitReplaceShareResult(participant, 'display', didReplace);
		} catch (error) {
			this.handleReplaceShareFailure(participant, {kind: 'display'}, error);
		}
		await this.adapter.applyPendingScreenShareRequestsInternal(room, participant);
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

	private async swapScreenShareVideoTrack(
		screenShareTrack: LocalVideoTrack,
		tracks: CapturedScreenShareTracks,
		nextContentSource: ScreenShareContentSource,
	): Promise<boolean> {
		assert.ok(screenShareTrack);
		assert.ok(tracks);
		const previousVideoMediaTrack = screenShareTrack.mediaStreamTrack;
		try {
			await screenShareTrack.replaceTrack(tracks.videoTrack, false);
			if (nextContentSource === 'device') {
				await applyCameraMirrorProcessor(screenShareTrack);
			}
			await this.refreshSimulcastTracks(screenShareTrack);
		} catch (error) {
			stopMediaTrack(tracks.videoTrack);
			stopMediaTrack(tracks.audioTrack);
			logger.error('Failed to replace active screen share video track', {error});
			return false;
		}
		if (previousVideoMediaTrack && previousVideoMediaTrack !== screenShareTrack.mediaStreamTrack) {
			this.adapter.cleanupActiveScreenShareEndListenerInternal();
			stopMediaTrack(previousVideoMediaTrack);
		}
		return true;
	}

	private async swapScreenShareAudioTrack(
		participant: LocalParticipant,
		tracks: CapturedScreenShareTracks,
	): Promise<void> {
		assert.ok(participant);
		assert.ok(tracks);
		let audioTrackAdopted = false;
		try {
			audioTrackAdopted = await this.adapter.replaceActiveScreenShareAudioTrackInternal(participant, tracks.audioTrack);
			if (audioTrackAdopted && tracks.audioTrack) {
				commitNativeAudioBridgeReplacement();
			}
		} catch (error) {
			if (!audioTrackAdopted) stopMediaTrack(tracks.audioTrack);
			logger.warn('Failed to replace active screen share audio track', {error});
		}
	}

	private async finalizeReplaceActiveTracks(
		room: Room,
		participant: LocalParticipant,
		tracks: CapturedScreenShareTracks,
		nextContentSource: ScreenShareContentSource,
		options: ScreenShareCaptureOptions | undefined,
		publishOptions: TrackPublishOptions | undefined,
	): Promise<void> {
		assert.ok(participant);
		assert.ok(tracks);
		await enforceLocalMediaPublicationCap(participant, VoiceTrackSource.ScreenShare);
		const effectivePublishOptions = await this.adapter.getEffectivePublishOptionsInternal(true, publishOptions);
		await this.adapter.updateActiveScreenShareSettings(
			room,
			getReplacementScreenShareSettingsOptions(options, tracks.audioTrack != null),
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
		logger.info('Replaced active screen share source', {audioIncluded: tracks.audioTrack != null});
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
		const nextContentSource = contentSource ?? this.adapter.getActiveScreenShareContentSourceInternal();
		if (nextContentSource !== 'device' && screenShareTrack.getProcessor()) {
			await screenShareTrack.stopProcessor(false);
		}
		const videoSwapped = await this.swapScreenShareVideoTrack(screenShareTrack, tracks, nextContentSource);
		if (!videoSwapped) return false;
		await this.swapScreenShareAudioTrack(participant, tracks);
		await this.finalizeReplaceActiveTracks(room, participant, tracks, nextContentSource, options, publishOptions);
		return true;
	}

	private async refreshSimulcastTracks(screenShareTrack: LocalVideoTrack): Promise<void> {
		const simulcastCodecs = (
			screenShareTrack as LocalVideoTrack & {
				simulcastCodecs?: Map<unknown, SimulcastTrackInfoLike>;
			}
		).simulcastCodecs;
		if (!simulcastCodecs?.size) {
			return;
		}
		for (const simulcastTrackInfo of simulcastCodecs.values()) {
			const previousTrack = simulcastTrackInfo.mediaStreamTrack;
			let nextTrack: MediaStreamTrack | undefined;
			try {
				nextTrack = screenShareTrack.mediaStreamTrack.clone();
				await simulcastTrackInfo.sender?.replaceTrack(nextTrack);
				simulcastTrackInfo.mediaStreamTrack = nextTrack;
			} catch (error) {
				stopMediaTrack(nextTrack);
				try {
					await simulcastTrackInfo.sender?.replaceTrack(null);
				} catch (replaceError) {
					logger.warn('Failed to stop stale screen share simulcast track after replacement failed', {
						error: replaceError,
					});
				}
				logger.warn('Failed to replace active screen share simulcast track', {error});
			} finally {
				stopMediaTrack(previousTrack);
			}
		}
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
