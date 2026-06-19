// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import i18n from '@app/app/I18n';
import {GenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModal';
import {SoundType} from '@app/features/notification/utils/SoundUtils';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import * as SoundCommands from '@app/features/ui/commands/SoundCommands';
import {getElectronAPI} from '@app/features/ui/utils/NativeUtils';
import {getStreamKey} from '@app/features/voice/components/StreamKeys';
import AdaptiveScreenShareEngine from '@app/features/voice/engine/AdaptiveScreenShareEngine';
import type {NativeVoiceEngineLocalTrackPublication} from '@app/features/voice/engine/native_voice_engine/nativeVoiceEngineEventMapper';
import {markScreenShareCaptureEnded} from '@app/features/voice/engine/ScreenShareCaptureDiagnostics';
import type {NegotiationReason} from '@app/features/voice/engine/ScreenShareCodecNegotiation';
import ScreenShareCodecNegotiation from '@app/features/voice/engine/ScreenShareCodecNegotiation';
import {Store} from '@app/features/voice/engine/Store';
import {getVoiceConnectionContextFromMediaEngine} from '@app/features/voice/engine/VoiceMediaEngineBridge';
import {
	normalizeVoiceMediaGraphViewerStreamKeys,
	selectVoiceMediaGraphViewerStreamKeys,
} from '@app/features/voice/engine/VoiceMediaGraph';
import {voiceMediaGraphStore} from '@app/features/voice/engine/VoiceMediaGraphStore';
import {VoiceScreenShareLifecycleStore} from '@app/features/voice/engine/VoiceScreenShareLifecycleStore';
import type {
	VoiceScreenShareEvent,
	VoiceScreenShareSourceType,
} from '@app/features/voice/engine/VoiceScreenShareStateMachine';
import {addWatchedStreamKey, stopWatchingStreamKey} from '@app/features/voice/engine/VoiceStreamWatchState';
import {
	getLocalScreenSharePublications,
	getLocalScreenShareVideoPublications,
	unpublishLocalMediaPublications,
} from '@app/features/voice/engine/VoiceTrackPublicationUtils';
import {isVoiceEngineV2AppNativeScreenShareBridgeAvailable} from '@app/features/voice/engine/v2/VoiceEngineV2AppNativeBridge';
import {VoiceEngineV2AppScreenShareAudioPump} from '@app/features/voice/engine/v2/VoiceEngineV2AppScreenShareAudioPump';
import {VoiceEngineV2AppScreenShareCaptureCoordinator} from '@app/features/voice/engine/v2/VoiceEngineV2AppScreenShareCaptureCoordinator';
import {VoiceEngineV2AppScreenShareCodecMigration} from '@app/features/voice/engine/v2/VoiceEngineV2AppScreenShareCodecMigration';
import {
	selectVoiceEngineV2AppScreenShareSetEnabledOptions,
	type VoiceEngineV2AppScreenShareControllerGateway,
	VoiceEngineV2AppScreenShareControllerRouting,
	type VoiceEngineV2AppScreenShareSetEnabledSelection,
} from '@app/features/voice/engine/v2/VoiceEngineV2AppScreenShareControllerRouting';
import {VoiceEngineV2AppScreenShareLiveKitFlows} from '@app/features/voice/engine/v2/VoiceEngineV2AppScreenShareLiveKitFlows';
import {VoiceEngineV2AppScreenSharePreviewTracking} from '@app/features/voice/engine/v2/VoiceEngineV2AppScreenSharePreviewTracking';
import {
	type NativeScreenShareReconnectSnapshot,
	VoiceEngineV2AppScreenShareReconnect,
} from '@app/features/voice/engine/v2/VoiceEngineV2AppScreenShareReconnect';
import {
	applyVoiceEngineV2AppScreenShareState,
	type VoiceScreenShareStateOptions,
} from '@app/features/voice/engine/v2/VoiceEngineV2AppScreenShareStateSync';
import {VoiceEngineV2AppScreenShareTrackPlumbing} from '@app/features/voice/engine/v2/VoiceEngineV2AppScreenShareTrackPlumbing';
import {routeVoiceEngineV2AppSelectedMedia} from '@app/features/voice/engine/v2/VoiceEngineV2AppSelectedMediaMode';
import type {VoiceEngineV2AppSourceLifecycleBridge} from '@app/features/voice/engine/v2/VoiceEngineV2AppSourceLifecycleBridge';
import type {NativeScreenShareOptions} from '@app/features/voice/engine/voice_screen_share_manager/DisplayMediaCapture';
import type {NativeEngineAudioTrackFrame} from '@app/features/voice/engine/voice_screen_share_manager/NativeEngineAudioTrackPump';
import {
	captureScreenSharePublicationCleanup,
	type DeviceScreenShareCaptureOptions,
	logger,
	mergeScreenShareCaptureCleanupSnapshots,
	releaseScreenShareCaptureCleanup,
	getEffectivePublishOptions as resolveEffectivePublishOptions,
	type ScreenShareCaptureCleanupSnapshot,
	type ScreenShareCodecReadinessStatus,
	type SimulcastTrackInfoLike,
	scheduleScreenShareEncoderVerification,
	stopMediaTrack,
} from '@app/features/voice/engine/voice_screen_share_manager/shared';
import ActiveScreenShareSource from '@app/features/voice/state/ActiveScreenShareSource';
import LocalVoiceState from '@app/features/voice/state/LocalVoiceState';
import VoiceSettings from '@app/features/voice/state/VoiceSettings';
import {
	prepareHighFidelityScreenShareAudioTrack,
	SCREEN_SHARE_AUDIO_PUBLISH_OPTIONS,
} from '@app/features/voice/utils/AudioPublishOptions';
import {
	markScreenShareCodecEncodeRuntimeFailure,
	type ScreenShareContentSource,
} from '@app/features/voice/utils/CodecCapabilityDetector';
import {disarmVirtmic} from '@app/features/voice/utils/LinuxScreenShareAudio';
import {
	captureNativeAudioTrackForLinuxRouting,
	commitNativeAudioBridgeReplacement,
	disarmNativeAudio,
	type NativeAudioFramePump,
} from '@app/features/voice/utils/NativeAudioCaptureBridge';
import type {NativeAudioStartOptions} from '@app/types/electron.d';
import type {VoiceEngineV2ScreenOptions} from '@fluxer/voice_engine_v2';
import {msg} from '@lingui/core/macro';
import {
	type LocalAudioTrack,
	type LocalParticipant,
	type LocalTrackPublication,
	type LocalVideoTrack,
	type Room,
	type ScreenShareCaptureOptions,
	Track,
	type TrackPublishOptions,
	type VideoCodec,
} from 'livekit-client';

export type {NativeScreenShareReconnectSnapshot} from '@app/features/voice/engine/v2/VoiceEngineV2AppScreenShareReconnect';
export type {DeviceScreenShareCaptureOptions} from '@app/features/voice/engine/voice_screen_share_manager/shared';

const SCREEN_SHARE_ENDED_MODAL_KEY = 'voice-screen-share-ended';

const SCREEN_SHARE_ENDED_DESCRIPTOR = msg({
	message: 'Screen share ended',
	comment: 'Title of a modal shown when an active screen share stops unexpectedly.',
	context: 'screen-share',
});
const SCREEN_SHARE_SOURCE_STOPPED_DESCRIPTOR = msg({
	message: 'The screen share source stopped sending video, so your screen share was stopped.',
	comment:
		'Body of a modal shown when a browser screen share track ends outside the app, for example from the browser sharing controls.',
	context: 'screen-share',
});

export interface ScreenShareReconnectSnapshot {
	videoTrack: MediaStreamTrack;
	audioTrack?: MediaStreamTrack;
	audioMuted: boolean;
	contentSource: ScreenShareContentSource;
}

type ScreenShareSetEnabledSelection = VoiceEngineV2AppScreenShareSetEnabledSelection;

interface ScreenShareStartControls {
	sendUpdate: boolean;
	playSound: boolean;
}

function buildNativeScreenSharePublicationError(message: string): Error {
	const error = new Error(message);
	error.name = 'VoiceEngineV2AppNativeScreenSharePublicationError';
	return error;
}

const selectScreenShareSetEnabledOptions = selectVoiceEngineV2AppScreenShareSetEnabledOptions;

class VoiceEngineV2AppScreenShareExecutionAdapter extends Store {
	private readonly lifecycle: VoiceScreenShareLifecycleStore;
	private readonly trackPlumbing: VoiceEngineV2AppScreenShareTrackPlumbing;
	private activeScreenShareEndListener: (() => void) | null = null;
	private endedScreenShareStopInFlight: Promise<void> | null = null;
	nativeEngineScreenSharePreviewTrackSid: string | null = null;
	nativeEngineScreenSharePreviewStartToken = 0;
	nativeEngineScreenShareAudioPump: NativeAudioFramePump | null = null;
	encoderVerificationTimer: NodeJS.Timeout | null = null;
	screenShareMigrationGeneration = 0;
	nativeEngineScreenShareEndDisposer: (() => void) | null = null;
	nativeEngineScreenShareLifecycleBoundCaptureId: string | null = null;
	sourceLifecycleBridge: VoiceEngineV2AppSourceLifecycleBridge | null = null;

	readonly audioPump: VoiceEngineV2AppScreenShareAudioPump;
	readonly previewTracking: VoiceEngineV2AppScreenSharePreviewTracking;
	readonly captureCoordinator: VoiceEngineV2AppScreenShareCaptureCoordinator;
	readonly codecMigration: VoiceEngineV2AppScreenShareCodecMigration;
	readonly reconnect: VoiceEngineV2AppScreenShareReconnect;
	readonly liveKitFlows: VoiceEngineV2AppScreenShareLiveKitFlows;
	readonly controllerRouting: VoiceEngineV2AppScreenShareControllerRouting;

	constructor() {
		super();
		this.lifecycle = new VoiceScreenShareLifecycleStore({update: (fn) => this.update(fn)});
		this.trackPlumbing = new VoiceEngineV2AppScreenShareTrackPlumbing({
			getActiveContentSource: () => this.getActiveScreenShareContentSourceInternal(),
		});
		this.audioPump = new VoiceEngineV2AppScreenShareAudioPump(this);
		this.previewTracking = new VoiceEngineV2AppScreenSharePreviewTracking(this);
		this.captureCoordinator = new VoiceEngineV2AppScreenShareCaptureCoordinator(this);
		this.codecMigration = new VoiceEngineV2AppScreenShareCodecMigration(this);
		this.reconnect = new VoiceEngineV2AppScreenShareReconnect(this);
		this.liveKitFlows = new VoiceEngineV2AppScreenShareLiveKitFlows(this);
		this.controllerRouting = new VoiceEngineV2AppScreenShareControllerRouting(this);
	}

	setControllerGateway(gateway: VoiceEngineV2AppScreenShareControllerGateway | null): void {
		this.controllerRouting.setGateway(gateway);
	}

	get isScreenSharePending(): boolean {
		return this.lifecycle.pendingOperationActive;
	}

	get streamingPriorityHeld(): boolean {
		return this.lifecycle.streamingPriorityHeld;
	}

	getIsScreenSharePending(): boolean {
		return this.isScreenSharePending;
	}

	syncNativeEngineScreenSharePublishedTrackSidInternal(
		enabled: boolean,
		trackSid: string | undefined,
		publication?: NativeVoiceEngineLocalTrackPublication,
	): void {
		assert.equal(typeof enabled, 'boolean');
		if (enabled && trackSid) {
			this.captureCoordinator.recordPublishedTrackSid(trackSid, publication);
		}
		if (!enabled && (!trackSid || this.captureCoordinator.activeCapturePublishedTrackSid === trackSid)) {
			this.captureCoordinator.activeCapturePublishedTrackSid = null;
		}
	}

	adoptNativeEngineScreenShareOptionsInternal(nextOptions: NativeScreenShareOptions): void {
		assert.ok(nextOptions, 'nextOptions is required');
		assert.ok(nextOptions.source, 'nextOptions.source is required');
		this.captureCoordinator.activeCaptureOptions = nextOptions;
	}

	setSourceLifecycleBridge(bridge: VoiceEngineV2AppSourceLifecycleBridge | null): void {
		this.sourceLifecycleBridge = bridge;
	}

	hasActiveScreenShareResources(): boolean {
		return Boolean(
			this.captureCoordinator.activeCaptureId ||
				this.captureCoordinator.deviceCaptureActive ||
				this.nativeEngineScreenShareAudioPump ||
				this.nativeEngineScreenSharePreviewTrackSid,
		);
	}

	stopNativeScreenShareForTerminalUnload(): void {
		const captureId = this.captureCoordinator.activeCaptureId;
		this.captureCoordinator.activeCaptureId = null;
		this.captureCoordinator.activeCapturePublishedTrackSid = null;
		if (captureId) {
			try {
				void getElectronAPI()
					?.nativeScreenCapture?.stop(captureId)
					?.catch((error) => {
						logger.warn('Failed to stop native screen capture during terminal unload', {captureId, error});
					});
			} catch (error) {
				logger.warn('Failed to stop native screen capture during terminal unload', {captureId, error});
			}
			markScreenShareCaptureEnded('terminal-unload');
		}
		if (this.streamingPriorityHeld) {
			this.transitionScreenShareLifecycleInternal({type: 'share.streamingPriority.set', active: false});
			try {
				getElectronAPI()?.releaseStreamingPriority?.();
			} catch (error) {
				logger.warn('Failed to release streaming priority during terminal unload', {error});
			}
		}
	}

	setStreamingPriorityInternal(active: boolean): void {
		if (active === this.streamingPriorityHeld) return;
		this.transitionScreenShareLifecycleInternal({type: 'share.streamingPriority.set', active});
		try {
			const api = getElectronAPI();
			if (active) {
				api?.acquireStreamingPriority?.();
			} else {
				api?.releaseStreamingPriority?.();
			}
		} catch (error) {
			logger.warn('Failed to update native streaming priority', {active, error});
		}
	}

	ensureScreenShareKeepAliveSinkInternal(participant: LocalParticipant, preferredTrack?: LocalVideoTrack): void {
		this.trackPlumbing.ensureKeepAliveSink(participant, preferredTrack);
	}

	clearScreenShareKeepAliveSinkInternal(): void {
		this.trackPlumbing.clearKeepAliveSink();
	}

	isScreenShareCodecRepublishInFlight(): boolean {
		return this.lifecycle.codecRepublishInFlight;
	}

	transitionScreenShareLifecycleInternal(event: VoiceScreenShareEvent): void {
		this.lifecycle.transition(event);
	}

	private transitionScreenShareCodecReadiness(status: ScreenShareCodecReadinessStatus): void {
		this.transitionScreenShareLifecycleInternal({
			type:
				status === 'loading'
					? 'share.codecReadiness.loading'
					: status === 'ready'
						? 'share.codecReadiness.ready'
						: 'share.codecReadiness.timeout',
		});
	}

	async getEffectivePublishOptionsInternal(
		enabled: boolean,
		publishOptions?: TrackPublishOptions,
	): Promise<TrackPublishOptions | undefined> {
		return resolveEffectivePublishOptions(enabled, publishOptions, {
			onCodecReadiness: (status) => this.transitionScreenShareCodecReadiness(status),
		});
	}

	queuePendingStopRequestInternal(options?: {sendUpdate?: boolean; playSound?: boolean}): void {
		this.lifecycle.queueStopRequest(options);
	}

	queuePendingCodecRepublishRequestInternal(
		codec: VideoCodec,
		reason: NegotiationReason,
		options: {force?: boolean} = {},
	): void {
		this.lifecycle.queueCodecRepublishRequest(codec, reason, options);
	}

	deferActiveCodecRepublishRequestInternal(
		codec: VideoCodec,
		reason: NegotiationReason,
		options: {force?: boolean} = {},
	): void {
		this.lifecycle.deferCodecRepublishRequest(codec, reason, options);
	}

	async applyPendingScreenShareRequestsInternal(room: Room | null, participant: LocalParticipant): Promise<void> {
		await this.lifecycle.drainQueuedRequests({
			isScreenShareEnabled: () => participant.isScreenShareEnabled,
			applyStop: (request) => this.setScreenShareEnabled(room, false, request),
			applyCodecRepublish: async (request) => {
				await this.renegotiateActiveScreenShareCodec(room, request.codec, request.reason, {force: request.force});
			},
		});
	}

	getActiveScreenShareContentSourceInternal(): ScreenShareContentSource {
		const sourceId = ActiveScreenShareSource.getSourceId();
		if (sourceId?.startsWith('window:')) return 'app';
		return 'display';
	}

	getActiveScreenShareSourceTypeInternal(): VoiceScreenShareSourceType {
		const contentSource = this.getActiveScreenShareContentSourceInternal();
		if (contentSource === 'device') return 'device';
		return 'display';
	}

	getNativeScreenShareSourceType(nativeOptions: NativeScreenShareOptions): VoiceScreenShareSourceType {
		return nativeOptions.source.kind === 'window' ? 'native-app' : 'native-display';
	}

	getScreenShareSourceTypeForContentSourceInternal(
		contentSource: ScreenShareContentSource,
	): VoiceScreenShareSourceType {
		if (contentSource === 'device') return 'device';
		return 'display';
	}

	applyScreenShareContentHintInternal(
		participant: LocalParticipant,
		contentSource: ScreenShareContentSource = this.getActiveScreenShareContentSourceInternal(),
		preferredTrack?: LocalVideoTrack,
	): void {
		this.trackPlumbing.applyContentHint(participant, contentSource, preferredTrack);
	}

	applyScreenShareContentHintToMediaTrackInternal(
		mediaStreamTrack: MediaStreamTrack | undefined,
		contentSource: ScreenShareContentSource = this.getActiveScreenShareContentSourceInternal(),
	): void {
		this.trackPlumbing.applyContentHintToMediaTrack(mediaStreamTrack, contentSource);
	}

	async enforceScreenShareSenderParametersInternal(
		participant: LocalParticipant,
		publishOptions?: TrackPublishOptions,
	): Promise<void> {
		await this.trackPlumbing.enforceSenderParameters(participant, publishOptions);
	}

	async enforceScreenShareTrackSenderParametersInternal(
		track: LocalVideoTrack | undefined,
		publishOptions?: TrackPublishOptions,
	): Promise<void> {
		await this.trackPlumbing.enforceTrackSenderParameters(track, publishOptions);
	}

	bindScreenShareSenderParameterReapplyInternal(
		participant: LocalParticipant,
		publishOptions?: TrackPublishOptions,
		preferredTrack?: LocalVideoTrack,
	): void {
		this.trackPlumbing.bindSenderParameterReapply(participant, publishOptions, preferredTrack);
	}

	applyScreenShareAudioContentHintInternal(participant: LocalParticipant): void {
		this.trackPlumbing.applyAudioContentHint(participant);
	}

	private getLocalStreamKey(): string | null {
		const connection = getVoiceConnectionContextFromMediaEngine();
		const {guildId, channelId, connectionId} = connection ?? {};
		if (!connectionId) {
			logger.debug('Skipping local stream watcher sync without an active voice connection', {
				connectionState: connection,
			});
			return null;
		}
		return getStreamKey(guildId ?? null, channelId ?? null, connectionId);
	}

	applyScreenShareStateInternal(enabled: boolean, options: VoiceScreenShareStateOptions): void {
		applyVoiceEngineV2AppScreenShareState(enabled, options);
	}

	prepareScreenShareReconnect(room: Room | null): ScreenShareReconnectSnapshot | null {
		const participant = room?.localParticipant;
		if (!participant?.isScreenShareEnabled) {
			return null;
		}
		const screenSharePublication = participant.getTrackPublication(Track.Source.ScreenShare);
		const videoTrack = screenSharePublication?.videoTrack?.mediaStreamTrack;
		if (!videoTrack || videoTrack.readyState === 'ended') {
			return null;
		}
		const screenShareAudioPublication = participant.getTrackPublication(Track.Source.ScreenShareAudio);
		const audioTrack =
			screenShareAudioPublication?.audioTrack?.mediaStreamTrack ??
			(screenShareAudioPublication?.track as LocalAudioTrack | undefined)?.mediaStreamTrack;
		const liveAudioTrack = audioTrack && audioTrack.readyState !== 'ended' ? audioTrack : undefined;
		return {
			videoTrack,
			...(liveAudioTrack ? {audioTrack: liveAudioTrack} : {}),
			audioMuted: screenShareAudioPublication?.isMuted ?? false,
			contentSource: this.getActiveScreenShareContentSourceInternal(),
		};
	}

	syncLocalScreenShareAudioStateInternal(participant: LocalParticipant, enabled: boolean): void {
		const hasAudioTrack = Boolean(participant.getTrackPublication(Track.Source.ScreenShareAudio));
		LocalVoiceState.updateSelfStreamAudio(enabled && hasAudioTrack);
	}

	syncPersistedScreenShareAudioPreferenceInternal(participant: LocalParticipant): void {
		const screenShareAudioPublication = participant.getTrackPublication(Track.Source.ScreenShareAudio);
		const muteStreamAudio = !(screenShareAudioPublication && !screenShareAudioPublication.isMuted);
		if (VoiceSettings.getMuteStreamAudio() !== muteStreamAudio) {
			VoiceSettings.updateSettings({muteStreamAudio});
		}
	}

	async unmuteScreenShareAudioPublicationInternal(participant: LocalParticipant, reason: string): Promise<void> {
		const publication = participant.getTrackPublication(Track.Source.ScreenShareAudio);
		if (!publication || !publication.isMuted) {
			return;
		}
		try {
			await publication.unmute();
		} catch (error) {
			logger.warn('Failed to unmute screen-share audio publication', {error, reason});
		}
	}

	private cleanupScreenShareAudioRoutingState(): void {
		this.cleanupScreenShareAudioCaptureRouting();
		ActiveScreenShareSource.clear();
	}

	private cleanupScreenShareAudioCaptureRouting(): void {
		void getElectronAPI()
			?.virtmic?.stop()
			?.catch((error) => {
				logger.warn('Failed to stop virtmic during screen-share audio routing cleanup', {error});
			});
		disarmVirtmic();
		disarmNativeAudio();
	}

	getScreenShareCaptureCleanupSnapshotInternal(participant: LocalParticipant): ScreenShareCaptureCleanupSnapshot {
		return captureScreenSharePublicationCleanup(...getLocalScreenSharePublications(participant));
	}

	private async releaseScreenShareCapture(
		participant: LocalParticipant,
		snapshot?: ScreenShareCaptureCleanupSnapshot,
	): Promise<void> {
		await releaseScreenShareCaptureCleanup(
			mergeScreenShareCaptureCleanupSnapshots(snapshot, this.getScreenShareCaptureCleanupSnapshotInternal(participant)),
		);
	}

	getScreenShareSimulcastCleanupSnapshotInternal(screenShareTrack: LocalVideoTrack): ScreenShareCaptureCleanupSnapshot {
		const snapshot: ScreenShareCaptureCleanupSnapshot = {mediaTracks: [], senders: []};
		const simulcastCodecs = (
			screenShareTrack as LocalVideoTrack & {
				simulcastCodecs?: Map<unknown, SimulcastTrackInfoLike>;
			}
		).simulcastCodecs;
		for (const simulcastTrackInfo of simulcastCodecs?.values() ?? []) {
			snapshot.mediaTracks.push(simulcastTrackInfo.mediaStreamTrack);
			if (simulcastTrackInfo.sender) {
				snapshot.senders.push(simulcastTrackInfo.sender);
			}
		}
		return snapshot;
	}

	cleanupActiveScreenShareEndListenerInternal(): void {
		this.activeScreenShareEndListener?.();
		this.activeScreenShareEndListener = null;
	}

	cancelEncoderVerificationInternal(): void {
		if (this.encoderVerificationTimer != null) {
			clearTimeout(this.encoderVerificationTimer);
			this.encoderVerificationTimer = null;
		}
		this.trackPlumbing.cleanupKeyFrameRequests();
		this.transitionScreenShareLifecycleInternal({type: 'share.encoderVerification.cleared'});
	}

	showScreenShareEndedModalInternal(description: string): void {
		ModalCommands.pushWithKey(
			ModalCommands.modal(() => (
				<GenericErrorModal
					title={i18n._(SCREEN_SHARE_ENDED_DESCRIPTOR)}
					message={description}
					data-flx="voice.screen-share-manager.screen-share-ended-modal"
				/>
			)),
			SCREEN_SHARE_ENDED_MODAL_KEY,
		);
	}

	startEncoderVerificationInternal(
		room: Room | null,
		participant: LocalParticipant,
		codec?: VideoCodec,
		preferredTrack?: LocalVideoTrack,
	): void {
		this.cancelEncoderVerificationInternal();
		if (!codec) return;
		const publication = preferredTrack ? undefined : participant.getTrackPublication(Track.Source.ScreenShare);
		const track = preferredTrack ?? (publication?.videoTrack as LocalVideoTrack | undefined);
		const sender = track?.sender;
		if (!sender) {
			logger.warn('No sender found for screen share encoder verification');
			return;
		}
		this.encoderVerificationTimer = scheduleScreenShareEncoderVerification(
			() => sender.getStats(),
			codec,
			(failure) => {
				const failureReason =
					failure.reason === 'codec-mismatch' ? 'screen-share-codec-mismatch' : 'screen-share-encode-stalled';
				if (!markScreenShareCodecEncodeRuntimeFailure(failure.codec, failureReason)) return;
				void this.recoverActiveScreenShareAfterEncoderFailure(room, failure.codec).catch((error) => {
					logger.warn('Failed to recover screen share after encoder verification failure', {
						error,
						codec: failure.codec,
						failureReason,
					});
				});
			},
		);
		this.trackPlumbing.bindKeyFrameRequests(room, participant, track);
		this.transitionScreenShareLifecycleInternal({type: 'share.encoderVerification.scheduled'});
	}

	private async recoverActiveScreenShareAfterEncoderFailure(room: Room | null, failedCodec: VideoCodec): Promise<void> {
		await ScreenShareCodecNegotiation.publishLocalCapabilities(room, 'manual', {});
		const codec = ScreenShareCodecNegotiation.selectScreenShareCodec(VoiceSettings.getPreferredScreenShareCodec());
		if (codec === failedCodec) return;
		await this.renegotiateActiveScreenShareCodec(room, codec, 'manual', {force: true});
	}

	monitorActiveScreenShareEndInternal(
		room: Room | null,
		participant: LocalParticipant,
		preferredTrack?: LocalVideoTrack,
	): void {
		this.cleanupActiveScreenShareEndListenerInternal();
		const publication = preferredTrack ? undefined : participant.getTrackPublication(Track.Source.ScreenShare);
		const videoTrack = preferredTrack ?? publication?.videoTrack;
		const mediaStreamTrack = videoTrack?.mediaStreamTrack;
		if (!mediaStreamTrack) {
			return;
		}
		const isCurrentScreenShareTrack = (): boolean =>
			getLocalScreenShareVideoPublications(participant).some((screenSharePublication) => {
				const track =
					(screenSharePublication.videoTrack as LocalVideoTrack | undefined) ??
					(screenSharePublication.track as LocalVideoTrack | undefined);
				return track === videoTrack && track.mediaStreamTrack === mediaStreamTrack;
			});
		const stopEndedScreenShare = (trigger: string): void => {
			if (this.endedScreenShareStopInFlight) return;
			if (!isCurrentScreenShareTrack()) {
				return;
			}
			logger.info('Screen share media track ended; disabling screen share', {
				trigger,
				readyState: mediaStreamTrack.readyState,
			});
			this.showScreenShareEndedModalInternal(i18n._(SCREEN_SHARE_SOURCE_STOPPED_DESCRIPTOR));
			this.transitionScreenShareLifecycleInternal({type: 'share.endedStop.start'});
			const stopPromise = this.setScreenShareEnabled(room, false, {sendUpdate: true, playSound: true})
				.catch((error) => {
					logger.warn('Failed to disable screen share after media track ended', {error});
				})
				.finally(() => {
					if (this.endedScreenShareStopInFlight === stopPromise) {
						this.endedScreenShareStopInFlight = null;
					}
					this.transitionScreenShareLifecycleInternal({type: 'share.endedStop.finish'});
				});
			this.endedScreenShareStopInFlight = stopPromise;
		};
		const onEnded = (): void => stopEndedScreenShare('ended-event');
		mediaStreamTrack.addEventListener('ended', onEnded);
		this.activeScreenShareEndListener = () => {
			mediaStreamTrack.removeEventListener('ended', onEnded);
		};
		if (mediaStreamTrack.readyState === 'ended') {
			queueMicrotask(() => stopEndedScreenShare('already-ended'));
		}
	}

	async restoreScreenShareReconnect(
		room: Room | null,
		snapshot: ScreenShareReconnectSnapshot,
		publishOptions?: TrackPublishOptions,
	): Promise<boolean> {
		return routeVoiceEngineV2AppSelectedMedia({
			js: () => this.liveKitFlows.restoreReconnect(room, snapshot, publishOptions),
			native: async () => {
				throw buildNativeScreenSharePublicationError(
					'Native voice engine selected; refusing browser screen-share reconnect publication',
				);
			},
		});
	}

	async cleanupLingeringScreenShareTracks(
		participant: LocalParticipant,
		snapshot?: ScreenShareCaptureCleanupSnapshot,
	): Promise<void> {
		this.clearScreenShareKeepAliveSinkInternal();
		this.trackPlumbing.cleanupSenderParameterReapply();
		this.cleanupScreenShareAudioRoutingState();
		const publications = getLocalScreenSharePublications(participant);
		const cleanupSnapshot = mergeScreenShareCaptureCleanupSnapshots(
			snapshot,
			captureScreenSharePublicationCleanup(...publications),
		);
		const cleanupResult = await unpublishLocalMediaPublications(participant, publications);
		for (const failure of cleanupResult.failedPublications) {
			logger.warn('Failed to unpublish lingering screen share track', {
				error: failure.error,
				source: failure.publication.source,
			});
		}
		await this.releaseScreenShareCapture(participant, cleanupSnapshot);
	}

	handleLocalScreenShareTrackUnpublished(room: Room, playSound: boolean, publication?: LocalTrackPublication): void {
		this.clearScreenShareKeepAliveSinkInternal();
		this.cleanupActiveScreenShareEndListenerInternal();
		AdaptiveScreenShareEngine.stop();
		const participant = room.localParticipant;
		const cleanupSnapshot = captureScreenSharePublicationCleanup(
			publication,
			participant.getTrackPublication(Track.Source.ScreenShareAudio),
		);
		this.syncLocalStreamWatchStateInternal(false);
		this.syncLocalScreenShareAudioStateInternal(participant, false);
		if (!this.isScreenSharePending) {
			this.applyScreenShareStateInternal(false, {reason: 'user', sendUpdate: true});
		}
		void this.cleanupLingeringScreenShareTracks(participant, cleanupSnapshot).catch((error) => {
			logger.warn('Failed to clean up screen-share audio after video unpublish', {error});
		});
		if (playSound && !this.isScreenSharePending) {
			SoundCommands.playSound(SoundType.ScreenShareStop);
		}
	}

	syncLocalStreamWatchStateInternal(enabled: boolean): void {
		const streamKey = this.getLocalStreamKey();
		if (!streamKey) {
			return;
		}
		const graphKeys = selectVoiceMediaGraphViewerStreamKeys(voiceMediaGraphStore.getGraphSnapshot());
		const current =
			graphKeys.length > 0
				? graphKeys
				: normalizeVoiceMediaGraphViewerStreamKeys(LocalVoiceState.getViewerStreamKeys());
		this.transitionScreenShareLifecycleInternal({
			type: 'share.localWatcher.sync',
			enabled,
			streamKey,
			currentViewerStreamKeys: current,
		});
		for (const command of this.lifecycle.snapshot.context.watchCommands) {
			if (command.type === 'watch.add') {
				addWatchedStreamKey(command.key);
			} else {
				stopWatchingStreamKey(command.key, {clearPinned: false});
			}
		}
		this.transitionScreenShareLifecycleInternal({type: 'share.clearWatchCommands'});
		if (!enabled && !current.includes(streamKey)) {
			logger.debug('Local stream watcher already absent while disabling screen share', {
				current,
				expected: streamKey,
			});
		}
	}

	async replaceActiveScreenShareAudioTrackInternal(
		participant: LocalParticipant,
		audioTrack: MediaStreamTrack | undefined,
	): Promise<boolean> {
		const screenShareAudioPublication = participant.getTrackPublication(Track.Source.ScreenShareAudio);
		const existingAudioTrack =
			screenShareAudioPublication?.audioTrack ?? (screenShareAudioPublication?.track as LocalAudioTrack | undefined);
		const previousAudioMediaTrack = existingAudioTrack?.mediaStreamTrack;
		if (audioTrack) {
			prepareHighFidelityScreenShareAudioTrack(audioTrack);
			if (existingAudioTrack) {
				await existingAudioTrack.replaceTrack(audioTrack, false);
				if (previousAudioMediaTrack && previousAudioMediaTrack !== existingAudioTrack.mediaStreamTrack) {
					stopMediaTrack(previousAudioMediaTrack);
				}
				await this.unmuteScreenShareAudioPublicationInternal(participant, 'replace screen-share audio track');
				return true;
			}
			await participant.publishTrack(audioTrack, SCREEN_SHARE_AUDIO_PUBLISH_OPTIONS);
			await this.unmuteScreenShareAudioPublicationInternal(participant, 'publish screen-share audio track');
			return true;
		}
		this.cleanupScreenShareAudioCaptureRouting();
		if (existingAudioTrack) {
			await participant.unpublishTrack(existingAudioTrack);
			stopMediaTrack(previousAudioMediaTrack);
		}
		return false;
	}

	async ensureLinuxScreenShareAudioPublication(
		room: Room | null,
		linuxRule?: NonNullable<NativeAudioStartOptions['linuxRule']>,
		options: {includeSelfWindowAudio?: boolean; replaceExisting?: boolean} = {},
	): Promise<boolean> {
		const participant = room?.localParticipant;
		if (!participant || !participant.isScreenShareEnabled) return false;
		if (!linuxRule) return false;
		const capturedTrack = await captureNativeAudioTrackForLinuxRouting(linuxRule, options);
		if (!capturedTrack) return false;
		let adopted = false;
		try {
			adopted = await this.replaceActiveScreenShareAudioTrackInternal(participant, capturedTrack);
			if (adopted) {
				commitNativeAudioBridgeReplacement();
			}
		} catch (error) {
			logger.warn('Failed to publish mid-stream Linux native screen-share audio track', {error});
			if (!adopted) {
				try {
					capturedTrack.stop();
				} catch (stopError) {
					logger.warn('Failed to stop rejected Linux native screen-share audio track', {error: stopError});
				}
			}
			return false;
		}
		this.syncLocalScreenShareAudioStateInternal(participant, true);
		this.syncPersistedScreenShareAudioPreferenceInternal(participant);
		return true;
	}

	async setScreenShareEnabled(
		room: Room | null,
		enabled: boolean,
		options?: ScreenShareCaptureOptions & {
			sendUpdate?: boolean;
			playSound?: boolean;
			restartIfEnabled?: boolean;
			reason?: string;
		},
		publishOptions?: TrackPublishOptions,
	): Promise<void> {
		assert.equal(typeof enabled, 'boolean');
		await this.controllerRouting.setEnabled(room, enabled, options, publishOptions);
	}

	async executeScreenShareSetEnabledDirect(
		room: Room | null,
		enabled: boolean,
		options?: ScreenShareCaptureOptions & {
			sendUpdate?: boolean;
			playSound?: boolean;
			restartIfEnabled?: boolean;
			reason?: string;
		},
		publishOptions?: TrackPublishOptions,
	): Promise<void> {
		assert.equal(typeof enabled, 'boolean');
		const selection = selectScreenShareSetEnabledOptions(options);
		await routeVoiceEngineV2AppSelectedMedia({
			js: () =>
				this.liveKitFlows.setEnabled(
					room,
					enabled,
					{
						...selection.captureOptions,
						sendUpdate: selection.sendUpdate,
						playSound: selection.playSound,
						restartIfEnabled: selection.restartIfEnabled,
					},
					publishOptions,
				),
			native: () => this.setNativeSelectedScreenShareEnabled(enabled, selection, publishOptions),
		});
	}

	async startDeviceScreenShare(
		room: Room | null,
		options?: DeviceScreenShareCaptureOptions,
		publishOptions?: TrackPublishOptions,
	): Promise<void> {
		await routeVoiceEngineV2AppSelectedMedia({
			js: () => this.liveKitFlows.startDeviceScreenShare(room, options, publishOptions),
			native: async () => {
				const {sendUpdate = true, playSound = true} = options || {};
				await this.assertNativeScreenShareBridgeAvailable('device screen-share capture');
				await this.captureCoordinator.startDeviceCapture(options, {sendUpdate, playSound}, publishOptions);
			},
		});
	}

	async replaceActiveDisplayScreenShare(
		room: Room | null,
		options?: ScreenShareCaptureOptions,
		publishOptions?: TrackPublishOptions,
	): Promise<boolean> {
		return routeVoiceEngineV2AppSelectedMedia({
			js: () => this.liveKitFlows.replaceActiveDisplayShare(room, options, publishOptions),
			native: async () => {
				await this.assertNativeScreenShareBridgeAvailable('desktop screen-share source switch');
				return this.captureCoordinator.replaceActiveDisplayFromActiveSource(options, publishOptions);
			},
		});
	}

	async replaceActiveDeviceScreenShare(
		room: Room | null,
		options?: DeviceScreenShareCaptureOptions,
		publishOptions?: TrackPublishOptions,
	): Promise<boolean> {
		return routeVoiceEngineV2AppSelectedMedia({
			js: () => this.liveKitFlows.replaceActiveDeviceShare(room, options, publishOptions),
			native: async () => {
				await this.assertNativeScreenShareBridgeAvailable('device screen-share source switch');
				return this.captureCoordinator.replaceActiveDeviceCapture(options, publishOptions);
			},
		});
	}

	private async assertNativeScreenShareBridgeAvailable(context: string): Promise<void> {
		assert.equal(typeof context, 'string');
		if (await isVoiceEngineV2AppNativeScreenShareBridgeAvailable()) return;
		logger.error('Native voice engine selected without native screen-share bridge support', {context});
		throw buildNativeScreenSharePublicationError(
			`Native voice engine selected but native screen-share bridge is unavailable for ${context}`,
		);
	}

	private async setNativeSelectedScreenShareEnabled(
		enabled: boolean,
		selection: ScreenShareSetEnabledSelection,
		publishOptions?: TrackPublishOptions,
	): Promise<void> {
		assert.equal(typeof enabled, 'boolean');
		assert.ok(selection.captureOptions !== null && typeof selection.captureOptions === 'object');
		assert.equal(typeof selection.restartIfEnabled, 'boolean');
		if (!enabled) {
			await this.captureCoordinator.stopCapture({
				sendUpdate: selection.sendUpdate,
				playSound: selection.playSound,
				reason: selection.reason ?? 'native-engine-screen-share-disabled',
			});
			return;
		}
		await this.startNativeSelectedScreenShare(this.captureCoordinator.createCaptureId(), selection, publishOptions);
	}

	private async startNativeSelectedScreenShare(
		captureId: string,
		selection: ScreenShareSetEnabledSelection,
		publishOptions?: TrackPublishOptions,
	): Promise<void> {
		assert.ok(captureId.length > 0, 'native screen-share enable requires a captureId');
		assert.ok(selection.captureOptions !== null && typeof selection.captureOptions === 'object');
		const controls: ScreenShareStartControls = {
			sendUpdate: selection.sendUpdate,
			playSound: selection.playSound,
		};
		await this.assertNativeScreenShareBridgeAvailable('desktop screen-share capture');
		const nativeOptions = await this.captureCoordinator.resolveOptionsFromActiveSource(selection.captureOptions);
		if (!nativeOptions) {
			logger.warn('Native voice engine selected without a desktop source for screen-share enable');
			throw buildNativeScreenSharePublicationError(
				'Native voice engine selected but no desktop source is selected for screen-share capture',
			);
		}
		await this.captureCoordinator.startCapture({...nativeOptions, captureId}, controls, publishOptions);
	}

	async executeNativeControllerScreenSharePublish(
		captureId: string,
		options?: ScreenShareCaptureOptions & {
			sendUpdate?: boolean;
			playSound?: boolean;
			restartIfEnabled?: boolean;
			reason?: string;
		},
		publishOptions?: TrackPublishOptions,
	): Promise<void> {
		assert.ok(captureId.length > 0, 'native controller screen-share publish requires a captureId');
		await this.startNativeSelectedScreenShare(captureId, selectScreenShareSetEnabledOptions(options), publishOptions);
	}

	async applyPendingScreenShareRequestsForRoom(room: Room | null): Promise<void> {
		const participant = room?.localParticipant;
		if (!participant) return;
		await this.applyPendingScreenShareRequestsInternal(room, participant);
	}

	async releaseNativeEngineScreenShareResourcesInternal(args: {
		reason: string;
		preserveRestoreState?: boolean;
		releaseStreamingPriority?: boolean;
		unpublishRemote?: boolean;
		preserveStreamStateOnUnpublish?: boolean;
	}): Promise<void> {
		await this.captureCoordinator.releaseResources(args);
	}

	async startNativeEngineScreenShareInternal(
		nativeOptions: NativeScreenShareOptions,
		options?: {sendUpdate?: boolean; playSound?: boolean},
		publishOptions?: TrackPublishOptions,
	): Promise<void> {
		await this.captureCoordinator.startCapture(nativeOptions, options, publishOptions);
	}

	async stopNativeEngineScreenShareInternal(options?: {
		sendUpdate?: boolean;
		playSound?: boolean;
		reason?: string;
	}): Promise<void> {
		await this.captureCoordinator.stopCapture(options);
	}

	stopMediaTrackInternal(track: MediaStreamTrack | undefined): void {
		stopMediaTrack(track);
	}

	prepareNativeScreenShareReconnect(): NativeScreenShareReconnectSnapshot | null {
		return this.reconnect.prepare();
	}

	async releaseNativeScreenShareForReconnect(snapshot: NativeScreenShareReconnectSnapshot): Promise<void> {
		await this.reconnect.release(snapshot);
	}

	async restoreNativeScreenShareReconnect(snapshot: NativeScreenShareReconnectSnapshot): Promise<boolean> {
		return this.reconnect.restore(snapshot);
	}

	async startNativeDeviceScreenShare(
		options?: DeviceScreenShareCaptureOptions,
		startOptions?: {sendUpdate?: boolean; playSound?: boolean},
		publishOptions?: TrackPublishOptions,
	): Promise<void> {
		await this.captureCoordinator.startDeviceCapture(options, startOptions, publishOptions);
	}

	async startNativeDisplayScreenShare(
		room: Room | null,
		nativeOptions: NativeScreenShareOptions,
		options?: {sendUpdate?: boolean; playSound?: boolean},
		publishOptions?: TrackPublishOptions,
	): Promise<void> {
		await this.captureCoordinator.startDisplayCapture(room, nativeOptions, options, publishOptions);
	}

	async publishControllerScreenViaLiveKitFlows(room: Room | null, options: VoiceEngineV2ScreenOptions): Promise<void> {
		if (typeof options.captureId !== 'string' || options.captureId.length === 0) {
			throw new Error('Controller screen-share publish requires a captureId');
		}
		await this.controllerRouting.publishViaLiveKitFlows(room, options);
	}

	async unpublishControllerScreenViaLiveKitFlows(room: Room | null): Promise<void> {
		await this.controllerRouting.unpublishViaLiveKitFlows(room);
	}

	async publishControllerScreenViaNativeCapture(options: VoiceEngineV2ScreenOptions): Promise<void> {
		if (typeof options.captureId !== 'string' || options.captureId.length === 0) {
			throw new Error('Controller screen-share publish requires a captureId');
		}
		await this.controllerRouting.publishViaNativeCapture(options);
	}

	async unpublishControllerScreenViaNativeCapture(): Promise<void> {
		await this.controllerRouting.unpublishViaNativeCapture();
	}

	async replaceActiveNativeDisplayScreenShare(
		room: Room | null,
		nativeOptions: NativeScreenShareOptions,
		options?: ScreenShareCaptureOptions,
		publishOptions?: TrackPublishOptions,
	): Promise<boolean> {
		return this.captureCoordinator.replaceActiveDisplayCapture(room, nativeOptions, options, publishOptions);
	}

	async replaceActiveNativeDisplayScreenShareFromActiveSource(
		options?: ScreenShareCaptureOptions,
		publishOptions?: TrackPublishOptions,
	): Promise<boolean> {
		return this.captureCoordinator.replaceActiveDisplayFromActiveSource(options, publishOptions);
	}

	async replaceActiveNativeDeviceScreenShare(
		options?: DeviceScreenShareCaptureOptions,
		publishOptions?: TrackPublishOptions,
	): Promise<boolean> {
		return this.captureCoordinator.replaceActiveDeviceCapture(options, publishOptions);
	}

	async updateActiveScreenShareSettings(
		room: Room | null,
		options?: ScreenShareCaptureOptions,
		publishOptions?: TrackPublishOptions,
	): Promise<boolean> {
		return this.codecMigration.updateActiveSettings(room, options, publishOptions);
	}

	async renegotiateActiveScreenShareCodec(
		room: Room | null,
		codec: VideoCodec,
		reason: NegotiationReason,
		options: {force?: boolean} = {},
	): Promise<boolean> {
		return this.codecMigration.renegotiateActiveCodec(room, codec, reason, options);
	}

	setScreenShareAudioMuted(room: Room | null, muted: boolean): void {
		assert.equal(typeof muted, 'boolean');
		void routeVoiceEngineV2AppSelectedMedia({
			js: async () => {
				const participant = room?.localParticipant;
				if (!participant) return;
				const publication = participant.getTrackPublication(Track.Source.ScreenShareAudio);
				if (!publication) return;
				const operation = muted ? publication.mute() : publication.unmute();
				operation.catch((error) => {
					logger.warn('Failed to apply immediate screen share audio mute', {error, muted});
				});
				this.syncLocalScreenShareAudioStateInternal(participant, !muted);
			},
			native: async () => {
				await this.captureCoordinator.updateActiveSettings(room, {audio: !muted}).catch((error) => {
					logger.warn('Failed to apply native-engine screen share audio mute', {error, muted});
				});
			},
		});
	}

	async toggleScreenShareFromKeybind(room: Room | null): Promise<void> {
		const current = LocalVoiceState.getSelfStream();
		await this.setScreenShareEnabled(room, !current);
	}

	resetStreamTracking(): void {
		void this.captureCoordinator
			.releaseResources({
				reason: 'native-engine-screen-share-reset',
			})
			.catch((error) => {
				logger.warn('Failed to release native-engine screen-share resources during reset', {error});
			});
		this.clearScreenShareKeepAliveSinkInternal();
		this.setStreamingPriorityInternal(false);
		this.transitionScreenShareLifecycleInternal({type: 'share.reset'});
		this.cleanupActiveScreenShareEndListenerInternal();
		this.cancelEncoderVerificationInternal();
		AdaptiveScreenShareEngine.stop();
		this.endedScreenShareStopInFlight = null;
		this.cleanupScreenShareAudioRoutingState();
	}
}

export {VoiceEngineV2AppScreenShareExecutionAdapter};

export default new VoiceEngineV2AppScreenShareExecutionAdapter();

export type {NativeEngineAudioTrackFrame};
