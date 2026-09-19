// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import i18n from '@app/app/I18n';
import {GenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModal';
import Authentication from '@app/features/auth/state/Authentication';
import {SoundType} from '@app/features/notification/utils/SoundUtils';
import {Platform} from '@app/features/platform/types/Platform';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import * as SoundCommands from '@app/features/ui/commands/SoundCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {getElectronAPI} from '@app/features/ui/utils/NativeUtils';
import * as VoiceSettingsCommands from '@app/features/voice/commands/VoiceSettingsCommands';
import {countStreamViewers, getStreamKey} from '@app/features/voice/components/StreamKeys';
import ScreenShareCodecNegotiation, {
	getScreenShareCodecPreferenceOrder,
} from '@app/features/voice/engine/ScreenShareCodecNegotiation';
import {
	clampScreenShareLevelIndex,
	createScreenShareDeliveryState,
	describeScreenShareDelivery,
	evaluateScreenShareDelivery,
	resolveScreenShareDeliveryNoticeTone,
	type ScreenShareDeliveryEvaluation,
	type ScreenShareDeliveryNotice,
	type ScreenShareDeliverySample,
	type ScreenShareDeliveryState,
} from '@app/features/voice/engine/ScreenShareUnderperformance';
import {Store} from '@app/features/voice/engine/Store';
import {
	getAllVoiceStatesFromMediaEngine,
	getVoiceConnectionContextFromMediaEngine,
	updateLocalParticipantFromRoom,
} from '@app/features/voice/engine/VoiceMediaEngineBridge';
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
import {
	selectVoiceEngineV2AppScreenShareSetEnabledOptions,
	type VoiceEngineV2AppScreenShareControllerGateway,
	VoiceEngineV2AppScreenShareControllerRouting,
} from '@app/features/voice/engine/v2/VoiceEngineV2AppScreenShareControllerRouting';
import {VoiceEngineV2AppScreenShareLiveKitFlows} from '@app/features/voice/engine/v2/VoiceEngineV2AppScreenShareLiveKitFlows';
import {
	applyVoiceEngineV2AppScreenShareState,
	type VoiceScreenShareStateOptions,
} from '@app/features/voice/engine/v2/VoiceEngineV2AppScreenShareStateSync';
import {VoiceEngineV2AppScreenShareTrackPlumbing} from '@app/features/voice/engine/v2/VoiceEngineV2AppScreenShareTrackPlumbing';
import type {VoiceEngineV2AppSourceLifecycleBridge} from '@app/features/voice/engine/v2/VoiceEngineV2AppSourceLifecycleBridge';
import {ensureNativeMicrophonePermissionForDeviceShare} from '@app/features/voice/engine/voice_screen_share_manager/NativePermissionGate';
import {
	captureScreenSharePublicationCleanup,
	type DeviceScreenShareCaptureOptions,
	type DisplayScreenShareCaptureContext,
	getScreenShareLayeringForCodec,
	logger,
	mergeScreenShareCaptureCleanupSnapshots,
	releaseScreenShareCaptureCleanup,
	getEffectivePublishOptions as resolveEffectivePublishOptions,
	type ScreenShareCaptureCleanupSnapshot,
	type ScreenShareCodecReadinessStatus,
	startScreenShareDeliveryMonitor,
	stopMediaTrack,
} from '@app/features/voice/engine/voice_screen_share_manager/shared';
import ActiveScreenShareSource, {
	type PublishedScreenShareSource,
} from '@app/features/voice/state/ActiveScreenShareSource';
import LocalVoiceState from '@app/features/voice/state/LocalVoiceState';
import ScreenShareDelivery, {
	type ScreenShareDeliveryLadder,
	type ScreenShareDeliverySnapshot,
} from '@app/features/voice/state/ScreenShareDelivery';
import VoiceSettings from '@app/features/voice/state/VoiceSettings';
import {
	prepareHighFidelityScreenShareAudioTrack,
	SCREEN_SHARE_AUDIO_PUBLISH_OPTIONS,
} from '@app/features/voice/utils/AudioPublishOptions';
import {
	buildScreenShareCodecProfile,
	getVideoPublishCodecDenial,
	markScreenShareCodecEncodeRuntimeFailure,
	resolveScreenShareEncoderVerificationAction,
	type ScreenShareContentSource,
	type ScreenShareEncoderVerificationAction,
} from '@app/features/voice/utils/CodecCapabilityDetector';
import {disarmVirtmic} from '@app/features/voice/utils/LinuxScreenShareAudio';
import {
	captureNativeAudioTrackForLinuxRouting,
	captureNativeAudioTrackForWindowPid,
	commitNativeAudioBridgeReplacement,
	disarmNativeAudio,
	reconfigureLinuxNativeAudioRouting,
} from '@app/features/voice/utils/NativeAudioCaptureBridge';
import {
	rankScreenShareCodecs,
	resolveScreenShareAlternativeCodec,
	resolveScreenShareCodecSubstitution,
} from '@app/features/voice/utils/ScreenShareCodecSelection';
import {
	recordScreenShareDeliveryDecision,
	recordScreenShareEncoderVerification,
	recordScreenShareEndedModal,
	recordScreenShareRequestedCodec,
	recordScreenShareStartError,
	recordScreenShareStarted,
	recordScreenShareStopped,
	type ScreenShareEndedModal,
} from '@app/features/voice/utils/ScreenShareLifecycleLog';
import {SCREEN_SHARE_DEGRADATION_PREFERENCE, type ScreenShareLevel} from '@app/features/voice/utils/ScreenShareOptions';
import {ScreenShareRollbackIncompleteError} from '@app/features/voice/utils/ScreenShareRollbackIncompleteError';
import {handleScreenShareError} from '@app/features/voice/utils/ScreenShareUtils';
import {formatScreenShareDeliveryNotice} from '@app/features/voice/utils/VoiceMessageDescriptors';
import type {NativeAudioStartOptions} from '@app/types/electron.d';
import type {VoiceEngineV2ScreenOptions} from '@fluxer/voice_engine_v2';
import {msg} from '@lingui/core/macro';
import {
	createLocalAudioTrack,
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
const SCREEN_SHARE_CODEC_POLICY_FAILED_DESCRIPTOR = msg({
	message:
		'Your screen share could not be published with a compatible video codec, so it was stopped. Try sharing again.',
	comment:
		'Body of a modal shown when the video codec the browser negotiated for an active screen share is outside the codecs the app allows and no allowed codec could be published, which forces the share to stop.',
	context: 'screen-share',
});
const SCREEN_SHARE_ENCODER_FAILED_DESCRIPTOR = msg({
	message: 'Your video encoder stopped encoding frames, so your screen share was stopped. Try sharing again.',
	comment:
		'Body of a modal shown when the local video encoder produces no frames for an active screen share, which forces the share to stop.',
	context: 'screen-share',
});

type ScreenShareVideoConstraints = MediaTrackConstraints & {
	colorSpace?: string;
	cursor?: 'always' | 'motion' | 'never';
};

export interface ScreenShareSourceSnapshot {
	publishedSource: PublishedScreenShareSource | null;
	sourceId: string | null;
	isOwnWindow: boolean;
	sourceDimensions: {width: number; height: number} | null;
}

export interface ScreenShareReconnectSnapshot {
	videoTrack: MediaStreamTrack;
	audioTrack?: MediaStreamTrack;
	audioMuted: boolean;
	contentSource: ScreenShareContentSource;
	source: ScreenShareSourceSnapshot;
	delivery: ScreenShareDeliverySnapshot;
}

export function shouldRestoreScreenShareAfterReconnect(
	restoreStream: boolean,
	snapshot: ScreenShareReconnectSnapshot | null,
): boolean {
	return restoreStream || snapshot !== null;
}

interface ScreenShareDeliveryMonitor {
	readonly room: Room | null;
	readonly participant: LocalParticipant;
	readonly track: LocalVideoTrack;
	readonly codec: VideoCodec;
	readonly adaptive: boolean;
	ladder: ScreenShareDeliveryLadder;
	state: ScreenShareDeliveryState;
	probeBlocked: boolean;
	monitoredMaxBitrate: number;
	multiLayer: boolean;
}

const selectScreenShareSetEnabledOptions = selectVoiceEngineV2AppScreenShareSetEnabledOptions;
const SCREEN_SHARE_VERIFIED_CODEC_CORRECTION_MAX = 1;

function buildStartedLowerNotice(
	levels: ReadonlyArray<ScreenShareLevel>,
	levelIndex: number,
): ScreenShareDeliveryNotice {
	const level = levels[levelIndex];
	const target = levels[0];
	return {
		kind: 'started-lower',
		width: level.width,
		height: level.height,
		frameRate: level.frameRate,
		targetWidth: target.width,
		targetHeight: target.height,
		targetFrameRate: target.frameRate,
	};
}

class VoiceEngineV2AppScreenShareExecutionAdapter extends Store {
	private readonly lifecycle: VoiceScreenShareLifecycleStore;
	private readonly trackPlumbing: VoiceEngineV2AppScreenShareTrackPlumbing;
	private activeScreenShareEndListener: (() => void) | null = null;
	private endedScreenShareStopInFlight: Promise<void> | null = null;
	encoderVerificationTimer: (() => void) | null = null;
	private deliveryMonitor: ScreenShareDeliveryMonitor | null = null;
	private applyFailedSender: RTCRtpSender | null = null;
	private screenShareTrackingHolds = 0;
	private screenShareTrackingGeneration = 0;
	private readonly reconciledCodecPairs = new Set<string>();
	private readonly verifiedCodecCorrectionsByTrack = new WeakMap<MediaStreamTrack, number>();
	sourceLifecycleBridge: VoiceEngineV2AppSourceLifecycleBridge | null = null;

	readonly liveKitFlows: VoiceEngineV2AppScreenShareLiveKitFlows;
	readonly controllerRouting: VoiceEngineV2AppScreenShareControllerRouting;

	constructor() {
		super();
		this.lifecycle = new VoiceScreenShareLifecycleStore({update: (fn) => this.update(fn)});
		this.trackPlumbing = new VoiceEngineV2AppScreenShareTrackPlumbing({
			getActiveContentSource: () => this.getActiveScreenShareContentSourceInternal(),
			getActiveLevel: () => this.getActiveScreenShareLevelInternal(),
			noteSenderParametersApplied: (track, applied) => this.noteScreenShareApplyResultInternal(track, applied),
		});
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

	setSourceLifecycleBridge(bridge: VoiceEngineV2AppSourceLifecycleBridge | null): void {
		this.sourceLifecycleBridge = bridge;
	}

	stopNativeScreenShareForTerminalUnload(): void {
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

	isScreenSharePublicationReplaceInFlight(): boolean {
		return this.lifecycle.publicationReplaceInFlight;
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

	async applyPendingScreenShareRequestsInternal(room: Room | null, participant: LocalParticipant): Promise<void> {
		await this.lifecycle.drainQueuedRequests({
			isScreenShareEnabled: () => participant.isScreenShareEnabled,
			applyStop: (request) => this.setScreenShareEnabled(room, false, request),
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

	async enforceScreenShareSenderParametersInternal(
		participant: LocalParticipant,
		publishOptions?: TrackPublishOptions,
	): Promise<boolean> {
		return this.trackPlumbing.enforceSenderParameters(participant, publishOptions?.videoCodec);
	}

	private syncMonitoredScreenShareLadderInternal(monitor: ScreenShareDeliveryMonitor): ScreenShareDeliveryLadder {
		const committed = ScreenShareDelivery.ladder;
		if (committed !== null) monitor.ladder = committed;
		return monitor.ladder;
	}

	private getActiveScreenShareLevelInternal(): ScreenShareLevel | null {
		const monitor = this.deliveryMonitor;
		if (monitor !== null) {
			const {levels} = this.syncMonitoredScreenShareLadderInternal(monitor);
			return levels[clampScreenShareLevelIndex(monitor.state.levelIndex, levels.length)];
		}
		const ladder = ScreenShareDelivery.ladder;
		return ladder === null ? null : ladder.levels[0];
	}

	private noteScreenShareApplyResultInternal(track: LocalVideoTrack, applied: boolean): void {
		if (applied || !track.sender) return;
		this.applyFailedSender = track.sender;
	}

	private isScreenShareApplyFailedInternal(track: LocalVideoTrack): boolean {
		return track.sender !== undefined && track.sender === this.applyFailedSender;
	}

	private toastScreenShareDeliveryNoticeInternal(notice: ScreenShareDeliveryNotice): void {
		ToastCommands.createToast({
			type: resolveScreenShareDeliveryNoticeTone(notice) === 'warning' ? 'error' : 'info',
			children: formatScreenShareDeliveryNotice(i18n, notice),
		});
	}

	noteScreenShareCodecRecoveryInternal(previousCodec: VideoCodec | undefined, codec: VideoCodec): void {
		if (previousCodec === undefined || previousCodec === codec) return;
		if (getVideoPublishCodecDenial(previousCodec) !== 'runtime-failed') return;
		this.setScreenShareDeliveryNoticeInternal({kind: 'encoder-recovered', codec});
	}

	private setScreenShareDeliveryNoticeInternal(notice: ScreenShareDeliveryNotice | null): void {
		if (!ScreenShareDelivery.adaptive) return;
		if (this.deliveryMonitor) {
			this.deliveryMonitor.state.notice = notice;
		}
		if (ScreenShareDelivery.setNotice(notice) && notice !== null) {
			this.toastScreenShareDeliveryNoticeInternal(notice);
		}
	}

	private restartScreenShareDeliveryPlanInternal(): void {
		this.applyFailedSender = null;
		this.setScreenShareDeliveryNoticeInternal(null);
		const monitor = this.deliveryMonitor;
		if (!monitor) return;
		monitor.state = {...createScreenShareDeliveryState(), ticks: monitor.state.ticks};
	}

	async resetScreenShareDeliveryToFullQuality(): Promise<void> {
		ScreenShareDelivery.resetToFullQuality();
		this.restartScreenShareDeliveryPlanInternal();
		const monitor = this.deliveryMonitor;
		if (!monitor) return;
		monitor.probeBlocked = false;
		await this.trackPlumbing.enforceTrackSenderParameters(monitor.track, monitor.codec);
	}

	applyScreenShareAudioContentHintInternal(participant: LocalParticipant): void {
		this.trackPlumbing.applyAudioContentHint(participant);
	}

	private countScreenShareViewersInternal(): number {
		const streamKey = this.getLocalStreamKey();
		if (streamKey === null) return 0;
		return countStreamViewers(getAllVoiceStatesFromMediaEngine(), streamKey, Authentication.currentUserId);
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
		const delivery = ScreenShareDelivery.snapshot();
		return {
			videoTrack,
			...(liveAudioTrack ? {audioTrack: liveAudioTrack} : {}),
			audioMuted: screenShareAudioPublication?.isMuted ?? false,
			contentSource: this.getActiveScreenShareContentSourceInternal(),
			source: {
				publishedSource: ActiveScreenShareSource.getPublishedSource(),
				sourceId: ActiveScreenShareSource.getSourceId(),
				isOwnWindow: ActiveScreenShareSource.isOwnWindow(),
				sourceDimensions: ActiveScreenShareSource.getSourceDimensions(),
			},
			delivery: delivery.notice?.kind === 'apply-failed' ? {...delivery, notice: null} : delivery,
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
		if (!publication?.isMuted) {
			return;
		}
		try {
			await publication.unmute();
		} catch (error) {
			logger.warn('Failed to unmute screen-share audio publication', {error, reason});
		}
	}

	private endScreenShareTracking(): void {
		ActiveScreenShareSource.clear();
		ScreenShareDelivery.endShare();
		this.applyFailedSender = null;
		this.reconciledCodecPairs.clear();
	}

	private cleanupScreenShareAudioRoutingState(): void {
		this.cleanupScreenShareAudioCaptureRouting();
		if (this.screenShareTrackingHolds === 0) {
			this.endScreenShareTracking();
			return;
		}
		if (ScreenShareDelivery.notice?.kind === 'apply-failed') {
			this.setScreenShareDeliveryNoticeInternal(null);
		}
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

	cleanupActiveScreenShareEndListenerInternal(): void {
		this.activeScreenShareEndListener?.();
		this.activeScreenShareEndListener = null;
	}

	cancelEncoderVerificationInternal(): void {
		this.encoderVerificationTimer?.();
		this.encoderVerificationTimer = null;
		this.deliveryMonitor = null;
		this.trackPlumbing.cleanupKeyFrameRequests();
		this.transitionScreenShareLifecycleInternal({type: 'share.encoderVerification.cleared'});
	}

	showScreenShareEndedModalInternal(description: string, modal: ScreenShareEndedModal): void {
		recordScreenShareEndedModal(modal);
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

	private isScreenShareTrackPublishedInternal(participant: LocalParticipant, track: LocalVideoTrack): boolean {
		return getLocalScreenShareVideoPublications(participant).some((publication) => {
			const publishedTrack =
				(publication.videoTrack as LocalVideoTrack | undefined) ?? (publication.track as LocalVideoTrack | undefined);
			return publishedTrack === track;
		});
	}

	private async recoverActiveScreenShareAfterEncoderFailure(
		room: Room | null,
		participant: LocalParticipant,
		track: LocalVideoTrack,
		failedCodec: VideoCodec,
	): Promise<void> {
		if (!this.isScreenShareTrackPublishedInternal(participant, track)) return;
		await ScreenShareCodecNegotiation.publishLocalCapabilities(room, 'manual');
		if (this.isScreenSharePending || !this.isScreenShareTrackPublishedInternal(participant, track)) return;
		const codec = ScreenShareCodecNegotiation.selectScreenShareCodec(VoiceSettings.getPreferredScreenShareCodec());
		if (codec !== failedCodec && (await this.liveKitFlows.republishActiveShareWithCodec(room, track, codec))) return;
		this.stopScreenShareAfterEncoderFailure(room, participant, track, failedCodec, 'stalled');
	}

	async reconcileScreenShareCodecInternal(room: Room | null): Promise<void> {
		const participant = room?.localParticipant;
		if (!room || !participant) return;
		if (this.isScreenSharePending || this.isScreenSharePublicationReplaceInFlight()) return;
		const publication = participant.getTrackPublication(Track.Source.ScreenShare);
		const track = publication?.videoTrack as LocalVideoTrack | undefined;
		if (!track || !this.isScreenShareTrackPublishedInternal(participant, track)) return;
		const published = (publication as {options?: TrackPublishOptions} | undefined)?.options?.videoCodec;
		const selected = ScreenShareCodecNegotiation.getSelectedCodec();
		if (!published || !selected || published === selected) return;
		const pair = `${published}|${selected}`;
		if (this.reconciledCodecPairs.has(pair)) return;
		this.reconciledCodecPairs.add(pair);
		const republished = await this.liveKitFlows.republishActiveShareWithCodec(room, track, selected);
		if (!republished) {
			logger.warn('Failed to reconcile the active screen share codec with the negotiated codec', {
				published,
				selected,
			});
		}
	}

	async republishActiveScreenShareForNegotiatedCodecInternal(room: Room | null, codec: VideoCodec): Promise<void> {
		const participant = room?.localParticipant;
		if (!room || !participant) return;
		const publication = participant.getTrackPublication(Track.Source.ScreenShare);
		const track = publication?.videoTrack as LocalVideoTrack | undefined;
		if (!track || !this.isScreenShareTrackPublishedInternal(participant, track)) return;
		const publishedOptions = ((publication as {options?: TrackPublishOptions}).options ?? {}) as TrackPublishOptions;
		if (publishedOptions.videoCodec === codec) return;
		const republished = await this.liveKitFlows.republishActiveShareWithCodec(room, track, codec);
		if (!republished) {
			logger.warn('Failed to republish active screen share after a codec negotiation change', {
				codec,
				previousCodec: publishedOptions.videoCodec,
			});
		}
	}

	private async correctVerifiedScreenShareCodec(
		room: Room | null,
		participant: LocalParticipant,
		track: LocalVideoTrack,
		action: Extract<ScreenShareEncoderVerificationAction, {kind: 'correct-negotiated'}>,
	): Promise<void> {
		if (!this.isScreenShareTrackPublishedInternal(participant, track)) return;
		const mediaStreamTrack = track.mediaStreamTrack;
		const corrections = this.verifiedCodecCorrectionsByTrack.get(mediaStreamTrack) ?? 0;
		if (corrections < SCREEN_SHARE_VERIFIED_CODEC_CORRECTION_MAX && action.alternative) {
			this.verifiedCodecCorrectionsByTrack.set(mediaStreamTrack, corrections + 1);
			const recovered = await this.liveKitFlows.republishActiveShareWithCodec(room, track, action.alternative);
			if (recovered) return;
		}
		this.stopScreenShareAfterEncoderFailure(room, participant, track, action.requested, 'codec-policy');
	}

	private stopScreenShareAfterEncoderFailure(
		room: Room | null,
		participant: LocalParticipant,
		track: LocalVideoTrack,
		codec: VideoCodec,
		cause: 'stalled' | 'codec-policy',
	): void {
		if (cause === 'stalled') {
			logger.error('Screen share encoder produced no frames and no other codec took over; disabling screen share', {
				codec,
			});
		} else {
			logger.error('Screen share kept publishing a codec outside the publish policy; disabling screen share', {
				codec,
			});
		}
		if (this.endedScreenShareStopInFlight) return;
		if (!this.isScreenShareTrackPublishedInternal(participant, track)) return;
		this.showScreenShareEndedModalInternal(
			i18n._(
				cause === 'stalled' ? SCREEN_SHARE_ENCODER_FAILED_DESCRIPTOR : SCREEN_SHARE_CODEC_POLICY_FAILED_DESCRIPTOR,
			),
			cause === 'stalled' ? 'encoder-failed' : 'codec-policy-failed',
		);
		recordScreenShareStopped('codec-republish-failed');
		this.transitionScreenShareLifecycleInternal({type: 'share.endedStop.start'});
		const stopPromise = this.setScreenShareEnabled(room, false, {sendUpdate: true, playSound: true})
			.catch((error) => {
				logger.warn('Failed to disable screen share after encoder failure', {error, codec});
			})
			.finally(() => {
				if (this.endedScreenShareStopInFlight === stopPromise) {
					this.endedScreenShareStopInFlight = null;
				}
				this.transitionScreenShareLifecycleInternal({type: 'share.endedStop.finish'});
			});
		this.endedScreenShareStopInFlight = stopPromise;
	}

	startEncoderVerificationInternal(
		room: Room | null,
		participant: LocalParticipant,
		codec?: VideoCodec,
		preferredTrack?: LocalVideoTrack,
	): void {
		this.cancelEncoderVerificationInternal();
		if (!codec) return;
		recordScreenShareRequestedCodec(codec);
		const publication = preferredTrack ? undefined : participant.getTrackPublication(Track.Source.ScreenShare);
		const track = preferredTrack ?? (publication?.videoTrack as LocalVideoTrack | undefined);
		if (!track?.sender) {
			logger.warn('No sender found for screen share delivery monitoring');
			return;
		}
		const ladder = ScreenShareDelivery.ladder;
		if (ladder === null) {
			logger.warn('No delivery ladder found for screen share delivery monitoring');
			return;
		}
		const memory = ScreenShareDelivery.verdicts[codec];
		const startIndex = clampScreenShareLevelIndex(memory?.settledIndex ?? 0, ladder.levels.length);
		const state = createScreenShareDeliveryState(startIndex);
		state.notice = ScreenShareDelivery.notice;
		const monitor: ScreenShareDeliveryMonitor = {
			room,
			participant,
			track,
			codec,
			adaptive: ScreenShareDelivery.adaptive,
			ladder,
			state,
			probeBlocked: memory?.probeBlockedUntil != null && memory.probeBlockedUntil > Date.now(),
			monitoredMaxBitrate: ladder.levels[startIndex].maxBitrate,
			multiLayer: getScreenShareLayeringForCodec(
				codec,
				ladder.levels[0].width * ladder.levels[0].height,
				ScreenShareDelivery.multiLayer,
			).multiLayer,
		};
		this.deliveryMonitor = monitor;
		if (startIndex > 0) {
			this.setScreenShareDeliveryNoticeInternal(buildStartedLowerNotice(ladder.levels, startIndex));
			void this.trackPlumbing.enforceTrackSenderParameters(track, codec);
		} else {
			const substitution = this.resolveScreenShareSubstitutionNoticeInternal(codec);
			if (substitution) this.setScreenShareDeliveryNoticeInternal(substitution);
		}
		this.encoderVerificationTimer = startScreenShareDeliveryMonitor({
			track,
			codec,
			onEncodeFailure: (failure) => {
				monitor.state.ticks += 1;
				this.handleScreenShareEncoderVerificationAction(monitor, resolveScreenShareEncoderVerificationAction(failure));
			},
			onSample: (sample) => this.handleScreenShareDeliverySampleInternal(monitor, sample),
		});
		this.trackPlumbing.bindKeyFrameRequests(room, participant, track);
		this.transitionScreenShareLifecycleInternal({type: 'share.encoderVerification.scheduled'});
	}

	private resolveScreenShareSubstitutionNoticeInternal(codec: VideoCodec): ScreenShareDeliveryNotice | null {
		const pin = VoiceSettings.getPreferredScreenShareCodec();
		if (pin === 'auto') return null;
		const substitution = resolveScreenShareCodecSubstitution(pin, getScreenShareCodecPreferenceOrder(pin), codec);
		if (substitution === null) return null;
		return {kind: substitution === 'viewer' ? 'codec-viewer' : 'codec-device', codec, pinnedCodec: pin};
	}

	private resolveAlternativeScreenShareCodecInternal(codec: VideoCodec): VideoCodec | null {
		const {order} = rankScreenShareCodecs({
			profile: buildScreenShareCodecProfile(),
			encoderModeSetting: VoiceSettings.getScreenShareEncoderMode(),
			pin: VoiceSettings.getPreferredScreenShareCodec(),
			verdicts: {...ScreenShareDelivery.verdicts, [codec]: {short: true, ratio: 0}},
		});
		const {knownDecode, unknownParticipants} = ScreenShareCodecNegotiation.getRemoteDecodeInputs();
		return resolveScreenShareAlternativeCodec(order, codec, knownDecode, unknownParticipants);
	}

	private async handleScreenShareDeliverySampleInternal(
		monitor: ScreenShareDeliveryMonitor,
		sample: ScreenShareDeliverySample,
	): Promise<void> {
		if (this.deliveryMonitor !== monitor) return;
		if (!this.isScreenShareApplyFailedInternal(monitor.track)) {
			const enforcement = await this.trackPlumbing.enforceTrackSenderParameters(monitor.track, monitor.codec);
			if (this.deliveryMonitor !== monitor) return;
			monitor.monitoredMaxBitrate = enforcement.monitoredMaxBitrate;
			monitor.multiLayer = enforcement.multiLayer;
		}
		const ladder = this.syncMonitoredScreenShareLadderInternal(monitor);
		const levelZeroPixels = ladder.levels[0].width * ladder.levels[0].height;
		const counters = ScreenShareDelivery.counters;
		const shareTracked = ScreenShareDelivery.isSharing;
		const evaluation = evaluateScreenShareDelivery(monitor.state, sample, {
			adaptive: monitor.adaptive,
			levels: ladder.levels,
			keep: ladder.target.keep,
			context: ladder.context,
			codec: monitor.codec,
			pinned: VoiceSettings.getPreferredScreenShareCodec() === monitor.codec,
			alternativeCodec: this.resolveAlternativeScreenShareCodecInternal(monitor.codec),
			codecFailed: getVideoPublishCodecDenial(monitor.codec) === 'runtime-failed',
			lastApplyFailed: this.isScreenShareApplyFailedInternal(monitor.track),
			paused: this.isScreenSharePending || this.isScreenSharePublicationReplaceInFlight(),
			viewers: this.countScreenShareViewersInternal(),
			multiLayer: monitor.multiLayer,
			multiLayerAvailable: getScreenShareLayeringForCodec(monitor.codec, levelZeroPixels, true).multiLayer,
			monitoredMaxBitrate: monitor.monitoredMaxBitrate,
			shareCounters: shareTracked
				? {...counters, probeBlocked: counters.probeBlocked || monitor.probeBlocked}
				: {...counters, switches: Number.POSITIVE_INFINITY, probeBlocked: true},
		});
		const toast = ScreenShareDelivery.record(
			monitor.codec,
			evaluation,
			describeScreenShareDelivery(monitor.state, ladder.levels),
		);
		if (toast && evaluation.notice !== null) {
			this.toastScreenShareDeliveryNoticeInternal(evaluation.notice);
		}
		if (evaluation.decision.kind !== 'none' || evaluation.noticeChanged) {
			recordScreenShareDeliveryDecision({
				codec: monitor.codec,
				decision: evaluation.decision.kind,
				levelIndex: evaluation.levelIndex,
				notice: evaluation.notice?.kind ?? null,
				shortCause: evaluation.shortCause,
				toast,
			});
		}
		await this.applyScreenShareDeliveryDecisionInternal(monitor, evaluation);
	}

	private async applyScreenShareDeliveryDecisionInternal(
		monitor: ScreenShareDeliveryMonitor,
		evaluation: ScreenShareDeliveryEvaluation,
	): Promise<void> {
		switch (evaluation.decision.kind) {
			case 'none':
				return;
			case 'step':
			case 'probe':
			case 'revert-probe':
			case 'reapply-level':
				await this.trackPlumbing.enforceTrackSenderParameters(monitor.track, monitor.codec);
				return;
			case 'republish-layering': {
				const previous = ScreenShareDelivery.multiLayer;
				ScreenShareDelivery.setMultiLayer(evaluation.decision.multiLayer);
				const republished = await this.liveKitFlows.republishActiveShareWithCodec(
					monitor.room,
					monitor.track,
					monitor.codec,
				);
				if (!republished) ScreenShareDelivery.setMultiLayer(previous);
				return;
			}
			case 'switch-codec':
				ScreenShareDelivery.demoteForShare(
					monitor.codec,
					evaluation.memoryWrite?.kind === 'short' ? evaluation.memoryWrite.ratio : 0,
				);
				await ScreenShareCodecNegotiation.publishLocalCapabilities(monitor.room, 'manual');
				return;
			case 'recover-stalled':
				markScreenShareCodecEncodeRuntimeFailure(monitor.codec, 'screen-share-encode-stalled');
				this.handleScreenShareEncoderVerificationAction(monitor, {kind: 'recover-stalled', codec: monitor.codec});
				return;
			case 'stop-stalled':
				this.handleScreenShareEncoderVerificationAction(monitor, {kind: 'stop-stalled', codec: monitor.codec});
				return;
		}
	}

	private handleScreenShareEncoderVerificationAction(
		monitor: ScreenShareDeliveryMonitor,
		action: ScreenShareEncoderVerificationAction,
	): void {
		const {room, participant, track} = monitor;
		switch (action.kind) {
			case 'stop-stalled':
				recordScreenShareEncoderVerification('stop-stalled', null);
				this.stopScreenShareAfterEncoderFailure(room, participant, track, action.codec, 'stalled');
				return;
			case 'recover-stalled':
				recordScreenShareEncoderVerification('recover-stalled', null);
				logger.warn('Screen share encoder verification failed', {
					codec: action.codec,
					failureReason: 'screen-share-encode-stalled',
				});
				void this.recoverActiveScreenShareAfterEncoderFailure(room, participant, track, action.codec).catch((error) => {
					logger.warn('Failed to recover screen share after encoder verification failure', {
						error,
						codec: action.codec,
					});
				});
				return;
			case 'accept-negotiated':
				recordScreenShareEncoderVerification('accept-negotiated', action.negotiated);
				logger.info('Screen share publisher negotiated a different codec inside the publish policy', {
					requested: action.requested,
					negotiated: action.negotiated,
				});
				return;
			case 'correct-negotiated':
				recordScreenShareEncoderVerification('correct-negotiated', action.negotiated);
				logger.warn('Screen share is sending a codec outside the publish policy', {
					requested: action.requested,
					negotiated: action.negotiated,
					alternative: action.alternative,
				});
				void this.correctVerifiedScreenShareCodec(room, participant, track, action).catch((error) => {
					logger.warn('Failed to correct screen share codec after encoder verification', {
						error,
						requested: action.requested,
					});
				});
				return;
		}
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
			this.showScreenShareEndedModalInternal(i18n._(SCREEN_SHARE_SOURCE_STOPPED_DESCRIPTOR), 'source-stopped');
			recordScreenShareStopped('media-track-ended');
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
		return this.liveKitFlows.restoreReconnect(room, snapshot, publishOptions);
	}

	async restoreScreenShareAfterReconnect(
		room: Room | null,
		snapshot: ScreenShareReconnectSnapshot | null,
	): Promise<boolean> {
		if (snapshot !== null) return this.restoreScreenShareReconnect(room, snapshot);
		await this.setScreenShareEnabled(room, true, {sendUpdate: false});
		return LocalVoiceState.getSelfStream();
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
		const cleanupErrors: Array<unknown> = [];
		const cleanupResult = await unpublishLocalMediaPublications(participant, publications);
		for (const failure of cleanupResult.failedPublications) {
			cleanupErrors.push(failure.error);
			logger.warn('Failed to unpublish lingering screen share track', {
				error: failure.error,
				source: failure.publication.source,
			});
		}
		try {
			await this.releaseScreenShareCapture(participant, cleanupSnapshot);
		} catch (error) {
			cleanupErrors.push(error);
		}
		if (cleanupErrors.length > 0) {
			throw new ScreenShareRollbackIncompleteError(cleanupErrors);
		}
	}

	handleLocalScreenShareTrackUnpublished(room: Room, playSound: boolean, publication?: LocalTrackPublication): void {
		recordScreenShareStopped(this.isScreenSharePending ? 'user' : 'server-unpublish');
		this.clearScreenShareKeepAliveSinkInternal();
		this.cleanupActiveScreenShareEndListenerInternal();
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
			if (error instanceof ScreenShareRollbackIncompleteError) handleScreenShareError(error);
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
		if (!participant?.isScreenShareEnabled) return false;
		if (!linuxRule) return false;
		if (options.replaceExisting !== true) {
			const reconfigured = await reconfigureLinuxNativeAudioRouting(linuxRule, options);
			if (reconfigured !== 'unsupported') {
				this.syncLocalScreenShareAudioStateInternal(participant, true);
				this.syncPersistedScreenShareAudioPreferenceInternal(participant);
				return true;
			}
		}
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

	async ensureWindowScreenShareAudioPublication(room: Room | null, sourceId: string): Promise<boolean> {
		const participant = room?.localParticipant;
		if (!participant?.isScreenShareEnabled) return false;
		const targetPid = await getElectronAPI()
			?.nativeAudio?.resolveAudioRootPidForSource(sourceId)
			.catch((error) => {
				logger.warn('Failed to resolve the shared window audio process', {error, sourceId});
				return null;
			});
		if (targetPid == null) return false;
		const capturedTrack = await captureNativeAudioTrackForWindowPid(targetPid);
		if (!capturedTrack) return false;
		let adopted = false;
		try {
			adopted = await this.replaceActiveScreenShareAudioTrackInternal(participant, capturedTrack);
			if (adopted) {
				commitNativeAudioBridgeReplacement();
			}
		} catch (error) {
			logger.warn('Failed to publish mid-stream window screen-share audio track', {error, sourceId});
		}
		if (!adopted) {
			stopMediaTrack(capturedTrack);
			return false;
		}
		this.syncLocalScreenShareAudioStateInternal(participant, true);
		this.syncPersistedScreenShareAudioPreferenceInternal(participant);
		return true;
	}

	getActiveScreenShareVideoDeviceId(room: Room | null): string {
		const publication = room?.localParticipant?.getTrackPublication(Track.Source.ScreenShare);
		return publication?.videoTrack?.mediaStreamTrack.getSettings().deviceId ?? '';
	}

	async ensureDeviceScreenShareMicPublication(room: Room | null, audioDeviceId: string): Promise<boolean> {
		const participant = room?.localParticipant;
		if (!participant?.isScreenShareEnabled) return false;
		await ensureNativeMicrophonePermissionForDeviceShare('replace');
		const micTrack = await createLocalAudioTrack({
			deviceId: audioDeviceId && audioDeviceId !== 'default' ? audioDeviceId : undefined,
			echoCancellation: false,
			noiseSuppression: false,
			autoGainControl: false,
			voiceIsolation: false,
			channelCount: 2,
			sampleRate: 48000,
		});
		let adopted = false;
		try {
			adopted = await this.replaceActiveScreenShareAudioTrackInternal(participant, micTrack.mediaStreamTrack);
		} catch (error) {
			logger.warn('Failed to publish the device screen-share microphone track', {error});
		}
		if (!adopted) {
			try {
				await micTrack.stop();
			} catch (stopError) {
				logger.warn('Failed to stop the rejected device screen-share microphone track', {error: stopError});
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
			preserveStreamAudioPreferences?: boolean;
			keepShareTracking?: boolean;
		},
		publishOptions?: TrackPublishOptions,
	): Promise<void> {
		assert.equal(typeof enabled, 'boolean');
		if (enabled) {
			recordScreenShareStarted();
		}
		const leavesShareRunning = enabled ? options?.restartIfEnabled === true : options?.keepShareTracking === true;
		const generation = this.screenShareTrackingGeneration;
		if (leavesShareRunning) {
			this.screenShareTrackingHolds += 1;
		}
		try {
			await this.controllerRouting.setEnabled(room, enabled, options, publishOptions);
		} catch (error) {
			if (enabled) {
				recordScreenShareStartError(error);
			}
			throw error;
		} finally {
			if (leavesShareRunning) {
				this.screenShareTrackingHolds -= 1;
				assert.ok(this.screenShareTrackingHolds >= 0, 'screen share tracking holds post-condition: never negative');
				if (
					enabled &&
					this.screenShareTrackingHolds === 0 &&
					generation === this.screenShareTrackingGeneration &&
					room?.localParticipant?.isScreenShareEnabled !== true
				) {
					this.endScreenShareTracking();
				}
			}
		}
		if (!enabled && options?.preserveStreamAudioPreferences !== true) {
			VoiceSettingsCommands.update({
				shareAppAudio: true,
				shareDesktopAudio: true,
				shareDeviceAudio: true,
				muteStreamAudio: false,
			});
		}
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
		await this.liveKitFlows.setEnabled(
			room,
			enabled,
			{
				...selection.captureOptions,
				sendUpdate: selection.sendUpdate,
				playSound: selection.playSound,
				restartIfEnabled: selection.restartIfEnabled,
			},
			publishOptions,
		);
	}

	async startDeviceScreenShare(
		room: Room | null,
		options?: DeviceScreenShareCaptureOptions,
		publishOptions?: TrackPublishOptions,
	): Promise<void> {
		recordScreenShareStarted();
		try {
			await this.liveKitFlows.startDeviceScreenShare(room, options, publishOptions);
		} catch (error) {
			recordScreenShareStartError(error);
			throw error;
		}
	}

	async replaceActiveDisplayScreenShare(
		room: Room | null,
		options?: ScreenShareCaptureOptions,
		publishOptions?: TrackPublishOptions,
		captureContext?: DisplayScreenShareCaptureContext,
	): Promise<boolean> {
		return this.liveKitFlows.replaceActiveDisplayShare(room, options, publishOptions, captureContext);
	}

	async replaceActiveDeviceScreenShare(
		room: Room | null,
		options?: DeviceScreenShareCaptureOptions,
		publishOptions?: TrackPublishOptions,
	): Promise<boolean> {
		return this.liveKitFlows.replaceActiveDeviceShare(room, options, publishOptions);
	}

	async applyPendingScreenShareRequestsForRoom(room: Room | null): Promise<void> {
		const participant = room?.localParticipant;
		if (!participant) return;
		await this.applyPendingScreenShareRequestsInternal(room, participant);
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

	private async applyActiveScreenShareAudioSetting(participant: LocalParticipant, audio: boolean): Promise<boolean> {
		assert.ok(participant);
		assert.equal(typeof audio, 'boolean');
		const screenShareAudioPublication = participant.getTrackPublication(Track.Source.ScreenShareAudio);
		if (!screenShareAudioPublication) {
			if (audio) {
				logger.info('Cannot enable screen share audio without restarting screen share');
			}
			return false;
		}
		try {
			if (audio) {
				await screenShareAudioPublication.unmute();
			} else {
				await screenShareAudioPublication.mute();
			}
			return true;
		} catch (error) {
			logger.warn('Failed to update active screen share audio state', {error, includeAudio: audio});
			return false;
		}
	}

	private async applyActiveScreenShareResolutionSetting(
		screenShareTrack: LocalVideoTrack,
		resolution: NonNullable<ScreenShareCaptureOptions['resolution']>,
	): Promise<boolean> {
		assert.ok(screenShareTrack);
		assert.ok(resolution);
		const mediaStreamTrack = screenShareTrack.mediaStreamTrack;
		const currentConstraints = mediaStreamTrack.getConstraints() as ScreenShareVideoConstraints;
		const nextConstraints: ScreenShareVideoConstraints = {...currentConstraints};
		if (resolution.width > 0) nextConstraints.width = {ideal: resolution.width};
		if (resolution.height > 0) nextConstraints.height = {ideal: resolution.height};
		if (resolution.frameRate !== undefined) {
			nextConstraints.frameRate = {ideal: resolution.frameRate, max: resolution.frameRate};
		}
		if (JSON.stringify(currentConstraints) === JSON.stringify(nextConstraints)) return true;
		try {
			await mediaStreamTrack.applyConstraints(nextConstraints);
			return true;
		} catch (error) {
			logger.warn('Failed to update active screen share constraints', {error, resolution});
			return false;
		}
	}

	async updateActiveScreenShareSettings(
		room: Room | null,
		options?: ScreenShareCaptureOptions,
		publishOptions?: TrackPublishOptions,
	): Promise<boolean> {
		assert.ok(options === undefined || typeof options === 'object');
		if (Platform.OS !== 'web') {
			logger.warn('Screen share updates are not supported on native');
			return false;
		}
		const participant = room?.localParticipant;
		if (!participant?.isScreenShareEnabled) return false;
		const screenSharePublication = participant.getTrackPublication(Track.Source.ScreenShare);
		const screenShareTrack = screenSharePublication?.videoTrack as LocalVideoTrack | undefined;
		if (!screenShareTrack) {
			logger.warn('No active screen share track to update');
			return false;
		}
		this.restartScreenShareDeliveryPlanInternal();
		if (typeof options?.audio === 'boolean') {
			await this.applyActiveScreenShareAudioSetting(participant, options.audio);
		}
		if (options?.resolution) {
			const applied = await this.applyActiveScreenShareResolutionSetting(screenShareTrack, options.resolution);
			this.noteScreenShareApplyResultInternal(screenShareTrack, applied);
		}
		if (options && Object.hasOwn(options, 'contentHint')) {
			screenShareTrack.mediaStreamTrack.contentHint = options.contentHint ?? '';
		}
		await screenShareTrack.setDegradationPreference(SCREEN_SHARE_DEGRADATION_PREFERENCE);
		await this.enforceScreenShareSenderParametersInternal(participant, publishOptions);
		this.ensureScreenShareKeepAliveSinkInternal(participant);
		updateLocalParticipantFromRoom(room);
		this.syncLocalScreenShareAudioStateInternal(participant, participant.isScreenShareEnabled);
		return true;
	}

	setScreenShareAudioMuted(room: Room | null, muted: boolean): void {
		assert.equal(typeof muted, 'boolean');
		const participant = room?.localParticipant;
		if (!participant) return;
		const publication = participant.getTrackPublication(Track.Source.ScreenShareAudio);
		if (!publication) return;
		const operation = muted ? publication.mute() : publication.unmute();
		operation.catch((error) => {
			logger.warn('Failed to apply immediate screen share audio mute', {error, muted});
		});
		this.syncLocalScreenShareAudioStateInternal(participant, !muted);
	}

	async toggleScreenShareFromKeybind(room: Room | null): Promise<void> {
		const current = LocalVoiceState.getSelfStream();
		await this.setScreenShareEnabled(room, !current);
	}

	resetStreamTracking(): void {
		this.clearScreenShareKeepAliveSinkInternal();
		this.setStreamingPriorityInternal(false);
		this.transitionScreenShareLifecycleInternal({type: 'share.reset'});
		this.cleanupActiveScreenShareEndListenerInternal();
		this.cancelEncoderVerificationInternal();
		this.endedScreenShareStopInFlight = null;
		this.screenShareTrackingGeneration += 1;
		this.cleanupScreenShareAudioCaptureRouting();
		this.endScreenShareTracking();
	}
}

export {VoiceEngineV2AppScreenShareExecutionAdapter};

export default new VoiceEngineV2AppScreenShareExecutionAdapter();
