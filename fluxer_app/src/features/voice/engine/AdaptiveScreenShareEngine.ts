// SPDX-License-Identifier: AGPL-3.0-or-later

import i18n from '@app/app/I18n';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {Store} from '@app/features/voice/engine/Store';
import {updateVoiceEngineV2ScreenEncodingFromMediaEngine} from '@app/features/voice/engine/VoiceMediaEngineBridge';
import {VoiceTrackSource} from '@app/features/voice/engine/VoiceTrackSource';
import VoiceSettings, {type ScreenshareResolution, type StreamingMode} from '@app/features/voice/state/VoiceSettings';
import {isNativeScreenShareTrack} from '@app/features/voice/utils/native_screen_capture_bridge/shared';
import {
	getScreenShareDimensions,
	getScreenShareEncoding,
	resolveScreenShareFrameRate,
	SCREEN_SHARE_DEGRADATION_PREFERENCE,
	SCREEN_SHARE_FORCED_VIDEO_BITRATE_BPS,
	SUPPORTED_SCREEN_SHARE_FRAME_RATES,
	type SupportedScreenShareFrameRate,
} from '@app/features/voice/utils/ScreenShareOptions';
import {msg} from '@lingui/core/macro';
import type {LocalParticipant, LocalVideoTrack, Room, Track} from 'livekit-client';

const logger = new Logger('AdaptiveScreenShareEngine');
const SCREEN_SHARE_SOURCE = VoiceTrackSource.ScreenShare as Track.Source;

const POLL_INTERVAL_MS = 2500;
const STEP_DOWN_THRESHOLD = 4;
const STEP_UP_THRESHOLD = 3;
const FRAME_RATE_GRACE_FPS = 8;
const STEP_DOWN_COOLDOWN_MS = 10_000;
const STEP_UP_COOLDOWN_MS = 5_000;
const MAX_STEP_UP_COOLDOWN_MS = 30_000;
const BANDWIDTH_BITRATE_STEP_FACTOR = 0.6;
const OBSERVED_TARGET_BITRATE_HEADROOM = 1.1;

const SCREEN_SHARE_240P_DESCRIPTOR = msg({
	message: '240p',
	comment: 'Resolution label in adaptive screen-share quality toasts.',
	context: 'adaptive-screen-share',
});
const SCREEN_SHARE_480P_DESCRIPTOR = msg({
	message: '480p',
	comment: 'Resolution label in adaptive screen-share quality toasts.',
	context: 'adaptive-screen-share',
});
const SCREEN_SHARE_720P_DESCRIPTOR = msg({
	message: '720p',
	comment: 'Resolution label in adaptive screen-share quality toasts.',
	context: 'adaptive-screen-share',
});
const SCREEN_SHARE_1080P_DESCRIPTOR = msg({
	message: '1080p',
	comment: 'Resolution label in adaptive screen-share quality toasts.',
	context: 'adaptive-screen-share',
});
const SCREEN_SHARE_1440P_DESCRIPTOR = msg({
	message: '1440p',
	comment: 'Resolution label in adaptive screen-share quality toasts.',
	context: 'adaptive-screen-share',
});
const SCREEN_SHARE_SOURCE_QUALITY_DESCRIPTOR = msg({
	message: 'source quality',
	comment: 'Resolution label in adaptive screen-share quality toasts.',
	context: 'adaptive-screen-share',
});
const BANDWIDTH_LIMITED_DESCRIPTOR = msg({
	message: 'bandwidth limited',
	comment: 'Reason label in adaptive screen-share quality toasts.',
	context: 'adaptive-screen-share',
});
const CPU_LIMITED_DESCRIPTOR = msg({
	message: 'CPU limited',
	comment: 'Reason label in adaptive screen-share quality toasts.',
	context: 'adaptive-screen-share',
});
const QUALITY_LIMITED_DESCRIPTOR = msg({
	message: 'quality limited',
	comment: 'Fallback reason label in adaptive screen-share quality toasts.',
	context: 'adaptive-screen-share',
});
const LOWERED_SCREEN_SHARE_BITRATE_DESCRIPTOR = msg({
	message: 'Lowered screen share bitrate - {reason}',
	comment: 'Toast shown after adaptive screen-share quality lowers bitrate. {reason} is the limitation reason.',
	context: 'adaptive-screen-share',
});
const LOWERED_SCREEN_SHARE_FRAME_RATE_DESCRIPTOR = msg({
	message: 'Lowered screen share to {frameRate} FPS - {reason}',
	comment:
		'Toast shown after adaptive screen-share quality lowers frame rate. {frameRate} is a number and {reason} is the limitation reason.',
	context: 'adaptive-screen-share',
});
const LOWERED_SCREEN_SHARE_RESOLUTION_DESCRIPTOR = msg({
	message: 'Lowered screen share to {resolution} - {reason}',
	comment:
		'Toast shown after adaptive screen-share quality lowers resolution. {resolution} is a resolution label and {reason} is the limitation reason.',
	context: 'adaptive-screen-share',
});
const RAISED_SCREEN_SHARE_RESOLUTION_DESCRIPTOR = msg({
	message: 'Raised screen share to {resolution}',
	comment: 'Toast shown after adaptive screen-share quality raises resolution. {resolution} is a resolution label.',
	context: 'adaptive-screen-share',
});

const RESOLUTION_LADDER: ReadonlyArray<ScreenshareResolution> = [
	'source',
	'ultra',
	'high',
	'medium',
	'low_480p',
	'low_240p',
];

export type QualityLimitationReason = 'none' | 'cpu' | 'bandwidth' | 'other' | 'unknown';

export interface AdaptiveQualitySnapshot {
	configuredResolution: ScreenshareResolution;
	configuredFrameRate: SupportedScreenShareFrameRate;
	effectiveResolution: ScreenshareResolution;
	effectiveFrameRate: SupportedScreenShareFrameRate;
	limitationReason: QualityLimitationReason;
	isAdapted: boolean;
}

export interface OutboundVideoAdaptationStats {
	timestamp: number;
	qualityLimitationReason: QualityLimitationReason;
	framesEncoded?: number;
	framesSent?: number;
	framesPerSecond?: number;
	frameWidth?: number;
	frameHeight?: number;
	targetBitrate?: number;
}

export type PreviousOutboundVideoSample = OutboundVideoAdaptationStats | null;

interface ScreenShareSender {
	track: LocalVideoTrack;
	sender: RTCRtpSender;
}

function getConfiguredQuality(): {
	resolution: ScreenshareResolution;
	frameRate: SupportedScreenShareFrameRate;
} {
	return {
		resolution: VoiceSettings.getScreenshareResolution(),
		frameRate: resolveScreenShareFrameRate(VoiceSettings.getVideoFrameRate()),
	};
}

function getResolutionIndex(resolution: ScreenshareResolution): number {
	return RESOLUTION_LADDER.indexOf(resolution);
}

function getLowerResolution(resolution: ScreenshareResolution): ScreenshareResolution | null {
	const index = getResolutionIndex(resolution);
	if (index < 0 || index >= RESOLUTION_LADDER.length - 1) return null;
	return RESOLUTION_LADDER[index + 1];
}

function getLowerFrameRate(frameRate: SupportedScreenShareFrameRate): SupportedScreenShareFrameRate | null {
	const index = SUPPORTED_SCREEN_SHARE_FRAME_RATES.indexOf(frameRate);
	if (index <= 0) return null;
	return SUPPORTED_SCREEN_SHARE_FRAME_RATES[index - 1] ?? null;
}

function shouldPreferFrameRateStepDown(mode: StreamingMode, reason: QualityLimitationReason): boolean {
	if (mode === 'gaming') return false;
	if (mode === 'screenshare') return true;
	return reason === 'cpu';
}

function getSenderMaxBitrate(sender: RTCRtpSender): number | null {
	const encodings = sender.getParameters().encodings;
	const maxBitrate = encodings?.find((encoding) => typeof encoding.maxBitrate === 'number')?.maxBitrate;
	return typeof maxBitrate === 'number' && maxBitrate > 0 ? maxBitrate : null;
}

function getHigherResolution(
	resolution: ScreenshareResolution,
	configuredResolution: ScreenshareResolution,
): ScreenshareResolution | null {
	const currentIndex = getResolutionIndex(resolution);
	const configuredIndex = getResolutionIndex(configuredResolution);
	if (currentIndex < 0 || configuredIndex < 0 || currentIndex <= configuredIndex) return null;
	return RESOLUTION_LADDER[currentIndex - 1];
}

function getFrameCounter(stats: OutboundVideoAdaptationStats): number | undefined {
	return stats.framesSent ?? stats.framesEncoded;
}

function formatResolutionForToast(resolution: ScreenshareResolution): string {
	switch (resolution) {
		case 'low_240p':
			return i18n._(SCREEN_SHARE_240P_DESCRIPTOR);
		case 'low_480p':
			return i18n._(SCREEN_SHARE_480P_DESCRIPTOR);
		case 'medium':
			return i18n._(SCREEN_SHARE_720P_DESCRIPTOR);
		case 'high':
			return i18n._(SCREEN_SHARE_1080P_DESCRIPTOR);
		case 'ultra':
			return i18n._(SCREEN_SHARE_1440P_DESCRIPTOR);
		case 'source':
			return i18n._(SCREEN_SHARE_SOURCE_QUALITY_DESCRIPTOR);
	}
}

function formatLimitationReasonForToast(reason: QualityLimitationReason): string {
	if (reason === 'bandwidth') return i18n._(BANDWIDTH_LIMITED_DESCRIPTOR);
	if (reason === 'cpu') return i18n._(CPU_LIMITED_DESCRIPTOR);
	return i18n._(QUALITY_LIMITED_DESCRIPTOR);
}

function showAdaptiveScreenShareToast(message: string): void {
	try {
		ToastCommands.createToast({
			type: 'info',
			children: message,
			timeout: 5000,
		});
	} catch (error) {
		logger.debug('Failed to show adaptive screen-share quality toast', {error});
	}
}

export function parseQualityLimitationReason(value: unknown): QualityLimitationReason {
	if (value === 'none' || value === 'cpu' || value === 'bandwidth' || value === 'other') {
		return value;
	}
	return 'unknown';
}

export function isLimitedQualityReason(reason: QualityLimitationReason): boolean {
	return reason === 'cpu' || reason === 'bandwidth';
}

function isOutboundVideoReport(report: RTCStats): boolean {
	const outbound = report as RTCOutboundRtpStreamStats & {
		kind?: string;
		mediaType?: string;
	};
	if (outbound.type !== 'outbound-rtp') return false;
	return outbound.kind === 'video' || outbound.mediaType === 'video';
}

export function extractOutboundVideoAdaptationStats(stats: RTCStatsReport): OutboundVideoAdaptationStats | null {
	for (const raw of stats.values()) {
		const report = raw as RTCOutboundRtpStreamStats & {
			qualityLimitationReason?: string;
			framesEncoded?: number;
			framesSent?: number;
			framesPerSecond?: number;
			frameWidth?: number;
			frameHeight?: number;
			targetBitrate?: number;
		};
		if (!isOutboundVideoReport(report)) continue;
		return {
			timestamp: report.timestamp,
			qualityLimitationReason: parseQualityLimitationReason(report.qualityLimitationReason),
			framesEncoded: typeof report.framesEncoded === 'number' ? report.framesEncoded : undefined,
			framesSent: typeof report.framesSent === 'number' ? report.framesSent : undefined,
			framesPerSecond: typeof report.framesPerSecond === 'number' ? report.framesPerSecond : undefined,
			frameWidth: typeof report.frameWidth === 'number' ? report.frameWidth : undefined,
			frameHeight: typeof report.frameHeight === 'number' ? report.frameHeight : undefined,
			targetBitrate: typeof report.targetBitrate === 'number' ? report.targetBitrate : undefined,
		};
	}
	return null;
}

export function computeOutboundFrameRate(
	current: OutboundVideoAdaptationStats,
	previous: PreviousOutboundVideoSample,
): number | null {
	if (typeof current.framesPerSecond === 'number' && Number.isFinite(current.framesPerSecond)) {
		return current.framesPerSecond;
	}
	if (!previous) return null;
	const currentFrames = getFrameCounter(current);
	const previousFrames = getFrameCounter(previous);
	if (currentFrames === undefined || previousFrames === undefined) return null;
	const frameDelta = currentFrames - previousFrames;
	const timeDeltaMs = current.timestamp - previous.timestamp;
	if (frameDelta < 0 || timeDeltaMs <= 0) return null;
	return (frameDelta * 1000) / timeDeltaMs;
}

export function isOutboundFrameRateBelowTarget(
	current: OutboundVideoAdaptationStats,
	previous: PreviousOutboundVideoSample,
	targetFrameRate: number,
): boolean {
	const frameRate = computeOutboundFrameRate(current, previous);
	if (frameRate == null) return false;
	return frameRate < Math.max(1, targetFrameRate - FRAME_RATE_GRACE_FPS);
}

export function computeAdaptiveBitrate(
	frameRate: number,
	maxBitrateBps?: number,
	observedTargetBitrateBps?: number,
): number {
	const base = getScreenShareEncoding(frameRate, maxBitrateBps).maxBitrate ?? 0;
	if (typeof observedTargetBitrateBps !== 'number' || observedTargetBitrateBps <= 0) return base;
	return Math.min(base, Math.round(observedTargetBitrateBps * OBSERVED_TARGET_BITRATE_HEADROOM));
}

export function computeResolutionScale(
	fromResolution: ScreenshareResolution,
	toResolution: ScreenshareResolution,
): number {
	const from = getScreenShareDimensions(fromResolution);
	const to = getScreenShareDimensions(toResolution);
	return Math.min(to.width / from.width, to.height / from.height);
}

export async function applyResolutionFrameRateAndBitrate(
	track: LocalVideoTrack,
	sender: RTCRtpSender,
	resolution: ScreenshareResolution,
	frameRate: SupportedScreenShareFrameRate,
	maxBitrateBps?: number,
	observedTargetBitrateBps?: number,
): Promise<void> {
	const dimensions = getScreenShareDimensions(resolution);
	if (isNativeScreenShareTrack(track.mediaStreamTrack)) {
		const updated = updateVoiceEngineV2ScreenEncodingFromMediaEngine({
			width: dimensions.width,
			height: dimensions.height,
			frameRate,
			maxBitrateBps: computeAdaptiveBitrate(frameRate, maxBitrateBps, observedTargetBitrateBps),
		});
		if (!updated) {
			logger.warn('Skipped native screen-share encoding update because v2 screen state is unavailable', {
				width: dimensions.width,
				height: dimensions.height,
				frameRate,
			});
		}
		return;
	}
	const currentSettings = track.mediaStreamTrack.getSettings?.();
	const scaleResolutionDownBy =
		typeof currentSettings?.width === 'number' &&
		typeof currentSettings.height === 'number' &&
		currentSettings.width > 0 &&
		currentSettings.height > 0
			? Math.max(1, currentSettings.width / dimensions.width, currentSettings.height / dimensions.height)
			: undefined;
	await track.mediaStreamTrack.applyConstraints({
		width: {ideal: dimensions.width},
		height: {ideal: dimensions.height},
		frameRate: {ideal: frameRate, max: frameRate},
	});
	if (typeof track.setDegradationPreference === 'function') {
		await track.setDegradationPreference(SCREEN_SHARE_DEGRADATION_PREFERENCE);
	}
	const parameters = sender.getParameters();
	const encodings = parameters.encodings?.length ? parameters.encodings : [{}];
	const maxBitrate = computeAdaptiveBitrate(frameRate, maxBitrateBps, observedTargetBitrateBps);
	parameters.degradationPreference = SCREEN_SHARE_DEGRADATION_PREFERENCE;
	parameters.encodings = encodings.map((encoding) => ({
		...encoding,
		...(scaleResolutionDownBy !== undefined ? {scaleResolutionDownBy} : {}),
		maxBitrate,
		maxFramerate: frameRate,
		priority: encoding.priority ?? 'high',
		networkPriority: encoding.networkPriority ?? 'high',
	}));
	await sender.setParameters(parameters);
}

class AdaptiveScreenShareEngine extends Store {
	effectiveResolution: ScreenshareResolution | null = null;
	effectiveFrameRate: SupportedScreenShareFrameRate | null = null;
	limitationReason: QualityLimitationReason = 'none';
	isAdapted = false;
	private configuredResolution: ScreenshareResolution | null = null;
	private configuredFrameRate: SupportedScreenShareFrameRate | null = null;
	private room: Room | null = null;
	private timer: NodeJS.Timeout | null = null;
	private pollInFlightGeneration: number | null = null;
	private pollGeneration = 0;
	private stepDownStreak = 0;
	private stepUpStreak = 0;
	private lastStepDownAt = 0;
	private lastStepUpAt = 0;
	private stepUpCooldownMs = STEP_UP_COOLDOWN_MS;
	private previousSample: PreviousOutboundVideoSample = null;
	private bandwidthBitrateStepActive = false;

	get qualitySnapshot(): AdaptiveQualitySnapshot {
		const configured = getConfiguredQuality();
		const configuredResolution = this.configuredResolution ?? configured.resolution;
		const configuredFrameRate = this.configuredFrameRate ?? configured.frameRate;
		return {
			configuredResolution,
			configuredFrameRate,
			effectiveResolution: this.effectiveResolution ?? configuredResolution,
			effectiveFrameRate: this.effectiveFrameRate ?? configuredFrameRate,
			limitationReason: this.limitationReason,
			isAdapted: this.isAdapted,
		};
	}

	start(room: Room | null | undefined): void {
		this.stop();
		if (!room || !VoiceSettings.getAdaptiveScreenShareQuality()) {
			return;
		}
		if (!this.getScreenShareSender(room)) {
			return;
		}
		const configured = getConfiguredQuality();
		let generation = 0;
		this.update(() => {
			this.room = room;
			this.configuredResolution = configured.resolution;
			this.configuredFrameRate = configured.frameRate;
			this.effectiveResolution = configured.resolution;
			this.effectiveFrameRate = configured.frameRate;
			this.limitationReason = 'none';
			this.isAdapted = false;
			generation = ++this.pollGeneration;
			this.timer = setInterval(() => {
				void this.poll(generation);
			}, POLL_INTERVAL_MS);
		});
		void this.poll(generation);
		logger.info('Started adaptive screen share quality monitor', configured);
	}

	stop(): void {
		if (this.timer) {
			clearInterval(this.timer);
		}
		this.update(() => {
			this.timer = null;
			this.room = null;
			this.pollInFlightGeneration = null;
			this.pollGeneration++;
			this.stepDownStreak = 0;
			this.stepUpStreak = 0;
			this.lastStepDownAt = 0;
			this.lastStepUpAt = 0;
			this.stepUpCooldownMs = STEP_UP_COOLDOWN_MS;
			this.previousSample = null;
			this.bandwidthBitrateStepActive = false;
			this.configuredResolution = null;
			this.configuredFrameRate = null;
			this.effectiveResolution = null;
			this.effectiveFrameRate = null;
			this.limitationReason = 'none';
			this.isAdapted = false;
		});
	}

	async restoreConfiguredQuality(): Promise<void> {
		const room = this.room;
		const snapshot = this.qualitySnapshot;
		const wasAdapted = this.isAdapted;
		const screenShare = room ? this.getScreenShareSender(room) : null;
		this.stop();
		if (!wasAdapted || !screenShare) {
			return;
		}
		try {
			await applyResolutionFrameRateAndBitrate(
				screenShare.track,
				screenShare.sender,
				snapshot.configuredResolution,
				snapshot.configuredFrameRate,
				SCREEN_SHARE_FORCED_VIDEO_BITRATE_BPS,
			);
			logger.info('Restored configured screen share quality', {
				resolution: snapshot.configuredResolution,
				frameRate: snapshot.configuredFrameRate,
			});
		} catch (error) {
			logger.warn('Failed to restore configured screen share quality', {error});
		}
	}

	private getScreenShareSender(room: Room): ScreenShareSender | null {
		const participant = room.localParticipant as LocalParticipant | undefined;
		const publication = participant?.getTrackPublication(SCREEN_SHARE_SOURCE);
		const track = publication?.videoTrack as LocalVideoTrack | undefined;
		const sender = track?.sender;
		if (!track || !sender || track.mediaStreamTrack.readyState === 'ended') {
			return null;
		}
		return {track, sender};
	}

	private async poll(generation: number): Promise<void> {
		if (this.pollInFlightGeneration != null || generation !== this.pollGeneration) return;
		const room = this.room;
		if (!room) return;
		if (!VoiceSettings.getAdaptiveScreenShareQuality()) {
			await this.restoreConfiguredQuality();
			return;
		}
		const screenShare = this.getScreenShareSender(room);
		if (!screenShare) {
			this.stop();
			return;
		}
		this.pollInFlightGeneration = generation;
		try {
			const report = await screenShare.sender.getStats();
			if (generation !== this.pollGeneration) return;
			const stats = extractOutboundVideoAdaptationStats(report);
			if (!stats) return;
			const previousSample = this.previousSample;
			this.previousSample = stats;
			const targetFrameRate = this.effectiveFrameRate ?? this.configuredFrameRate ?? getConfiguredQuality().frameRate;
			const frameRateLimited = isOutboundFrameRateBelowTarget(stats, previousSample, targetFrameRate);
			const qualityLimited = isLimitedQualityReason(stats.qualityLimitationReason);
			this.update(() => {
				this.limitationReason = stats.qualityLimitationReason;
			});
			if (qualityLimited || frameRateLimited) {
				this.stepDownStreak++;
				this.stepUpStreak = 0;
			} else if (stats.qualityLimitationReason === 'none') {
				this.stepDownStreak = 0;
				this.bandwidthBitrateStepActive = false;
				if (this.isAdapted) {
					this.stepUpStreak++;
				}
			} else {
				this.stepDownStreak = 0;
				this.stepUpStreak = 0;
			}
			const now = Date.now();
			if (this.stepDownStreak >= STEP_DOWN_THRESHOLD && now - this.lastStepDownAt >= STEP_DOWN_COOLDOWN_MS) {
				await this.stepDown(screenShare, stats);
				return;
			}
			if (
				this.stepUpStreak >= STEP_UP_THRESHOLD &&
				now - this.lastStepUpAt >= this.stepUpCooldownMs &&
				now - this.lastStepDownAt >= STEP_DOWN_COOLDOWN_MS
			) {
				await this.stepUp(screenShare);
			}
		} catch (error) {
			logger.debug('Failed to poll adaptive screen share quality stats', {error});
		} finally {
			if (this.pollInFlightGeneration === generation) {
				this.pollInFlightGeneration = null;
			}
		}
	}

	private async stepDown(screenShare: ScreenShareSender, stats: OutboundVideoAdaptationStats): Promise<void> {
		const reason = stats.qualityLimitationReason;
		const currentResolution =
			this.effectiveResolution ?? this.configuredResolution ?? getConfiguredQuality().resolution;
		const currentFrameRate = this.effectiveFrameRate ?? this.configuredFrameRate ?? getConfiguredQuality().frameRate;
		const streamingMode = VoiceSettings.getStreamingMode();
		const nextFrameRate = shouldPreferFrameRateStepDown(streamingMode, reason)
			? getLowerFrameRate(currentFrameRate)
			: null;
		const currentSenderMaxBitrate =
			getSenderMaxBitrate(screenShare.sender) ??
			computeAdaptiveBitrate(currentFrameRate, SCREEN_SHARE_FORCED_VIDEO_BITRATE_BPS, stats.targetBitrate);
		const canApplyBandwidthBitrateStep =
			reason === 'bandwidth' && !nextFrameRate && !this.bandwidthBitrateStepActive && currentSenderMaxBitrate > 0;
		const nextResolution =
			nextFrameRate || canApplyBandwidthBitrateStep ? currentResolution : getLowerResolution(currentResolution);
		if (!nextResolution) {
			this.stepDownStreak = 0;
			return;
		}
		const nextMaxBitrateBps = canApplyBandwidthBitrateStep
			? Math.max(1, Math.round(currentSenderMaxBitrate * BANDWIDTH_BITRATE_STEP_FACTOR))
			: SCREEN_SHARE_FORCED_VIDEO_BITRATE_BPS;
		const nextEffectiveFrameRate = nextFrameRate ?? currentFrameRate;
		const stepKind = nextFrameRate ? 'framerate' : canApplyBandwidthBitrateStep ? 'bitrate' : 'resolution';
		try {
			await applyResolutionFrameRateAndBitrate(
				screenShare.track,
				screenShare.sender,
				nextResolution,
				nextEffectiveFrameRate,
				nextMaxBitrateBps,
				canApplyBandwidthBitrateStep ? undefined : stats.targetBitrate,
			);
			this.update(() => {
				this.effectiveResolution = nextResolution;
				this.effectiveFrameRate = nextEffectiveFrameRate;
				this.isAdapted = true;
				this.stepDownStreak = 0;
				this.stepUpStreak = 0;
				this.lastStepDownAt = Date.now();
				this.stepUpCooldownMs = Math.min(this.stepUpCooldownMs * 2, MAX_STEP_UP_COOLDOWN_MS);
				this.bandwidthBitrateStepActive = canApplyBandwidthBitrateStep;
			});
			logger.info('Reduced screen share quality adaptively', {
				fromResolution: currentResolution,
				toResolution: nextResolution,
				fromFrameRate: currentFrameRate,
				toFrameRate: nextEffectiveFrameRate,
				maxBitrateBps: nextMaxBitrateBps,
				stepKind,
				reason,
			});
			const reasonLabel = formatLimitationReasonForToast(reason);
			if (stepKind === 'bitrate') {
				showAdaptiveScreenShareToast(i18n._(LOWERED_SCREEN_SHARE_BITRATE_DESCRIPTOR, {reason: reasonLabel}));
			} else if (stepKind === 'framerate') {
				showAdaptiveScreenShareToast(
					i18n._(LOWERED_SCREEN_SHARE_FRAME_RATE_DESCRIPTOR, {
						frameRate: nextEffectiveFrameRate,
						reason: reasonLabel,
					}),
				);
			} else {
				showAdaptiveScreenShareToast(
					i18n._(LOWERED_SCREEN_SHARE_RESOLUTION_DESCRIPTOR, {
						resolution: formatResolutionForToast(nextResolution),
						reason: reasonLabel,
					}),
				);
			}
		} catch (error) {
			this.stepDownStreak = 0;
			logger.warn('Failed to reduce screen share quality adaptively', {
				error,
				fromResolution: currentResolution,
				toResolution: nextResolution,
				fromFrameRate: currentFrameRate,
				toFrameRate: nextEffectiveFrameRate,
				maxBitrateBps: nextMaxBitrateBps,
				stepKind,
				reason,
			});
		}
	}

	private async stepUp(screenShare: ScreenShareSender): Promise<void> {
		const configured = getConfiguredQuality();
		const configuredResolution = this.configuredResolution ?? configured.resolution;
		const configuredFrameRate = this.configuredFrameRate ?? configured.frameRate;
		const currentResolution = this.effectiveResolution ?? configuredResolution;
		const nextResolution = getHigherResolution(currentResolution, configuredResolution);
		if (!nextResolution) {
			this.update(() => {
				this.effectiveResolution = configuredResolution;
				this.effectiveFrameRate = configuredFrameRate;
				this.isAdapted = false;
				this.stepUpStreak = 0;
				this.stepUpCooldownMs = STEP_UP_COOLDOWN_MS;
				this.bandwidthBitrateStepActive = false;
			});
			return;
		}
		try {
			await applyResolutionFrameRateAndBitrate(
				screenShare.track,
				screenShare.sender,
				nextResolution,
				configuredFrameRate,
				SCREEN_SHARE_FORCED_VIDEO_BITRATE_BPS,
			);
			this.update(() => {
				this.effectiveResolution = nextResolution;
				this.effectiveFrameRate = configuredFrameRate;
				this.isAdapted = nextResolution !== configuredResolution;
				this.stepUpStreak = 0;
				this.lastStepUpAt = Date.now();
				this.bandwidthBitrateStepActive = false;
				if (!this.isAdapted) {
					this.stepUpCooldownMs = STEP_UP_COOLDOWN_MS;
				}
			});
			logger.info('Raised screen share quality adaptively', {
				fromResolution: currentResolution,
				toResolution: nextResolution,
				frameRate: configuredFrameRate,
			});
			showAdaptiveScreenShareToast(
				i18n._(RAISED_SCREEN_SHARE_RESOLUTION_DESCRIPTOR, {
					resolution: formatResolutionForToast(nextResolution),
				}),
			);
		} catch (error) {
			this.stepUpStreak = 0;
			logger.warn('Failed to raise screen share quality adaptively', {
				error,
				fromResolution: currentResolution,
				toResolution: nextResolution,
				frameRate: configuredFrameRate,
			});
		}
	}
}

export default new AdaptiveScreenShareEngine();
