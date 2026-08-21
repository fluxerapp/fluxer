// SPDX-License-Identifier: AGPL-3.0-or-later

import {getDesktopTroubleshootingSettings} from '@app/features/devtools/utils/DesktopTroubleshootingUtils';
import {Logger} from '@app/features/platform/utils/AppLogger';
import ScreenShareCodecNegotiation from '@app/features/voice/engine/ScreenShareCodecNegotiation';
import SoftwareEncoderWarning from '@app/features/voice/state/SoftwareEncoderWarning';
import VoiceSettings from '@app/features/voice/state/VoiceSettings';
import {
	adjustScreenShareEncodingForCodec,
	getCodecCapabilityReport,
} from '@app/features/voice/utils/CodecCapabilityDetector';
import {getGpuEncoderReportSync, loadGpuEncoderReport} from '@app/features/voice/utils/GpuEncoderCapabilities';
import {loadNativeHardwareEncoderCapabilities} from '@app/features/voice/utils/NativeHardwareEncoderCapabilities';
import {
	getScreenShareEncoding,
	resolveStreamingModeSettings,
	SCREEN_SHARE_DEGRADATION_PREFERENCE,
} from '@app/features/voice/utils/ScreenShareOptions';
import {classifyVideoEncoderAcceleration} from '@app/features/voice/utils/VideoAccelerationClassification';
import {
	BackupCodecPolicy,
	type LocalTrackPublication,
	type ScreenShareCaptureOptions,
	type TrackPublishOptions,
	type VideoCodec,
	type VideoEncoding,
} from 'livekit-client';

export const logger = new Logger('VoiceEngineV2ScreenShareSupport');
const GPU_ENCODER_REPORT_START_TIMEOUT_MS = 500;

export interface DeviceScreenShareCaptureOptions {
	videoDeviceId?: string;
	previewVideoDeviceId?: string;
	audioDeviceId?: string;
	resolution?: ScreenShareCaptureOptions['resolution'];
	sendUpdate?: boolean;
	playSound?: boolean;
}

export interface CapturedScreenShareTracks {
	videoTrack: MediaStreamTrack;
	audioTrack?: MediaStreamTrack;
}

export interface SimulcastTrackInfoLike {
	mediaStreamTrack: MediaStreamTrack;
	sender?: RTCRtpSender;
}

export interface PendingScreenShareStopRequest {
	sendUpdate: boolean;
	playSound: boolean;
}

export type ScreenShareCodecReadinessStatus = 'loading' | 'ready' | 'timeout';

export interface ScreenSharePublishOptionsResolutionOptions {
	onCodecReadiness?: (status: ScreenShareCodecReadinessStatus) => void;
}

export interface ScreenShareCaptureCleanupSnapshot {
	mediaTracks: Array<MediaStreamTrack>;
	senders: Array<RTCRtpSender>;
}

interface ScreenShareTrackLike {
	mediaStreamTrack?: MediaStreamTrack;
	sender?: RTCRtpSender;
	simulcastCodecs?: Map<unknown, SimulcastTrackInfoLike>;
}

export function getPreferredScreenShareCodec(): VideoCodec {
	return ScreenShareCodecNegotiation.selectScreenShareCodec(VoiceSettings.getPreferredScreenShareCodec());
}

function pushUnique<T>(items: Array<T>, item: T | undefined | null): void {
	if (!item || items.includes(item)) return;
	items.push(item);
}

function captureScreenShareTrackCleanup(
	snapshot: ScreenShareCaptureCleanupSnapshot,
	track: ScreenShareTrackLike | undefined | null,
): void {
	pushUnique(snapshot.mediaTracks, track?.mediaStreamTrack);
	pushUnique(snapshot.senders, track?.sender);
	for (const simulcastTrackInfo of track?.simulcastCodecs?.values() ?? []) {
		pushUnique(snapshot.mediaTracks, simulcastTrackInfo.mediaStreamTrack);
		pushUnique(snapshot.senders, simulcastTrackInfo.sender);
	}
}

export function captureScreenSharePublicationCleanup(
	...publications: Array<LocalTrackPublication | undefined | null>
): ScreenShareCaptureCleanupSnapshot {
	const snapshot: ScreenShareCaptureCleanupSnapshot = {mediaTracks: [], senders: []};
	for (const publication of publications) {
		captureScreenShareTrackCleanup(snapshot, publication?.track);
		captureScreenShareTrackCleanup(snapshot, publication?.videoTrack);
		captureScreenShareTrackCleanup(snapshot, publication?.audioTrack);
	}
	return snapshot;
}

export function mergeScreenShareCaptureCleanupSnapshots(
	...snapshots: Array<ScreenShareCaptureCleanupSnapshot | undefined | null>
): ScreenShareCaptureCleanupSnapshot {
	const merged: ScreenShareCaptureCleanupSnapshot = {mediaTracks: [], senders: []};
	for (const snapshot of snapshots) {
		for (const mediaTrack of snapshot?.mediaTracks ?? []) {
			pushUnique(merged.mediaTracks, mediaTrack);
		}
		for (const sender of snapshot?.senders ?? []) {
			pushUnique(merged.senders, sender);
		}
	}
	return merged;
}

async function detachScreenShareSender(sender: RTCRtpSender): Promise<void> {
	try {
		if (sender.transport?.state === 'closed') return;
		await sender.replaceTrack(null);
	} catch (error) {
		logger.warn('Failed to detach screen share sender during cleanup', {error});
	}
}

export async function releaseScreenShareCaptureCleanup(snapshot: ScreenShareCaptureCleanupSnapshot): Promise<void> {
	await Promise.all(snapshot.senders.map(detachScreenShareSender));
	for (const mediaTrack of snapshot.mediaTracks) {
		stopMediaTrack(mediaTrack);
	}
}

function getCodecSpecificScreenShareBitrateCeiling(codec: VideoCodec): number | undefined {
	const acceleration = getCodecCapabilityReport()[codec].hardwareAccelerated;
	if (codec === 'h264' && acceleration === 'hardware') return 20000000;
	if ((codec === 'av1' || codec === 'vp9') && acceleration === 'software') return 40000000;
	return undefined;
}

function clampScreenShareEncoding(encoding: VideoEncoding | undefined, codec: VideoCodec): VideoEncoding | undefined {
	if (!encoding) return undefined;
	const maxBitrateBps = VoiceSettings.getScreenShareMaxBitrateBpsOverride();
	const codecCeilingBps = getCodecSpecificScreenShareBitrateCeiling(codec);
	const bitrateCeilingBps =
		maxBitrateBps !== undefined && codecCeilingBps !== undefined
			? Math.min(maxBitrateBps, codecCeilingBps)
			: (maxBitrateBps ?? codecCeilingBps);
	return {
		...encoding,
		maxBitrate:
			typeof encoding.maxBitrate === 'number' && bitrateCeilingBps !== undefined
				? Math.min(encoding.maxBitrate, bitrateCeilingBps)
				: encoding.maxBitrate,
		priority: encoding.priority ?? 'high',
	};
}

export function getEffectiveScreenShareEncoding(publishOptions?: TrackPublishOptions): VideoEncoding | undefined {
	const preferredVideoCodec = publishOptions?.videoCodec ?? getPreferredScreenShareCodec();
	let screenShareEncoding = publishOptions?.screenShareEncoding;
	if (!screenShareEncoding) {
		const settings = resolveStreamingModeSettings(
			VoiceSettings.getStreamingMode(),
			VoiceSettings.getScreenshareResolution(),
			VoiceSettings.getVideoFrameRate(),
		);
		screenShareEncoding = getScreenShareEncoding(
			settings.resolution,
			settings.frameRate,
			VoiceSettings.getScreenShareMaxBitrateBpsOverride(),
		);
	}
	if (!screenShareEncoding) return undefined;
	return clampScreenShareEncoding(
		adjustScreenShareEncodingForCodec(screenShareEncoding, preferredVideoCodec),
		preferredVideoCodec,
	);
}

function getScreenShareScalabilityModeForCodec(codec: VideoCodec): TrackPublishOptions['scalabilityMode'] | undefined {
	if (codec !== 'av1' && codec !== 'vp9') return undefined;
	const preference = VoiceSettings.getScreenShareScalabilityModeOverride();
	if (preference === 'single_layer') return 'L1T1';
	if (preference === 'temporal') return 'L1T3';
	if (preference === 'spatial') return 'L3T3_KEY';
	const streamingMode = VoiceSettings.getStreamingMode();
	if (streamingMode === 'gaming') return 'L1T3';
	if (streamingMode === 'screenshare') return 'L3T3_KEY';
	const quality = VoiceSettings.getScreenShareSoftwareQualityOverride();
	if (!quality) return undefined;
	const gpuReport = getGpuEncoderReportSync();
	const softwareBiased = VoiceSettings.getScreenShareEncoderMode() === 'software' || gpuReport?.[codec] === 'software';
	if (!softwareBiased) return undefined;
	if (quality === 'realtime') return 'L1T1';
	if (quality === 'balanced') return 'L1T3';
	return 'L3T3_KEY';
}

function isSvcScreenShareCodec(codec: VideoCodec): boolean {
	return codec === 'av1' || codec === 'vp9';
}

function getConfiguredBackupCodecForPrimary(primaryCodec: VideoCodec):
	| false
	| {
			codec: 'h264';
	  }
	| undefined {
	if (VoiceSettings.getScreenShareBackupCodecModeOverride() !== 'h264_simulcast') return undefined;
	if (primaryCodec === 'h264' || primaryCodec === 'vp8') return false;
	return {codec: 'h264'};
}

function getEffectiveScreenShareScalabilityMode(
	codec: VideoCodec,
	publishOptions?: TrackPublishOptions,
	options: {respectExplicit?: boolean} = {},
): TrackPublishOptions['scalabilityMode'] | undefined {
	if (options.respectExplicit !== false && publishOptions?.scalabilityMode) return publishOptions.scalabilityMode;
	return getScreenShareScalabilityModeForCodec(codec);
}

async function waitForGpuEncoderReportForPublish(options?: ScreenSharePublishOptionsResolutionOptions): Promise<void> {
	let timeoutId: NodeJS.Timeout | undefined;
	options?.onCodecReadiness?.('loading');
	const timeout = new Promise<'timeout'>((resolve) => {
		timeoutId = setTimeout(() => resolve('timeout'), GPU_ENCODER_REPORT_START_TIMEOUT_MS);
	});
	try {
		const result = await Promise.race([
			Promise.all([
				loadGpuEncoderReport(),
				loadNativeHardwareEncoderCapabilities(),
				getDesktopTroubleshootingSettings(),
			]).then(() => 'loaded' as const),
			timeout,
		]);
		if (result === 'timeout') {
			options?.onCodecReadiness?.('timeout');
			logger.warn('GPU encoder report timed out before screen share publish; using cached codec capabilities', {
				timeoutMs: GPU_ENCODER_REPORT_START_TIMEOUT_MS,
			});
		} else {
			options?.onCodecReadiness?.('ready');
		}
	} finally {
		if (timeoutId !== undefined) {
			clearTimeout(timeoutId);
		}
	}
}

export async function getEffectivePublishOptions(
	enabled: boolean,
	publishOptions?: TrackPublishOptions,
	options?: ScreenSharePublishOptionsResolutionOptions,
): Promise<TrackPublishOptions | undefined> {
	if (!enabled) {
		return publishOptions;
	}
	await waitForGpuEncoderReportForPublish(options);
	const preferredVideoCodec = publishOptions?.videoCodec ?? getPreferredScreenShareCodec();
	const backupCodec = publishOptions?.backupCodec ?? getConfiguredBackupCodecForPrimary(preferredVideoCodec);
	const backupCodecPolicy =
		publishOptions?.backupCodecPolicy ?? (backupCodec ? BackupCodecPolicy.SIMULCAST : undefined);
	const scalabilityMode = getEffectiveScreenShareScalabilityMode(preferredVideoCodec, publishOptions, {
		respectExplicit: true,
	});
	return {
		...publishOptions,
		videoCodec: preferredVideoCodec,
		screenShareEncoding: getEffectiveScreenShareEncoding({...publishOptions, videoCodec: preferredVideoCodec}),
		degradationPreference: SCREEN_SHARE_DEGRADATION_PREFERENCE,
		simulcast: isSvcScreenShareCodec(preferredVideoCodec) ? false : publishOptions?.simulcast,
		scalabilityMode,
		...(backupCodec !== undefined ? {backupCodec} : {}),
		...(backupCodecPolicy !== undefined ? {backupCodecPolicy} : {}),
	};
}

export function applyScreenShareContentHint(
	track: MediaStreamTrack | undefined,
	hint?: 'detail' | 'text' | 'motion',
): void {
	if (!track) return;
	try {
		track.contentHint = hint ?? '';
	} catch (error) {
		logger.warn('Failed to apply screen share content hint', {error, hint});
	}
}

export async function enforceScreenShareSenderParameters(
	sender: RTCRtpSender | undefined,
	publishOptions?: TrackPublishOptions,
	codecOverride?: VideoCodec,
): Promise<boolean> {
	if (!sender) return false;
	const preferredVideoCodec = codecOverride ?? publishOptions?.videoCodec ?? getPreferredScreenShareCodec();
	const screenShareEncoding = getEffectiveScreenShareEncoding({...publishOptions, videoCodec: preferredVideoCodec});
	const scalabilityMode = getEffectiveScreenShareScalabilityMode(preferredVideoCodec, publishOptions, {
		respectExplicit: !codecOverride || codecOverride === publishOptions?.videoCodec,
	});
	try {
		const params = sender.getParameters();
		const encodings = params.encodings?.length ? params.encodings : [{}];
		params.degradationPreference = SCREEN_SHARE_DEGRADATION_PREFERENCE;
		params.encodings = encodings.map((encoding) => ({
			...encoding,
			...(screenShareEncoding?.maxBitrate !== undefined ? {maxBitrate: screenShareEncoding.maxBitrate} : {}),
			...(screenShareEncoding?.maxFramerate !== undefined ? {maxFramerate: screenShareEncoding.maxFramerate} : {}),
			priority: screenShareEncoding?.priority ?? encoding.priority ?? 'high',
			networkPriority: screenShareEncoding?.priority ?? encoding.networkPriority ?? 'high',
			...(scalabilityMode ? {scalabilityMode} : {}),
		}));
		await sender.setParameters(params);
		return true;
	} catch (error) {
		logger.warn('Failed to enforce screen share sender parameters', {error, screenShareEncoding, scalabilityMode});
		return false;
	}
}

export function stopMediaTrack(track: MediaStreamTrack | undefined): void {
	if (!track) {
		return;
	}
	try {
		track.stop();
	} catch (error) {
		logger.warn('Failed to stop unused screen share media track', {error});
	}
}

export function stopUnselectedStreamTracks(
	stream: MediaStream,
	selectedTracks: ReadonlyArray<MediaStreamTrack | undefined>,
): void {
	const selected = new Set(selectedTracks.filter((track): track is MediaStreamTrack => Boolean(track)));
	for (const track of stream.getTracks()) {
		if (!selected.has(track)) {
			stopMediaTrack(track);
		}
	}
}

export function getReplacementScreenShareSettingsOptions(
	options: ScreenShareCaptureOptions | undefined,
	hasReplacementAudioTrack: boolean,
): ScreenShareCaptureOptions | undefined {
	if (!options || typeof options.audio !== 'boolean' || !hasReplacementAudioTrack) {
		return options;
	}
	return {
		...options,
		audio: true,
	};
}

const ENCODER_VERIFICATION_DELAY_MS = 2500;
const UNKNOWN_ENCODER_IMPLEMENTATION = 'software encoder';

interface CodecStatsEntry {
	type: string;
	id: string;
	mimeType?: string;
}

interface OutboundVideoStatsEntry {
	type: string;
	kind?: string;
	mediaType?: string;
	codecId?: string;
	mediaSourceId?: string;
	framesEncoded?: number;
	framesSent?: number;
	encoderImplementation?: string;
	powerEfficientEncoder?: boolean;
}

interface VideoSourceStatsEntry {
	type: string;
	kind?: string;
	trackIdentifier?: string;
	frames?: number;
	framesPerSecond?: number;
}

export interface SoftwareVideoEncoderInfo {
	implementation: string;
	powerEfficientEncoder: boolean | null;
}

export interface StalledVideoEncoderInfo {
	codec: VideoCodec;
	framesEncoded: number;
	framesSent: number | null;
	sourceFrames: number | null;
	sourceFramesPerSecond: number | null;
}

export interface MissingExpectedVideoEncoderInfo {
	reason: 'codec-mismatch';
	codec: VideoCodec;
	activeCodecs: Array<VideoCodec>;
	outboundVideoReports: number;
}

export type ScreenShareEncoderVerificationFailure =
	| (StalledVideoEncoderInfo & {reason: 'stalled'})
	| MissingExpectedVideoEncoderInfo;

function getStatsKind(report: OutboundVideoStatsEntry, codecs: Map<string, CodecStatsEntry>): string | undefined {
	if (report.kind || report.mediaType) return report.kind ?? report.mediaType;
	if (!report.codecId) return undefined;
	const codec = codecs.get(report.codecId);
	return codec?.mimeType?.startsWith('video/') ? 'video' : undefined;
}

function codecMatchesTarget(mimeType: string | undefined, codec: VideoCodec | undefined): boolean {
	if (!codec) return true;
	if (!mimeType) return false;
	const lower = mimeType.toLowerCase();
	if (codec === 'av1') return lower === 'video/av1' || lower === 'video/av1x';
	if (codec === 'h265') return lower === 'video/h265' || lower === 'video/hevc';
	return lower === `video/${codec}`;
}

function getVideoCodecFromMimeType(mimeType: string | undefined): VideoCodec | null {
	const lower = mimeType?.toLowerCase();
	if (!lower?.startsWith('video/')) return null;
	const codec = lower.slice('video/'.length);
	if (codec === 'av1' || codec === 'av1x') return 'av1';
	if (codec === 'h265' || codec === 'hevc') return 'h265';
	if (codec === 'h264') return 'h264';
	if (codec === 'vp9') return 'vp9';
	if (codec === 'vp8') return 'vp8';
	return null;
}

function finiteNumber(value: unknown): number | null {
	return typeof value === 'number' && Number.isFinite(value) ? value : null;
}

export function findSoftwareVideoEncoder(stats: RTCStatsReport, codec?: VideoCodec): SoftwareVideoEncoderInfo | null {
	const codecs = new Map<string, CodecStatsEntry>();
	const reports: Array<OutboundVideoStatsEntry> = [];
	for (const raw of stats.values()) {
		const report = raw as CodecStatsEntry & OutboundVideoStatsEntry;
		if (report.type === 'codec') {
			codecs.set(report.id, report);
		}
		if (report.type === 'outbound-rtp') {
			reports.push(report);
		}
	}
	let softwareEncoder: SoftwareVideoEncoderInfo | null = null;
	for (const report of reports) {
		if (getStatsKind(report, codecs) !== 'video') continue;
		if (report.codecId && !codecMatchesTarget(codecs.get(report.codecId)?.mimeType, codec)) continue;
		const implementation =
			typeof report.encoderImplementation === 'string' && report.encoderImplementation.length > 0
				? report.encoderImplementation
				: null;
		const powerEfficientEncoder =
			typeof report.powerEfficientEncoder === 'boolean' ? report.powerEfficientEncoder : null;
		const acceleration = classifyVideoEncoderAcceleration(implementation, powerEfficientEncoder);
		if (acceleration === 'hardware') return null;
		if (acceleration === 'software' && softwareEncoder === null) {
			softwareEncoder = {
				implementation: implementation ?? UNKNOWN_ENCODER_IMPLEMENTATION,
				powerEfficientEncoder,
			};
		}
	}
	return softwareEncoder;
}

export function shouldTriggerSoftwareEncoderWarning(codec: VideoCodec): boolean {
	if (VoiceSettings.getScreenShareEncoderMode() === 'software') return false;
	return getCodecCapabilityReport()[codec].hardwareAccelerated === 'hardware';
}

export function findStalledVideoEncoder(stats: RTCStatsReport, codec?: VideoCodec): StalledVideoEncoderInfo | null {
	const reportsById = new Map<string, CodecStatsEntry & VideoSourceStatsEntry>();
	const reports: Array<OutboundVideoStatsEntry> = [];
	for (const raw of stats.values()) {
		const report = raw as CodecStatsEntry & VideoSourceStatsEntry & OutboundVideoStatsEntry;
		if (typeof report.id === 'string') {
			reportsById.set(report.id, report);
		}
		if (report.type === 'outbound-rtp') {
			reports.push(report);
		}
	}
	for (const report of reports) {
		if (getStatsKind(report, reportsById) !== 'video') continue;
		const mimeType = report.codecId ? reportsById.get(report.codecId)?.mimeType : undefined;
		if (report.codecId && !codecMatchesTarget(mimeType, codec)) continue;
		const resolvedCodec = codec ?? getVideoCodecFromMimeType(mimeType);
		if (!resolvedCodec) continue;
		const framesEncoded = finiteNumber(report.framesEncoded);
		if (framesEncoded === null || framesEncoded > 0) continue;
		const source = report.mediaSourceId ? reportsById.get(report.mediaSourceId) : undefined;
		const sourceFrames = finiteNumber(source?.frames);
		const sourceFramesPerSecond = finiteNumber(source?.framesPerSecond);
		const sourceIsProducing = (sourceFrames ?? 0) >= 2 || (sourceFramesPerSecond ?? 0) > 0;
		if (!sourceIsProducing) continue;
		return {
			codec: resolvedCodec,
			framesEncoded,
			framesSent: finiteNumber(report.framesSent),
			sourceFrames,
			sourceFramesPerSecond,
		};
	}
	return null;
}

export function findMissingExpectedVideoEncoder(
	stats: RTCStatsReport,
	codec: VideoCodec,
): MissingExpectedVideoEncoderInfo | null {
	const reportsById = new Map<string, CodecStatsEntry>();
	const reports: Array<OutboundVideoStatsEntry> = [];
	for (const raw of stats.values()) {
		const report = raw as CodecStatsEntry & OutboundVideoStatsEntry;
		if (typeof report.id === 'string') {
			reportsById.set(report.id, report);
		}
		if (report.type === 'outbound-rtp') {
			reports.push(report);
		}
	}
	const activeCodecs = new Set<VideoCodec>();
	let outboundVideoReports = 0;
	for (const report of reports) {
		if (getStatsKind(report, reportsById) !== 'video') continue;
		outboundVideoReports++;
		const mimeType = report.codecId ? reportsById.get(report.codecId)?.mimeType : undefined;
		if (codecMatchesTarget(mimeType, codec)) return null;
		const activeCodec = getVideoCodecFromMimeType(mimeType);
		if (activeCodec) {
			activeCodecs.add(activeCodec);
		}
	}
	if (outboundVideoReports === 0 || activeCodecs.size === 0) return null;
	return {
		reason: 'codec-mismatch',
		codec,
		activeCodecs: Array.from(activeCodecs),
		outboundVideoReports,
	};
}

export function scheduleScreenShareEncoderVerification(
	getStats: () => Promise<RTCStatsReport>,
	codec: VideoCodec,
	onEncodeFailure?: (failure: ScreenShareEncoderVerificationFailure) => void,
): NodeJS.Timeout {
	return setTimeout(async () => {
		try {
			const expectedHardware = getCodecCapabilityReport()[codec].hardwareAccelerated;
			const stats = await getStats();
			const stalledEncoder = findStalledVideoEncoder(stats, codec);
			if (stalledEncoder) {
				logger.warn('Screen share video encode is stalled', stalledEncoder);
				onEncodeFailure?.({...stalledEncoder, reason: 'stalled'});
				return;
			}
			const missingExpectedEncoder = findMissingExpectedVideoEncoder(stats, codec);
			if (missingExpectedEncoder) {
				logger.warn('Screen share video encoder is using a different codec than requested', missingExpectedEncoder);
				onEncodeFailure?.(missingExpectedEncoder);
				return;
			}
			const encoder = findSoftwareVideoEncoder(stats, codec);
			if (encoder) {
				logger.warn('Screen share is using a software encoder', {
					codec,
					encoderImplementation: encoder.implementation,
					powerEfficientEncoder: encoder.powerEfficientEncoder,
					expectedHardware,
				});
				if (shouldTriggerSoftwareEncoderWarning(codec)) {
					SoftwareEncoderWarning.triggerWarning(codec, encoder.implementation);
				}
			} else {
				logger.info('Screen share encoder verified', {codec});
			}
		} catch (error) {
			logger.debug('Failed to verify screen share encoder', {error});
		}
	}, ENCODER_VERIFICATION_DELAY_MS);
}
