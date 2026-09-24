// SPDX-License-Identifier: AGPL-3.0-or-later

import {getDesktopTroubleshootingSettings} from '@app/features/devtools/utils/DesktopTroubleshootingUtils';
import {Logger} from '@app/features/platform/utils/AppLogger';
import ScreenShareCodecNegotiation from '@app/features/voice/engine/ScreenShareCodecNegotiation';
import ActiveScreenShareSource from '@app/features/voice/state/ActiveScreenShareSource';
import SoftwareEncoderWarning from '@app/features/voice/state/SoftwareEncoderWarning';
import VoiceSettings from '@app/features/voice/state/VoiceSettings';
import {
	getCodecCapabilityReport,
	LIVEKIT_SUPPORTED_CODECS,
	markScreenShareCodecSoftwareEncodeObserved,
	resolveVideoPublishCodecPolicy,
	type VideoPublishCodecPolicy,
} from '@app/features/voice/utils/CodecCapabilityDetector';
import {loadGpuEncoderReport} from '@app/features/voice/utils/GpuEncoderCapabilities';
import {loadNativeHardwareEncoderCapabilities} from '@app/features/voice/utils/NativeHardwareEncoderCapabilities';
import {
	buildScreenShareSenderParameters,
	classifyScreenShareLimit,
	resolveScreenShareDegradationPreference,
	resolveScreenShareLayering,
	resolveScreenShareSenderCodec,
	resolveScreenShareTarget,
	SCREEN_SHARE_MAX_VIDEO_BITRATE_BPS,
	type ScreenShareContext,
	type ScreenShareLayering,
	type ScreenShareLimitClass,
	type ScreenShareTarget,
} from '@app/features/voice/utils/ScreenShareOptions';
import {ScreenShareRollbackIncompleteError} from '@app/features/voice/utils/ScreenShareRollbackIncompleteError';
import {classifyVideoEncoderAcceleration} from '@app/features/voice/utils/VideoAccelerationClassification';
import {hasHigherVideoQuality} from '@app/features/voice/utils/VideoQualityEntitlement';
import {
	BackupCodecPolicy,
	type LocalParticipant,
	type LocalTrackPublication,
	type LocalVideoTrack,
	type ScreenShareCaptureOptions,
	Track,
	type TrackPublishOptions,
	type VideoCodec,
	type VideoEncoding,
} from 'livekit-client';

export const logger = new Logger('VoiceEngineV2ScreenShareSupport');
const GPU_ENCODER_REPORT_START_TIMEOUT_MS = 500;
const SCREEN_SHARE_MONITOR_FIRST_TICK_MS = 2500;
const SCREEN_SHARE_MONITOR_TICK_MS = 2000;

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
	displayCapture?: DisplayScreenShareCaptureContext;
}

export interface DisplayScreenShareCaptureContext {
	sourceId: string | null;
	displayShareEnvironment: string | null;
	requireAudio: boolean;
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

export interface ScreenShareSenderCleanupTarget {
	sender: RTCRtpSender;
	expectedTrack: MediaStreamTrack | null;
}

export interface ScreenShareCaptureCleanupSnapshot {
	mediaTracks: Array<MediaStreamTrack>;
	senders: Array<ScreenShareSenderCleanupTarget>;
}

interface ScreenShareTrackLike {
	mediaStream?: MediaStream;
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

function pushUniqueSender(snapshot: ScreenShareCaptureCleanupSnapshot, sender: RTCRtpSender | undefined): void {
	if (!sender) return;
	const expectedTrack = sender.track;
	if (snapshot.senders.some((target) => target.sender === sender && target.expectedTrack === expectedTrack)) return;
	snapshot.senders.push({sender, expectedTrack});
}

function captureScreenShareTrackCleanup(
	snapshot: ScreenShareCaptureCleanupSnapshot,
	track: ScreenShareTrackLike | undefined | null,
): void {
	for (const mediaStreamTrack of track?.mediaStream?.getTracks() ?? []) {
		pushUnique(snapshot.mediaTracks, mediaStreamTrack);
	}
	pushUnique(snapshot.mediaTracks, track?.mediaStreamTrack);
	pushUniqueSender(snapshot, track?.sender);
	for (const simulcastTrackInfo of track?.simulcastCodecs?.values() ?? []) {
		pushUnique(snapshot.mediaTracks, simulcastTrackInfo.mediaStreamTrack);
		pushUniqueSender(snapshot, simulcastTrackInfo.sender);
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
		for (const senderTarget of snapshot?.senders ?? []) {
			if (
				!merged.senders.some(
					(target) => target.sender === senderTarget.sender && target.expectedTrack === senderTarget.expectedTrack,
				)
			) {
				merged.senders.push(senderTarget);
			}
		}
	}
	return merged;
}

async function detachScreenShareSender(target: ScreenShareSenderCleanupTarget): Promise<void> {
	if (target.sender.track !== target.expectedTrack) return;
	if (target.sender.transport?.state === 'closed') return;
	await target.sender.replaceTrack(null);
}

export async function releaseScreenShareCaptureCleanup(snapshot: ScreenShareCaptureCleanupSnapshot): Promise<void> {
	const cleanupErrors: Array<unknown> = [];
	const senderResults = await Promise.allSettled(snapshot.senders.map(detachScreenShareSender));
	for (const result of senderResults) {
		if (result.status === 'rejected') cleanupErrors.push(result.reason);
	}
	for (const mediaTrack of snapshot.mediaTracks) {
		try {
			mediaTrack.stop();
		} catch (error) {
			cleanupErrors.push(error);
		}
		if (mediaTrack.readyState === 'live') {
			cleanupErrors.push(new Error('Screen share capture track remained live after cleanup'));
		}
	}
	if (cleanupErrors.length > 0) {
		throw new ScreenShareRollbackIncompleteError(cleanupErrors);
	}
}

function clampScreenShareEncoding(encoding: VideoEncoding): VideoEncoding {
	return {
		...encoding,
		maxBitrate:
			typeof encoding.maxBitrate === 'number'
				? Math.min(encoding.maxBitrate, SCREEN_SHARE_MAX_VIDEO_BITRATE_BPS)
				: encoding.maxBitrate,
		priority: encoding.priority ?? 'high',
	};
}

export function resolveActiveScreenShareContext(): ScreenShareContext {
	return ActiveScreenShareSource.getShareContext() ?? 'display';
}

export function resolveConfiguredScreenShareTarget(
	context: ScreenShareContext,
	sourceDimensions: {width: number; height: number} | null,
): ScreenShareTarget {
	return resolveScreenShareTarget({
		mode: VoiceSettings.getStreamingMode(),
		storedResolution: VoiceSettings.getScreenshareResolution(),
		storedFrameRate: VoiceSettings.getVideoFrameRate(),
		entitled: hasHigherVideoQuality(),
		context,
		sourceDimensions,
		hintSetting: VoiceSettings.getScreenShareContentHint(),
		codec: getPreferredScreenShareCodec(),
		softwareEncoderClamp: ActiveScreenShareSource.isSoftwareEncoderClamped(),
	});
}

export function resolveActiveScreenShareTarget(
	context: ScreenShareContext = resolveActiveScreenShareContext(),
): ScreenShareTarget {
	return resolveConfiguredScreenShareTarget(context, ActiveScreenShareSource.getSourceDimensions());
}

function recommitClampedScreenShareTarget(committed: ScreenShareTarget): ScreenShareTarget {
	if (committed.softwareEncoderClamped) return committed;
	const resolved = resolveActiveScreenShareTarget(committed.context);
	if (!resolved.softwareEncoderClamped) return committed;
	ActiveScreenShareSource.setTarget(resolved);
	return ActiveScreenShareSource.getTarget() ?? resolved;
}

export function ensureCommittedScreenShareTarget(): ScreenShareTarget {
	const committed = ActiveScreenShareSource.getTarget();
	if (committed !== null) return committed;
	const target = resolveActiveScreenShareTarget();
	ActiveScreenShareSource.setTarget(target);
	return target;
}

export function getStatsKind(
	report: {kind?: string; mediaType?: string; codecId?: string},
	reportsById: ReadonlyMap<string, {mimeType?: string}>,
): string | undefined {
	if (report.kind || report.mediaType) return report.kind ?? report.mediaType;
	if (!report.codecId) return undefined;
	const codec = reportsById.get(report.codecId);
	return codec?.mimeType?.startsWith('video/') ? 'video' : undefined;
}

function resolveScreenShareEncoding(target: ScreenShareTarget, publishOptions?: TrackPublishOptions): VideoEncoding {
	if (publishOptions?.screenShareEncoding) {
		return clampScreenShareEncoding(publishOptions.screenShareEncoding);
	}
	return {maxBitrate: target.maxBitrate, maxFramerate: target.frameRate, priority: 'high'};
}

function getScreenShareLayeringForCodec(codec: VideoCodec | undefined): ScreenShareLayering {
	return resolveScreenShareLayering({
		codec,
		svcSetting: VoiceSettings.getScreenShareScalabilityModeOverride(),
	});
}

function resolveBackupCodecWithinPolicy(
	configured: TrackPublishOptions['backupCodec'] | undefined,
	policy: VideoPublishCodecPolicy,
): TrackPublishOptions['backupCodec'] {
	if (configured === undefined || configured === true) return policy.backupCodec;
	if (configured === false) return false;
	return configured.codec !== policy.primary && policy.allowed.includes(configured.codec) ? configured : false;
}

async function waitForScreenShareCapabilityLoads(): Promise<'loaded' | 'timeout'> {
	let timeoutId: NodeJS.Timeout | undefined;
	const timeout = new Promise<'timeout'>((resolve) => {
		timeoutId = setTimeout(() => resolve('timeout'), GPU_ENCODER_REPORT_START_TIMEOUT_MS);
	});
	try {
		return await Promise.race([
			Promise.all([
				loadGpuEncoderReport(),
				loadNativeHardwareEncoderCapabilities(),
				getDesktopTroubleshootingSettings(),
			]).then(() => 'loaded' as const),
			timeout,
		]);
	} finally {
		if (timeoutId !== undefined) {
			clearTimeout(timeoutId);
		}
	}
}

async function waitForGpuEncoderReportForPublish(options?: ScreenSharePublishOptionsResolutionOptions): Promise<void> {
	options?.onCodecReadiness?.('loading');
	const result = await waitForScreenShareCapabilityLoads();
	if (result === 'timeout') {
		options?.onCodecReadiness?.('timeout');
		logger.warn('GPU encoder report timed out before screen share publish; using cached codec capabilities', {
			timeoutMs: GPU_ENCODER_REPORT_START_TIMEOUT_MS,
		});
	} else {
		options?.onCodecReadiness?.('ready');
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
	const committed = ensureCommittedScreenShareTarget();
	await waitForGpuEncoderReportForPublish(options);
	const target = recommitClampedScreenShareTarget(committed);
	const policy = resolveVideoPublishCodecPolicy(publishOptions?.videoCodec ?? getPreferredScreenShareCodec());
	const preferredVideoCodec = policy.primary;
	const backupCodec = resolveBackupCodecWithinPolicy(publishOptions?.backupCodec, policy);
	const backupCodecPolicy =
		publishOptions?.backupCodecPolicy ?? (backupCodec ? BackupCodecPolicy.SIMULCAST : undefined);
	const layering = getScreenShareLayeringForCodec(preferredVideoCodec);
	if (target.softwareEncoderClamped && VoiceSettings.getScreenShareEncoderMode() !== 'software') {
		logger.warn('Screen share target clamped to the software H.264 budget', {
			codec: preferredVideoCodec,
			resolution: target.resolution,
			frameRate: target.frameRate,
		});
		SoftwareEncoderWarning.triggerWarning(preferredVideoCodec, UNKNOWN_ENCODER_IMPLEMENTATION);
	}
	return {
		...publishOptions,
		videoCodec: preferredVideoCodec,
		screenShareEncoding: resolveScreenShareEncoding(target, publishOptions),
		degradationPreference: resolveScreenShareDegradationPreference(target),
		simulcast: layering.simulcast,
		scalabilityMode: layering.scalabilityMode,
		backupCodec,
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

export function getNegotiatedSenderVideoCodec(sender: RTCRtpSender | undefined): VideoCodec | undefined {
	const mimeType = sender?.getParameters().codecs?.[0]?.mimeType?.toLowerCase();
	if (!mimeType) return undefined;
	const codec = mimeType.replace('video/', '');
	const normalized = codec === 'av1x' ? 'av1' : codec;
	return LIVEKIT_SUPPORTED_CODECS.includes(normalized as VideoCodec) ? (normalized as VideoCodec) : undefined;
}

export function getPublishedScreenShareMaxBitrateBps(
	participant: LocalParticipant | null | undefined,
): number | undefined {
	const sender = participant?.getTrackPublication(Track.Source.ScreenShare)?.videoTrack?.sender;
	if (!sender) return undefined;
	let total = 0;
	for (const encoding of sender.getParameters().encodings ?? []) {
		if (typeof encoding.maxBitrate !== 'number') return undefined;
		total += encoding.maxBitrate;
	}
	return total > 0 ? total : undefined;
}

export interface ScreenShareSenderCodecOptions {
	codecOverride?: VideoCodec;
	publishedCodec?: VideoCodec;
}

function readScreenShareCaptureSize(sender: RTCRtpSender): {width: number; height: number} | null {
	const settings = sender.track?.getSettings();
	if (!settings?.width || !settings.height) return null;
	return {width: settings.width, height: settings.height};
}

function isSameScreenShareEncoding(current: RTCRtpEncodingParameters, next: RTCRtpEncodingParameters): boolean {
	return (
		current.maxBitrate === next.maxBitrate &&
		current.maxFramerate === next.maxFramerate &&
		current.scaleResolutionDownBy === next.scaleResolutionDownBy &&
		current.priority === next.priority &&
		current.networkPriority === next.networkPriority &&
		current.scalabilityMode === next.scalabilityMode
	);
}

interface ScreenShareSenderEnforcement {
	applied: boolean;
}

export async function enforceScreenShareSenderParameters(
	sender: RTCRtpSender,
	target: ScreenShareTarget,
	options: ScreenShareSenderCodecOptions = {},
): Promise<ScreenShareSenderEnforcement> {
	try {
		const codec = resolveScreenShareSenderCodec(
			options.codecOverride,
			getNegotiatedSenderVideoCodec(sender),
			options.publishedCodec,
		);
		const params = sender.getParameters();
		const encodings = params.encodings?.length ? params.encodings : [{}];
		const next = buildScreenShareSenderParameters({
			encodings,
			target,
			capture: readScreenShareCaptureSize(sender),
			scalabilityMode: getScreenShareLayeringForCodec(codec).scalabilityMode,
		});
		const applied = next.encodings.map((encoding, index) =>
			encodings[index].active === false ? encodings[index] : encoding,
		);
		const inSync =
			params.degradationPreference === next.degradationPreference &&
			params.encodings?.length === applied.length &&
			applied.every((encoding, index) => isSameScreenShareEncoding(encodings[index], encoding));
		if (inSync) return {applied: true};
		params.degradationPreference = next.degradationPreference;
		params.encodings = applied;
		await sender.setParameters(params);
		return {applied: true};
	} catch (error) {
		logger.warn('Failed to enforce screen share sender parameters', {error, target});
		return {applied: false};
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
	active?: boolean;
	framesEncoded?: number;
	framesSent?: number;
	bytesSent?: number;
	headerBytesSent?: number;
	totalEncodeTime?: number;
	targetBitrate?: number;
	qualityLimitationReason?: string;
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
	const encoderMode = VoiceSettings.getScreenShareEncoderMode();
	if (encoderMode === 'software') return false;
	if (encoderMode === 'hardware') return true;
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
		if (report.active === false) continue;
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

interface ScreenShareSendSnapshot {
	timestampMs: number;
	framesEncoded: number;
	encodeTimeMs: number;
	bytesSent: number;
	targetBitrateBps: number | null;
	cpuLimited: boolean;
	sourceFramesPerSecond: number | null;
}

function collectScreenShareSendSnapshot(stats: RTCStatsReport): ScreenShareSendSnapshot {
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
	const snapshot: ScreenShareSendSnapshot = {
		timestampMs: Date.now(),
		framesEncoded: 0,
		encodeTimeMs: 0,
		bytesSent: 0,
		targetBitrateBps: null,
		cpuLimited: false,
		sourceFramesPerSecond: null,
	};
	for (const report of reports) {
		if (getStatsKind(report, reportsById) !== 'video') continue;
		if (report.active === false) continue;
		snapshot.framesEncoded += finiteNumber(report.framesEncoded) ?? 0;
		snapshot.encodeTimeMs += (finiteNumber(report.totalEncodeTime) ?? 0) * 1000;
		snapshot.bytesSent += (finiteNumber(report.bytesSent) ?? 0) + (finiteNumber(report.headerBytesSent) ?? 0);
		const targetBitrate = finiteNumber(report.targetBitrate);
		if (targetBitrate !== null) {
			snapshot.targetBitrateBps = (snapshot.targetBitrateBps ?? 0) + targetBitrate;
		}
		if (report.qualityLimitationReason === 'cpu') {
			snapshot.cpuLimited = true;
		}
		const source = report.mediaSourceId ? reportsById.get(report.mediaSourceId) : undefined;
		const sourceFramesPerSecond = finiteNumber(source?.framesPerSecond);
		if (sourceFramesPerSecond !== null) {
			snapshot.sourceFramesPerSecond = sourceFramesPerSecond;
		}
	}
	return snapshot;
}

function classifyScreenShareSendLimit(
	previous: ScreenShareSendSnapshot | null,
	current: ScreenShareSendSnapshot,
	cpuLimitedTicks: number,
): ScreenShareLimitClass | null {
	const target = ActiveScreenShareSource.getTarget();
	if (previous === null || target === null) return null;
	return classifyScreenShareLimit({
		frameRate: target.frameRate,
		maxBitrate: target.maxBitrate,
		elapsedMs: current.timestampMs - previous.timestampMs,
		framesEncoded: current.framesEncoded - previous.framesEncoded,
		encodeTimeMs: current.encodeTimeMs - previous.encodeTimeMs,
		bytesSent: current.bytesSent - previous.bytesSent,
		targetBitrateBps: current.targetBitrateBps,
		sourceFramesPerSecond: current.sourceFramesPerSecond,
		cpuLimitedTicks,
	});
}

function syncScreenShareCaptureSize(sender: RTCRtpSender): void {
	if (ActiveScreenShareSource.getTarget() === null) return;
	const capture = readScreenShareCaptureSize(sender);
	if (capture === null) return;
	const known = ActiveScreenShareSource.getSourceDimensions();
	if (known !== null && known.width === capture.width && known.height === capture.height) return;
	ActiveScreenShareSource.setSourceDimensions(capture);
	ActiveScreenShareSource.setTarget(resolveActiveScreenShareTarget());
	logger.info('Screen share capture size changed; re-resolved the publish target', capture);
}

function countEncodedVideoFrames(stats: RTCStatsReport): number | null {
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
	let total: number | null = null;
	for (const report of reports) {
		if (getStatsKind(report, reportsById) !== 'video') continue;
		const framesEncoded = finiteNumber(report.framesEncoded);
		if (framesEncoded === null) continue;
		total = (total ?? 0) + framesEncoded;
	}
	return total;
}

function verifyScreenShareEncoderStart(
	stats: RTCStatsReport,
	codec: VideoCodec,
	onEncodeFailure: (failure: ScreenShareEncoderVerificationFailure) => void,
): ScreenShareEncoderVerification {
	const expectedHardware = getCodecCapabilityReport()[codec].hardwareAccelerated;
	const stalledEncoder = findStalledVideoEncoder(stats, codec);
	if (stalledEncoder) {
		logger.warn('Screen share video encode is stalled', stalledEncoder);
		onEncodeFailure({...stalledEncoder, reason: 'stalled'});
		return 'failed';
	}
	const missingExpectedEncoder = findMissingExpectedVideoEncoder(stats, codec);
	if (missingExpectedEncoder) {
		logger.warn('Screen share video encoder is using a different codec than requested', missingExpectedEncoder);
		onEncodeFailure(missingExpectedEncoder);
		return 'failed';
	}
	if ((countEncodedVideoFrames(stats) ?? 0) === 0) return 'inconclusive';
	const encoder = findSoftwareVideoEncoder(stats, codec);
	if (!encoder) {
		logger.info('Screen share encoder verified', {codec});
		return 'verified';
	}
	logger.warn('Screen share is using a software encoder', {
		codec,
		encoderImplementation: encoder.implementation,
		powerEfficientEncoder: encoder.powerEfficientEncoder,
		expectedHardware,
	});
	const warnAboutSoftwareEncoder = shouldTriggerSoftwareEncoderWarning(codec);
	if (VoiceSettings.getScreenShareEncoderMode() !== 'software') {
		markScreenShareCodecSoftwareEncodeObserved(codec);
	}
	if (warnAboutSoftwareEncoder) {
		SoftwareEncoderWarning.triggerWarning(codec, encoder.implementation);
	}
	return 'verified';
}

export function getScreenShareBackupSenders(track: LocalVideoTrack): Array<{sender: RTCRtpSender; codec: unknown}> {
	const simulcastCodecs = (track as LocalVideoTrack & {simulcastCodecs?: Map<unknown, SimulcastTrackInfoLike>})
		.simulcastCodecs;
	const senders: Array<{sender: RTCRtpSender; codec: unknown}> = [];
	for (const [codec, simulcastTrackInfo] of simulcastCodecs ?? []) {
		if (simulcastTrackInfo.sender) senders.push({sender: simulcastTrackInfo.sender, codec});
	}
	return senders;
}

type ScreenShareEncoderVerification = 'failed' | 'verified' | 'inconclusive';

export interface ScreenShareEncoderMonitorOptions {
	track: LocalVideoTrack;
	codec: VideoCodec;
	onEncodeFailure: (failure: ScreenShareEncoderVerificationFailure) => void;
	reEnforce: () => Promise<void>;
}

export function startScreenShareEncoderMonitor(options: ScreenShareEncoderMonitorOptions): () => void {
	let cancelled = false;
	let timer: ReturnType<typeof setTimeout> | undefined;
	let verified = false;
	let framesEncoded: number | null = null;
	let sendSnapshot: ScreenShareSendSnapshot | null = null;
	let cpuLimitedTicks = 0;
	const tick = async (): Promise<void> => {
		const sender = options.track.sender;
		if (!sender) return;
		const stats = await sender.getStats();
		syncScreenShareCaptureSize(sender);
		const encoded = countEncodedVideoFrames(stats);
		ActiveScreenShareSource.setEncoding(encoded !== null && framesEncoded !== null && encoded > framesEncoded);
		framesEncoded = encoded;
		const snapshot = collectScreenShareSendSnapshot(stats);
		cpuLimitedTicks = snapshot.cpuLimited ? cpuLimitedTicks + 1 : 0;
		const limit = classifyScreenShareSendLimit(sendSnapshot, snapshot, cpuLimitedTicks);
		sendSnapshot = snapshot;
		if (limit !== ActiveScreenShareSource.getLimit()) {
			logger.info('Screen share send limit changed', {limit, codec: options.codec});
		}
		ActiveScreenShareSource.setLimit(limit);
		if (!verified) {
			const verification = verifyScreenShareEncoderStart(stats, options.codec, options.onEncodeFailure);
			if (verification === 'failed') return;
			verified = verification === 'verified';
		}
		await options.reEnforce();
	};
	const schedule = (delayMs: number): void => {
		timer = setTimeout(() => {
			void tick()
				.catch((error) => {
					logger.warn('Failed to verify the screen share encoder', {error});
				})
				.finally(() => {
					if (!cancelled) schedule(SCREEN_SHARE_MONITOR_TICK_MS);
				});
		}, delayMs);
	};
	schedule(SCREEN_SHARE_MONITOR_FIRST_TICK_MS);
	return () => {
		cancelled = true;
		clearTimeout(timer);
		ActiveScreenShareSource.setEncoding(false);
		ActiveScreenShareSource.setLimit(null);
	};
}
