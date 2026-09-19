// SPDX-License-Identifier: AGPL-3.0-or-later

import {getCachedDesktopTroubleshootingSettings} from '@app/features/devtools/utils/DesktopTroubleshootingUtils';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {
	guessPlatform,
	isChromiumBrowser,
	isDesktop,
	isFirefoxBrowser,
	type NativePlatform,
} from '@app/features/ui/utils/NativeUtils';
import VoiceSettings from '@app/features/voice/state/VoiceSettings';
import {getGpuEncoderReportSync, type HardwareEncodeAnswer} from '@app/features/voice/utils/GpuEncoderCapabilities';
import {
	getNativeHardwareEncoderCapabilitiesSync,
	hasNativeHardwareEncoder,
	resetNativeHardwareEncoderCapabilities,
} from '@app/features/voice/utils/NativeHardwareEncoderCapabilities';
import {getOpenH264StatusSync, resetOpenH264Status} from '@app/features/voice/utils/OpenH264Status';
import {
	LAST_RESORT_VIDEO_CODEC,
	rankScreenShareCodecs,
	type ScreenShareCodecBrowser,
	type ScreenShareCodecProfile,
	type ScreenShareCodecProfileEntry,
	type ScreenShareCodecRanking,
} from '@app/features/voice/utils/ScreenShareCodecSelection';
import {normaliseStreamingModeForContext} from '@app/features/voice/utils/ScreenShareOptions';
import {isVideoCodecDecodeExcluded} from '@app/features/voice/utils/VideoDecoderCapabilities';
import type {TrackPublishDefaults, TrackPublishOptions} from 'livekit-client';
import {BackupCodecPolicy, supportsVideoCodec, type VideoCodec, type VideoEncoding} from 'livekit-client';

const logger = new Logger('CodecCapabilityDetector');
export const LIVEKIT_SUPPORTED_CODECS: ReadonlyArray<VideoCodec> = ['vp8', 'h264', 'vp9', 'av1', 'h265'];
const PUBLISH_CODEC_FALLBACK_ORDER: ReadonlyArray<VideoCodec> = ['h264', 'vp9', 'vp8', 'av1', 'h265'];

export interface CodecCapabilities {
	vp8: boolean;
	vp9: boolean;
	h264: boolean;
	h265: boolean;
	av1: boolean;
}

export type CodecSupportReason =
	| 'supported'
	| 'unsupported-browser'
	| 'unsupported-system'
	| 'unavailable-on-platform'
	| 'capabilities-unavailable'
	| 'opt-in-required'
	| 'runtime-failed';

export interface CodecSupportInfo {
	supported: boolean;
	reason: CodecSupportReason;
	detail: string;
	hardwareAccelerated: HardwareEncodeAnswer;
}

export interface CodecCapabilityReport {
	vp8: CodecSupportInfo;
	vp9: CodecSupportInfo;
	h264: CodecSupportInfo;
	h265: CodecSupportInfo;
	av1: CodecSupportInfo;
}

export type CodecPreference = 'auto' | VideoCodec;
export type ScreenShareEncoderMode = 'auto' | 'hardware' | 'software';
export type ScreenShareScalabilityModePreference = 'auto' | 'single_layer' | 'temporal';
export type AutomaticScreenShareCodecReason =
	| 'firefox-vp8'
	| 'non-chromium-h264'
	| 'non-chromium-vp8'
	| `hardware-${VideoCodec}`
	| `software-${VideoCodec}`;

export interface AutomaticScreenShareCodecSelection {
	codec: VideoCodec;
	reason: AutomaticScreenShareCodecReason;
}

export interface ScreenShareEncoderPath {
	codec: VideoCodec;
	hardwareUnavailable: boolean;
}

export type ScreenShareContentHint = 'auto' | 'detail' | 'motion' | 'text';

let cachedCapabilities: CodecCapabilities | null = null;
let cachedReport: CodecCapabilityReport | null = null;
let cachedReportGpuKey: object | null | undefined;
let cachedReportNativeHardwareEncoderKey: object | null | undefined;
let cachedReportHardwareAccelerationDisabled: boolean | undefined;
let cachedReportAv1OptIn: boolean | undefined;
let cachedReportHevcOptIn: boolean | undefined;
const runtimeEncodeFailureCodecs = new Set<VideoCodec>();
const observedSoftwareEncodeCodecs = new Set<VideoCodec>();

interface RawProbeResult {
	caps: CodecCapabilities;
	probedSuccessfully: boolean;
}

interface CodecPolicyContext {
	platform: NativePlatform;
	firefox: boolean;
	chromium: boolean;
}

function probeRawCapabilities(): RawProbeResult {
	const caps: CodecCapabilities = {
		vp8: false,
		vp9: false,
		h264: false,
		h265: false,
		av1: false,
	};
	let probedSuccessfully = false;
	try {
		const capabilities = RTCRtpSender.getCapabilities?.('video');
		if (!capabilities) {
			logger.debug('RTCRtpSender.getCapabilities not available; assuming baseline codec support only');
			caps.vp8 = true;
			caps.h264 = true;
		} else {
			const mimeTypes = new Set(capabilities.codecs.map((c) => c.mimeType.toLowerCase()));
			caps.vp8 = mimeTypes.has('video/vp8');
			caps.vp9 = mimeTypes.has('video/vp9');
			caps.h264 = mimeTypes.has('video/h264');
			caps.h265 = mimeTypes.has('video/h265');
			caps.av1 = mimeTypes.has('video/av1') || mimeTypes.has('video/av1x');
			probedSuccessfully = true;
		}
	} catch (error) {
		logger.warn('Failed to probe codec capabilities, assuming VP8/H.264 only', {error});
		caps.vp8 = true;
		caps.h264 = true;
	}
	const openH264 = getOpenH264StatusSync();
	if (openH264?.enabled && openH264.downloaded && !caps.h264) {
		caps.h264 = true;
		logger.info('H.264 force-enabled via OpenH264 software codec');
	}
	if (probedSuccessfully) {
		logger.info('Codec capabilities probed', {capabilities: caps});
	}
	return {caps, probedSuccessfully};
}

function probeEncodingCapabilities(): CodecCapabilities {
	if (cachedCapabilities) return cachedCapabilities;
	const {caps} = probeRawCapabilities();
	cachedCapabilities = caps;
	return caps;
}

function isDesktopHardwareAccelerationDisabled(): boolean {
	return isDesktop() && getCachedDesktopTroubleshootingSettings()?.disableHardwareAcceleration === true;
}

function buildScreenShareCodecPolicyContext(): CodecPolicyContext {
	return {
		platform: guessPlatform(),
		firefox: isFirefoxBrowser(),
		chromium: isChromiumBrowser(),
	};
}

function getScreenShareCodecPolicyUnsupported(
	codec: keyof CodecCapabilities,
	context: CodecPolicyContext,
): Omit<CodecSupportInfo, 'hardwareAccelerated'> | null {
	if (codec === 'av1' && !VoiceSettings.getScreenShareAv1OptIn()) {
		return {
			supported: false,
			reason: 'opt-in-required',
			detail:
				'AV1 screen sharing is off by default because it may cause compatibility issues for viewers. We’re working on improving this. Turn on AV1 screen sharing in Advanced settings to use it.',
		};
	}
	if (codec === 'h265' && !VoiceSettings.getScreenShareHevcOptIn()) {
		return {
			supported: false,
			reason: 'opt-in-required',
			detail:
				'H.265 (HEVC) screen sharing is off by default because it may cause compatibility issues for viewers. We’re working on improving this. Turn on H.265 screen sharing in Advanced settings to use it.',
		};
	}
	if (context.firefox) {
		switch (codec) {
			case 'av1':
				return {
					supported: false,
					reason: 'unsupported-browser',
					detail: 'Firefox doesn\u2019t support AV1 encoding for WebRTC yet.',
				};
			case 'vp9':
				return {
					supported: false,
					reason: 'unsupported-browser',
					detail: 'Firefox\u2019s WebRTC stack doesn\u2019t expose VP9 as a publishable codec.',
				};
			case 'h265':
				return {
					supported: false,
					reason: 'unsupported-browser',
					detail: 'Firefox doesn\u2019t support H.265 encoding for WebRTC.',
				};
			default:
				break;
		}
	}
	return null;
}

function hasPublishPathNativeHardwareEncoder(codec: VideoCodec): boolean {
	if (!hasNativeHardwareEncoder(codec)) return false;
	const backend = getNativeHardwareEncoderCapabilitiesSync()?.backend;
	return backend !== 'videotoolbox' && backend !== 'nvenc';
}

function buildReport(): CodecCapabilityReport {
	const {caps, probedSuccessfully} = probeRawCapabilities();
	const context = buildScreenShareCodecPolicyContext();
	const gpuReport = getGpuEncoderReportSync();
	const nativeHardwareEncoder = getNativeHardwareEncoderCapabilitiesSync();
	const linuxNvidiaWebRtcEncodeLimited =
		context.platform === 'linux' && gpuReport?.gpuFamily?.startsWith('nvidia-') === true;
	const hardwareAccelerationDisabled = isDesktopHardwareAccelerationDisabled();
	function hwAccel(codec: keyof CodecCapabilities): HardwareEncodeAnswer {
		if (hardwareAccelerationDisabled) {
			return 'software';
		}
		if (observedSoftwareEncodeCodecs.has(codec)) {
			return 'software';
		}
		if (hasPublishPathNativeHardwareEncoder(codec)) {
			return 'hardware';
		}
		return gpuReport ? gpuReport[codec] : 'unknown';
	}
	type DescribedUnsupported = Omit<CodecSupportInfo, 'hardwareAccelerated'>;
	function unsupported(codec: keyof CodecCapabilities, info: DescribedUnsupported): CodecSupportInfo {
		return {...info, hardwareAccelerated: hwAccel(codec)};
	}
	function describe(codec: keyof CodecCapabilities): CodecSupportInfo {
		const policyUnsupported = getScreenShareCodecPolicyUnsupported(codec, context);
		if (policyUnsupported) {
			return unsupported(codec, policyUnsupported);
		}
		if (runtimeEncodeFailureCodecs.has(codec)) {
			return unsupported(codec, {
				supported: false,
				reason: 'runtime-failed',
				detail: 'This codec failed while publishing during the current session.',
			});
		}
		const supportedByNativeHardware = hasPublishPathNativeHardwareEncoder(codec);
		if (caps[codec] || supportedByNativeHardware) {
			return {
				supported: true,
				reason: 'supported',
				detail: supportedByNativeHardware
					? 'Available through the native hardware encoder.'
					: linuxNvidiaWebRtcEncodeLimited && hwAccel(codec) === 'software'
						? 'Available through Chromium software encoding. NVIDIA NVENC is not exposed to WebRTC on Linux.'
						: 'Available on your system.',
				hardwareAccelerated: hwAccel(codec),
			};
		}
		if (!probedSuccessfully) {
			return unsupported(codec, {
				supported: false,
				reason: 'capabilities-unavailable',
				detail:
					'WebRTC capability probing is unavailable in this browser; only baseline VP8/H.264 are assumed available.',
			});
		}
		if (codec === 'h265') {
			const nativeNvencDetail =
				nativeHardwareEncoder?.backend === 'nvenc' && nativeHardwareEncoder.reason
					? ` Native NVENC is unavailable: ${nativeHardwareEncoder.reason}.`
					: '';
			return unsupported(codec, {
				supported: false,
				reason: 'unsupported-system',
				detail:
					context.chromium && linuxNvidiaWebRtcEncodeLimited
						? `Chromium on Linux does not expose NVIDIA NVENC to WebRTC, and the NVIDIA VA-API bridge used by Chromium does not support encoding.${nativeNvencDetail}`
						: context.chromium
							? 'Your browser/system doesn\u2019t expose an H.265 (HEVC) encoder. H.265 needs hardware support on most platforms.'
							: 'H.265 (HEVC) encoding requires a Chromium-based browser with hardware HEVC support.',
			});
		}
		if (codec === 'av1') {
			return unsupported(codec, {
				supported: false,
				reason: 'unsupported-system',
				detail:
					'Your browser/system doesn\u2019t expose an AV1 encoder. AV1 needs hardware support or recent Chromium with software fallback.',
			});
		}
		if (codec === 'vp9') {
			return unsupported(codec, {
				supported: false,
				reason: 'unsupported-system',
				detail: 'Your browser/system doesn\u2019t expose a VP9 encoder.',
			});
		}
		return unsupported(codec, {
			supported: false,
			reason: 'unsupported-system',
			detail: `Your browser/system doesn\u2019t expose a ${codec.toUpperCase()} encoder.`,
		});
	}
	return {
		vp8: describe('vp8'),
		vp9: describe('vp9'),
		h264: describe('h264'),
		h265: describe('h265'),
		av1: describe('av1'),
	};
}

export function getCodecCapabilities(): CodecCapabilities {
	return probeEncodingCapabilities();
}

export function getCodecCapabilityReport(): CodecCapabilityReport {
	const currentGpu = getGpuEncoderReportSync();
	const currentNativeHardwareEncoder = getNativeHardwareEncoderCapabilitiesSync();
	const hardwareAccelerationDisabled = isDesktopHardwareAccelerationDisabled();
	const av1OptIn = VoiceSettings.getScreenShareAv1OptIn();
	const hevcOptIn = VoiceSettings.getScreenShareHevcOptIn();
	if (
		cachedReport &&
		cachedReportGpuKey === currentGpu &&
		cachedReportNativeHardwareEncoderKey === currentNativeHardwareEncoder &&
		cachedReportHardwareAccelerationDisabled === hardwareAccelerationDisabled &&
		cachedReportAv1OptIn === av1OptIn &&
		cachedReportHevcOptIn === hevcOptIn
	)
		return cachedReport;
	cachedReport = buildReport();
	cachedReportGpuKey = currentGpu;
	cachedReportNativeHardwareEncoderKey = currentNativeHardwareEncoder;
	cachedReportHardwareAccelerationDisabled = hardwareAccelerationDisabled;
	cachedReportAv1OptIn = av1OptIn;
	cachedReportHevcOptIn = hevcOptIn;
	return cachedReport;
}

export function getLiveKitSupportedCodecs(): ReadonlyArray<VideoCodec> {
	return LIVEKIT_SUPPORTED_CODECS;
}

export function isCodecLiveKitSupported(codec: string): codec is VideoCodec {
	return (LIVEKIT_SUPPORTED_CODECS as ReadonlyArray<string>).includes(codec);
}

export function isH265EncodingSupported(): boolean {
	return probeEncodingCapabilities().h265;
}

export function isVP9EncodingSupported(): boolean {
	return probeEncodingCapabilities().vp9;
}

export function isAV1EncodingSupported(): boolean {
	return probeEncodingCapabilities().av1;
}

export function selectOptimalCameraCodec(preference: CodecPreference = 'auto'): VideoCodec {
	if (preference !== 'auto') {
		const caps = probeEncodingCapabilities();
		if (caps[preference]) return preference;
		logger.warn('Preferred camera codec not supported, falling back to auto', {preference});
	}
	const caps = probeEncodingCapabilities();
	const platform = guessPlatform();
	if (isFirefoxBrowser()) {
		return 'vp8';
	}
	if (!isChromiumBrowser() && !isDesktop()) {
		if (caps.h264) return 'h264';
		if (caps.vp8) return 'vp8';
		return 'h264';
	}
	if (caps.vp9) {
		return 'vp9';
	}
	if (caps.h264 && platform === 'windows') return 'h264';
	return 'vp8';
}

export function resolveEffectiveScreenShareEncoderMode(mode: ScreenShareEncoderMode): ScreenShareEncoderMode {
	if (mode !== 'hardware') return mode;
	const report = getGpuEncoderReportSync();
	const codecs: ReadonlyArray<VideoCodec> = ['av1', 'h265', 'h264', 'vp9', 'vp8'];
	const nativeHardwareEncoder = getNativeHardwareEncoderCapabilitiesSync();
	if (!report && !nativeHardwareEncoder) return mode;
	const capabilityReport = getCodecCapabilityReport();
	return codecs.some((codec) => capabilityReport[codec].hardwareAccelerated === 'hardware') ? 'hardware' : 'auto';
}

export function buildScreenShareCodecProfile(): ScreenShareCodecProfile {
	const report = getCodecCapabilityReport();
	const browser: ScreenShareCodecBrowser = isFirefoxBrowser() ? 'firefox' : isChromiumBrowser() ? 'chromium' : 'other';
	const entry = (codec: VideoCodec): ScreenShareCodecProfileEntry => ({
		allowed: isVideoCodecAllowedForPublish(codec),
		supported: report[codec].supported,
		hardware: report[codec].hardwareAccelerated === 'hardware',
	});
	return {
		browser,
		desktop: isDesktop(),
		codecs: {
			vp8: entry('vp8'),
			vp9: entry('vp9'),
			h264: entry('h264'),
			h265: entry('h265'),
			av1: entry('av1'),
		},
	};
}

function rankAutomaticScreenShareCodecs(
	profile: ScreenShareCodecProfile,
	encoderModeSetting: ScreenShareEncoderMode,
): ScreenShareCodecRanking {
	return rankScreenShareCodecs({profile, encoderModeSetting, pin: 'auto'});
}

function describeAutomaticScreenShareCodecReason(
	profile: ScreenShareCodecProfile,
	codec: VideoCodec,
): AutomaticScreenShareCodecReason {
	if (profile.browser === 'firefox' && codec === 'vp8') return 'firefox-vp8';
	if (profile.browser === 'other' && !profile.desktop) {
		return codec === 'h264' ? 'non-chromium-h264' : 'non-chromium-vp8';
	}
	return profile.codecs[codec].hardware ? `hardware-${codec}` : `software-${codec}`;
}

export function selectAutomaticScreenShareCodec(
	encoderModeSetting: ScreenShareEncoderMode = 'auto',
): AutomaticScreenShareCodecSelection {
	const profile = buildScreenShareCodecProfile();
	const codec = rankAutomaticScreenShareCodecs(profile, encoderModeSetting).order[0];
	return {codec, reason: describeAutomaticScreenShareCodecReason(profile, codec)};
}

export function describeScreenShareEncoderPath(encoderModeSetting: ScreenShareEncoderMode): ScreenShareEncoderPath {
	const {order, hardwareUnavailable} = rankAutomaticScreenShareCodecs(
		buildScreenShareCodecProfile(),
		encoderModeSetting,
	);
	return {codec: order[0], hardwareUnavailable};
}

export function selectOptimalScreenShareCodec(
	preference: CodecPreference = 'auto',
	encoderMode: ScreenShareEncoderMode = 'auto',
): VideoCodec {
	if (preference !== 'auto') {
		const report = getCodecCapabilityReport();
		if (report[preference].supported) {
			return preference;
		}
		logger.warn('Preferred screen share codec not supported, falling back to auto', {preference});
	}
	return selectAutomaticScreenShareCodec(encoderMode).codec;
}

export function markScreenShareCodecEncodeRuntimeFailure(codec: VideoCodec, reason: string): boolean {
	if (codec === 'vp8') return false;
	if (runtimeEncodeFailureCodecs.has(codec)) return false;
	runtimeEncodeFailureCodecs.add(codec);
	cachedReport = null;
	logger.warn('Excluding codec from screen-share encode after runtime failure', {codec, reason});
	return true;
}

export function markScreenShareCodecSoftwareEncodeObserved(codec: VideoCodec): boolean {
	if (observedSoftwareEncodeCodecs.has(codec)) return false;
	observedSoftwareEncodeCodecs.add(codec);
	cachedReport = null;
	logger.warn('Treating this codec as software-encoded for the rest of the session', {codec});
	return true;
}

export type VideoPublishCodecDenial = 'sender-cannot-encode' | 'policy' | 'runtime-failed' | 'decoder-excluded';

export interface VideoPublishCodecPolicy {
	allowed: ReadonlyArray<VideoCodec>;
	requested: VideoCodec;
	primary: VideoCodec;
	backupCodec: false | {codec: 'h264'};
}

export interface VideoPublishCodecPolicyViolation {
	requested: VideoCodec;
	negotiated: VideoCodec;
	alternative: VideoCodec | null;
}

export type ScreenShareEncoderVerificationAction =
	| {kind: 'recover-stalled'; codec: VideoCodec}
	| {kind: 'stop-stalled'; codec: VideoCodec}
	| {kind: 'accept-negotiated'; requested: VideoCodec; negotiated: ReadonlyArray<VideoCodec>}
	| {
			kind: 'correct-negotiated';
			requested: VideoCodec;
			negotiated: ReadonlyArray<VideoCodec>;
			alternative: VideoCodec | null;
	  };

export function getVideoPublishCodecDenial(codec: VideoCodec): VideoPublishCodecDenial | null {
	if (!supportsVideoCodec(codec)) return 'sender-cannot-encode';
	if (getScreenShareCodecPolicyUnsupported(codec, buildScreenShareCodecPolicyContext())) return 'policy';
	if (runtimeEncodeFailureCodecs.has(codec)) return 'runtime-failed';
	if (isVideoCodecDecodeExcluded(codec)) return 'decoder-excluded';
	return null;
}

export function isVideoCodecAllowedForPublish(codec: VideoCodec): boolean {
	return getVideoPublishCodecDenial(codec) === null;
}

export function getAllowedVideoPublishCodecs(): ReadonlyArray<VideoCodec> {
	return LIVEKIT_SUPPORTED_CODECS.filter(isVideoCodecAllowedForPublish);
}

export function selectVideoPublishCodecAlternative(requested: VideoCodec): VideoCodec | null {
	return (
		PUBLISH_CODEC_FALLBACK_ORDER.find((codec) => codec !== requested && isVideoCodecAllowedForPublish(codec)) ?? null
	);
}

export function resolveVideoPublishCodecPolicy(requested: VideoCodec): VideoPublishCodecPolicy {
	const allowed = getAllowedVideoPublishCodecs();
	const primary = allowed.includes(requested)
		? requested
		: (PUBLISH_CODEC_FALLBACK_ORDER.find((codec) => allowed.includes(codec)) ?? LAST_RESORT_VIDEO_CODEC);
	if (primary !== requested) {
		logger.warn('Requested publish codec is outside the publish policy; substituting', {
			requested,
			primary,
			denial: getVideoPublishCodecDenial(requested),
			allowed,
		});
	}
	const backupCodec =
		primary !== 'h264' && primary !== 'vp8' && allowed.includes('h264') ? {codec: 'h264' as const} : false;
	return {allowed, requested, primary, backupCodec};
}

export function getCameraPublishCodecPolicy(): VideoPublishCodecPolicy {
	return resolveVideoPublishCodecPolicy(selectOptimalCameraCodec(VoiceSettings.getPreferredVideoCodec()));
}

export function buildCameraPublishOptions(codec?: VideoCodec): TrackPublishOptions {
	const policy = codec ? resolveVideoPublishCodecPolicy(codec) : getCameraPublishCodecPolicy();
	return {
		videoCodec: policy.primary,
		backupCodec: policy.backupCodec,
		...(policy.backupCodec ? {backupCodecPolicy: BackupCodecPolicy.SIMULCAST} : {}),
	};
}

export function getRoomVideoPublishDefaults(): Pick<TrackPublishDefaults, 'videoCodec' | 'backupCodec'> {
	const policy = getCameraPublishCodecPolicy();
	return {
		videoCodec: policy.primary,
		backupCodec: policy.allowed.includes('h264') ? {codec: 'h264'} : false,
	};
}

export function findVideoPublishCodecPolicyViolation(
	requested: VideoCodec,
	published: VideoCodec | undefined,
	negotiated?: VideoCodec,
): VideoPublishCodecPolicyViolation | null {
	if (published && !isVideoCodecAllowedForPublish(published)) {
		return {requested, negotiated: published, alternative: selectVideoPublishCodecAlternative(requested)};
	}
	if (negotiated && negotiated !== requested) {
		return {
			requested,
			negotiated,
			alternative: isVideoCodecAllowedForPublish(negotiated)
				? negotiated
				: selectVideoPublishCodecAlternative(requested),
		};
	}
	return null;
}

export function resolveScreenShareEncoderVerificationAction(
	failure:
		| {reason: 'stalled'; codec: VideoCodec}
		| {reason: 'codec-mismatch'; codec: VideoCodec; activeCodecs: ReadonlyArray<VideoCodec>},
): ScreenShareEncoderVerificationAction {
	if (failure.reason === 'stalled') {
		if (runtimeEncodeFailureCodecs.has(failure.codec)) return {kind: 'stop-stalled', codec: failure.codec};
		markScreenShareCodecEncodeRuntimeFailure(failure.codec, 'screen-share-encode-stalled');
		return {kind: 'recover-stalled', codec: failure.codec};
	}
	const unexpected = failure.activeCodecs.filter((codec) => codec !== failure.codec);
	if (unexpected.length === 0) {
		return {kind: 'accept-negotiated', requested: failure.codec, negotiated: failure.activeCodecs};
	}
	return {
		kind: 'correct-negotiated',
		requested: failure.codec,
		negotiated: unexpected,
		alternative:
			unexpected.find((codec) => isVideoCodecAllowedForPublish(codec)) ??
			selectVideoPublishCodecAlternative(failure.codec),
	};
}

export function selectNativeScreenCaptureScreenShareCodec(preference: CodecPreference = 'auto'): VideoCodec {
	return selectOptimalScreenShareCodec(preference);
}

export function shouldUseNativeScreenCaptureForScreenShareCodec(_codec: VideoCodec): boolean {
	return true;
}

export type ScreenShareContentSource = 'app' | 'device' | 'display';

export function resolveScreenShareContentHint(
	preference: ScreenShareContentHint | undefined = 'auto',
): 'detail' | 'text' | 'motion' | undefined {
	return preference === undefined || preference === 'auto' ? undefined : preference;
}

export function resolveScreenShareContentHintForContext(
	preference: ScreenShareContentHint | undefined,
	_codec: VideoCodec,
	source: ScreenShareContentSource,
	streamingMode: 'custom' | 'gaming' | 'screenshare',
): 'detail' | 'text' | 'motion' | undefined {
	const mode = normaliseStreamingModeForContext(streamingMode, source);
	if (mode === 'gaming') return 'motion';
	if (mode === 'screenshare') return 'text';
	return resolveScreenShareContentHint(preference);
}

export function adjustScreenShareEncodingForCodec(encoding: VideoEncoding, _codec: VideoCodec): VideoEncoding {
	return encoding;
}

export function getBackupCodecForPrimary(primaryCodec: VideoCodec):
	| false
	| {
			codec: 'vp8' | 'h264';
	  } {
	switch (primaryCodec) {
		case 'vp8':
			return false;
		case 'h264':
			return false;
		case 'vp9':
		case 'av1':
		case 'h265':
			return {codec: 'h264'};
		default:
			return {codec: 'h264'};
	}
}

export function resetCachedCodecCapabilities(): void {
	cachedCapabilities = null;
	cachedReport = null;
	cachedReportGpuKey = undefined;
	cachedReportNativeHardwareEncoderKey = undefined;
	cachedReportHardwareAccelerationDisabled = undefined;
	cachedReportAv1OptIn = undefined;
	cachedReportHevcOptIn = undefined;
	runtimeEncodeFailureCodecs.clear();
	observedSoftwareEncodeCodecs.clear();
	resetNativeHardwareEncoderCapabilities();
	resetOpenH264Status();
}
