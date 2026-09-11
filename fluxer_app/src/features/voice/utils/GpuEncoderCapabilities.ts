// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import {
	getElectronAPI,
	getNativePlatformSync,
	isDesktop,
	type NativePlatform,
} from '@app/features/ui/utils/NativeUtils';
import VoiceSettings from '@app/features/voice/state/VoiceSettings';
import {
	getScreenShareBitrateBps,
	resolveEffectiveScreenShareDimensions,
	resolveStreamingModeSettings,
	SCREEN_SHARE_MAX_VIDEO_BITRATE_BPS,
} from '@app/features/voice/utils/ScreenShareOptions';
import {hasHigherVideoQuality} from '@app/features/voice/utils/VideoQualityEntitlement';
import type {GpuDeviceInfo, GpuInfo} from '@app/types/electron.d';
import type {VideoCodec} from 'livekit-client';

const logger = new Logger('GpuEncoderCapabilities');

export type HardwareEncodeAnswer = 'hardware' | 'software' | 'unknown';

export interface HardwareEncodeReport {
	av1: HardwareEncodeAnswer;
	h265: HardwareEncodeAnswer;
	h264: HardwareEncodeAnswer;
	vp9: HardwareEncodeAnswer;
	vp8: HardwareEncodeAnswer;
	gpuLabel?: string;
	gpuFamily?: string;
	raw?: GpuInfo;
}

export const PCI_VENDOR_NVIDIA = 0x10de;
export const PCI_VENDOR_AMD = 0x1002;
export const PCI_VENDOR_INTEL = 0x8086;
export const PCI_VENDOR_APPLE = 0x106b;

export interface GpuFamilyRule {
	family: string;
	caps: Pick<HardwareEncodeReport, 'av1' | 'h265' | 'h264' | 'vp9' | 'vp8'>;
}

export const NVIDIA_AV1_FAMILIES: GpuFamilyRule = {
	family: 'nvidia-ada-or-blackwell',
	caps: {av1: 'hardware', h265: 'hardware', h264: 'hardware', vp9: 'software', vp8: 'software'},
};
export const NVIDIA_PRE_ADA: GpuFamilyRule = {
	family: 'nvidia-pre-ada',
	caps: {av1: 'software', h265: 'hardware', h264: 'hardware', vp9: 'software', vp8: 'software'},
};
export const NVIDIA_PRE_MAXWELL2: GpuFamilyRule = {
	family: 'nvidia-pre-maxwell2',
	caps: {av1: 'software', h265: 'software', h264: 'hardware', vp9: 'software', vp8: 'software'},
};
export const AMD_RDNA3_PLUS: GpuFamilyRule = {
	family: 'amd-rdna3-plus',
	caps: {av1: 'hardware', h265: 'hardware', h264: 'hardware', vp9: 'software', vp8: 'software'},
};
export const AMD_VCN_NO_AV1: GpuFamilyRule = {
	family: 'amd-vcn-pre-rdna3',
	caps: {av1: 'software', h265: 'hardware', h264: 'hardware', vp9: 'software', vp8: 'software'},
};
export const INTEL_AV1_FAMILY: GpuFamilyRule = {
	family: 'intel-arc-or-xe-lpg-plus',
	caps: {av1: 'hardware', h265: 'hardware', h264: 'hardware', vp9: 'hardware', vp8: 'software'},
};
export const INTEL_GEN9_PLUS: GpuFamilyRule = {
	family: 'intel-gen9-plus-no-av1',
	caps: {av1: 'software', h265: 'hardware', h264: 'hardware', vp9: 'hardware', vp8: 'software'},
};
export const APPLE_SILICON: GpuFamilyRule = {
	family: 'apple-silicon',
	caps: {av1: 'software', h265: 'hardware', h264: 'hardware', vp9: 'software', vp8: 'software'},
};
export const APPLE_AV1_ENCODE: GpuFamilyRule = {
	family: 'apple-m4-pro-max-or-newer',
	caps: {av1: 'hardware', h265: 'hardware', h264: 'hardware', vp9: 'software', vp8: 'software'},
};
const INTEL_METEOR_LAKE_IDS = new Set([0x7d40, 0x7d45, 0x7d55, 0x7d60, 0x7dd5]);
const INTEL_LUNAR_LAKE_IDS = new Set([0x6420, 0x64a0, 0x64b0]);
const INTEL_ARROW_LAKE_IDS = new Set([0x7d41, 0x7d51, 0x7d67, 0x7dd1, 0xb640]);

function isIntelDg2(deviceId: number): boolean {
	if (deviceId >= 0x5690 && deviceId <= 0x5697) return true;
	if (deviceId >= 0x56a0 && deviceId <= 0x56a6) return true;
	if (deviceId >= 0x56b0 && deviceId <= 0x56b3) return true;
	if (deviceId >= 0x56ba && deviceId <= 0x56bd) return true;
	if (deviceId >= 0x56c0 && deviceId <= 0x56c2) return true;
	return false;
}

function isIntelBattlemage(deviceId: number): boolean {
	return (deviceId >= 0xe200 && deviceId <= 0xe216) || (deviceId >= 0xe220 && deviceId <= 0xe22f);
}

function isIntelPantherLake(deviceId: number): boolean {
	return deviceId >= 0xb080 && deviceId <= 0xb0bf;
}

function classifyIntelByPciId(deviceId: number): GpuFamilyRule | null {
	if (isIntelDg2(deviceId)) return INTEL_AV1_FAMILY;
	if (isIntelBattlemage(deviceId)) return INTEL_AV1_FAMILY;
	if (isIntelPantherLake(deviceId)) return INTEL_AV1_FAMILY;
	if (INTEL_METEOR_LAKE_IDS.has(deviceId)) return INTEL_AV1_FAMILY;
	if (INTEL_LUNAR_LAKE_IDS.has(deviceId)) return INTEL_AV1_FAMILY;
	if (INTEL_ARROW_LAKE_IDS.has(deviceId)) return INTEL_AV1_FAMILY;
	return null;
}

function classifyNvidiaByPciId(deviceId: number): GpuFamilyRule | null {
	if (deviceId >= 0x2900 && deviceId <= 0x2fff) return NVIDIA_AV1_FAMILIES;
	if (deviceId >= 0x2680 && deviceId <= 0x28ff) return NVIDIA_AV1_FAMILIES;
	if (deviceId >= 0x2330 && deviceId <= 0x2339) return NVIDIA_PRE_ADA;
	if (deviceId >= 0x2200 && deviceId <= 0x25ff) return NVIDIA_PRE_ADA;
	if (deviceId >= 0x1e00 && deviceId <= 0x21ff) return NVIDIA_PRE_ADA;
	if (deviceId >= 0x1d80 && deviceId <= 0x1dff) return NVIDIA_PRE_ADA;
	if (deviceId >= 0x1b80 && deviceId <= 0x1d7f) return NVIDIA_PRE_ADA;
	if (deviceId >= 0x1340 && deviceId <= 0x17ff) return NVIDIA_PRE_ADA;
	return null;
}

function classifyAmdByPciId(deviceId: number): GpuFamilyRule | null {
	if (deviceId >= 0x7550 && deviceId <= 0x755f) return AMD_RDNA3_PLUS;
	if (deviceId >= 0x7440 && deviceId <= 0x74ff) return AMD_RDNA3_PLUS;
	if (deviceId >= 0x73a0 && deviceId <= 0x73ff) return AMD_VCN_NO_AV1;
	if (deviceId >= 0x7310 && deviceId <= 0x734f) return AMD_VCN_NO_AV1;
	if (deviceId >= 0x66a0 && deviceId <= 0x66af) return AMD_VCN_NO_AV1;
	if (deviceId === 0x6860 || deviceId === 0x687f) return AMD_VCN_NO_AV1;
	if (deviceId >= 0x67c0 && deviceId <= 0x67ff) return AMD_VCN_NO_AV1;
	return null;
}

export function classifyByPciId(vendorId: number, deviceId: number): GpuFamilyRule | null {
	if (!deviceId) return null;
	if (vendorId === PCI_VENDOR_INTEL) return classifyIntelByPciId(deviceId);
	if (vendorId === PCI_VENDOR_NVIDIA) return classifyNvidiaByPciId(deviceId);
	if (vendorId === PCI_VENDOR_AMD) return classifyAmdByPciId(deviceId);
	return null;
}

export function classifyByRenderer(renderer: string, vendorId: number): GpuFamilyRule | null {
	const r = renderer;
	if (vendorId === PCI_VENDOR_NVIDIA || /\bNVIDIA\b/i.test(r)) {
		if (/\bRTX\s*(50|60|70|80|90)\d{2}\b/i.test(r)) return NVIDIA_AV1_FAMILIES;
		if (/\bRTX\s*40\d{2}\b/i.test(r)) return NVIDIA_AV1_FAMILIES;
		if (/\bRTX\s*(20|30)\d{2}\b/i.test(r)) return NVIDIA_PRE_ADA;
		if (/\bGTX\s*(9|10|16)\d{2}\b/i.test(r)) return NVIDIA_PRE_ADA;
		if (/\bGTX\s*[78]\d{2}\b/i.test(r)) return NVIDIA_PRE_MAXWELL2;
		if (/\b(L(?:4|20|30|40[SG]?)|RTX\s*Ada|RTX\s*\d{4}\s*Ada|B(?:100|200))\b/i.test(r)) return NVIDIA_AV1_FAMILIES;
		if (/\b(A100|A40|A30|A10|H100|H200|T4|V100|P100|P40|P4|M\d{2,4})\b/i.test(r)) return NVIDIA_PRE_ADA;
		return NVIDIA_PRE_ADA;
	}
	if (vendorId === PCI_VENDOR_AMD || /\b(AMD|Radeon|ATI)\b/i.test(r)) {
		if (/\bRX\s*9\d{3}\b/i.test(r)) return AMD_RDNA3_PLUS;
		if (/\bRX\s*7\d{3}\b/i.test(r)) return AMD_RDNA3_PLUS;
		if (/\bRadeon\s*(7[4-9]\d|8[0-9]\d|9[0-9]\d)M\b/i.test(r)) return AMD_RDNA3_PLUS;
		if (/\b(navi3[1-9]|navi4\d|gfx11(\d{2})?|gfx12(\d{2})?)\b/i.test(r)) return AMD_RDNA3_PLUS;
		if (/\bRX\s*([56])\d{3}\b/i.test(r)) return AMD_VCN_NO_AV1;
		if (/\bRX\s*(Vega|[45]\d{2}|5\d{2}X)\b/i.test(r)) return AMD_VCN_NO_AV1;
		if (/\b(navi2\d|navi1\d|vega|polaris|gfx10\d{2}|gfx9\d{2})\b/i.test(r)) return AMD_VCN_NO_AV1;
		return AMD_VCN_NO_AV1;
	}
	if (vendorId === PCI_VENDOR_INTEL || /\bIntel\b/i.test(r)) {
		if (/\bArc\b/i.test(r)) return INTEL_AV1_FAMILY;
		if (/\bCore\s*Ultra\b/i.test(r)) return INTEL_AV1_FAMILY;
		if (/\b(MTL|LNL|ARL|PTL|BMG|Xe2|Xe-LPG)\b/.test(r)) return INTEL_AV1_FAMILY;
		if (/\b(UHD|Iris|HD)\s*Graphics\b/i.test(r)) return INTEL_GEN9_PLUS;
		return INTEL_GEN9_PLUS;
	}
	if (vendorId === PCI_VENDOR_APPLE || /\bApple\s*(M\d|GPU)\b/i.test(r)) {
		if (/\bApple\s*M([4-9]|\d{2,})\s*(Pro|Max|Ultra)\b/i.test(r)) return APPLE_AV1_ENCODE;
		return APPLE_SILICON;
	}
	return null;
}

export function classifyDevice(vendorId: number, deviceId: number, renderer: string): GpuFamilyRule | null {
	return classifyByPciId(vendorId, deviceId) ?? classifyByRenderer(renderer, vendorId);
}

function pickPrimaryDevice(devices: ReadonlyArray<GpuDeviceInfo>): GpuDeviceInfo | undefined {
	return [...devices].sort((a, b) => {
		const score = (device: GpuDeviceInfo): number => {
			let value = 0;
			if (device.active) value += 1000;
			if (device.headless === false) value += 100;
			if (device.dedicatedVideoMemory)
				value += Math.min(500, Math.floor(device.dedicatedVideoMemory / 1024 / 1024 / 1024));
			if (device.integrated === false) value += 50;
			return value;
		};
		return score(b) - score(a);
	})[0];
}

function buildLabel(info: GpuInfo, device: GpuDeviceInfo | undefined): string | undefined {
	const renderer = info.glRenderer;
	const deviceString = device?.deviceString;
	if (deviceString && device?.vendorName && !new RegExp(`\\b${device.vendorName}\\b`, 'i').test(deviceString)) {
		return `${device.vendorName} ${deviceString}`;
	}
	return renderer || deviceString || device?.vendorName || info.machineModelName;
}

export function reportFromGpuInfo(info: GpuInfo): HardwareEncodeReport {
	const device = pickPrimaryDevice(info.devices);
	const renderer = [
		info.glRenderer,
		device?.deviceString,
		info.machineModelName,
		info.machineModelVersion,
		device?.vendorName,
	]
		.filter((value): value is string => typeof value === 'string' && value.length > 0)
		.join(' ');
	const vendorId = device?.vendorId ?? 0;
	const deviceId = device?.deviceId ?? 0;
	const rule = classifyDevice(vendorId, deviceId, renderer);
	if (!rule) {
		logger.info('GPU did not match any known family — leaving hardware-encode answers as unknown', {
			renderer,
			vendorId: vendorId ? `0x${vendorId.toString(16)}` : undefined,
			deviceId: deviceId ? `0x${deviceId.toString(16)}` : undefined,
		});
		return {
			av1: 'unknown',
			h265: 'unknown',
			h264: 'unknown',
			vp9: 'unknown',
			vp8: 'unknown',
			gpuLabel: buildLabel(info, device),
			raw: info,
		};
	}
	logger.info('Classified GPU for hardware-encode capability', {family: rule.family, renderer});
	return {
		...rule.caps,
		gpuLabel: buildLabel(info, device),
		gpuFamily: rule.family,
		raw: info,
	};
}

export const NEGOTIABLE_H264_PROBE_CONTENT_TYPE =
	'video/H264;level-asymmetry-allowed=1;packetization-mode=1;profile-level-id=42e01f';

const WEBRTC_ENCODE_PROBE_CONTENT_TYPES: Record<VideoCodec, ReadonlyArray<string>> = {
	av1: ['video/AV1'],
	h265: ['video/H265'],
	h264: [NEGOTIABLE_H264_PROBE_CONTENT_TYPE],
	vp9: ['video/VP9'],
	vp8: ['video/VP8'],
};

interface EncodeProbeVideoConfig {
	width: number;
	height: number;
	bitrate: number;
	framerate: number;
}

const DEFAULT_ENCODE_PROBE_VIDEO_CONFIG: EncodeProbeVideoConfig = {
	width: 1920,
	height: 1080,
	bitrate: SCREEN_SHARE_MAX_VIDEO_BITRATE_BPS,
	framerate: 60,
};

function resolveEncodeProbeVideoConfig(): EncodeProbeVideoConfig {
	try {
		const settings = resolveStreamingModeSettings(
			VoiceSettings.getStreamingMode(),
			VoiceSettings.getScreenshareResolution(),
			VoiceSettings.getVideoFrameRate(),
			hasHigherVideoQuality(),
		);
		const {width, height} = resolveEffectiveScreenShareDimensions(settings.resolution);
		return {
			width,
			height,
			bitrate: getScreenShareBitrateBps(settings.resolution, settings.frameRate),
			framerate: settings.frameRate,
		};
	} catch (error) {
		logger.debug('Share settings were unavailable, probing at the largest share size instead', {error});
		return DEFAULT_ENCODE_PROBE_VIDEO_CONFIG;
	}
}

interface WebRtcEncodingInfoResult {
	supported?: boolean;
	powerEfficient?: boolean;
}

interface MediaCapabilitiesLike {
	encodingInfo?: (config: unknown) => Promise<WebRtcEncodingInfoResult | undefined>;
}

async function probeCodecEncodeEfficiency(
	mediaCapabilities: MediaCapabilitiesLike,
	contentTypes: ReadonlyArray<string>,
	video: EncodeProbeVideoConfig,
): Promise<HardwareEncodeAnswer> {
	let sawSupported = false;
	for (const contentType of contentTypes) {
		try {
			const info = await mediaCapabilities.encodingInfo?.({
				type: 'webrtc',
				video: {contentType, ...video},
			});
			if (!info?.supported) continue;
			sawSupported = true;
			if (info.powerEfficient === true) return 'hardware';
		} catch {}
	}
	return sawSupported ? 'software' : 'unknown';
}

export async function probeWebRtcEncodeEfficiency(): Promise<Record<VideoCodec, HardwareEncodeAnswer> | null> {
	if (typeof navigator === 'undefined') return null;
	const mediaCapabilities = (navigator as Navigator & {mediaCapabilities?: MediaCapabilitiesLike}).mediaCapabilities;
	if (!mediaCapabilities?.encodingInfo) return null;
	const codecs: ReadonlyArray<VideoCodec> = ['av1', 'h265', 'h264', 'vp9', 'vp8'];
	const video = resolveEncodeProbeVideoConfig();
	const answers = await Promise.all(
		codecs.map((codec) =>
			probeCodecEncodeEfficiency(mediaCapabilities, WEBRTC_ENCODE_PROBE_CONTENT_TYPES[codec], video),
		),
	);
	const result = {} as Record<VideoCodec, HardwareEncodeAnswer>;
	codecs.forEach((codec, index) => {
		result[codec] = answers[index] ?? 'unknown';
	});
	return result;
}

export function reconcileHardwareEncodeReport(
	report: HardwareEncodeReport,
	efficiency: Record<VideoCodec, HardwareEncodeAnswer> | null,
	platform: NativePlatform,
	vendorId: number,
): HardwareEncodeReport {
	const isNvidiaReport = vendorId === PCI_VENDOR_NVIDIA || report.gpuFamily?.startsWith('nvidia-') === true;
	const adjust = (codec: VideoCodec): HardwareEncodeAnswer => {
		if (report[codec] !== 'hardware') return report[codec];
		return efficiency?.[codec] === 'hardware' ? 'hardware' : 'software';
	};
	if (platform === 'linux' && isNvidiaReport) {
		return {
			...report,
			av1: adjust('av1'),
			h265: adjust('h265'),
			h264: adjust('h264'),
			vp9: adjust('vp9'),
			vp8: adjust('vp8'),
		};
	}
	return {...report, h264: adjust('h264')};
}

let cachedReport: HardwareEncodeReport | null = null;
let pendingPromise: Promise<HardwareEncodeReport | null> | null = null;

function fetchReport(): Promise<HardwareEncodeReport | null> {
	if (!isDesktop()) return Promise.resolve(null);
	const electron = getElectronAPI();
	if (!electron?.getGpuInfo) return Promise.resolve(null);
	return Promise.allSettled([electron.getGpuInfo(), probeWebRtcEncodeEfficiency()]).then(([gpuResult, probeResult]) => {
		if (gpuResult.status !== 'fulfilled') {
			logger.warn('Failed to fetch GPU info from Electron main', {error: gpuResult.reason});
			return null;
		}
		const info = gpuResult.value;
		const baseReport = reportFromGpuInfo(info);
		const efficiency = probeResult.status === 'fulfilled' ? probeResult.value : null;
		const report = reconcileHardwareEncodeReport(
			baseReport,
			efficiency,
			getNativePlatformSync(),
			pickPrimaryDevice(info.devices)?.vendorId ?? 0,
		);
		cachedReport = report;
		return report;
	});
}

export function loadGpuEncoderReport(): Promise<HardwareEncodeReport | null> {
	if (cachedReport) return Promise.resolve(cachedReport);
	if (pendingPromise) return pendingPromise;
	pendingPromise = fetchReport().finally(() => {
		pendingPromise = null;
	});
	return pendingPromise;
}

export function getGpuEncoderReportSync(): HardwareEncodeReport | null {
	return cachedReport;
}

export function resetGpuEncoderReport(): void {
	cachedReport = null;
	pendingPromise = null;
}

export function hasHardwareEncodeFor(codec: VideoCodec): HardwareEncodeAnswer {
	const report = cachedReport;
	if (!report) return 'unknown';
	return report[codec];
}

if (typeof window !== 'undefined' && isDesktop()) {
	void loadGpuEncoderReport();
}
