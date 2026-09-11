// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import SoftwareEncoderWarning from '@app/features/voice/state/SoftwareEncoderWarning';
import {
	classifyVideoDecoderAcceleration,
	isSoftwareVideoImplementation,
} from '@app/features/voice/utils/VideoAccelerationClassification';
import type {VideoCodec} from 'livekit-client';

const logger = new Logger('ScreenShareCodecDiagnostics');
const DECODER_VERIFICATION_DELAY_MS = 5000;
const UNKNOWN_DECODER_IMPLEMENTATION = 'software decoder';
const UNKNOWN_CODEC_LABEL = 'video';

export {isSoftwareVideoImplementation};

interface CodecStatsEntry {
	type: string;
	id: string;
	mimeType?: string;
}

interface InboundVideoStatsEntry {
	type: string;
	kind?: string;
	mediaType?: string;
	codecId?: string;
	packetsReceived?: number;
	bytesReceived?: number;
	framesDecoded?: number;
	framesReceived?: number;
	decoderImplementation?: string;
	powerEfficientDecoder?: boolean;
}

export interface SoftwareVideoDecoderInfo {
	codec: string;
	implementation: string;
	powerEfficientDecoder: boolean | null;
}

export interface StalledVideoDecoderInfo {
	codec: VideoCodec;
	mimeType?: string;
	packetsReceived: number;
	bytesReceived: number;
	framesDecoded: number;
	framesReceived: number;
}

function isSoftwareVideoStats(implementation: string | null, powerEfficient: boolean | null): boolean {
	return classifyVideoDecoderAcceleration(implementation, powerEfficient) === 'software';
}

function getCodecLabel(mimeType: string | undefined): string {
	if (!mimeType) return UNKNOWN_CODEC_LABEL;
	return mimeType.replace(/^video\//i, '').toUpperCase();
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

function getStatsKind(report: InboundVideoStatsEntry, codecs: Map<string, CodecStatsEntry>): string | undefined {
	if (report.kind || report.mediaType) return report.kind ?? report.mediaType;
	if (!report.codecId) return undefined;
	const codec = codecs.get(report.codecId);
	return codec?.mimeType?.startsWith('video/') ? 'video' : undefined;
}

export function findSoftwareVideoDecoder(stats: RTCStatsReport): SoftwareVideoDecoderInfo | null {
	const codecs = new Map<string, CodecStatsEntry>();
	const reports: Array<InboundVideoStatsEntry> = [];
	for (const raw of stats.values()) {
		const report = raw as CodecStatsEntry & InboundVideoStatsEntry;
		if (report.type === 'codec') {
			codecs.set(report.id, report);
		}
		if (report.type === 'inbound-rtp') {
			reports.push(report);
		}
	}
	for (const report of reports) {
		if (getStatsKind(report, codecs) !== 'video') continue;
		const implementation =
			typeof report.decoderImplementation === 'string' && report.decoderImplementation.length > 0
				? report.decoderImplementation
				: null;
		const powerEfficientDecoder =
			typeof report.powerEfficientDecoder === 'boolean' ? report.powerEfficientDecoder : null;
		if (!isSoftwareVideoStats(implementation, powerEfficientDecoder)) continue;
		return {
			codec: getCodecLabel(report.codecId ? codecs.get(report.codecId)?.mimeType : undefined),
			implementation: implementation ?? UNKNOWN_DECODER_IMPLEMENTATION,
			powerEfficientDecoder,
		};
	}
	return null;
}

export function findStalledVideoDecoder(stats: RTCStatsReport): StalledVideoDecoderInfo | null {
	const codecs = new Map<string, CodecStatsEntry>();
	const reports: Array<InboundVideoStatsEntry> = [];
	for (const raw of stats.values()) {
		const report = raw as CodecStatsEntry & InboundVideoStatsEntry;
		if (report.type === 'codec') {
			codecs.set(report.id, report);
		}
		if (report.type === 'inbound-rtp') {
			reports.push(report);
		}
	}
	for (const report of reports) {
		if (getStatsKind(report, codecs) !== 'video') continue;
		const framesDecoded = finiteNumber(report.framesDecoded);
		if (framesDecoded === null || framesDecoded > 0) continue;
		const framesReceived = finiteNumber(report.framesReceived);
		if (framesReceived === null || framesReceived < 1) continue;
		const packetsReceived = finiteNumber(report.packetsReceived) ?? 0;
		const bytesReceived = finiteNumber(report.bytesReceived) ?? 0;
		const mimeType = report.codecId ? codecs.get(report.codecId)?.mimeType : undefined;
		const codec = getVideoCodecFromMimeType(mimeType);
		if (!codec) continue;
		return {
			codec,
			mimeType,
			packetsReceived,
			bytesReceived,
			framesDecoded,
			framesReceived,
		};
	}
	return null;
}

export function confirmDecodeStall(
	first: StalledVideoDecoderInfo | null,
	second: StalledVideoDecoderInfo | null,
): StalledVideoDecoderInfo | null {
	if (!first || !second) return null;
	if (first.codec !== second.codec || first.mimeType !== second.mimeType) return null;
	if (first.framesDecoded !== 0 || second.framesDecoded !== 0) return null;
	if (second.framesReceived <= first.framesReceived) return null;
	return second;
}

export function scheduleScreenShareDecoderVerification(
	getStats: () => Promise<RTCStatsReport | undefined>,
	onComplete?: () => void,
	onDecodeFailure?: (failure: StalledVideoDecoderInfo) => void,
): () => void {
	let cancelled = false;
	let timer: ReturnType<typeof setTimeout> | null = null;
	const scheduleConfirmation = (first: StalledVideoDecoderInfo): void => {
		timer = setTimeout(async () => {
			timer = null;
			try {
				const stats = await getStats();
				const confirmed = confirmDecodeStall(first, stats ? findStalledVideoDecoder(stats) : null);
				if (cancelled || !confirmed) return;
				logger.warn('Screen share video decode is stalled', confirmed);
				onDecodeFailure?.(confirmed);
			} catch (error) {
				logger.debug('Failed to confirm the screen share decode stall', {error});
			} finally {
				if (!cancelled) {
					onComplete?.();
				}
			}
		}, DECODER_VERIFICATION_DELAY_MS);
	};
	timer = setTimeout(async () => {
		timer = null;
		let firstStall: StalledVideoDecoderInfo | null = null;
		try {
			const stats = await getStats();
			if (!stats) return;
			firstStall = findStalledVideoDecoder(stats);
			const decoder = findSoftwareVideoDecoder(stats);
			if (!decoder) return;
			logger.warn('Screen share is using a software decoder', decoder);
			SoftwareEncoderWarning.triggerDecoderWarning(decoder.codec, decoder.implementation);
		} catch (error) {
			logger.debug('Failed to verify screen share decoder', {error});
		} finally {
			if (!cancelled) {
				if (firstStall) {
					scheduleConfirmation(firstStall);
				} else {
					onComplete?.();
				}
			}
		}
	}, DECODER_VERIFICATION_DELAY_MS);
	return () => {
		cancelled = true;
		if (!timer) return;
		clearTimeout(timer);
		timer = null;
	};
}
