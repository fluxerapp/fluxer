// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import SoftwareEncoderWarning from '@app/features/voice/state/SoftwareEncoderWarning';
import {classifyVideoDecoderAcceleration} from '@app/features/voice/utils/VideoAccelerationClassification';
import type {VideoCodec} from 'livekit-client';

const logger = new Logger('ScreenShareCodecDiagnostics');
const DECODE_SAMPLE_INTERVAL_MS = 5000;
const UNKNOWN_DECODER_IMPLEMENTATION = 'software decoder';

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
	timestamp?: number;
	packetsReceived?: number;
	bytesReceived?: number;
	framesDecoded?: number;
	keyFramesDecoded?: number;
	framesReceived?: number;
	framesDropped?: number;
	freezeCount?: number;
	totalInterFrameDelay?: number;
	totalSquaredInterFrameDelay?: number;
	decoderImplementation?: string;
	powerEfficientDecoder?: boolean;
}

export interface SoftwareVideoDecoderInfo {
	codec: string | null;
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
	framesDropped: number | null;
}

export interface InboundVideoDecodeSample {
	codec: VideoCodec | null;
	mimeType?: string;
	atMs: number;
	packetsReceived: number;
	bytesReceived: number;
	framesDecoded: number | null;
	keyFramesDecoded: number | null;
	framesReceived: number | null;
	framesDropped: number | null;
	freezeCount: number | null;
	totalInterFrameDelay: number | null;
	totalSquaredInterFrameDelay: number | null;
	decoderImplementation: string | null;
	powerEfficientDecoder: boolean | null;
}

export interface InboundVideoDecodeHealth {
	codec: VideoCodec | null;
	intervalMs: number;
	decodedFps: number | null;
	framesDecodedDelta: number | null;
	meanInterFrameDelayMs: number | null;
	interFrameDelayStdDevMs: number | null;
	freezes: number | null;
	freezeCount: number | null;
	framesDroppedDelta: number | null;
	framesDecoded: number | null;
	bytesReceivedDelta: number;
}

function isSoftwareVideoStats(implementation: string | null, powerEfficient: boolean | null): boolean {
	return classifyVideoDecoderAcceleration(implementation, powerEfficient) === 'software';
}

function getCodecLabel(mimeType: string | undefined): string | null {
	if (!mimeType) return null;
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

function counterDelta(previous: number | null, current: number | null): number | null {
	if (previous === null || current === null) return null;
	if (current < previous) return null;
	return current - previous;
}

function roundTo(value: number, decimals: number): number {
	const factor = 10 ** decimals;
	return Math.round(value * factor) / factor;
}

function getStatsKind(report: InboundVideoStatsEntry, codecs: Map<string, CodecStatsEntry>): string | undefined {
	if (report.kind || report.mediaType) return report.kind ?? report.mediaType;
	if (!report.codecId) return undefined;
	const codec = codecs.get(report.codecId);
	return codec?.mimeType?.startsWith('video/') ? 'video' : undefined;
}

function collectInboundVideoStats(stats: RTCStatsReport): {
	codecs: Map<string, CodecStatsEntry>;
	reports: Array<InboundVideoStatsEntry>;
} {
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
	return {codecs, reports};
}

export function findInboundVideoDecodeSample(stats: RTCStatsReport): InboundVideoDecodeSample | null {
	const {codecs, reports} = collectInboundVideoStats(stats);
	for (const report of reports) {
		if (getStatsKind(report, codecs) !== 'video') continue;
		const mimeType = report.codecId ? codecs.get(report.codecId)?.mimeType : undefined;
		return {
			codec: getVideoCodecFromMimeType(mimeType),
			mimeType,
			atMs: finiteNumber(report.timestamp) ?? Date.now(),
			packetsReceived: finiteNumber(report.packetsReceived) ?? 0,
			bytesReceived: finiteNumber(report.bytesReceived) ?? 0,
			framesDecoded: finiteNumber(report.framesDecoded),
			keyFramesDecoded: finiteNumber(report.keyFramesDecoded),
			framesReceived: finiteNumber(report.framesReceived),
			framesDropped: finiteNumber(report.framesDropped),
			freezeCount: finiteNumber(report.freezeCount),
			totalInterFrameDelay: finiteNumber(report.totalInterFrameDelay),
			totalSquaredInterFrameDelay: finiteNumber(report.totalSquaredInterFrameDelay),
			decoderImplementation:
				typeof report.decoderImplementation === 'string' && report.decoderImplementation.length > 0
					? report.decoderImplementation
					: null,
			powerEfficientDecoder: typeof report.powerEfficientDecoder === 'boolean' ? report.powerEfficientDecoder : null,
		};
	}
	return null;
}

export function findSoftwareVideoDecoder(sample: InboundVideoDecodeSample): SoftwareVideoDecoderInfo | null {
	if (!isSoftwareVideoStats(sample.decoderImplementation, sample.powerEfficientDecoder)) return null;
	return {
		codec: getCodecLabel(sample.mimeType),
		implementation: sample.decoderImplementation ?? UNKNOWN_DECODER_IMPLEMENTATION,
		powerEfficientDecoder: sample.powerEfficientDecoder,
	};
}

export function findStalledVideoDecoder(sample: InboundVideoDecodeSample): StalledVideoDecoderInfo | null {
	if (sample.codec === null) return null;
	if (sample.framesDecoded !== 0) return null;
	if (sample.keyFramesDecoded !== 0) return null;
	if (sample.framesReceived === null || sample.framesReceived < 1) return null;
	return {
		codec: sample.codec,
		mimeType: sample.mimeType,
		packetsReceived: sample.packetsReceived,
		bytesReceived: sample.bytesReceived,
		framesDecoded: sample.framesDecoded,
		framesReceived: sample.framesReceived,
		framesDropped: sample.framesDropped,
	};
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

export function computeInboundVideoDecodeHealth(
	previous: InboundVideoDecodeSample,
	current: InboundVideoDecodeSample,
): InboundVideoDecodeHealth | null {
	const intervalMs = current.atMs - previous.atMs;
	if (intervalMs <= 0) return null;
	const framesDecodedDelta = counterDelta(previous.framesDecoded, current.framesDecoded);
	const interFrameDelayDelta = counterDelta(previous.totalInterFrameDelay, current.totalInterFrameDelay);
	const squaredInterFrameDelayDelta = counterDelta(
		previous.totalSquaredInterFrameDelay,
		current.totalSquaredInterFrameDelay,
	);
	const meanInterFrameDelay =
		framesDecodedDelta === null || framesDecodedDelta === 0 || interFrameDelayDelta === null
			? null
			: interFrameDelayDelta / framesDecodedDelta;
	const variance =
		meanInterFrameDelay === null || framesDecodedDelta === null || squaredInterFrameDelayDelta === null
			? null
			: squaredInterFrameDelayDelta / framesDecodedDelta - meanInterFrameDelay ** 2;
	return {
		codec: current.codec,
		intervalMs: Math.round(intervalMs),
		decodedFps: framesDecodedDelta === null ? null : roundTo((framesDecodedDelta * 1000) / intervalMs, 1),
		framesDecodedDelta,
		meanInterFrameDelayMs: meanInterFrameDelay === null ? null : roundTo(meanInterFrameDelay * 1000, 1),
		interFrameDelayStdDevMs: variance === null || variance < 0 ? null : roundTo(Math.sqrt(variance) * 1000, 1),
		freezes: counterDelta(previous.freezeCount, current.freezeCount),
		freezeCount: current.freezeCount,
		framesDroppedDelta: counterDelta(previous.framesDropped, current.framesDropped),
		framesDecoded: current.framesDecoded,
		bytesReceivedDelta: Math.max(0, current.bytesReceived - previous.bytesReceived),
	};
}

export function monitorScreenShareDecodeHealth(
	getStats: () => Promise<RTCStatsReport | undefined>,
	onDecodeStall?: (failure: StalledVideoDecoderInfo) => void,
): () => void {
	let cancelled = false;
	let timer: ReturnType<typeof setTimeout> | null = null;
	let previousSample: InboundVideoDecodeSample | null = null;
	let pendingStall: StalledVideoDecoderInfo | null = null;
	let reportedStall = false;
	let isSoftwareDecoder = false;
	let warnedSoftwareDecoder = false;
	let isFrozen = false;

	const noteSoftwareDecoder = (sample: InboundVideoDecodeSample): void => {
		const decoder = findSoftwareVideoDecoder(sample);
		if (!decoder) {
			isSoftwareDecoder = false;
			return;
		}
		if (isSoftwareDecoder) return;
		isSoftwareDecoder = true;
		logger.warn('Screen share is using a software decoder', decoder);
		if (warnedSoftwareDecoder) return;
		warnedSoftwareDecoder = true;
		SoftwareEncoderWarning.triggerDecoderWarning(decoder.codec, decoder.implementation);
	};

	const noteDecodeStall = (sample: InboundVideoDecodeSample): void => {
		const stall = findStalledVideoDecoder(sample);
		const confirmed = confirmDecodeStall(pendingStall, stall);
		pendingStall = stall;
		if (!confirmed || reportedStall) return;
		reportedStall = true;
		logger.warn('Screen share video decode is stalled', confirmed);
		onDecodeStall?.(confirmed);
	};

	const noteDecodeHealth = (sample: InboundVideoDecodeSample): void => {
		const health = previousSample === null ? null : computeInboundVideoDecodeHealth(previousSample, sample);
		if (!health) return;
		if (health.framesDecodedDelta === 0 && health.bytesReceivedDelta > 0) {
			if (isFrozen) return;
			isFrozen = true;
			logger.warn('Screen share video is receiving bytes but decoding no frames', health);
			return;
		}
		if (isFrozen && health.framesDecodedDelta !== null && health.framesDecodedDelta > 0) {
			isFrozen = false;
			logger.info('Screen share video decoding resumed', health);
			return;
		}
		logger.debug('Screen share video decode health', health);
	};

	const sampleOnce = async (): Promise<void> => {
		try {
			const stats = await getStats();
			if (cancelled || !stats) return;
			const sample = findInboundVideoDecodeSample(stats);
			if (!sample) return;
			noteSoftwareDecoder(sample);
			noteDecodeStall(sample);
			noteDecodeHealth(sample);
			previousSample = sample;
		} catch (error) {
			logger.debug('Failed to sample the screen share decoder', {error});
		}
	};

	const scheduleNextSample = (): void => {
		timer = setTimeout(async () => {
			timer = null;
			await sampleOnce();
			if (cancelled) return;
			scheduleNextSample();
		}, DECODE_SAMPLE_INTERVAL_MS);
	};

	scheduleNextSample();
	return () => {
		cancelled = true;
		if (timer === null) return;
		clearTimeout(timer);
		timer = null;
	};
}
