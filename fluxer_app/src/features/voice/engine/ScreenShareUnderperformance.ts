// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ScreenShareEncoderMode} from '@app/features/voice/utils/CodecCapabilityDetector';
import type {
	ScreenShareContext,
	ScreenShareKeptAxis,
	ScreenShareLevel,
} from '@app/features/voice/utils/ScreenShareOptions';
import type {VideoCodec} from 'livekit-client';

export type ScreenShareQualityLimitationReason = 'none' | 'cpu' | 'bandwidth' | 'other' | 'unknown';

export function parseScreenShareQualityLimitationReason(value: unknown): ScreenShareQualityLimitationReason {
	if (value === 'none' || value === 'cpu' || value === 'bandwidth' || value === 'other') {
		return value;
	}
	return 'unknown';
}

export const SCREEN_SHARE_DELIVERY_FIRST_TICK_MS = 2500;
export const SCREEN_SHARE_DELIVERY_TICK_MS = 2000;
const GAP_MS = 3000;
const WARMUP_MIN_TICKS = 3;
const RAMP_MAX_TICKS = 14;
const RAMP_LOOKBACK_TICKS = 3;
const RAMP_RISE_RATIO = 1.05;
const RAMP_STALL_TICKS = 3;
const STALL_TICKS = 2;
const WINDOW_TICKS = 5;
const ACT_TICKS = 4;
const SHORT_RATIO = 0.85;
const RESOLUTION_SHORT_RATIO = 0.9;
const WATCHABLE_TEXT_FPS = 7.5;
const MOTION_CAP_RATIO = 0.85;
const CONNECTION_TARGET_RATIO = 0.85;
const TARGET_FILL_RATIO = 0.65;
const ENCODE_DUTY_LIMIT = 1.0;
const CPU_REASON_TICKS = 3;
const IDLE_EXPECTED_FPS = 2;
const IDLE_SENT_RATIO = 0.25;
const SOURCE_LIMITED_MIN_FPS = 2;
const CLEAR_OK_TICKS = 5;
const PROBE_BUSY_OK_TICKS = 30;
const MULTI_LAYER_MIN_VIEWERS = 2;
const MULTI_LAYER_ON_TICKS = 5;
const MULTI_LAYER_OFF_TICKS = 30;
const MAX_LAYERING_CHANGES = 2;
const MAX_UP_PROBES = 4;
const MAX_DELIVERY_SWITCHES = 1;
const PROBE_RETRY_MS = 86_400_000;
const MEMORY_TTL_MS = 14 * 24 * 60 * 60 * 1000;
const MAX_MEMORY_ENTRIES = 64;

interface DeliveryStatsEntry {
	type?: string;
	id?: string;
	kind?: string;
	mediaType?: string;
	mimeType?: string;
	codecId?: string;
	mediaSourceId?: string;
	active?: boolean;
	framesEncoded?: number;
	bytesSent?: number;
	headerBytesSent?: number;
	totalEncodeTime?: number;
	frameWidth?: number;
	frameHeight?: number;
	targetBitrate?: number;
	qualityLimitationReason?: unknown;
	encoderImplementation?: string;
	timestamp?: number;
	frames?: number;
	framesPerSecond?: number;
	width?: number;
	height?: number;
}

export interface ScreenShareDeliveryCounters {
	timestamp: number;
	framesEncoded: number;
	bytesSent: number;
	headerBytesSent: number;
	totalEncodeTime: number | null;
	sourceFrames: number | null;
}

export interface ScreenShareDeliverySample {
	active: boolean;
	counters: ScreenShareDeliveryCounters | null;
	dtMs: number | null;
	encodedFrames: number | null;
	encodedFps: number | null;
	sourceFrames: number | null;
	sourceFps: number | null;
	reportedSourceFps: number | null;
	sentBps: number | null;
	encodeSecondsPerFrame: number | null;
	fill: number | null;
	sentWidth: number | null;
	sentHeight: number | null;
	captureWidth: number | null;
	captureHeight: number | null;
	targetBitrate: number | null;
	limitationReason: ScreenShareQualityLimitationReason;
	encoderImplementation: string | null;
	countersReset: boolean;
}

export type ScreenShareDeliveryTickKind = 'inactive' | 'gap' | 'stalled' | 'idle' | 'short' | 'resolution-short' | 'ok';

export type ScreenShareDeliveryNotice =
	| {kind: 'device-framerate'; width: number; height: number; frameRate: number}
	| {kind: 'device-resolution'; width: number; height: number; frameRate: number}
	| {
			kind: 'device-crossover';
			width: number;
			height: number;
			frameRate: number;
			targetWidth: number;
			targetHeight: number;
			targetFrameRate: number;
	  }
	| {kind: 'codec-switched'; codec: VideoCodec; previousCodec: VideoCodec}
	| {kind: 'device-short'; width: number; height: number; frameRate: number; deliveredFrameRate: number}
	| {kind: 'connection-short'; width: number; height: number; frameRate: number; deliveredFrameRate: number}
	| {kind: 'browser-resolution'; width: number; height: number; deliveredWidth: number; deliveredHeight: number}
	| {kind: 'capture-resolution'; width: number; height: number; deliveredWidth: number; deliveredHeight: number}
	| {
			kind: 'started-lower';
			width: number;
			height: number;
			frameRate: number;
			targetWidth: number;
			targetHeight: number;
			targetFrameRate: number;
	  }
	| {kind: 'pinned-codec-short'; codec: VideoCodec}
	| {kind: 'encoder-recovered'; codec: VideoCodec}
	| {kind: 'codec-viewer'; codec: VideoCodec; pinnedCodec: VideoCodec}
	| {kind: 'codec-device'; codec: VideoCodec; pinnedCodec: VideoCodec}
	| {kind: 'apply-failed'};

export type ScreenShareDeliveryDecision =
	| {kind: 'none'}
	| {kind: 'step'; levelIndex: number}
	| {kind: 'probe'; levelIndex: number}
	| {kind: 'revert-probe'; levelIndex: number}
	| {kind: 'reapply-level'}
	| {kind: 'switch-codec'; codec: VideoCodec}
	| {kind: 'republish-layering'; multiLayer: boolean}
	| {kind: 'recover-stalled'}
	| {kind: 'stop-stalled'};

export type ScreenShareDeliveryMemoryWrite =
	| {kind: 'short'; ratio: number}
	| {kind: 'settled'; settledIndex: number}
	| {kind: 'probe-blocked'};

export interface ScreenShareDeliveryShareCounters {
	switches: number;
	probes: number;
	layerings: number;
	probeBlocked: boolean;
	toastShown: boolean;
}

export interface ScreenShareDeliveryContext {
	adaptive: boolean;
	levels: ReadonlyArray<ScreenShareLevel>;
	keep: ScreenShareKeptAxis;
	context: ScreenShareContext;
	codec: VideoCodec;
	pinned: boolean;
	alternativeCodec: VideoCodec | null;
	codecFailed: boolean;
	lastApplyFailed: boolean;
	paused: boolean;
	viewers: number;
	multiLayer: boolean;
	multiLayerAvailable: boolean;
	monitoredMaxBitrate: number;
	shareCounters: ScreenShareDeliveryShareCounters;
}

interface ScreenShareDeliveryWindowTick {
	kind: ScreenShareDeliveryTickKind;
	ratio: number;
	encodedFps: number;
	sourceFps: number | null;
	targetBitrate: number | null;
	cpu: boolean;
	duty: number | null;
	fill: number | null;
	sentWidth: number | null;
	sentHeight: number | null;
}

export type ScreenShareDeliveryShortCause =
	| 'cpu'
	| 'stalled'
	| 'duty'
	| 'ratio'
	| 'ratio-unmeasured'
	| 'fill'
	| 'connection'
	| 'connection-unmeasured';

export interface ScreenShareDeliveryState {
	ticks: number;
	levelIndex: number;
	samples: number;
	warmedUp: boolean;
	targets: Array<number | null>;
	window: Array<ScreenShareDeliveryWindowTick>;
	stalledRun: number;
	okRun: number;
	busyOkRun: number;
	multiLayerRun: number;
	singleLayerRun: number;
	probeFrom: number | null;
	settledWritten: boolean;
	resolutionReapplied: boolean;
	pinnedNoticeShown: boolean;
	applyFailed: boolean;
	holdingShort: boolean;
	notice: ScreenShareDeliveryNotice | null;
	shortCause: ScreenShareDeliveryShortCause | null;
	sourceFrameRate: number | null;
	deliveredFrameRate: number | null;
	sentWidth: number | null;
	sentHeight: number | null;
}

export interface ScreenShareDeliveryEvaluation {
	kind: ScreenShareDeliveryTickKind;
	decision: ScreenShareDeliveryDecision;
	notice: ScreenShareDeliveryNotice | null;
	noticeChanged: boolean;
	memoryWrite: ScreenShareDeliveryMemoryWrite | null;
	levelIndex: number;
	warmingUp: boolean;
	shortCause: ScreenShareDeliveryShortCause | null;
}

export interface ScreenShareDeliveryDescription {
	level: ScreenShareLevel;
	levelIndex: number;
	deliveredFrameRate: number | null;
	sentWidth: number | null;
	sentHeight: number | null;
	sourceFrameRate: number | null;
	notice: ScreenShareDeliveryNotice | null;
	holdingShort: boolean;
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

function finiteOrNull(value: unknown): number | null {
	return typeof value === 'number' && Number.isFinite(value) ? value : null;
}

function median(values: ReadonlyArray<number>): number {
	const sorted = [...values].sort((first, second) => first - second);
	return sorted[Math.floor((sorted.length - 1) / 2)];
}

const INACTIVE_SAMPLE_FIELDS = {
	active: false,
	counters: null,
	dtMs: null,
	encodedFrames: null,
	encodedFps: null,
	sourceFrames: null,
	sourceFps: null,
	reportedSourceFps: null,
	sentBps: null,
	encodeSecondsPerFrame: null,
	fill: null,
	sentWidth: null,
	sentHeight: null,
	captureWidth: null,
	captureHeight: null,
	targetBitrate: null,
	limitationReason: 'unknown',
	encoderImplementation: null,
	countersReset: false,
} as const satisfies ScreenShareDeliverySample;

export function readScreenShareDeliverySample(
	stats: RTCStatsReport,
	previousCounters: ScreenShareDeliveryCounters | null,
): ScreenShareDeliverySample {
	const reportsById = new Map<string, DeliveryStatsEntry>();
	const outbound: Array<DeliveryStatsEntry> = [];
	for (const raw of stats.values()) {
		const report = raw as DeliveryStatsEntry;
		if (typeof report.id === 'string') {
			reportsById.set(report.id, report);
		}
		if (report.type === 'outbound-rtp') {
			outbound.push(report);
		}
	}
	let selected: DeliveryStatsEntry | null = null;
	let selectedPixels = -1;
	for (const report of outbound) {
		if (getStatsKind(report, reportsById) !== 'video') continue;
		if (report.active === false) continue;
		const pixels = (finiteOrNull(report.frameWidth) ?? 0) * (finiteOrNull(report.frameHeight) ?? 0);
		if (pixels <= selectedPixels) continue;
		selected = report;
		selectedPixels = pixels;
	}
	if (selected === null) return {...INACTIVE_SAMPLE_FIELDS};
	const source = selected.mediaSourceId ? reportsById.get(selected.mediaSourceId) : undefined;
	const timestamp = finiteOrNull(selected.timestamp) ?? 0;
	const framesEncoded = finiteOrNull(selected.framesEncoded) ?? 0;
	const bytesSent = finiteOrNull(selected.bytesSent) ?? 0;
	const headerBytesSent = finiteOrNull(selected.headerBytesSent) ?? 0;
	const totalEncodeTime = finiteOrNull(selected.totalEncodeTime);
	const sourceFrames = finiteOrNull(source?.frames);
	const countersReset =
		previousCounters !== null &&
		(timestamp < previousCounters.timestamp ||
			framesEncoded < previousCounters.framesEncoded ||
			bytesSent < previousCounters.bytesSent ||
			headerBytesSent < previousCounters.headerBytesSent ||
			(totalEncodeTime !== null &&
				previousCounters.totalEncodeTime !== null &&
				totalEncodeTime < previousCounters.totalEncodeTime) ||
			(sourceFrames !== null &&
				previousCounters.sourceFrames !== null &&
				sourceFrames < previousCounters.sourceFrames));
	const previous = countersReset ? null : previousCounters;
	const dtMs = previous === null ? null : timestamp - previous.timestamp;
	const seconds = dtMs !== null && dtMs > 0 ? dtMs / 1000 : null;
	const encodedFrames = framesEncoded - (previous?.framesEncoded ?? 0);
	const previousSourceFrames = previous === null ? 0 : previous.sourceFrames;
	const sourceFrameDelta =
		sourceFrames === null || previousSourceFrames === null ? null : sourceFrames - previousSourceFrames;
	const reportedSourceFps = finiteOrNull(source?.framesPerSecond);
	const targetBitrate = finiteOrNull(selected.targetBitrate);
	const previousEncodeTime = previous === null ? null : previous.totalEncodeTime;
	const sentWithHeaderBps =
		seconds === null || previous === null
			? null
			: ((bytesSent - previous.bytesSent + headerBytesSent - previous.headerBytesSent) * 8) / seconds;
	return {
		active: true,
		counters: {timestamp, framesEncoded, bytesSent, headerBytesSent, totalEncodeTime, sourceFrames},
		dtMs,
		encodedFrames,
		encodedFps: seconds === null ? null : encodedFrames / seconds,
		sourceFrames: sourceFrameDelta,
		sourceFps: seconds !== null && sourceFrameDelta !== null ? sourceFrameDelta / seconds : (reportedSourceFps ?? null),
		reportedSourceFps,
		sentBps: seconds === null || previous === null ? null : ((bytesSent - previous.bytesSent) * 8) / seconds,
		encodeSecondsPerFrame:
			totalEncodeTime === null || previousEncodeTime === null || encodedFrames <= 0
				? null
				: (totalEncodeTime - previousEncodeTime) / encodedFrames,
		fill: sentWithHeaderBps === null || targetBitrate === null ? null : sentWithHeaderBps / targetBitrate,
		sentWidth: finiteOrNull(selected.frameWidth),
		sentHeight: finiteOrNull(selected.frameHeight),
		captureWidth: finiteOrNull(source?.width),
		captureHeight: finiteOrNull(source?.height),
		targetBitrate,
		limitationReason: parseScreenShareQualityLimitationReason(selected.qualityLimitationReason),
		encoderImplementation: typeof selected.encoderImplementation === 'string' ? selected.encoderImplementation : null,
		countersReset,
	};
}

function isStalledSample(sample: ScreenShareDeliverySample): boolean {
	if (sample.encodedFrames !== 0) return false;
	return (sample.sourceFrames ?? 0) >= 2 || (sample.reportedSourceFps ?? 0) > 0;
}

function resolveExpectedFrameRate(sample: ScreenShareDeliverySample, level: ScreenShareLevel): number {
	if (sample.limitationReason === 'cpu') return level.frameRate;
	if (sample.sourceFps === null) return level.frameRate;
	return Math.min(level.frameRate, sample.sourceFps);
}

function resolveExpectedPixels(
	sample: ScreenShareDeliverySample,
	level: ScreenShareLevel,
	context: ScreenShareContext,
): number | null {
	const levelPixels = level.width * level.height;
	if (context === 'device') return levelPixels;
	if (sample.captureWidth === null || sample.captureHeight === null) return null;
	return Math.min(levelPixels, sample.captureWidth * sample.captureHeight);
}

function measureScreenShareTick(
	sample: ScreenShareDeliverySample,
	level: ScreenShareLevel,
	ctx: ScreenShareDeliveryContext,
): {kind: ScreenShareDeliveryTickKind; ratio: number} {
	if (!sample.active || ctx.paused) return {kind: 'inactive', ratio: 1};
	if (sample.countersReset || (sample.dtMs !== null && sample.dtMs > GAP_MS)) return {kind: 'gap', ratio: 1};
	if (isStalledSample(sample)) return {kind: 'stalled', ratio: 0};
	if (sample.encodedFps === null) return {kind: 'gap', ratio: 1};
	const expectedFrameRate = resolveExpectedFrameRate(sample, level);
	const ratio = expectedFrameRate > 0 ? sample.encodedFps / expectedFrameRate : 1;
	if (expectedFrameRate < IDLE_EXPECTED_FPS) return {kind: 'idle', ratio};
	if (
		sample.sourceFps === null &&
		sample.sentBps !== null &&
		sample.sentBps < IDLE_SENT_RATIO * ctx.monitoredMaxBitrate
	) {
		return {kind: 'idle', ratio};
	}
	if (ratio < SHORT_RATIO) return {kind: 'short', ratio};
	if (ctx.multiLayer) return {kind: 'ok', ratio};
	const expectedPixels = resolveExpectedPixels(sample, level, ctx.context);
	if (expectedPixels !== null && sample.sentWidth !== null && sample.sentHeight !== null) {
		if (sample.sentWidth * sample.sentHeight < RESOLUTION_SHORT_RATIO * expectedPixels) {
			return {kind: 'resolution-short', ratio};
		}
	}
	return {kind: 'ok', ratio};
}

export function clampScreenShareLevelIndex(levelIndex: number, levelCount: number): number {
	return Math.max(0, Math.min(levelIndex, levelCount - 1));
}

export function createScreenShareDeliveryState(levelIndex = 0): ScreenShareDeliveryState {
	return {
		ticks: 0,
		levelIndex,
		samples: 0,
		warmedUp: false,
		targets: [],
		window: [],
		stalledRun: 0,
		okRun: 0,
		busyOkRun: 0,
		multiLayerRun: 0,
		singleLayerRun: 0,
		probeFrom: null,
		settledWritten: false,
		resolutionReapplied: false,
		pinnedNoticeShown: false,
		applyFailed: false,
		holdingShort: false,
		notice: null,
		shortCause: null,
		sourceFrameRate: null,
		deliveredFrameRate: null,
		sentWidth: null,
		sentHeight: null,
	};
}

export function resetScreenShareDeliveryWindow(state: ScreenShareDeliveryState): void {
	state.samples = 0;
	state.warmedUp = false;
	state.targets = [];
	state.window = [];
	state.stalledRun = 0;
	state.okRun = 0;
	state.busyOkRun = 0;
	state.multiLayerRun = 0;
	state.singleLayerRun = 0;
	state.settledWritten = false;
	state.sourceFrameRate = null;
	state.holdingShort = false;
}

function isSameNotice(left: ScreenShareDeliveryNotice | null, right: ScreenShareDeliveryNotice | null): boolean {
	if (left === null || right === null) return left === right;
	if (left.kind !== right.kind) return false;
	const leftFields = left as unknown as Record<string, unknown>;
	const rightFields = right as unknown as Record<string, unknown>;
	return Object.keys(leftFields).every((field) => leftFields[field] === rightFields[field]);
}

function setNotice(state: ScreenShareDeliveryState, notice: ScreenShareDeliveryNotice | null): void {
	if (isSameNotice(state.notice, notice)) return;
	state.notice = notice;
}

function resolveLevelNotice(
	levels: ReadonlyArray<ScreenShareLevel>,
	levelIndex: number,
): ScreenShareDeliveryNotice | null {
	const level = levels[levelIndex];
	const target = levels[0];
	if (levelIndex === 0) return null;
	const slowerRate = level.frameRate < target.frameRate;
	const smallerSize = level.width * level.height < target.width * target.height;
	if (slowerRate && smallerSize) {
		return {
			kind: 'device-crossover',
			width: level.width,
			height: level.height,
			frameRate: level.frameRate,
			targetWidth: target.width,
			targetHeight: target.height,
			targetFrameRate: target.frameRate,
		};
	}
	if (slowerRate) {
		return {kind: 'device-framerate', width: level.width, height: level.height, frameRate: level.frameRate};
	}
	return {kind: 'device-resolution', width: level.width, height: level.height, frameRate: level.frameRate};
}

function resolveNextLevelIndex(
	state: ScreenShareDeliveryState,
	ctx: ScreenShareDeliveryContext,
	deliveredFrameRate: number,
): number | null {
	const levels = ctx.levels;
	const index = state.levelIndex;
	let lastSegmentZero = 0;
	for (const level of levels) {
		if (level.segment === 0) lastSegmentZero = level.index;
	}
	if (index < lastSegmentZero) return index + 1;
	if (ctx.keep === 'resolution') {
		if (index + 1 >= levels.length) return null;
		if (index === lastSegmentZero && deliveredFrameRate >= WATCHABLE_TEXT_FPS) return null;
		return index + 1;
	}
	for (let candidate = Math.max(index + 1, lastSegmentZero + 1); candidate < levels.length; candidate++) {
		const level = levels[candidate];
		if (level.frameRate > deliveredFrameRate) continue;
		return level.frameRate >= MOTION_CAP_RATIO * deliveredFrameRate ? candidate : null;
	}
	return null;
}

function resolveSourceLimitedFrameRate(
	window: ReadonlyArray<ScreenShareDeliveryWindowTick>,
	level: ScreenShareLevel,
): number | null {
	if (window.some((tick) => tick.cpu)) return null;
	const rates: Array<number> = [];
	for (const tick of window) {
		if (tick.sourceFps === null) return null;
		rates.push(tick.sourceFps);
	}
	const sourceFrameRate = median(rates);
	if (sourceFrameRate < SOURCE_LIMITED_MIN_FPS) return null;
	return sourceFrameRate < SHORT_RATIO * level.frameRate ? sourceFrameRate : null;
}

function isShortTick(tick: ScreenShareDeliveryWindowTick): boolean {
	return tick.kind === 'short' || tick.kind === 'stalled';
}

function finish(
	state: ScreenShareDeliveryState,
	kind: ScreenShareDeliveryTickKind,
	decision: ScreenShareDeliveryDecision,
	memoryWrite: ScreenShareDeliveryMemoryWrite | null,
	previousNotice: ScreenShareDeliveryNotice | null,
): ScreenShareDeliveryEvaluation {
	return {
		kind,
		decision,
		notice: state.notice,
		noticeChanged: state.notice !== previousNotice,
		memoryWrite,
		levelIndex: state.levelIndex,
		warmingUp: !state.warmedUp,
		shortCause: state.shortCause,
	};
}

export function evaluateScreenShareDelivery(
	state: ScreenShareDeliveryState,
	sample: ScreenShareDeliverySample,
	ctx: ScreenShareDeliveryContext,
): ScreenShareDeliveryEvaluation {
	const evaluation = evaluateScreenShareDeliveryTick(state, sample, ctx);
	if (evaluation.decision.kind !== 'none') return evaluation;
	const multiLayer = resolveScreenShareLayerCrossing(state, ctx);
	if (multiLayer === null) return evaluation;
	return {...evaluation, decision: {kind: 'republish-layering', multiLayer}};
}

function resolveScreenShareLayerCrossing(
	state: ScreenShareDeliveryState,
	ctx: ScreenShareDeliveryContext,
): boolean | null {
	if (state.applyFailed) return null;
	if (state.levelIndex !== 0 || state.probeFrom !== null) return null;
	if (state.window.length < WINDOW_TICKS) return null;
	if (ctx.shareCounters.layerings >= MAX_LAYERING_CHANGES) return null;
	if (ctx.multiLayer && !ctx.multiLayerAvailable) return false;
	if (!ctx.multiLayer && ctx.multiLayerAvailable && state.multiLayerRun >= MULTI_LAYER_ON_TICKS) return true;
	if (ctx.multiLayer && state.singleLayerRun >= MULTI_LAYER_OFF_TICKS) return false;
	return null;
}

function evaluateScreenShareDeliveryTick(
	state: ScreenShareDeliveryState,
	sample: ScreenShareDeliverySample,
	ctx: ScreenShareDeliveryContext,
): ScreenShareDeliveryEvaluation {
	const previousNotice = state.notice;
	state.shortCause = null;
	state.ticks += 1;
	state.levelIndex = clampScreenShareLevelIndex(state.levelIndex, ctx.levels.length);
	if (state.probeFrom !== null) {
		state.probeFrom = clampScreenShareLevelIndex(state.probeFrom, ctx.levels.length);
	}
	const level = ctx.levels[state.levelIndex];
	const {kind, ratio} = measureScreenShareTick(sample, level, ctx);
	if (sample.encodedFps !== null) {
		state.deliveredFrameRate = sample.encodedFps;
	}
	if (sample.sentWidth !== null && sample.sentHeight !== null) {
		state.sentWidth = sample.sentWidth;
		state.sentHeight = sample.sentHeight;
	}
	if (ctx.adaptive && ctx.lastApplyFailed && !state.applyFailed) {
		state.applyFailed = true;
		setNotice(state, {kind: 'apply-failed'});
	}
	if (kind === 'inactive' || kind === 'gap') {
		resetScreenShareDeliveryWindow(state);
		return finish(state, kind, {kind: 'none'}, null, previousNotice);
	}
	if (kind === 'stalled') {
		state.stalledRun += 1;
		const stallTicks = state.ticks === 1 ? 1 : state.warmedUp ? STALL_TICKS : RAMP_STALL_TICKS;
		if (state.stalledRun >= stallTicks) {
			resetScreenShareDeliveryWindow(state);
			const recoverable = ctx.alternativeCodec !== null && !ctx.codecFailed;
			return finish(state, kind, {kind: recoverable ? 'recover-stalled' : 'stop-stalled'}, null, previousNotice);
		}
	} else {
		state.stalledRun = 0;
	}
	const wasWarmedUp = state.warmedUp;
	state.samples += 1;
	if (!wasWarmedUp) {
		state.targets.push(sample.targetBitrate);
		if (state.targets.length > RAMP_LOOKBACK_TICKS + 1) {
			state.targets.shift();
		}
		state.warmedUp = isRampOver(state, sample, ctx.monitoredMaxBitrate);
		return finish(state, kind, {kind: 'none'}, null, previousNotice);
	}
	if (!ctx.adaptive) return finish(state, kind, {kind: 'none'}, null, previousNotice);
	const manyViewers = ctx.viewers >= MULTI_LAYER_MIN_VIEWERS;
	state.multiLayerRun = manyViewers ? state.multiLayerRun + 1 : 0;
	state.singleLayerRun = manyViewers ? 0 : state.singleLayerRun + 1;
	if (kind === 'ok') {
		state.okRun += 1;
		const busy = sample.sourceFps === null || sample.sourceFps >= SHORT_RATIO * level.frameRate;
		state.busyOkRun = busy ? state.busyOkRun + 1 : 0;
	} else {
		state.okRun = 0;
		state.busyOkRun = 0;
	}
	if (
		state.okRun >= CLEAR_OK_TICKS &&
		state.levelIndex === 0 &&
		state.notice !== null &&
		state.notice.kind !== 'apply-failed'
	) {
		state.notice = null;
	}
	if (kind === 'idle') return finish(state, kind, {kind: 'none'}, null, previousNotice);
	state.window.push({
		kind,
		ratio,
		encodedFps: sample.encodedFps ?? 0,
		sourceFps: sample.sourceFps,
		targetBitrate: sample.targetBitrate,
		cpu: sample.limitationReason === 'cpu',
		duty: sample.encodeSecondsPerFrame === null ? null : sample.encodeSecondsPerFrame * level.frameRate,
		fill: sample.fill,
		sentWidth: sample.sentWidth,
		sentHeight: sample.sentHeight,
	});
	if (state.window.length > WINDOW_TICKS) {
		state.window.shift();
	}
	if (state.window.length < WINDOW_TICKS) return finish(state, kind, {kind: 'none'}, null, previousNotice);
	state.sourceFrameRate = resolveSourceLimitedFrameRate(state.window, level);
	if (state.applyFailed) return finish(state, kind, {kind: 'none'}, null, previousNotice);
	const shortTicks = state.window.filter(isShortTick).length;
	const resolutionShortTicks = state.window.filter((tick) => tick.kind === 'resolution-short').length;
	if (shortTicks >= ACT_TICKS) return decideShortWindow(state, ctx, level, kind, previousNotice);
	if (resolutionShortTicks >= ACT_TICKS) return decideResolutionWindow(state, ctx, level, kind, previousNotice);
	if (shortTicks === 0 && resolutionShortTicks === 0) return decideCleanWindow(state, ctx, kind, previousNotice);
	return finish(state, kind, {kind: 'none'}, null, previousNotice);
}

function isRampOver(
	state: ScreenShareDeliveryState,
	sample: ScreenShareDeliverySample,
	monitoredMaxBitrate: number,
): boolean {
	if (state.samples < WARMUP_MIN_TICKS) return false;
	if (state.samples >= RAMP_MAX_TICKS) return true;
	if (sample.targetBitrate === null) return false;
	if (sample.targetBitrate >= CONNECTION_TARGET_RATIO * monitoredMaxBitrate) return true;
	if (state.targets.length <= RAMP_LOOKBACK_TICKS) return false;
	const earlier = state.targets[0];
	return earlier !== null && sample.targetBitrate <= RAMP_RISE_RATIO * earlier;
}

function canWriteMemory(window: ReadonlyArray<ScreenShareDeliveryWindowTick>): boolean {
	if (window.every((tick) => tick.targetBitrate !== null)) return true;
	return window.filter((tick) => tick.cpu).length * 2 > window.length;
}

function windowMedian(
	ticks: ReadonlyArray<ScreenShareDeliveryWindowTick>,
	read: (tick: ScreenShareDeliveryWindowTick) => number | null,
): number | null {
	const values = ticks.map(read).filter((value): value is number => value !== null);
	return values.length === ticks.length ? median(values) : null;
}

function classifyShortWindow(
	windowTicks: ReadonlyArray<ScreenShareDeliveryWindowTick>,
	monitoredMaxBitrate: number,
): ScreenShareDeliveryShortCause {
	if (windowTicks.filter((tick) => tick.cpu).length >= CPU_REASON_TICKS) return 'cpu';
	const measured = windowTicks.filter((tick) => tick.kind !== 'stalled');
	if (measured.length === 0) return 'stalled';
	const dutyMedian = windowMedian(measured, (tick) => tick.duty);
	if (dutyMedian !== null && dutyMedian >= ENCODE_DUTY_LIMIT) return 'duty';
	const targetMedian = windowMedian(measured, (tick) => tick.targetBitrate);
	if (targetMedian === null) return 'ratio-unmeasured';
	if (targetMedian >= CONNECTION_TARGET_RATIO * monitoredMaxBitrate) return 'ratio';
	const fillMedian = windowMedian(measured, (tick) => tick.fill);
	if (fillMedian === null || fillMedian < TARGET_FILL_RATIO) return 'fill';
	return dutyMedian === null ? 'connection-unmeasured' : 'connection';
}

function decideShortWindow(
	state: ScreenShareDeliveryState,
	ctx: ScreenShareDeliveryContext,
	level: ScreenShareLevel,
	kind: ScreenShareDeliveryTickKind,
	previousNotice: ScreenShareDeliveryNotice | null,
): ScreenShareDeliveryEvaluation {
	const windowTicks = state.window;
	const deliveredFrameRate = median(windowTicks.map((tick) => tick.encodedFps));
	const cause = classifyShortWindow(windowTicks, ctx.monitoredMaxBitrate);
	const connection = cause === 'connection' || cause === 'connection-unmeasured';
	state.shortCause = cause;
	state.holdingShort = true;
	if (connection) {
		setNotice(state, {
			kind: 'connection-short',
			width: level.width,
			height: level.height,
			frameRate: level.frameRate,
			deliveredFrameRate: Math.round(deliveredFrameRate),
		});
		state.probeFrom = null;
		return finish(state, kind, {kind: 'none'}, null, previousNotice);
	}
	const writable = canWriteMemory(windowTicks);
	if (state.probeFrom !== null) {
		const revertTo = state.probeFrom;
		state.probeFrom = null;
		state.levelIndex = revertTo;
		resetScreenShareDeliveryWindow(state);
		setNotice(state, resolveLevelNotice(ctx.levels, revertTo));
		return finish(
			state,
			kind,
			{kind: 'revert-probe', levelIndex: revertTo},
			writable ? {kind: 'probe-blocked'} : null,
			previousNotice,
		);
	}
	const memoryWrite: ScreenShareDeliveryMemoryWrite | null =
		state.levelIndex === 0 && writable ? {kind: 'short', ratio: median(windowTicks.map((tick) => tick.ratio))} : null;
	const replacement =
		state.levelIndex === 0 && ctx.shareCounters.switches < MAX_DELIVERY_SWITCHES ? ctx.alternativeCodec : null;
	if (replacement !== null && !ctx.pinned) {
		resetScreenShareDeliveryWindow(state);
		setNotice(state, {kind: 'codec-switched', codec: replacement, previousCodec: ctx.codec});
		return finish(state, kind, {kind: 'switch-codec', codec: replacement}, memoryWrite, previousNotice);
	}
	if (replacement !== null && !state.pinnedNoticeShown) {
		state.pinnedNoticeShown = true;
		resetScreenShareDeliveryWindow(state);
		state.holdingShort = true;
		setNotice(state, {kind: 'pinned-codec-short', codec: ctx.codec});
		return finish(state, kind, {kind: 'none'}, memoryWrite, previousNotice);
	}
	const nextLevelIndex = resolveNextLevelIndex(state, ctx, deliveredFrameRate);
	if (nextLevelIndex === null) {
		setNotice(state, {
			kind: 'device-short',
			width: level.width,
			height: level.height,
			frameRate: level.frameRate,
			deliveredFrameRate: Math.round(deliveredFrameRate),
		});
		return finish(state, kind, {kind: 'none'}, memoryWrite, previousNotice);
	}
	state.levelIndex = nextLevelIndex;
	resetScreenShareDeliveryWindow(state);
	setNotice(state, resolveLevelNotice(ctx.levels, nextLevelIndex));
	return finish(state, kind, {kind: 'step', levelIndex: nextLevelIndex}, memoryWrite, previousNotice);
}

function decideResolutionWindow(
	state: ScreenShareDeliveryState,
	ctx: ScreenShareDeliveryContext,
	level: ScreenShareLevel,
	kind: ScreenShareDeliveryTickKind,
	previousNotice: ScreenShareDeliveryNotice | null,
): ScreenShareDeliveryEvaluation {
	if (!state.resolutionReapplied) {
		state.resolutionReapplied = true;
		resetScreenShareDeliveryWindow(state);
		return finish(state, kind, {kind: 'reapply-level'}, null, previousNotice);
	}
	const last = state.window[state.window.length - 1];
	state.holdingShort = true;
	if (last.sentWidth !== null && last.sentHeight !== null) {
		setNotice(state, {
			kind: ctx.context === 'device' ? 'capture-resolution' : 'browser-resolution',
			width: level.width,
			height: level.height,
			deliveredWidth: last.sentWidth,
			deliveredHeight: last.sentHeight,
		});
	}
	return finish(state, kind, {kind: 'none'}, null, previousNotice);
}

function decideCleanWindow(
	state: ScreenShareDeliveryState,
	ctx: ScreenShareDeliveryContext,
	kind: ScreenShareDeliveryTickKind,
	previousNotice: ScreenShareDeliveryNotice | null,
): ScreenShareDeliveryEvaluation {
	state.holdingShort = false;
	state.probeFrom = null;
	let memoryWrite: ScreenShareDeliveryMemoryWrite | null = null;
	if (!state.settledWritten && canWriteMemory(state.window)) {
		state.settledWritten = true;
		memoryWrite = {kind: 'settled', settledIndex: state.levelIndex};
	}
	const probeable =
		state.levelIndex > 0 &&
		state.busyOkRun >= PROBE_BUSY_OK_TICKS &&
		ctx.shareCounters.probes < MAX_UP_PROBES &&
		!ctx.shareCounters.probeBlocked;
	if (!probeable) return finish(state, kind, {kind: 'none'}, memoryWrite, previousNotice);
	const probeFrom = state.levelIndex;
	state.levelIndex = probeFrom - 1;
	resetScreenShareDeliveryWindow(state);
	state.probeFrom = probeFrom;
	return finish(state, kind, {kind: 'probe', levelIndex: state.levelIndex}, memoryWrite, previousNotice);
}

export function describeScreenShareDelivery(
	state: ScreenShareDeliveryState,
	levels: ReadonlyArray<ScreenShareLevel>,
): ScreenShareDeliveryDescription {
	const levelIndex = clampScreenShareLevelIndex(state.levelIndex, levels.length);
	return {
		level: levels[levelIndex],
		levelIndex,
		deliveredFrameRate: state.deliveredFrameRate,
		sentWidth: state.sentWidth,
		sentHeight: state.sentHeight,
		sourceFrameRate: state.sourceFrameRate,
		notice: state.notice,
		holdingShort: state.holdingShort,
	};
}

export type ScreenShareDeliveryNoticeTone = 'warning' | 'info';

const SCREEN_SHARE_DELIVERY_NOTICE_TONES = {
	'device-framerate': 'info',
	'device-resolution': 'info',
	'device-crossover': 'info',
	'codec-switched': 'info',
	'device-short': 'warning',
	'connection-short': 'warning',
	'browser-resolution': 'warning',
	'capture-resolution': 'warning',
	'started-lower': 'info',
	'pinned-codec-short': 'warning',
	'encoder-recovered': 'info',
	'codec-viewer': 'info',
	'codec-device': 'info',
	'apply-failed': 'warning',
} satisfies Record<ScreenShareDeliveryNotice['kind'], ScreenShareDeliveryNoticeTone>;

export function resolveScreenShareDeliveryNoticeTone(notice: ScreenShareDeliveryNotice): ScreenShareDeliveryNoticeTone {
	return SCREEN_SHARE_DELIVERY_NOTICE_TONES[notice.kind];
}

export interface ScreenShareDeliverySentInfo {
	width: number;
	height: number;
	fps: number;
}

export function showsScreenShareDeliverySentInfo(isOwnScreenShare: boolean, adaptive: boolean): boolean {
	return isOwnScreenShare && adaptive;
}

export function resolveScreenShareDeliverySentInfo(
	description: Pick<ScreenShareDeliveryDescription, 'deliveredFrameRate' | 'sentHeight' | 'sentWidth'> | null,
): ScreenShareDeliverySentInfo | null {
	if (description === null || description.sentWidth === null || description.sentHeight === null) return null;
	return {
		width: description.sentWidth,
		height: description.sentHeight,
		fps: description.deliveredFrameRate === null ? 0 : Math.round(description.deliveredFrameRate),
	};
}

export function shouldToastScreenShareNotice(
	shareCounters: ScreenShareDeliveryShareCounters,
	notice: ScreenShareDeliveryNotice | null,
): boolean {
	return notice !== null && !shareCounters.toastShown;
}

export interface ScreenShareDeliveryMemoryHeader {
	chromiumMajor: number | null;
	gpuKey: string;
	encoderMode: ScreenShareEncoderMode;
	hardwareAccelerationDisabled: boolean;
}

export interface ScreenShareDeliveryMemoryEntry {
	short: boolean;
	ratio: number;
	settledIndex: number | null;
	probeBlockedUntil: number | null;
	at: number;
}

export interface ScreenShareDeliveryMemory {
	header: ScreenShareDeliveryMemoryHeader;
	entries: Record<string, ScreenShareDeliveryMemoryEntry>;
}

export interface ScreenShareDeliveryMemoryKey {
	header: ScreenShareDeliveryMemoryHeader;
	memoryKey: string;
	hardware: Partial<Record<VideoCodec, boolean>>;
	now: number;
}

function buildEntryKey(codec: VideoCodec, hardware: boolean, memoryKey: string): string {
	return `${codec}|${hardware ? 'hardware' : 'software'}|${memoryKey}`;
}

function isSameHeader(left: ScreenShareDeliveryMemoryHeader, right: ScreenShareDeliveryMemoryHeader): boolean {
	return (
		left.chromiumMajor === right.chromiumMajor &&
		left.gpuKey === right.gpuKey &&
		left.encoderMode === right.encoderMode &&
		left.hardwareAccelerationDisabled === right.hardwareAccelerationDisabled
	);
}

function isStoredEntry(value: unknown): value is ScreenShareDeliveryMemoryEntry {
	if (typeof value !== 'object' || value === null) return false;
	const entry = value as ScreenShareDeliveryMemoryEntry;
	return (
		typeof entry.short === 'boolean' &&
		typeof entry.ratio === 'number' &&
		Number.isFinite(entry.ratio) &&
		(entry.settledIndex === null || (Number.isInteger(entry.settledIndex) && entry.settledIndex >= 0)) &&
		(entry.probeBlockedUntil === null || Number.isFinite(entry.probeBlockedUntil)) &&
		Number.isFinite(entry.at)
	);
}

function getLiveEntries(
	stored: ScreenShareDeliveryMemory | null,
	header: ScreenShareDeliveryMemoryHeader,
	now: number,
): Record<string, ScreenShareDeliveryMemoryEntry> {
	if (stored === null || !isSameHeader(stored.header, header)) return {};
	const entries: Record<string, ScreenShareDeliveryMemoryEntry> = {};
	for (const [key, entry] of Object.entries(stored.entries)) {
		if (!isStoredEntry(entry)) continue;
		if (now - entry.at > MEMORY_TTL_MS) continue;
		entries[key] = entry;
	}
	return entries;
}

export function readScreenShareDeliveryMemory(
	stored: ScreenShareDeliveryMemory | null,
	key: ScreenShareDeliveryMemoryKey,
): Partial<Record<VideoCodec, ScreenShareDeliveryMemoryEntry>> {
	const entries = getLiveEntries(stored, key.header, key.now);
	const verdicts: Partial<Record<VideoCodec, ScreenShareDeliveryMemoryEntry>> = {};
	for (const [codec, hardware] of Object.entries(key.hardware) as Array<[VideoCodec, boolean]>) {
		const entry = entries[buildEntryKey(codec, hardware, key.memoryKey)];
		if (entry) {
			verdicts[codec] = entry;
		}
	}
	return verdicts;
}

export function updateScreenShareDeliveryMemory(
	stored: ScreenShareDeliveryMemory | null,
	key: ScreenShareDeliveryMemoryKey,
	codec: VideoCodec,
	write: ScreenShareDeliveryMemoryWrite,
): ScreenShareDeliveryMemory {
	const entries = getLiveEntries(stored, key.header, key.now);
	const entryKey = buildEntryKey(codec, key.hardware[codec] === true, key.memoryKey);
	const previous = entries[entryKey] ?? {short: false, ratio: 1, settledIndex: null, probeBlockedUntil: null, at: 0};
	if (write.kind === 'short') {
		entries[entryKey] = {...previous, short: true, ratio: write.ratio, at: key.now};
	} else if (write.kind === 'settled') {
		entries[entryKey] = {...previous, short: false, settledIndex: write.settledIndex, at: key.now};
	} else {
		entries[entryKey] = {...previous, probeBlockedUntil: key.now + PROBE_RETRY_MS, at: key.now};
	}
	const keys = Object.keys(entries);
	if (keys.length <= MAX_MEMORY_ENTRIES) return {header: key.header, entries};
	const kept = keys
		.sort((left, right) => entries[right].at - entries[left].at)
		.slice(0, MAX_MEMORY_ENTRIES)
		.map((live): [string, ScreenShareDeliveryMemoryEntry] => [live, entries[live]]);
	return {header: key.header, entries: Object.fromEntries(kept)};
}

export function clearScreenShareDeliveryMemoryKey(
	stored: ScreenShareDeliveryMemory | null,
	key: ScreenShareDeliveryMemoryKey,
): ScreenShareDeliveryMemory {
	const entries = Object.entries(getLiveEntries(stored, key.header, key.now)).filter(
		([entryKey]) => !entryKey.endsWith(`|${key.memoryKey}`),
	);
	return {header: key.header, entries: Object.fromEntries(entries)};
}
