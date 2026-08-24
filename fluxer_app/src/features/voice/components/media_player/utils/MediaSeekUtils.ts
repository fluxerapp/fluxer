// SPDX-License-Identifier: AGPL-3.0-or-later

export const MIN_BUFFERED_RANGE_SECONDS = 1;

export const EMPTY_BUFFERED_SPANS: ReadonlyArray<BufferedSpanFraction> = Object.freeze([]);

export interface BufferedSpanFraction {
	offset: number;
	span: number;
}

const pendingSeekTargets = new WeakMap<HTMLMediaElement, number>();

export function armPendingSeekTarget(media: HTMLMediaElement, time: number): void {
	pendingSeekTargets.set(media, time);
}

export function readPendingSeekTarget(media: HTMLMediaElement | null | undefined): number | null {
	if (!media) return null;
	const target = pendingSeekTargets.get(media);
	return target === undefined ? null : target;
}

export function clearPendingSeekTarget(media: HTMLMediaElement | null | undefined): void {
	if (!media) return;
	pendingSeekTargets.delete(media);
}

export type SeekDirection = 'backward' | 'forward';

export interface SeekTapPoint {
	x: number;
	width: number;
	time: number;
}

export function clampPercentage(percentage: number): number {
	if (!Number.isFinite(percentage)) return 0;
	return Math.max(0, Math.min(100, percentage));
}

export function clampMediaTime(time: number, duration: number): number {
	if (!Number.isFinite(time)) return 0;
	if (!Number.isFinite(duration) || duration <= 0) return Math.max(0, time);
	return Math.max(0, Math.min(duration, time));
}

export function quantiseMediaTimeToSecond(time: number): number {
	if (!Number.isFinite(time)) return 0;
	return Math.trunc(time);
}

export function getFiniteMediaDuration(media: HTMLMediaElement | null | undefined): number {
	if (!media || !Number.isFinite(media.duration) || media.duration <= 0) return 0;
	return media.duration;
}

export function getEffectiveMediaDuration(media: HTMLMediaElement | null | undefined, fallbackDuration = 0): number {
	return (
		getFiniteMediaDuration(media) || (Number.isFinite(fallbackDuration) && fallbackDuration > 0 ? fallbackDuration : 0)
	);
}

export function getSeekPercentageFromClientX(clientX: number, rect: Pick<DOMRect, 'left' | 'width'>): number {
	if (!Number.isFinite(rect.width) || rect.width <= 0) return 0;
	return clampPercentage(((clientX - rect.left) / rect.width) * 100);
}

export function getBufferedPercentage(media: HTMLMediaElement): number {
	const duration = getFiniteMediaDuration(media);
	if (!duration || !media.buffered.length) return 0;

	const currentTime = media.currentTime;
	let bufferedEnd = 0;
	for (let i = 0; i < media.buffered.length; i++) {
		const start = media.buffered.start(i);
		const end = media.buffered.end(i);
		if (end - start < MIN_BUFFERED_RANGE_SECONDS) {
			continue;
		}
		if (currentTime >= start && currentTime <= end) {
			bufferedEnd = end;
			break;
		}
		if (end > bufferedEnd) {
			bufferedEnd = end;
		}
	}
	return clampPercentage((bufferedEnd / duration) * 100);
}

export function collectBufferedSpanFractions(
	media: HTMLMediaElement | null | undefined,
): ReadonlyArray<BufferedSpanFraction> {
	const duration = getFiniteMediaDuration(media);
	if (!media || duration <= 0) return EMPTY_BUFFERED_SPANS;
	const ranges = media.buffered;
	if (ranges == null || ranges.length === 0) return EMPTY_BUFFERED_SPANS;

	const spans: Array<BufferedSpanFraction> = [];
	for (let i = 0; i < ranges.length; i++) {
		const start = ranges.start(i);
		const end = ranges.end(i);
		if (!Number.isFinite(start) || !Number.isFinite(end)) continue;
		if (end - start < MIN_BUFFERED_RANGE_SECONDS) continue;
		const offset = Math.max(0, Math.min(1, start / duration));
		const span = Math.max(0, Math.min(1 - offset, (end - start) / duration));
		if (span <= 0) continue;
		spans.push({offset, span});
	}
	return spans.length === 0 ? EMPTY_BUFFERED_SPANS : spans;
}

export function areBufferedSpanFractionsEqual(
	a: ReadonlyArray<BufferedSpanFraction>,
	b: ReadonlyArray<BufferedSpanFraction>,
): boolean {
	if (a === b) return true;
	if (a.length !== b.length) return false;
	for (let i = 0; i < a.length; i++) {
		if (a[i].offset !== b[i].offset || a[i].span !== b[i].span) return false;
	}
	return true;
}

function isElementOfKind(element: Element, constructorName: string): boolean {
	const view = element.ownerDocument?.defaultView as unknown as Record<string, unknown> | null | undefined;
	const elementConstructor = view?.[constructorName];
	if (typeof elementConstructor !== 'function') return false;
	return element instanceof (elementConstructor as new () => Element);
}

export function detachMediaElementSource(media: HTMLMediaElement | null | undefined): void {
	if (!media) return;
	try {
		media.pause();
	} catch {}
	media.removeAttribute('src');
	if ('srcObject' in media && media.srcObject != null) {
		media.srcObject = null;
	}
	for (const child of Array.from(media.children)) {
		if (isElementOfKind(child, 'HTMLSourceElement')) {
			child.removeAttribute('src');
			child.removeAttribute('type');
		} else if (isElementOfKind(child, 'HTMLImageElement')) {
			child.removeAttribute('src');
		}
	}
	try {
		media.load();
	} catch {}
}

export function resolveDoubleTapSeekDirection(
	previousTap: SeekTapPoint | null,
	currentTap: SeekTapPoint,
	options: {
		maxIntervalMs?: number;
		sideZoneRatio?: number;
	} = {},
): SeekDirection | null {
	const {maxIntervalMs = 360, sideZoneRatio = 0.42} = options;
	if (!previousTap) return null;
	if (currentTap.time - previousTap.time > maxIntervalMs) return null;
	if (currentTap.width <= 0 || previousTap.width <= 0) return null;

	const currentRatio = currentTap.x / currentTap.width;
	const previousRatio = previousTap.x / previousTap.width;
	if (currentRatio <= sideZoneRatio && previousRatio <= sideZoneRatio) return 'backward';
	if (currentRatio >= 1 - sideZoneRatio && previousRatio >= 1 - sideZoneRatio) return 'forward';
	return null;
}

export const PLAYHEAD_SNAP_THRESHOLD_SECONDS = 0.5;

export const PLAYHEAD_CONVERGENCE_RATE = 0.1;

export interface PlayheadPredictionStep {
	predictedSeconds: number | null;
	actualSeconds: number;
	elapsedSeconds: number;
	playbackRate?: number;
}

export function stepPlayheadPrediction(step: PlayheadPredictionStep): number {
	const {predictedSeconds, actualSeconds, elapsedSeconds, playbackRate = 1} = step;
	const hasActual = Number.isFinite(actualSeconds);
	if (predictedSeconds === null || !Number.isFinite(predictedSeconds)) {
		return hasActual ? Math.max(0, actualSeconds) : 0;
	}
	const rate = Number.isFinite(playbackRate) && playbackRate > 0 ? playbackRate : 1;
	const elapsed = Number.isFinite(elapsedSeconds) && elapsedSeconds > 0 ? elapsedSeconds : 0;
	const advancedSeconds = predictedSeconds + elapsed * rate;
	if (!hasActual) return Math.max(0, advancedSeconds);
	const error = actualSeconds - advancedSeconds;
	if (Math.abs(error) > PLAYHEAD_SNAP_THRESHOLD_SECONDS) return Math.max(0, actualSeconds);
	return Math.max(0, advancedSeconds + PLAYHEAD_CONVERGENCE_RATE * error);
}
