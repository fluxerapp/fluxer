// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {isAutoplayBlockedError} from '@app/features/voice/components/media_player/hooks/useMediaPlayer';
import {afterEach, describe, expect, it} from 'vitest';
import {
	areBufferedSpanFractionsEqual,
	armPendingSeekTarget,
	clampMediaTime,
	clampPercentage,
	clearPendingSeekTarget,
	collectBufferedSpanFractions,
	detachMediaElementSource,
	getBufferedPercentage,
	getEffectiveMediaDuration,
	getSeekPercentageFromClientX,
	readPendingSeekTarget,
	resolveDoubleTapSeekDirection,
	stepPlayheadPrediction,
} from './MediaSeekUtils';

function mediaWithBufferedRanges({
	duration,
	currentTime,
	ranges,
}: {
	duration: number;
	currentTime: number;
	ranges: Array<[number, number]>;
}): HTMLMediaElement {
	return {
		duration,
		currentTime,
		buffered: {
			length: ranges.length,
			start: (index: number) => ranges[index]![0],
			end: (index: number) => ranges[index]![1],
		},
	} as unknown as HTMLMediaElement;
}

describe('MediaSeekUtils', () => {
	it('clamps progress and time values', () => {
		expect(clampPercentage(-25)).toBe(0);
		expect(clampPercentage(42)).toBe(42);
		expect(clampPercentage(125)).toBe(100);
		expect(clampPercentage(Number.NaN)).toBe(0);

		expect(clampMediaTime(-5, 100)).toBe(0);
		expect(clampMediaTime(30, 100)).toBe(30);
		expect(clampMediaTime(130, 100)).toBe(100);
	});

	it('maps pointer coordinates to a clamped seek percentage', () => {
		const rect = {left: 20, width: 200};

		expect(getSeekPercentageFromClientX(20, rect)).toBe(0);
		expect(getSeekPercentageFromClientX(120, rect)).toBe(50);
		expect(getSeekPercentageFromClientX(260, rect)).toBe(100);
		expect(getSeekPercentageFromClientX(-10, rect)).toBe(0);
	});

	it('uses the active buffered range when available', () => {
		const media = mediaWithBufferedRanges({
			duration: 100,
			currentTime: 25,
			ranges: [
				[0, 10],
				[20, 40],
				[60, 80],
			],
		});

		expect(getBufferedPercentage(media)).toBe(40);
	});

	it('falls back to the furthest buffered range outside the active range', () => {
		const media = mediaWithBufferedRanges({
			duration: 100,
			currentTime: 50,
			ranges: [
				[0, 10],
				[20, 40],
				[60, 80],
			],
		});

		expect(getBufferedPercentage(media)).toBe(80);
	});

	it('detects same-side double taps for mobile video seeking', () => {
		const first = {x: 20, width: 100, time: 1000};

		expect(resolveDoubleTapSeekDirection(first, {x: 24, width: 100, time: 1200})).toBe('backward');
		expect(resolveDoubleTapSeekDirection({x: 90, width: 100, time: 1000}, {x: 84, width: 100, time: 1200})).toBe(
			'forward',
		);
		expect(resolveDoubleTapSeekDirection(first, {x: 50, width: 100, time: 1200})).toBeNull();
		expect(resolveDoubleTapSeekDirection(first, {x: 24, width: 100, time: 1500})).toBeNull();
	});
});

describe('detachMediaElementSource', () => {
	const loadedElements: Array<HTMLMediaElement> = [];
	let restoreLoad: (() => void) | null = null;

	const stubLoad = (implementation: (element: HTMLMediaElement) => void): void => {
		const prototype = window.HTMLMediaElement.prototype;
		const original = Object.getOwnPropertyDescriptor(prototype, 'load');
		Object.defineProperty(prototype, 'load', {
			configurable: true,
			writable: true,
			value: function (this: HTMLMediaElement) {
				implementation(this);
			},
		});
		restoreLoad = () => {
			if (original) Object.defineProperty(prototype, 'load', original);
			else Reflect.deleteProperty(prototype as unknown as Record<string, unknown>, 'load');
		};
	};

	const buildVideoWithChildren = (): HTMLVideoElement => {
		const video = document.createElement('video');
		video.setAttribute('src', 'https://media.test/clip.mp4');
		const source = document.createElement('source');
		source.setAttribute('src', 'https://media.test/clip.webm');
		source.setAttribute('type', 'video/webm');
		const poster = document.createElement('img');
		poster.setAttribute('src', 'https://media.test/poster.webp');
		video.append(source, poster);
		document.body.append(video);
		return video;
	};

	afterEach(() => {
		loadedElements.length = 0;
		restoreLoad?.();
		restoreLoad = null;
		document.body.innerHTML = '';
	});

	it('strips every url off the element and its children and then reloads it', () => {
		stubLoad((element) => loadedElements.push(element));
		const video = buildVideoWithChildren();

		detachMediaElementSource(video);

		expect(video.getAttribute('src')).toBeNull();
		expect(video.querySelector('source')?.getAttribute('src')).toBeNull();
		expect(video.querySelector('source')?.getAttribute('type')).toBeNull();
		expect(video.querySelector('img')?.getAttribute('src')).toBeNull();
		expect(loadedElements).toEqual([video]);
	});

	it('still clears the urls when the element refuses to reload', () => {
		stubLoad(() => {
			throw new Error('load is not implemented');
		});
		const video = buildVideoWithChildren();

		expect(() => detachMediaElementSource(video)).not.toThrow();
		expect(video.getAttribute('src')).toBeNull();
		expect(video.querySelector('source')?.getAttribute('src')).toBeNull();
	});

	it('does nothing when there is no element left to release', () => {
		stubLoad((element) => loadedElements.push(element));

		expect(() => detachMediaElementSource(null)).not.toThrow();
		expect(loadedElements).toEqual([]);
	});
});

describe('buffered ranges shorter than a second', () => {
	it('ignores a sliver that would otherwise become the reported buffer', () => {
		const media = mediaWithBufferedRanges({
			duration: 100,
			currentTime: 50,
			ranges: [
				[0, 10],
				[60, 60.4],
			],
		});

		expect(getBufferedPercentage(media)).toBe(10);
	});

	it('ignores a sliver even when the playhead sits inside it', () => {
		const media = mediaWithBufferedRanges({
			duration: 100,
			currentTime: 60.2,
			ranges: [
				[0, 10],
				[60, 60.4],
			],
		});

		expect(getBufferedPercentage(media)).toBe(10);
	});
});

describe('isAutoplayBlockedError', () => {
	it('recognises only the browser refusing to start playback', () => {
		expect(isAutoplayBlockedError(new DOMException('blocked', 'NotAllowedError'))).toBe(true);
		expect(isAutoplayBlockedError(new DOMException('no source', 'NotSupportedError'))).toBe(false);
		expect(isAutoplayBlockedError(new Error('NotAllowedError'))).toBe(false);
		expect(isAutoplayBlockedError('NotAllowedError')).toBe(false);
		expect(isAutoplayBlockedError(null)).toBe(false);
	});
});

describe('collectBufferedSpanFractions', () => {
	it('emits one normalised span per disjoint buffered island, so the bar can leave the gap between them unpainted', () => {
		const media = mediaWithBufferedRanges({
			duration: 120,
			currentTime: 0,
			ranges: [
				[0, 30],
				[60, 90],
			],
		});

		expect(collectBufferedSpanFractions(media)).toStrictEqual([
			{offset: 0, span: 0.25},
			{offset: 0.5, span: 0.25},
		]);
	});

	it('drops a buffered island shorter than the minimum, so a sliver never paints as an island of its own', () => {
		const media = mediaWithBufferedRanges({
			duration: 120,
			currentTime: 0,
			ranges: [
				[0, 30],
				[60, 60.5],
				[90, 120],
			],
		});

		expect(collectBufferedSpanFractions(media)).toStrictEqual([
			{offset: 0, span: 0.25},
			{offset: 0.75, span: 0.25},
		]);
	});

	it('emits nothing while the duration is still unknown, so the bar does not paint against a NaN denominator', () => {
		const media = mediaWithBufferedRanges({duration: Number.NaN, currentTime: 0, ranges: [[0, 30]]});

		expect(collectBufferedSpanFractions(media)).toStrictEqual([]);
	});

	it('never lets a span run past the end of the track', () => {
		const media = mediaWithBufferedRanges({duration: 100, currentTime: 0, ranges: [[80, 400]]});

		const spans = collectBufferedSpanFractions(media);

		expect(spans).toHaveLength(1);
		expect(spans[0]!.offset + spans[0]!.span).toBe(1);
	});
});

describe('areBufferedSpanFractionsEqual', () => {
	it('reports two separately built span lists with the same contents as equal, so an unchanged buffer commits nothing', () => {
		expect(
			areBufferedSpanFractionsEqual(
				[
					{offset: 0, span: 0.25},
					{offset: 0.5, span: 0.25},
				],
				[
					{offset: 0, span: 0.25},
					{offset: 0.5, span: 0.25},
				],
			),
		).toBe(true);
	});

	it('reports a grown island as different, so real buffering progress still repaints', () => {
		expect(areBufferedSpanFractionsEqual([{offset: 0, span: 0.25}], [{offset: 0, span: 0.3}])).toBe(false);
	});

	it('reports a moved island as different, so a span that slid along the track still repaints', () => {
		expect(areBufferedSpanFractionsEqual([{offset: 0, span: 0.25}], [{offset: 0.1, span: 0.25}])).toBe(false);
	});

	it('reports a new island as different, so a second island appearing still repaints', () => {
		expect(
			areBufferedSpanFractionsEqual(
				[{offset: 0, span: 0.25}],
				[
					{offset: 0, span: 0.25},
					{offset: 0.5, span: 0.25},
				],
			),
		).toBe(false);
	});
});

describe('the pending seek latch', () => {
	it('hands the armed target back to a reader that only holds the element', () => {
		const media = document.createElement('audio');

		armPendingSeekTarget(media, 42);

		expect(readPendingSeekTarget(media)).toBe(42);
	});

	it('keeps each element on its own latch, so seeking one player does not suppress another', () => {
		const seeking = document.createElement('audio');
		const idle = document.createElement('audio');

		armPendingSeekTarget(seeking, 42);

		expect(readPendingSeekTarget(idle)).toBeNull();
	});

	it('reads back null once the latch is cleared, so a settled seek stops suppressing time updates', () => {
		const media = document.createElement('audio');
		armPendingSeekTarget(media, 42);

		clearPendingSeekTarget(media);

		expect(readPendingSeekTarget(media)).toBeNull();
	});

	it('treats a target of zero as armed rather than as an empty latch', () => {
		const media = document.createElement('audio');

		armPendingSeekTarget(media, 0);

		expect(readPendingSeekTarget(media)).toBe(0);
	});
});

describe('stepPlayheadPrediction', () => {
	it('seeds the prediction from the element clock on the first frame, when there is nothing to predict from yet', () => {
		expect(stepPlayheadPrediction({predictedSeconds: null, actualSeconds: 12.25, elapsedSeconds: 0})).toBe(12.25);
	});

	it('advances the prediction by the wall-clock delta between frames while the element clock stands still', () => {
		expect(stepPlayheadPrediction({predictedSeconds: 10, actualSeconds: 10, elapsedSeconds: 0.1})).toBeCloseTo(
			10.09,
			10,
		);
	});

	it('scales the advance by the playback rate, so a double-speed frame predicts twice the wall-clock delta', () => {
		expect(
			stepPlayheadPrediction({predictedSeconds: 10, actualSeconds: 10, elapsedSeconds: 0.1, playbackRate: 2}),
		).toBeCloseTo(10.18, 10);
	});

	it('closes a tenth of a small error toward the element clock rather than jumping to it', () => {
		expect(stepPlayheadPrediction({predictedSeconds: 10, actualSeconds: 10.3, elapsedSeconds: 0.1})).toBeCloseTo(
			10.12,
			10,
		);
	});

	it('hard-snaps to the element clock when a seek moves it further than the snap threshold', () => {
		expect(stepPlayheadPrediction({predictedSeconds: 10, actualSeconds: 40, elapsedSeconds: 0.016})).toBe(40);
	});

	it('hard-snaps a drift just over half a second rather than converging on it', () => {
		expect(stepPlayheadPrediction({predictedSeconds: 10, actualSeconds: 10.6, elapsedSeconds: 0})).toBe(10.6);
	});

	it('converges on a drift just under half a second rather than snapping to it', () => {
		expect(stepPlayheadPrediction({predictedSeconds: 10, actualSeconds: 10.4, elapsedSeconds: 0})).toBeCloseTo(
			10.04,
			10,
		);
	});

	it('converges on a drift sitting exactly on the snap threshold rather than snapping to it', () => {
		expect(stepPlayheadPrediction({predictedSeconds: 10, actualSeconds: 10.5, elapsedSeconds: 0})).toBeCloseTo(
			10.05,
			10,
		);
	});

	it('hard-snaps backwards too, so a seek to the head of the media does not creep back over many frames', () => {
		expect(stepPlayheadPrediction({predictedSeconds: 30, actualSeconds: 0, elapsedSeconds: 0.016})).toBe(0);
	});

	it('keeps predicting from the last prediction when the element clock reads a non-finite value', () => {
		expect(stepPlayheadPrediction({predictedSeconds: 10, actualSeconds: Number.NaN, elapsedSeconds: 0.5})).toBeCloseTo(
			10.5,
			10,
		);
	});

	it('never predicts a negative playhead', () => {
		expect(stepPlayheadPrediction({predictedSeconds: null, actualSeconds: -5, elapsedSeconds: 0})).toBe(0);
	});
});

describe('getEffectiveMediaDuration', () => {
	it('reports the length the element itself knows, ignoring the supplied fallback', () => {
		expect(getEffectiveMediaDuration({duration: 90} as HTMLMediaElement, 120)).toBe(90);
	});

	it('falls back to the supplied length while the element still reports none, so a fresh element still has a scale', () => {
		expect(getEffectiveMediaDuration({duration: Number.NaN} as HTMLMediaElement, 120)).toBe(120);
	});

	it('reports no length at all when the fallback runs past every real value, rather than scaling against infinity', () => {
		expect(getEffectiveMediaDuration(null, Number.POSITIVE_INFINITY)).toBe(0);
	});

	it('reports no length at all when the fallback is negative, rather than scaling against a backwards duration', () => {
		expect(getEffectiveMediaDuration(null, -30)).toBe(0);
	});
});
