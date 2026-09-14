// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createScrollIndicatorSnapshot,
	getScrollIndicatorStateValue,
	resolveActiveScrollIndicator,
	resolveScrollIndicatorEdgeCandidates,
	type ScrollIndicatorMeasurement,
	type ScrollIndicatorTargetMeasurement,
	selectActiveScrollIndicator,
	transitionScrollIndicatorSnapshot,
} from '@app/features/app/components/layout/ScrollIndicatorStateMachine';
import {describe, expect, it} from 'vitest';

function target(
	id: string,
	top: number,
	bottom: number,
	severity: ScrollIndicatorTargetMeasurement['severity'] = 'unread',
	order = 0,
): ScrollIndicatorTargetMeasurement {
	return {id, top, bottom, severity, order};
}

function measurement(targets: Array<ScrollIndicatorTargetMeasurement>, scrollTop = 100): ScrollIndicatorMeasurement {
	return {
		scrollTop,
		viewportHeight: 200,
		targets,
	};
}

describe('ScrollIndicatorStateMachine', () => {
	it('hides when there are no offscreen unread targets', () => {
		const snapshot = createScrollIndicatorSnapshot();
		const next = transitionScrollIndicatorSnapshot(snapshot, {
			type: 'scrollIndicator.measured',
			measurement: measurement([target('visible', 120, 160)]),
		});
		expect(getScrollIndicatorStateValue(next)).toBe('hidden');
		expect(selectActiveScrollIndicator(next)).toBeNull();
	});

	it('treats partially visible targets as visible, not floating-pill targets', () => {
		const candidates = resolveScrollIndicatorEdgeCandidates(
			measurement([target('partly-above', 90, 120), target('partly-below', 290, 320)]),
		);
		expect(candidates.topIndicator).toBeNull();
		expect(candidates.bottomIndicator).toBeNull();
	});

	it('selects the nearest offscreen target at the same severity', () => {
		const active = resolveActiveScrollIndicator(
			measurement([
				target('far-above', 0, 40, 'unread', 0),
				target('near-above', 80, 90, 'unread', 1),
				target('far-below', 500, 540, 'unread', 2),
			]),
		);
		expect(active?.direction).toBe('top');
		expect(active?.indicator.id).toBe('near-above');
		expect(active?.indicator.distance).toBe(10);
	});

	it('prioritizes mentions over plain unread targets regardless of distance', () => {
		const active = resolveActiveScrollIndicator(
			measurement([target('near-unread', 80, 90, 'unread', 0), target('far-mention', 600, 640, 'mention', 1)]),
		);
		expect(active?.direction).toBe('bottom');
		expect(active?.indicator.id).toBe('far-mention');
	});

	it('uses the latest scroll direction to break exact ties', () => {
		const active = resolveActiveScrollIndicator({
			...measurement([target('above', 80, 90, 'unread', 0), target('below', 310, 320, 'unread', 1)]),
			preferredDirection: 'bottom',
		});
		expect(active?.direction).toBe('bottom');
		expect(active?.indicator.id).toBe('below');
	});

	it('does not keep a stale active target after collapse or read-state changes remove it', () => {
		let snapshot = createScrollIndicatorSnapshot();
		snapshot = transitionScrollIndicatorSnapshot(snapshot, {
			type: 'scrollIndicator.measured',
			measurement: measurement([target('unread-above', 80, 90)]),
		});
		expect(getScrollIndicatorStateValue(snapshot)).toBe('visible');
		expect(selectActiveScrollIndicator(snapshot)?.indicator.id).toBe('unread-above');

		snapshot = transitionScrollIndicatorSnapshot(snapshot, {
			type: 'scrollIndicator.measured',
			measurement: measurement([]),
		});
		expect(getScrollIndicatorStateValue(snapshot)).toBe('hidden');
		expect(selectActiveScrollIndicator(snapshot)).toBeNull();
	});

	it('switches from an expanded folder child marker to the collapsed folder marker', () => {
		let snapshot = createScrollIndicatorSnapshot();
		snapshot = transitionScrollIndicatorSnapshot(snapshot, {
			type: 'scrollIndicator.measured',
			measurement: measurement([target('guild-inside-folder', 340, 380, 'mention')]),
		});
		expect(getScrollIndicatorStateValue(snapshot)).toBe('visible');
		expect(selectActiveScrollIndicator(snapshot)?.indicator.id).toBe('guild-inside-folder');

		snapshot = transitionScrollIndicatorSnapshot(snapshot, {
			type: 'scrollIndicator.measured',
			measurement: measurement([target('folder-1', 340, 380, 'mention')]),
		});
		expect(getScrollIndicatorStateValue(snapshot)).toBe('visible');
		expect(selectActiveScrollIndicator(snapshot)?.indicator.id).toBe('folder-1');
	});

	it('hides immediately when the previously offscreen target moves into the viewport', () => {
		let snapshot = createScrollIndicatorSnapshot();
		snapshot = transitionScrollIndicatorSnapshot(snapshot, {
			type: 'scrollIndicator.measured',
			measurement: measurement([target('unread-above', 80, 90)]),
		});
		snapshot = transitionScrollIndicatorSnapshot(snapshot, {
			type: 'scrollIndicator.measured',
			measurement: measurement([target('unread-above', 120, 160)]),
		});
		expect(getScrollIndicatorStateValue(snapshot)).toBe('hidden');
		expect(selectActiveScrollIndicator(snapshot)).toBeNull();
	});

	it('resets when the scroll container is unavailable', () => {
		let snapshot = createScrollIndicatorSnapshot();
		snapshot = transitionScrollIndicatorSnapshot(snapshot, {
			type: 'scrollIndicator.measured',
			measurement: measurement([target('unread-below', 320, 340)]),
		});
		expect(getScrollIndicatorStateValue(snapshot)).toBe('visible');

		snapshot = transitionScrollIndicatorSnapshot(snapshot, {type: 'scrollIndicator.reset'});
		expect(getScrollIndicatorStateValue(snapshot)).toBe('hidden');
		expect(selectActiveScrollIndicator(snapshot)).toBeNull();
	});
});
