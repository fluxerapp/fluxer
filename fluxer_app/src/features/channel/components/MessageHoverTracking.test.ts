// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	refreshMessageHoverTargets,
	registerMessageHoverTarget,
	resetMessageHoverTrackingForTests,
} from '@app/features/channel/components/MessageHoverTracking';
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';

let hitTarget: Element | null = null;
let elementFromPointSpy: ReturnType<typeof vi.spyOn>;
let matchesSpy: ReturnType<typeof vi.spyOn>;

function nextFrame(): Promise<void> {
	return new Promise((resolve) => {
		requestAnimationFrame(() => resolve());
	});
}

function createRow(): {row: HTMLElement; content: HTMLElement} {
	const row = document.createElement('div');
	const content = document.createElement('span');
	row.append(content);
	document.body.append(row);
	return {row, content};
}

function movePointerTo(target: Element | null, x = 10, y = 10): void {
	hitTarget = target;
	window.dispatchEvent(new MouseEvent('pointermove', {clientX: x, clientY: y, bubbles: true}));
}

function scrollTo(target: Element | null): void {
	hitTarget = target;
	window.dispatchEvent(new Event('scroll'));
}

beforeEach(() => {
	hitTarget = null;
	elementFromPointSpy = vi.spyOn(document, 'elementFromPoint').mockImplementation(() => hitTarget);
	matchesSpy = vi.spyOn(Element.prototype, 'matches');
});

afterEach(() => {
	vi.useRealTimers();
	resetMessageHoverTrackingForTests();
	elementFromPointSpy.mockRestore();
	matchesSpy.mockRestore();
	document.body.replaceChildren();
});

describe('MessageHoverTracking', () => {
	it('arms the row that owns the element under the pointer', async () => {
		const {row, content} = createRow();
		const listener = vi.fn();
		registerMessageHoverTarget(row, listener);
		await nextFrame();
		expect(listener).not.toHaveBeenCalled();

		movePointerTo(content);
		await nextFrame();

		expect(listener).toHaveBeenLastCalledWith(true);
	});

	it('keeps at most one row armed when the pointer moves between adjacent rows', async () => {
		const first = createRow();
		const second = createRow();
		const firstListener = vi.fn();
		const secondListener = vi.fn();
		registerMessageHoverTarget(first.row, firstListener);
		registerMessageHoverTarget(second.row, secondListener);

		movePointerTo(first.content);
		await nextFrame();
		expect(firstListener).toHaveBeenLastCalledWith(true);
		expect(secondListener).not.toHaveBeenCalled();

		movePointerTo(second.content);
		await nextFrame();
		expect(firstListener).toHaveBeenLastCalledWith(false);
		expect(secondListener).toHaveBeenLastCalledWith(true);
	});

	it('does not re-arm a row on window refocus when the pointer left during the blur', async () => {
		const {row, content} = createRow();
		const listener = vi.fn();
		registerMessageHoverTarget(row, listener);

		movePointerTo(content);
		await nextFrame();
		expect(listener).toHaveBeenLastCalledWith(true);

		window.dispatchEvent(new Event('blur'));
		expect(listener).toHaveBeenLastCalledWith(false);

		window.dispatchEvent(new Event('focus'));
		refreshMessageHoverTargets();
		await nextFrame();
		await nextFrame();

		expect(listener).toHaveBeenLastCalledWith(false);
		expect(listener.mock.calls.filter(([isHovered]) => isHovered === true)).toHaveLength(1);
	});

	it('never consults the retained :hover chain as a hover oracle', async () => {
		const {row, content} = createRow();
		registerMessageHoverTarget(row, vi.fn());

		movePointerTo(content);
		await nextFrame();
		window.dispatchEvent(new Event('blur'));
		window.dispatchEvent(new Event('focus'));
		refreshMessageHoverTargets();
		await nextFrame();

		expect(matchesSpy.mock.calls.flat()).not.toContain(':hover');
	});

	it('disarms and does not re-arm while rows scroll under a stationary pointer', async () => {
		const first = createRow();
		const second = createRow();
		const firstListener = vi.fn();
		const secondListener = vi.fn();
		registerMessageHoverTarget(first.row, firstListener);
		registerMessageHoverTarget(second.row, secondListener);

		movePointerTo(first.content);
		await nextFrame();
		expect(firstListener).toHaveBeenLastCalledWith(true);

		scrollTo(second.content);
		await nextFrame();

		expect(firstListener).toHaveBeenLastCalledWith(false);
		expect(secondListener).not.toHaveBeenCalled();
	});

	it('arms the row under the pointer once scrolling settles', async () => {
		vi.useFakeTimers();
		const first = createRow();
		const second = createRow();
		const firstListener = vi.fn();
		const secondListener = vi.fn();
		registerMessageHoverTarget(first.row, firstListener);
		registerMessageHoverTarget(second.row, secondListener);

		hitTarget = first.content;
		window.dispatchEvent(new MouseEvent('pointermove', {clientX: 10, clientY: 10, bubbles: true}));
		await vi.advanceTimersByTimeAsync(20);
		expect(firstListener).toHaveBeenLastCalledWith(true);

		scrollTo(second.content);
		await vi.advanceTimersByTimeAsync(20);
		expect(secondListener).not.toHaveBeenCalled();

		await vi.advanceTimersByTimeAsync(200);
		expect(secondListener).toHaveBeenLastCalledWith(true);
	});

	it('stays suspended for as long as momentum keeps firing scroll events', async () => {
		vi.useFakeTimers();
		const {row, content} = createRow();
		const listener = vi.fn();
		registerMessageHoverTarget(row, listener);

		hitTarget = content;
		window.dispatchEvent(new MouseEvent('pointermove', {clientX: 10, clientY: 10, bubbles: true}));
		await vi.advanceTimersByTimeAsync(20);
		expect(listener).toHaveBeenLastCalledWith(true);

		scrollTo(content);
		await vi.advanceTimersByTimeAsync(20);
		expect(listener).toHaveBeenLastCalledWith(false);

		for (let i = 0; i < 6; i++) {
			await vi.advanceTimersByTimeAsync(100);
			scrollTo(content);
		}
		expect(listener).toHaveBeenLastCalledWith(false);

		await vi.advanceTimersByTimeAsync(200);
		expect(listener).toHaveBeenLastCalledWith(true);
	});

	it('re-arms immediately when the pointer actually moves during a scroll', async () => {
		const {row, content} = createRow();
		const listener = vi.fn();
		registerMessageHoverTarget(row, listener);

		scrollTo(content);
		await nextFrame();
		expect(listener).not.toHaveBeenCalledWith(true);

		movePointerTo(content);
		await nextFrame();
		expect(listener).toHaveBeenLastCalledWith(true);
	});

	it('does not let a scroll-driven pointerover re-arm a row', async () => {
		const {row, content} = createRow();
		const listener = vi.fn();
		registerMessageHoverTarget(row, listener);

		scrollTo(content);
		await nextFrame();

		hitTarget = content;
		window.dispatchEvent(new MouseEvent('pointerover', {clientX: 10, clientY: 10, bubbles: true}));
		await nextFrame();

		expect(listener).not.toHaveBeenCalledWith(true);
	});

	it('re-arms the row underneath after an overlay that covered it is dismissed', async () => {
		const {row, content} = createRow();
		const overlay = document.createElement('div');
		document.body.append(overlay);
		const listener = vi.fn();
		registerMessageHoverTarget(row, listener);

		movePointerTo(content);
		await nextFrame();
		expect(listener).toHaveBeenLastCalledWith(true);

		hitTarget = overlay;
		refreshMessageHoverTargets();
		await nextFrame();
		expect(listener).toHaveBeenLastCalledWith(false);

		overlay.remove();
		hitTarget = content;
		refreshMessageHoverTargets();
		await nextFrame();

		expect(listener).toHaveBeenLastCalledWith(true);
	});

	it('does not treat a touch contact as hover', async () => {
		const {row, content} = createRow();
		const listener = vi.fn();
		registerMessageHoverTarget(row, listener);

		hitTarget = content;
		window.dispatchEvent(new PointerEvent('pointerdown', {clientX: 10, clientY: 10, pointerType: 'touch'}));
		await nextFrame();

		expect(listener).not.toHaveBeenCalledWith(true);
	});

	it('disarms when the pointer leaves the window', async () => {
		const {row, content} = createRow();
		const listener = vi.fn();
		registerMessageHoverTarget(row, listener);

		movePointerTo(content);
		await nextFrame();
		expect(listener).toHaveBeenLastCalledWith(true);

		window.dispatchEvent(new MouseEvent('pointerout', {relatedTarget: null}));

		expect(listener).toHaveBeenLastCalledWith(false);
	});

	it('releases ownership when an armed row unregisters', async () => {
		const {row, content} = createRow();
		const listener = vi.fn();
		const unregister = registerMessageHoverTarget(row, listener);
		const other = createRow();
		const otherListener = vi.fn();
		registerMessageHoverTarget(other.row, otherListener);

		movePointerTo(content);
		await nextFrame();
		expect(listener).toHaveBeenLastCalledWith(true);

		unregister();
		hitTarget = other.content;
		await nextFrame();

		expect(otherListener).toHaveBeenLastCalledWith(true);
		expect(listener).not.toHaveBeenLastCalledWith(true);
	});

	it('detaches every global listener once the last row unregisters', () => {
		const addEventListenerSpy = vi.spyOn(window, 'addEventListener');
		const removeEventListenerSpy = vi.spyOn(window, 'removeEventListener');
		const {row} = createRow();

		const unregister = registerMessageHoverTarget(row, vi.fn());
		const added = addEventListenerSpy.mock.calls.map(([type, listener]) => ({type, listener}));
		unregister();
		const removed = removeEventListenerSpy.mock.calls.map(([type, listener]) => ({type, listener}));

		expect(added.length).toBeGreaterThan(0);
		for (const addedListener of added) {
			expect(
				removed.some(
					(removedListener) =>
						removedListener.type === addedListener.type && removedListener.listener === addedListener.listener,
				),
			).toBe(true);
		}

		addEventListenerSpy.mockRestore();
		removeEventListenerSpy.mockRestore();
	});
});
