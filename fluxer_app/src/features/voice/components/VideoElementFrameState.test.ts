// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	clearVideoFrameWatchersForTests,
	getActiveVideoFrameWatcherCountForTests,
	watchVideoElementRenderedFrame,
} from '@app/features/voice/components/VideoElementFrameState';
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';

const POLL_TICK_MS = 125;

interface FakeVideoOptions {
	supportsFrameCallback: boolean;
	readyState?: number;
	videoWidth?: number;
	videoHeight?: number;
}

interface FakeVideo {
	element: HTMLVideoElement;
	presentFrame: () => void;
	cancelledFrameCallbacks: Array<number>;
	listenerCount: () => number;
}

function createFakeVideo(options: FakeVideoOptions): FakeVideo {
	const listeners = new Map<string, Set<() => void>>();
	const frameCallbacks = new Map<number, () => void>();
	const cancelledFrameCallbacks: Array<number> = [];
	let nextFrameCallbackHandle = 1;

	const element: Record<string, unknown> = {
		readyState: options.readyState ?? 0,
		videoWidth: options.videoWidth ?? 0,
		videoHeight: options.videoHeight ?? 0,
		addEventListener: (type: string, listener: () => void): void => {
			const existing = listeners.get(type);
			if (existing) {
				existing.add(listener);
				return;
			}
			listeners.set(type, new Set([listener]));
		},
		removeEventListener: (type: string, listener: () => void): void => {
			listeners.get(type)?.delete(listener);
		},
	};

	if (options.supportsFrameCallback) {
		element.requestVideoFrameCallback = (callback: () => void): number => {
			const handle = nextFrameCallbackHandle;
			nextFrameCallbackHandle += 1;
			frameCallbacks.set(handle, callback);
			return handle;
		};
		element.cancelVideoFrameCallback = (handle: number): void => {
			cancelledFrameCallbacks.push(handle);
			frameCallbacks.delete(handle);
		};
	}

	return {
		element: element as unknown as HTMLVideoElement,
		presentFrame: (): void => {
			for (const [handle, callback] of frameCallbacks) {
				frameCallbacks.delete(handle);
				callback();
			}
		},
		cancelledFrameCallbacks,
		listenerCount: (): number => {
			let total = 0;
			for (const registered of listeners.values()) {
				total += registered.size;
			}
			return total;
		},
	};
}

describe('watchVideoElementRenderedFrame', () => {
	beforeEach(() => {
		vi.useFakeTimers();
		vi.stubGlobal('window', globalThis);
		clearVideoFrameWatchersForTests();
	});

	afterEach(() => {
		clearVideoFrameWatchersForTests();
		vi.unstubAllGlobals();
		vi.useRealTimers();
	});

	it('reports a frame from the element the ref switches to after watching starts', () => {
		const initial = createFakeVideo({supportsFrameCallback: true});
		const videoRef: {current: HTMLVideoElement | null} = {current: initial.element};
		const onFrame = vi.fn();

		const dispose = watchVideoElementRenderedFrame({videoRef, onFrame});
		videoRef.current = createFakeVideo({
			supportsFrameCallback: true,
			readyState: 2,
			videoWidth: 1280,
			videoHeight: 720,
		}).element;

		vi.advanceTimersByTime(POLL_TICK_MS);

		expect(onFrame).toHaveBeenCalledTimes(1);
		expect(getActiveVideoFrameWatcherCountForTests()).toBe(0);

		vi.advanceTimersByTime(POLL_TICK_MS * 4);

		expect(onFrame).toHaveBeenCalledTimes(1);
		dispose();
	});

	it('reports one frame and drops the poll target when the frame callback fires first', () => {
		const video = createFakeVideo({supportsFrameCallback: true});
		const videoRef: {current: HTMLVideoElement | null} = {current: video.element};
		const onFrame = vi.fn();

		const dispose = watchVideoElementRenderedFrame({videoRef, onFrame});

		expect(getActiveVideoFrameWatcherCountForTests()).toBe(1);

		video.presentFrame();

		expect(onFrame).toHaveBeenCalledTimes(1);
		expect(getActiveVideoFrameWatcherCountForTests()).toBe(0);
		expect(video.listenerCount()).toBe(0);
		expect(vi.getTimerCount()).toBe(0);

		vi.advanceTimersByTime(POLL_TICK_MS * 4);

		expect(onFrame).toHaveBeenCalledTimes(1);
		dispose();
	});

	it('reports nothing and leaves no interval running when disposed before any frame', () => {
		const video = createFakeVideo({supportsFrameCallback: true});
		const videoRef: {current: HTMLVideoElement | null} = {current: video.element};
		const onFrame = vi.fn();

		const dispose = watchVideoElementRenderedFrame({videoRef, onFrame});
		dispose();

		videoRef.current = createFakeVideo({
			supportsFrameCallback: false,
			readyState: 2,
			videoWidth: 1280,
			videoHeight: 720,
		}).element;
		vi.advanceTimersByTime(POLL_TICK_MS * 4);

		expect(onFrame).not.toHaveBeenCalled();
		expect(getActiveVideoFrameWatcherCountForTests()).toBe(0);
		expect(vi.getTimerCount()).toBe(0);
		expect(video.cancelledFrameCallbacks).toHaveLength(1);
		expect(video.listenerCount()).toBe(0);
	});
});
