// SPDX-License-Identifier: AGPL-3.0-or-later

import {beforeEach, describe, expect, it, vi} from 'vitest';

const master = {connect: vi.fn(), disconnect: vi.fn()};
const createdGains: Array<{connect: ReturnType<typeof vi.fn>; disconnect: ReturnType<typeof vi.fn>}> = [];
let captureActive = true;

vi.mock('@app/features/notification/utils/SoundUtils', () => ({
	getSoundCaptureAudioContext: () => ({
		createMediaElementSource: vi.fn(() => ({connect: vi.fn(), disconnect: vi.fn()})),
		createGain: vi.fn(() => {
			const gain = {connect: vi.fn(), disconnect: vi.fn(), gain: {value: 1}};
			createdGains.push(gain);
			return gain;
		}),
	}),
	getSoundCaptureMasterGainNode: () => master,
	isSoundCaptureActive: () => captureActive,
	onSoundCaptureActivated: () => () => undefined,
}));

const {routeMediaElementForSoundCapture} = await import('@app/features/voice/utils/InAppMediaSoundCapture');

function createElement(): HTMLMediaElement {
	const listeners = new Map<string, () => void>();
	return {
		volume: 1,
		muted: false,
		addEventListener: (type: string, listener: () => void) => listeners.set(type, listener),
		removeEventListener: (type: string) => listeners.delete(type),
		listenerCount: () => listeners.size,
	} as unknown as HTMLMediaElement & {listenerCount: () => number};
}

describe('in-app media sound capture', () => {
	beforeEach(() => {
		createdGains.length = 0;
		captureActive = true;
	});

	it('disconnects the element from the master graph when the caller disposes', () => {
		const element = createElement();
		const dispose = routeMediaElementForSoundCapture(element);
		const gain = createdGains.at(-1);
		expect(gain?.connect).toHaveBeenCalledWith(master);
		dispose();
		expect(gain?.disconnect).toHaveBeenCalled();
	});

	it('removes the volume listener when the caller disposes', () => {
		const element = createElement() as HTMLMediaElement & {listenerCount: () => number};
		const dispose = routeMediaElementForSoundCapture(element);
		expect(element.listenerCount()).toBe(1);
		dispose();
		expect(element.listenerCount()).toBe(0);
	});

	it('reuses the existing source node when the same element is routed again', () => {
		const element = createElement();
		routeMediaElementForSoundCapture(element)();
		const gainsAfterFirst = createdGains.length;
		const dispose = routeMediaElementForSoundCapture(element);
		expect(createdGains.length).toBe(gainsAfterFirst);
		expect(createdGains.at(-1)?.connect).toHaveBeenCalledTimes(2);
		dispose();
	});
});
