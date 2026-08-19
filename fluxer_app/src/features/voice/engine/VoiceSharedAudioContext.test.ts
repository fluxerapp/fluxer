// SPDX-License-Identifier: AGPL-3.0-or-later

import {afterEach, describe, expect, it, vi} from 'vitest';

interface FakeAudioContext {
	state: AudioContextState;
	sampleRate: number;
	resumeCount: number;
}

interface FakeWindowHarness {
	requestedOptions: Array<AudioContextOptions | undefined>;
	contexts: Array<FakeAudioContext>;
	bodyListeners: Array<string>;
}

const HARDWARE_SAMPLE_RATE = 44100;

function installFakeWindow(
	config: {
		throwOn?: (options: AudioContextOptions | undefined) => boolean;
		initialState?: AudioContextState;
		honourSampleRate?: boolean;
	} = {},
): FakeWindowHarness {
	const harness: FakeWindowHarness = {requestedOptions: [], contexts: [], bodyListeners: []};
	const throwOn = config.throwOn ?? ((): boolean => false);
	const honourSampleRate = config.honourSampleRate ?? true;

	class FakeAudioContextImpl {
		state: AudioContextState = config.initialState ?? 'running';
		sampleRate: number;
		resumeCount = 0;

		constructor(options?: AudioContextOptions) {
			harness.requestedOptions.push(options);
			if (throwOn(options)) {
				throw new DOMException('sample rate not supported', 'NotSupportedError');
			}
			this.sampleRate = (honourSampleRate ? options?.sampleRate : undefined) ?? HARDWARE_SAMPLE_RATE;
			harness.contexts.push(this as unknown as FakeAudioContext);
		}

		async resume(): Promise<void> {
			this.resumeCount += 1;
			this.state = 'running';
		}

		addEventListener(): void {}
		removeEventListener(): void {}
	}

	vi.stubGlobal('window', {
		AudioContext: FakeAudioContextImpl,
		document: {
			body: {
				addEventListener: (type: string): void => {
					harness.bodyListeners.push(type);
				},
				removeEventListener: (): void => {},
			},
		},
	});
	return harness;
}

async function loadModule() {
	vi.resetModules();
	return await import('@app/features/voice/engine/VoiceSharedAudioContext');
}

describe('VoiceSharedAudioContext', () => {
	afterEach(() => {
		vi.unstubAllGlobals();
	});

	it('leaves the shared context on the hardware rate so it shares a graph with capture and playback', async () => {
		const harness = installFakeWindow();
		const {getSharedVoiceAudioContext} = await loadModule();

		const context = getSharedVoiceAudioContext();

		expect(harness.requestedOptions).toEqual([{latencyHint: 'interactive'}]);
		expect(context).toBe(harness.contexts[0] as unknown as AudioContext);
		expect(context?.sampleRate).toBe(HARDWARE_SAMPLE_RATE);
	});

	it('returns null without throwing when the construction fails', async () => {
		const harness = installFakeWindow({throwOn: () => true});
		const {getSharedVoiceAudioContext} = await loadModule();

		let context: AudioContext | null | undefined;
		expect(() => {
			context = getSharedVoiceAudioContext();
		}).not.toThrow();

		expect(context).toBeNull();
		expect(harness.contexts).toHaveLength(0);
		expect(harness.requestedOptions).toHaveLength(1);
	});

	it('caches the context across calls and rebuilds it once it has closed', async () => {
		const harness = installFakeWindow();
		const {getSharedVoiceAudioContext} = await loadModule();

		const first = getSharedVoiceAudioContext();
		expect(getSharedVoiceAudioContext()).toBe(first);
		expect(harness.requestedOptions).toHaveLength(1);

		harness.contexts[0].state = 'closed';
		const second = getSharedVoiceAudioContext();

		expect(second).not.toBe(first);
		expect(harness.contexts).toHaveLength(2);
		expect(harness.requestedOptions).toEqual([{latencyHint: 'interactive'}, {latencyHint: 'interactive'}]);
	});

	it('resumes a suspended context and arms the user-gesture resume listener', async () => {
		const harness = installFakeWindow({initialState: 'suspended'});
		const {getSharedVoiceAudioContext} = await loadModule();

		const context = getSharedVoiceAudioContext();

		expect(context).not.toBeNull();
		expect(harness.contexts[0].resumeCount).toBe(1);
		expect(harness.bodyListeners).toEqual(['click']);
	});

	it('createVoiceAudioContext reports the rate the browser actually resolved, not the requested one', async () => {
		installFakeWindow({honourSampleRate: false});
		const {createVoiceAudioContext} = await loadModule();

		const context = createVoiceAudioContext({latencyHint: 'interactive', sampleRate: 48000});

		expect(context?.sampleRate).toBe(HARDWARE_SAMPLE_RATE);
	});

	it('createVoiceAudioContext returns null instead of throwing when the rate is rejected', async () => {
		installFakeWindow({throwOn: (options) => options?.sampleRate !== undefined});
		const {createVoiceAudioContext} = await loadModule();

		expect(createVoiceAudioContext({latencyHint: 'interactive', sampleRate: 48000})).toBeNull();
	});
});
