// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	beginGlobalKeyCapture,
	type GlobalKeyCaptureApi,
	globalKeyEventToCombo,
	isGlobalKeyEventModifierKey,
} from '@app/features/input/components/KeybindRecorderCapture';
import type {GlobalKeyEvent} from '@app/features/platform/types/Electron';
import {describe, expect, it} from 'vitest';

const keyEvent = (event: Partial<GlobalKeyEvent>): GlobalKeyEvent => ({
	type: 'keydown',
	keycode: 0,
	keyName: 'A',
	backend: 'native',
	altKey: false,
	ctrlKey: false,
	shiftKey: false,
	metaKey: false,
	...event,
});

describe('globalKeyEventToCombo', () => {
	it('records Caps Lock from native global key events', () => {
		expect(globalKeyEventToCombo(keyEvent({keyName: 'CapsLock'}))).toMatchObject({
			key: 'CapsLock',
			code: 'CapsLock',
		});
	});

	it('normalizes spaced Caps Lock aliases from platform hooks', () => {
		expect(globalKeyEventToCombo(keyEvent({keyName: 'Caps Lock'}))).toMatchObject({
			key: 'CapsLock',
			code: 'CapsLock',
		});
	});

	it('records letters with physical DOM-style codes', () => {
		expect(globalKeyEventToCombo(keyEvent({keyName: 'A'}))).toMatchObject({
			key: 'a',
			code: 'KeyA',
		});
		expect(globalKeyEventToCombo(keyEvent({keyName: 'A', shiftKey: true}))).toMatchObject({
			key: 'A',
			code: 'KeyA',
			shift: true,
		});
	});

	it('recognizes side-specific native modifier names', () => {
		expect(isGlobalKeyEventModifierKey(keyEvent({keyName: 'ShiftRight'}))).toBe(true);
		expect(
			globalKeyEventToCombo(keyEvent({keyName: 'ShiftRight', shiftKey: true}), {modifierOnly: true}),
		).toMatchObject({
			key: 'Shift',
			code: 'ShiftRight',
			shift: true,
			modifierOnly: true,
		});
	});
});

const flushMicrotasks = async (): Promise<void> => {
	await Promise.resolve();
	await Promise.resolve();
};

interface CaptureApiHarness {
	api: GlobalKeyCaptureApi;
	calls: Array<string>;
	listeners: Array<(event: GlobalKeyEvent) => void>;
	resolveStart: (started: boolean) => void;
	rejectStart: (error: Error) => void;
}

const createCaptureApi = (): CaptureApiHarness => {
	const calls: Array<string> = [];
	const listeners: Array<(event: GlobalKeyEvent) => void> = [];
	let resolveStart: (started: boolean) => void = () => {};
	let rejectStart: (error: Error) => void = () => {};
	const api: GlobalKeyCaptureApi = {
		globalKeyHookStart: () => {
			calls.push('start');
			return new Promise<boolean>((resolve, reject) => {
				resolveStart = resolve;
				rejectStart = reject;
			});
		},
		globalKeyHookStop: () => {
			calls.push('stop');
			return Promise.resolve();
		},
		onGlobalKeyEvent: (callback) => {
			listeners.push(callback);
			return () => {
				const index = listeners.indexOf(callback);
				if (index !== -1) listeners.splice(index, 1);
			};
		},
	};
	return {
		api,
		calls,
		listeners,
		resolveStart: (started) => resolveStart(started),
		rejectStart: (error) => rejectStart(error),
	};
};

describe('beginGlobalKeyCapture', () => {
	it('subscribes, starts the hook, and pairs cancel with exactly one stop', async () => {
		const harness = createCaptureApi();
		const events: Array<GlobalKeyEvent> = [];
		const cancel = beginGlobalKeyCapture(harness.api, (event) => events.push(event));
		expect(harness.listeners).toHaveLength(1);
		harness.resolveStart(true);
		await flushMicrotasks();
		harness.listeners[0]?.(keyEvent({keyName: 'CapsLock'}));
		expect(events).toHaveLength(1);
		cancel();
		cancel();
		await flushMicrotasks();
		expect(harness.listeners).toHaveLength(0);
		expect(harness.calls).toEqual(['start', 'stop']);
	});

	it('never stops the hook when the start was rejected or unsuccessful', async () => {
		const failed = createCaptureApi();
		const cancelFailed = beginGlobalKeyCapture(failed.api, () => {});
		failed.resolveStart(false);
		await flushMicrotasks();
		expect(failed.listeners).toHaveLength(0);
		cancelFailed();
		await flushMicrotasks();
		expect(failed.calls).toEqual(['start']);

		const thrown = createCaptureApi();
		const cancelThrown = beginGlobalKeyCapture(thrown.api, () => {});
		thrown.rejectStart(new Error('denied'));
		await flushMicrotasks();
		expect(thrown.listeners).toHaveLength(0);
		cancelThrown();
		await flushMicrotasks();
		expect(thrown.calls).toEqual(['start']);
	});

	it('stops the hook after a late start resolution when cancelled early', async () => {
		const harness = createCaptureApi();
		const cancel = beginGlobalKeyCapture(harness.api, () => {});
		cancel();
		expect(harness.listeners).toHaveLength(0);
		harness.resolveStart(true);
		await flushMicrotasks();
		expect(harness.calls).toEqual(['start', 'stop']);
	});

	it('is a no-op without a complete capture api', () => {
		expect(beginGlobalKeyCapture(null, () => {})).toBeTypeOf('function');
		expect(beginGlobalKeyCapture({}, () => {})).toBeTypeOf('function');
		beginGlobalKeyCapture(null, () => {})();
		beginGlobalKeyCapture({}, () => {})();
	});
});
