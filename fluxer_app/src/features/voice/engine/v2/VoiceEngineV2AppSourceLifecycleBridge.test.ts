// SPDX-License-Identifier: AGPL-3.0-or-later

import type {VoiceEngineV2AppControllerHost} from '@app/features/voice/engine/v2/VoiceEngineV2AppControllerHost';
import {createVoiceEngineV2AppTestControllerHost} from '@app/features/voice/engine/v2/VoiceEngineV2AppControllerHostTestUtils';
import {
	VoiceEngineV2AppSourceLifecycleBridge,
	type VoiceEngineV2AppSourceLifecycleClosedCleanInfo,
} from '@app/features/voice/engine/v2/VoiceEngineV2AppSourceLifecycleBridge';
import {createVoiceEngineV2ShadowHostPorts} from '@app/features/voice/engine/v2/VoiceEngineV2ShadowHostPorts';
import type {NativeScreenCaptureLifecycleMessage} from '@app/types/electron.d';
import type {VoiceEngineV2Event} from '@fluxer/voice_engine_v2';
import {beforeEach, describe, expect, it} from 'vitest';

interface MockNativeBinding {
	dispatch(message: NativeScreenCaptureLifecycleMessage): void;
	subscribe(callback: (message: NativeScreenCaptureLifecycleMessage) => void): () => void;
	listenerCount(): number;
}

function createMockNativeBinding(): MockNativeBinding {
	const listeners = new Set<(message: NativeScreenCaptureLifecycleMessage) => void>();
	return {
		dispatch(message): void {
			for (const listener of listeners) {
				listener(message);
			}
		},
		subscribe(callback): () => void {
			listeners.add(callback);
			return () => {
				listeners.delete(callback);
			};
		},
		listenerCount(): number {
			return listeners.size;
		},
	};
}

function createFixedNow(): {next: () => number; advance: (ms: number) => void} {
	let value = 1000;
	return {
		next: () => value,
		advance: (ms) => {
			value += ms;
		},
	};
}

interface FakeRemoteTrack {
	on(event: 'ended', listener: () => void): void;
	off(event: 'ended', listener: () => void): void;
	emit(event: 'ended'): void;
	listenerCount(event: 'ended'): number;
}

function createFakeRemoteTrack(): FakeRemoteTrack {
	const listeners = new Set<() => void>();
	return {
		on(_event, listener): void {
			listeners.add(listener);
		},
		off(_event, listener): void {
			listeners.delete(listener);
		},
		emit(_event): void {
			for (const listener of Array.from(listeners)) {
				listener();
			}
		},
		listenerCount(_event): number {
			return listeners.size;
		},
	};
}

let host: VoiceEngineV2AppControllerHost;

beforeEach(() => {
	host = createVoiceEngineV2AppTestControllerHost({ports: createVoiceEngineV2ShadowHostPorts()});
});

describe('VoiceEngineV2AppSourceLifecycleBridge', () => {
	it('bind dispatches an active sourceLifecycle.transitioned event into the v2 host', () => {
		const binding = createMockNativeBinding();
		const dispatched: Array<VoiceEngineV2Event> = [];
		const bridge = new VoiceEngineV2AppSourceLifecycleBridge({
			dispatch: (event) => dispatched.push(event),
			subscribe: binding.subscribe,
			now: () => 1234,
		});

		const ok = bridge.bind({captureId: 'capture-1', sourceId: 'source-1'});
		expect(ok).toBe(true);
		expect(dispatched).toHaveLength(1);
		expect(dispatched[0]?.type).toBe('sourceLifecycle.transitioned');
		if (dispatched[0]?.type === 'sourceLifecycle.transitioned') {
			expect(dispatched[0].sourceId).toBe('source-1');
			expect(dispatched[0].kind).toBe('active');
			expect(dispatched[0].attempts).toBe(0);
			expect(dispatched[0].fault).toBeNull();
		}
		bridge.dispose();
	});

	it('an "error" native lifecycle event dispatches sourceLifecycle.transitioned with reconnecting + captureDeviceLost', () => {
		const binding = createMockNativeBinding();
		const dispatched: Array<VoiceEngineV2Event> = [];
		const bridge = new VoiceEngineV2AppSourceLifecycleBridge({
			dispatch: (event) => dispatched.push(event),
			subscribe: binding.subscribe,
		});

		bridge.bind({captureId: 'capture-2', sourceId: 'source-2'});
		dispatched.length = 0;

		binding.dispatch({captureId: 'capture-2', kind: 'error', message: 'stream lost'});

		expect(dispatched).toHaveLength(1);
		expect(dispatched[0]?.type).toBe('sourceLifecycle.transitioned');
		if (dispatched[0]?.type === 'sourceLifecycle.transitioned') {
			expect(dispatched[0].kind).toBe('reconnecting');
			expect(dispatched[0].sourceId).toBe('source-2');
			expect(dispatched[0].attempts).toBe(1);
			expect(dispatched[0].fault).toBe('captureDeviceLost');
		}
		bridge.dispose();
	});

	it('lifecycle events from the mock native binding flow into the v2 reducer snapshot', () => {
		const binding = createMockNativeBinding();
		const bridge = new VoiceEngineV2AppSourceLifecycleBridge({
			dispatch: (event) => host.dispatch(event),
			subscribe: binding.subscribe,
		});

		expect(bridge.bind({captureId: 'capture-3', sourceId: 'source-3'})).toBe(true);
		expect(host.snapshot.sourceLifecycles['source-3']?.kind).toBe('active');

		binding.dispatch({captureId: 'capture-3', kind: 'error', message: 'connection died'});

		const reconnecting = host.snapshot.sourceLifecycles['source-3'];
		expect(reconnecting?.kind).toBe('reconnecting');
		if (reconnecting?.kind === 'reconnecting') {
			expect(reconnecting.attempts).toBe(1);
			expect(reconnecting.lastFault).toBe('captureDeviceLost');
		}
		bridge.dispose();
	});

	it('ignores lifecycle events for unknown captureIds', () => {
		const binding = createMockNativeBinding();
		const dispatched: Array<VoiceEngineV2Event> = [];
		const bridge = new VoiceEngineV2AppSourceLifecycleBridge({
			dispatch: (event) => dispatched.push(event),
			subscribe: binding.subscribe,
		});

		bridge.bind({captureId: 'capture-known', sourceId: 'source-known'});
		dispatched.length = 0;

		binding.dispatch({captureId: 'capture-unknown', kind: 'error', message: 'lost'});

		expect(dispatched).toHaveLength(0);
		bridge.dispose();
	});

	it('translates "stalled" to networkError and routes diagnostic messages to diagnostics', async () => {
		const binding = createMockNativeBinding();
		const dispatched: Array<VoiceEngineV2Event> = [];
		const diagnosticsCalls: Array<{
			level: string;
			code: string;
			message: string;
			detail: unknown;
		}> = [];
		const bridge = new VoiceEngineV2AppSourceLifecycleBridge({
			dispatch: (event) => dispatched.push(event),
			subscribe: binding.subscribe,
			diagnostics: {
				async log(level, code, message, detail): Promise<void> {
					diagnosticsCalls.push({level, code, message, detail});
				},
			},
		});

		bridge.bind({captureId: 'cap-stall', sourceId: 'src-stall'});
		dispatched.length = 0;

		binding.dispatch({captureId: 'cap-stall', kind: 'stalled', message: 'frames slow'});
		binding.dispatch({captureId: 'cap-stall', kind: 'diagnostic', message: 'gpu temp ok'});

		expect(dispatched).toHaveLength(1);
		expect(dispatched[0]?.type).toBe('sourceLifecycle.transitioned');
		if (dispatched[0]?.type === 'sourceLifecycle.transitioned') {
			expect(dispatched[0].fault).toBe('networkError');
			expect(dispatched[0].kind).toBe('reconnecting');
		}
		await Promise.resolve();
		expect(diagnosticsCalls).toEqual([
			{
				level: 'info',
				code: 'nativeCaptureDiagnostic',
				message: 'gpu temp ok',
				detail: {captureId: 'cap-stall', sourceId: 'src-stall', source: 'unknown'},
			},
		]);
		bridge.dispose();
	});

	it('unbind removes the binding and stops forwarding events for that captureId', () => {
		const binding = createMockNativeBinding();
		const dispatched: Array<VoiceEngineV2Event> = [];
		const bridge = new VoiceEngineV2AppSourceLifecycleBridge({
			dispatch: (event) => dispatched.push(event),
			subscribe: binding.subscribe,
		});

		bridge.bind({captureId: 'cap-x', sourceId: 'src-x'});
		bridge.unbind('cap-x');
		dispatched.length = 0;

		binding.dispatch({captureId: 'cap-x', kind: 'error', message: 'too late'});

		expect(dispatched).toHaveLength(0);
		bridge.dispose();
	});

	it('unbind dispatches sourceLifecycle.removed so the reducer drops the snapshot slot', () => {
		const binding = createMockNativeBinding();
		const dispatched: Array<VoiceEngineV2Event> = [];
		const bridge = new VoiceEngineV2AppSourceLifecycleBridge({
			dispatch: (event) => dispatched.push(event),
			subscribe: binding.subscribe,
		});

		bridge.bind({captureId: 'cap-removed', sourceId: 'src-removed'});
		dispatched.length = 0;

		expect(bridge.unbind('cap-removed')).toBe(true);

		expect(dispatched).toHaveLength(1);
		expect(dispatched[0]?.type).toBe('sourceLifecycle.removed');
		if (dispatched[0]?.type === 'sourceLifecycle.removed') {
			expect(dispatched[0].sourceId).toBe('src-removed');
		}
		expect(bridge.unbind('cap-removed')).toBe(false);
		bridge.dispose();
	});

	it('dispose dispatches sourceLifecycle.removed for every remaining binding', () => {
		const binding = createMockNativeBinding();
		const dispatched: Array<VoiceEngineV2Event> = [];
		const bridge = new VoiceEngineV2AppSourceLifecycleBridge({
			dispatch: (event) => dispatched.push(event),
			subscribe: binding.subscribe,
		});

		bridge.bind({captureId: 'cap-1', sourceId: 'src-1'});
		bridge.bind({captureId: 'cap-2', sourceId: 'src-2'});
		dispatched.length = 0;

		bridge.dispose();

		expect(dispatched.map((event) => event.type)).toEqual(['sourceLifecycle.removed', 'sourceLifecycle.removed']);
		expect(dispatched.flatMap((event) => (event.type === 'sourceLifecycle.removed' ? [event.sourceId] : []))).toEqual([
			'src-1',
			'src-2',
		]);
	});

	it('dispose is re-entrant safe and does not re-dispatch removals', () => {
		const binding = createMockNativeBinding();
		const dispatched: Array<VoiceEngineV2Event> = [];
		let reentrant: VoiceEngineV2AppSourceLifecycleBridge | null = null;
		const bridge = new VoiceEngineV2AppSourceLifecycleBridge({
			dispatch: (event) => {
				dispatched.push(event);
				if (event.type === 'sourceLifecycle.removed') reentrant?.dispose();
			},
			subscribe: binding.subscribe,
		});

		bridge.bind({captureId: 'cap-reentrant', sourceId: 'src-reentrant'});
		dispatched.length = 0;
		reentrant = bridge;

		bridge.dispose();

		expect(dispatched.map((event) => event.type)).toEqual(['sourceLifecycle.removed']);
		expect(binding.listenerCount()).toBe(0);
	});

	it('dispose unsubscribes from the native binding', () => {
		const binding = createMockNativeBinding();
		const bridge = new VoiceEngineV2AppSourceLifecycleBridge({
			dispatch: () => {},
			subscribe: binding.subscribe,
		});

		expect(binding.listenerCount()).toBe(1);
		bridge.dispose();
		expect(binding.listenerCount()).toBe(0);
	});

	it('rejects malformed native lifecycle payloads', () => {
		const binding = createMockNativeBinding();
		const dispatched: Array<VoiceEngineV2Event> = [];
		const bridge = new VoiceEngineV2AppSourceLifecycleBridge({
			dispatch: (event) => dispatched.push(event),
			subscribe: binding.subscribe,
		});

		bridge.bind({captureId: 'cap-bad', sourceId: 'src-bad'});
		dispatched.length = 0;

		const badPayloads = [
			null,
			{captureId: '', kind: 'error', message: 'x'},
			{captureId: 'cap-bad', kind: 'bogus', message: 'x'},
			{captureId: 'cap-bad', kind: 42, message: 'x'},
		] as unknown as Array<NativeScreenCaptureLifecycleMessage>;
		for (const payload of badPayloads) {
			binding.dispatch(payload);
		}

		expect(dispatched).toHaveLength(0);
		bridge.dispose();
	});

	it('bind dispatches an active lifecycle event for a microphone source', () => {
		const binding = createMockNativeBinding();
		const dispatched: Array<VoiceEngineV2Event> = [];
		const bridge = new VoiceEngineV2AppSourceLifecycleBridge({
			dispatch: (event) => dispatched.push(event),
			subscribe: binding.subscribe,
			now: () => 5000,
		});

		const ok = bridge.bind({captureId: 'mic-capture-1', sourceId: 'mic-source-1'});
		expect(ok).toBe(true);
		expect(dispatched).toHaveLength(1);
		expect(dispatched[0]?.type).toBe('sourceLifecycle.transitioned');
		if (dispatched[0]?.type === 'sourceLifecycle.transitioned') {
			expect(dispatched[0].sourceId).toBe('mic-source-1');
			expect(dispatched[0].kind).toBe('active');
			expect(dispatched[0].fault).toBeNull();
		}
		bridge.dispose();
	});

	it('bind dispatches an active lifecycle event for a camera source', () => {
		const binding = createMockNativeBinding();
		const dispatched: Array<VoiceEngineV2Event> = [];
		const bridge = new VoiceEngineV2AppSourceLifecycleBridge({
			dispatch: (event) => dispatched.push(event),
			subscribe: binding.subscribe,
			now: () => 5000,
		});

		const ok = bridge.bind({captureId: 'cam-capture-1', sourceId: 'cam-source-1'});
		expect(ok).toBe(true);
		expect(dispatched).toHaveLength(1);
		expect(dispatched[0]?.type).toBe('sourceLifecycle.transitioned');
		if (dispatched[0]?.type === 'sourceLifecycle.transitioned') {
			expect(dispatched[0].sourceId).toBe('cam-source-1');
			expect(dispatched[0].kind).toBe('active');
			expect(dispatched[0].fault).toBeNull();
		}
		bridge.dispose();
	});

	it('clean closed reason does not trigger reconnect FSM', () => {
		const binding = createMockNativeBinding();
		const bridge = new VoiceEngineV2AppSourceLifecycleBridge({
			dispatch: (event) => host.dispatch(event),
			subscribe: binding.subscribe,
		});

		expect(bridge.bind({captureId: 'cap-clean', sourceId: 'src-clean'})).toBe(true);
		expect(host.snapshot.sourceLifecycles['src-clean']?.kind).toBe('active');

		binding.dispatch({captureId: 'cap-clean', kind: 'closed-clean', message: 'user stopped'});

		const stateAfterClean = host.snapshot.sourceLifecycles['src-clean'];
		expect(stateAfterClean?.kind).toBe('active');
		bridge.dispose();
	});

	it('error closed reason triggers reconnect FSM with captureDeviceLost', () => {
		const binding = createMockNativeBinding();
		const bridge = new VoiceEngineV2AppSourceLifecycleBridge({
			dispatch: (event) => host.dispatch(event),
			subscribe: binding.subscribe,
		});

		expect(bridge.bind({captureId: 'cap-err', sourceId: 'src-err'})).toBe(true);
		binding.dispatch({captureId: 'cap-err', kind: 'closed', message: 'device unplugged'});

		const reconnecting = host.snapshot.sourceLifecycles['src-err'];
		expect(reconnecting?.kind).toBe('reconnecting');
		if (reconnecting?.kind === 'reconnecting') {
			expect(reconnecting.lastFault).toBe('captureDeviceLost');
			expect(reconnecting.attempts).toBe(1);
		}
		bridge.dispose();
	});

	it('reportLifecycle routes non-IPC fault into the FSM', () => {
		const binding = createMockNativeBinding();
		const dispatched: Array<VoiceEngineV2Event> = [];
		const bridge = new VoiceEngineV2AppSourceLifecycleBridge({
			dispatch: (event) => dispatched.push(event),
			subscribe: binding.subscribe,
		});

		bridge.bind({captureId: 'cap-report', sourceId: 'src-report'});
		dispatched.length = 0;
		bridge.reportLifecycle({captureId: 'cap-report', kind: 'error', message: 'track ended'});

		expect(dispatched).toHaveLength(1);
		if (dispatched[0]?.type === 'sourceLifecycle.transitioned') {
			expect(dispatched[0].kind).toBe('reconnecting');
			expect(dispatched[0].fault).toBe('captureDeviceLost');
		}
		bridge.dispose();
	});

	it('bindRemoteTrackLifecycle dispatches active state for a remote screen-share', () => {
		const binding = createMockNativeBinding();
		const track = createFakeRemoteTrack();
		const dispatched: Array<VoiceEngineV2Event> = [];
		const bridge = new VoiceEngineV2AppSourceLifecycleBridge({
			dispatch: (event) => dispatched.push(event),
			subscribe: binding.subscribe,
			now: () => 7777,
		});

		const unbind = bridge.bindRemoteTrackLifecycle(track, {
			captureId: 'watch:remote-screen-share:user_1_aaa:SCR1',
			sourceId: 'watch:remote-screen-share:user_1_aaa:SCR1',
		});

		expect(typeof unbind).toBe('function');
		expect(dispatched).toHaveLength(1);
		expect(dispatched[0]?.type).toBe('sourceLifecycle.transitioned');
		if (dispatched[0]?.type === 'sourceLifecycle.transitioned') {
			expect(dispatched[0].sourceId).toBe('watch:remote-screen-share:user_1_aaa:SCR1');
			expect(dispatched[0].kind).toBe('active');
			expect(dispatched[0].fault).toBeNull();
		}
		unbind();
		bridge.dispose();
	});

	it('track ended on a remote screen-share dispatches networkError lifecycle event', () => {
		const binding = createMockNativeBinding();
		const track = createFakeRemoteTrack();
		const dispatched: Array<VoiceEngineV2Event> = [];
		const bridge = new VoiceEngineV2AppSourceLifecycleBridge({
			dispatch: (event) => dispatched.push(event),
			subscribe: binding.subscribe,
		});

		const unbind = bridge.bindRemoteTrackLifecycle(track, {
			captureId: 'watch:remote-screen-share:user_1_bbb:SCR2',
			sourceId: 'watch:remote-screen-share:user_1_bbb:SCR2',
		});
		dispatched.length = 0;

		track.emit('ended');

		expect(dispatched).toHaveLength(1);
		expect(dispatched[0]?.type).toBe('sourceLifecycle.transitioned');
		if (dispatched[0]?.type === 'sourceLifecycle.transitioned') {
			expect(dispatched[0].kind).toBe('reconnecting');
			expect(dispatched[0].fault).toBe('networkError');
			expect(dispatched[0].attempts).toBe(1);
			expect(dispatched[0].sourceId).toBe('watch:remote-screen-share:user_1_bbb:SCR2');
		}
		unbind();
		bridge.dispose();
	});

	it('bindRemoteTrackLifecycle unbind removes the binding and stops forwarding', () => {
		const binding = createMockNativeBinding();
		const track = createFakeRemoteTrack();
		const dispatched: Array<VoiceEngineV2Event> = [];
		const bridge = new VoiceEngineV2AppSourceLifecycleBridge({
			dispatch: (event) => dispatched.push(event),
			subscribe: binding.subscribe,
		});

		const unbind = bridge.bindRemoteTrackLifecycle(track, {
			captureId: 'watch:remote-camera:user_1_ccc:CAM3',
			sourceId: 'watch:remote-camera:user_1_ccc:CAM3',
		});
		unbind();
		dispatched.length = 0;

		track.emit('ended');

		expect(dispatched).toHaveLength(0);
		expect(track.listenerCount('ended')).toBe(0);
		bridge.dispose();
	});

	it('multiple watched remote tracks dispatch independently per captureId', () => {
		const binding = createMockNativeBinding();
		const trackA = createFakeRemoteTrack();
		const trackB = createFakeRemoteTrack();
		const dispatched: Array<VoiceEngineV2Event> = [];
		const bridge = new VoiceEngineV2AppSourceLifecycleBridge({
			dispatch: (event) => dispatched.push(event),
			subscribe: binding.subscribe,
		});

		const unbindA = bridge.bindRemoteTrackLifecycle(trackA, {
			captureId: 'watch:remote-screen-share:user_1_aaa:SCRA',
			sourceId: 'watch:remote-screen-share:user_1_aaa:SCRA',
		});
		const unbindB = bridge.bindRemoteTrackLifecycle(trackB, {
			captureId: 'watch:remote-screen-share:user_1_bbb:SCRB',
			sourceId: 'watch:remote-screen-share:user_1_bbb:SCRB',
		});
		dispatched.length = 0;

		trackA.emit('ended');

		expect(dispatched).toHaveLength(1);
		if (dispatched[0]?.type === 'sourceLifecycle.transitioned') {
			expect(dispatched[0].sourceId).toBe('watch:remote-screen-share:user_1_aaa:SCRA');
			expect(dispatched[0].fault).toBe('networkError');
		}

		trackB.emit('ended');

		expect(dispatched).toHaveLength(2);
		if (dispatched[1]?.type === 'sourceLifecycle.transitioned') {
			expect(dispatched[1].sourceId).toBe('watch:remote-screen-share:user_1_bbb:SCRB');
			expect(dispatched[1].fault).toBe('networkError');
		}

		unbindA();
		unbindB();
		bridge.dispose();
	});

	it('atMs is sourced from the injected now function', () => {
		const binding = createMockNativeBinding();
		const now = createFixedNow();
		const dispatched: Array<VoiceEngineV2Event> = [];
		const bridge = new VoiceEngineV2AppSourceLifecycleBridge({
			dispatch: (event) => dispatched.push(event),
			subscribe: binding.subscribe,
			now: now.next,
		});

		bridge.bind({captureId: 'cap-t', sourceId: 'src-t'});
		now.advance(500);
		binding.dispatch({captureId: 'cap-t', kind: 'error', message: ''});

		expect(dispatched).toHaveLength(2);
		const first = dispatched[0];
		const second = dispatched[1];
		if (first?.type !== 'sourceLifecycle.transitioned') throw new Error('expected sourceLifecycle.transitioned');
		if (second?.type !== 'sourceLifecycle.transitioned') throw new Error('expected sourceLifecycle.transitioned');
		expect(first.atMs).toBe(1000);
		expect(second.atMs).toBe(1500);
		bridge.dispose();
	});

	it('programmatic stop synthesizes closed-clean event for screen-share source', () => {
		const binding = createMockNativeBinding();
		const dispatched: Array<VoiceEngineV2Event> = [];
		const closedCleans: Array<VoiceEngineV2AppSourceLifecycleClosedCleanInfo> = [];
		const bridge = new VoiceEngineV2AppSourceLifecycleBridge({
			dispatch: (event) => dispatched.push(event),
			subscribe: binding.subscribe,
			onClosedClean: (info) => closedCleans.push(info),
			now: () => 9_000,
		});

		bridge.bind({captureId: 'cap-screen', sourceId: 'src-screen'});
		dispatched.length = 0;

		binding.dispatch({
			captureId: 'cap-screen',
			kind: 'closed-clean',
			message: 'programmatic-stop',
			source: 'programmatic',
		});

		expect(closedCleans).toHaveLength(1);
		expect(closedCleans[0]?.captureId).toBe('cap-screen');
		expect(closedCleans[0]?.sourceId).toBe('src-screen');
		expect(closedCleans[0]?.source).toBe('programmatic');
		expect(closedCleans[0]?.atMs).toBe(9_000);
		expect(dispatched).toHaveLength(0);
		bridge.dispose();
	});

	it('delegate-fired and programmatic-synthesized events are deduped within a window', () => {
		const binding = createMockNativeBinding();
		const now = createFixedNow();
		const dispatched: Array<VoiceEngineV2Event> = [];
		const closedCleans: Array<VoiceEngineV2AppSourceLifecycleClosedCleanInfo> = [];
		const bridge = new VoiceEngineV2AppSourceLifecycleBridge({
			dispatch: (event) => dispatched.push(event),
			subscribe: binding.subscribe,
			onClosedClean: (info) => closedCleans.push(info),
			now: now.next,
		});

		bridge.bind({captureId: 'cap-dup', sourceId: 'src-dup'});
		dispatched.length = 0;

		binding.dispatch({
			captureId: 'cap-dup',
			kind: 'closed-clean',
			message: 'user stopped',
			source: 'delegate',
		});
		now.advance(100);
		binding.dispatch({
			captureId: 'cap-dup',
			kind: 'closed-clean',
			message: 'programmatic-stop',
			source: 'programmatic',
		});

		expect(closedCleans).toHaveLength(1);
		expect(closedCleans[0]?.source).toBe('delegate');
		expect(dispatched).toHaveLength(0);

		now.advance(500);
		binding.dispatch({
			captureId: 'cap-dup',
			kind: 'closed-clean',
			message: 'second-cycle stop',
			source: 'programmatic',
		});
		expect(closedCleans).toHaveLength(2);
		expect(closedCleans[1]?.source).toBe('programmatic');
		bridge.dispose();
	});
});
