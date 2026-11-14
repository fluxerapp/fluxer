// SPDX-License-Identifier: AGPL-3.0-or-later

import type {
	VoiceEngineV2CommandResult,
	VoiceEngineV2Implementation,
} from '@fluxer/voice_engine_v2/src/implementations/VoiceEngineV2ImplementationBase';
import type {VoiceEngineV2Command} from '@fluxer/voice_engine_v2/src/protocol/commands';
import {createVoiceEngineV2MemoryEventLogSpillSink} from '@fluxer/voice_engine_v2/src/runtime/eventLogRing';
import {
	assertEventLogInvariants,
	VoiceEngineV2Runtime,
	type VoiceEngineV2RuntimeDiagnostic,
} from '@fluxer/voice_engine_v2/src/runtime/VoiceEngineV2Runtime';
import {waitForRuntime} from '@fluxer/voice_engine_v2/src/testing/conformance';
import {FakeVoiceEngineV2Driver} from '@fluxer/voice_engine_v2/src/testing/FakeVoiceEngineV2Driver';
import {VoiceEngineV2TestImplementation} from '@fluxer/voice_engine_v2/src/testing/VoiceEngineV2TestImplementation';
import {describe, expect, it} from 'vitest';

class DeferredAllResolveImplementation implements VoiceEngineV2Implementation {
	readonly kind = 'js' as const;
	readonly started: Array<VoiceEngineV2Command> = [];
	readonly pending: Array<() => void> = [];

	execute(command: VoiceEngineV2Command): Promise<VoiceEngineV2CommandResult> {
		this.started.push(command);
		return new Promise<VoiceEngineV2CommandResult>((resolve) => {
			this.pending.push(() => resolve({ok: true}));
		});
	}
}

class ThrowingImplementation implements VoiceEngineV2Implementation {
	readonly kind = 'js' as const;
	readonly seen: Array<VoiceEngineV2Command> = [];

	execute(command: VoiceEngineV2Command): Promise<VoiceEngineV2CommandResult> {
		this.seen.push(command);
		throw new Error('synthetic failure');
	}
}

class ListenerThrowsImplementation implements VoiceEngineV2Implementation {
	readonly kind = 'js' as const;
	execute(): Promise<VoiceEngineV2CommandResult> {
		return Promise.resolve({ok: true});
	}
}

interface RuntimeTestOptions {
	queuedCommandsCap?: number;
}

function createTestRuntime(
	implementation: VoiceEngineV2Implementation,
	options: RuntimeTestOptions = {},
): VoiceEngineV2Runtime {
	return new VoiceEngineV2Runtime(implementation, {
		eventLogSpillSink: createVoiceEngineV2MemoryEventLogSpillSink(),
		verifyEventLogInvariantsOnDispatch: true,
		...options,
	});
}

describe('VoiceEngineV2Runtime queue caps', () => {
	it('rejects commands beyond queuedCommandsCap and surfaces a queueFull diagnostic', () => {
		const implementation = new DeferredAllResolveImplementation();
		const runtime = createTestRuntime(implementation, {queuedCommandsCap: 2});
		const diagnostics: Array<VoiceEngineV2RuntimeDiagnostic> = [];
		runtime.subscribeDiagnostics((diagnostic) => {
			diagnostics.push(diagnostic);
		});

		runtime.dispatch({type: 'implementation.prewarmRequested'});
		runtime.dispatch({type: 'implementation.prewarmRequested'});
		const sizeBeforeOverflow = runtime.commandQueueSize;
		runtime.dispatch({type: 'implementation.prewarmRequested'});

		expect(sizeBeforeOverflow).toBe(2);
		expect(runtime.commandQueueSize).toBe(2);
		expect(runtime.queueFullDropTotal).toBe(1);
		expect(diagnostics).toHaveLength(1);
		const diagnostic = diagnostics[0];
		if (diagnostic === undefined) throw new Error('expected diagnostic');
		expect(diagnostic.kind).toBe('voiceEngineV2.queueFull');
		expect(diagnostic.queue).toBe('queuedCommands');
		expect(diagnostic.cap).toBe(2);
		expect(diagnostic.droppedCommandType).toBe('implementation.prewarm');
	});
});

describe('VoiceEngineV2Runtime queue-cap rejection events', () => {
	it('dispatches the dropped command failure event so waiters settle without the timeout', async () => {
		const implementation = new DeferredAllResolveImplementation();
		const runtime = createTestRuntime(implementation, {queuedCommandsCap: 1});
		const failures: Array<{type: string; operationId: number; message: string}> = [];
		runtime.subscribe(({event}) => {
			if (event.type !== 'gateway.voiceStateClearFailed') return;
			failures.push({type: event.type, operationId: event.operationId, message: event.error.message});
		});

		runtime.dispatch({type: 'connection.connectRequested', options: {url: 'wss://voice', token: 't'}});
		runtime.dispatch({type: 'gateway.voiceStateClearRequested', guildId: null});
		await waitForRuntime();

		expect(runtime.queueFullDropTotal).toBe(1);
		expect(failures).toHaveLength(1);
		const failure = failures[0];
		if (failure === undefined) throw new Error('expected a rejection failure event');
		expect(failure.message).toMatch(/queue overflow/);
		expect(failure.message).toMatch(/queuedCommands/);
		expect(runtime.isOperationPending(failure.operationId)).toBe(false);
		assertEventLogInvariants(runtime);
	});

	it('rejects a stale dropped command via staleCompletionRejected instead of a synthesized failure', async () => {
		const implementation = new DeferredAllResolveImplementation();
		const runtime = createTestRuntime(implementation, {queuedCommandsCap: 1});
		const staleRejected: Array<number> = [];
		const unpublishFailed: Array<number> = [];
		const publishFailed: Array<number> = [];
		runtime.subscribe(({event}) => {
			if (event.type === 'command.staleCompletionRejected') staleRejected.push(event.operationId);
			if (event.type === 'screen.unpublishFailed') unpublishFailed.push(event.operationId);
			if (event.type === 'screen.publishFailed') publishFailed.push(event.operationId);
		});

		runtime.dispatch({type: 'connection.externallyEstablished', options: {url: 'wss://voice', token: ''}});
		runtime.dispatch({type: 'gateway.voiceStateClearRequested', guildId: null});
		const publish = runtime.dispatch({
			type: 'screen.publishRequested',
			options: {captureId: 'capture-1', width: 1920, height: 1080},
		});
		const unpublish = runtime.dispatch({type: 'screen.unpublishRequested'});
		await waitForRuntime();

		const publishOperationId = publish.commands[0]?.operationId;
		const unpublishOperationId = unpublish.commands[0]?.operationId;
		if (publishOperationId === undefined) throw new Error('expected a planned screen publish');
		if (unpublishOperationId === undefined) throw new Error('expected a planned screen unpublish');
		expect(runtime.queueFullDropTotal).toBe(2);
		expect(staleRejected).toEqual([publishOperationId]);
		expect(publishFailed).toEqual([]);
		expect(unpublishFailed).toEqual([unpublishOperationId]);
		assertEventLogInvariants(runtime);
	});
});

describe('VoiceEngineV2Runtime terminal failure dispatch', () => {
	it('dispatches a command.failed event when a listener throws mid-dispatch', async () => {
		const driver = new FakeVoiceEngineV2Driver();
		const runtime = createTestRuntime(new VoiceEngineV2TestImplementation(driver));
		const events: Array<string> = [];
		let triggered = false;

		runtime.subscribe(({event}) => {
			events.push(event.type);
			if (!triggered && event.type === 'connection.connectSucceeded') {
				triggered = true;
				throw new Error('listener boom');
			}
		});

		runtime.dispatch({type: 'connection.connectRequested', options: {url: 'wss://voice', token: 't'}});
		await waitForRuntime();

		expect(events).toContain('connection.connectFailed');
	});

	it('never silently swallows command failures (no .catch(() => {}) on the queue chain)', async () => {
		const implementation = new ThrowingImplementation();
		const runtime = createTestRuntime(implementation);
		const events: Array<string> = [];
		runtime.subscribe(({event}) => {
			events.push(event.type);
		});

		runtime.dispatch({type: 'connection.connectRequested', options: {url: 'wss://voice', token: 't'}});
		await waitForRuntime();

		expect(implementation.seen.length).toBeGreaterThan(0);
		expect(events).toContain('connection.connectFailed');
	});

	it('continues processing the next resource-queued command after a failure', async () => {
		const driver = new FakeVoiceEngineV2Driver();
		let failed = false;
		const runtime = createTestRuntime(new VoiceEngineV2TestImplementation(driver));
		runtime.subscribe(({event}) => {
			if (!failed && event.type === 'connection.connectSucceeded') {
				failed = true;
				throw new Error('one-shot boom');
			}
		});

		runtime.dispatch({type: 'connection.connectRequested', options: {url: 'wss://voice', token: 't'}});
		await waitForRuntime();
		runtime.dispatch({type: 'connection.disconnectRequested', reason: 'shutdown'});
		await waitForRuntime();

		expect(driver.calls.map((call) => call.type)).toContain('disconnect');
	});
});

describe('VoiceEngineV2Runtime cancellation tombstone', () => {
	it('skips execution when an operationId is tombstoned before the resource queue runs it', async () => {
		const implementation = new DeferredAllResolveImplementation();
		const runtime = createTestRuntime(implementation);

		runtime.dispatch({type: 'implementation.prewarmRequested'});
		runtime.dispatch({type: 'implementation.prewarmRequested'});
		const queued = runtime.commandQueue;
		expect(queued).toHaveLength(2);
		const first = queued[0];
		const second = queued[1];
		if (first === undefined || second === undefined) throw new Error('expected two queued');

		runtime.cancelOperation(second.operationId);
		expect(runtime.cancelledOperationCount).toBe(1);

		await waitForRuntime();
		implementation.pending[0]?.();
		await waitForRuntime();

		const startedIds = implementation.started.map((command) => command.operationId);
		expect(startedIds).toContain(first.operationId);
		expect(startedIds).not.toContain(second.operationId);
		expect(runtime.cancelledOperationCount).toBe(0);
	});
});

describe('VoiceEngineV2Runtime listener-reentrant dispatch ordering', () => {
	it('enqueues the outer transition commands before commands from a reentrant dispatch', () => {
		const implementation = new DeferredAllResolveImplementation();
		const runtime = createTestRuntime(implementation);
		let reentered = false;
		runtime.subscribe(({event}) => {
			if (!reentered && event.type === 'connection.connectRequested') {
				reentered = true;
				runtime.dispatch({type: 'implementation.prewarmRequested'});
			}
		});

		runtime.dispatch({type: 'connection.connectRequested', options: {url: 'wss://voice', token: 't'}});

		expect(reentered).toBe(true);
		expect(runtime.commandQueue.map((command) => command.type)).toEqual([
			'connection.connect',
			'implementation.prewarm',
		]);
	});
});

describe('VoiceEngineV2Runtime listener throws are not silently swallowed', () => {
	it('keeps the runtime usable after a listener throws on a non-command event', async () => {
		const runtime = createTestRuntime(new ListenerThrowsImplementation());
		let thrown = false;
		const events: Array<string> = [];
		runtime.subscribe(({event}) => {
			events.push(event.type);
			if (thrown) return;
			thrown = true;
			throw new Error('listener fault');
		});
		expect(() => runtime.dispatch({type: 'implementation.prewarmRequested'})).toThrow('listener fault');
		await waitForRuntime();
		expect(events).toContain('implementation.prewarmSucceeded');
	});
});
