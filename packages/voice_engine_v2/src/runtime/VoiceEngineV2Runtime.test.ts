// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import type {
	VoiceEngineV2CommandResult,
	VoiceEngineV2Implementation,
} from '@fluxer/voice_engine_v2/src/implementations';
import type {VoiceEngineV2Command} from '@fluxer/voice_engine_v2/src/protocol/commands';
import {createVoiceEngineV2MemoryEventLogSpillSink} from '@fluxer/voice_engine_v2/src/runtime/eventLogRing';
import {
	assertEventLogInvariants,
	isVoiceEngineV2ProgrammerError,
	VOICE_ENGINE_V2_CANCELLED_OPERATIONS_CAP,
	VOICE_ENGINE_V2_DIAGNOSTIC_LISTENERS_CAP,
	VOICE_ENGINE_V2_LISTENERS_CAP,
	VOICE_ENGINE_V2_QUEUED_COMMANDS_CAP,
	VOICE_ENGINE_V2_RESOURCE_QUEUES_CAP,
	VoiceEngineV2Runtime,
	type VoiceEngineV2RuntimeDiagnostic,
} from '@fluxer/voice_engine_v2/src/runtime/VoiceEngineV2Runtime';
import {
	FakeVoiceEngineV2Driver,
	VoiceEngineV2TestImplementation,
	waitForRuntime,
} from '@fluxer/voice_engine_v2/src/testing';
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
	resourceQueuesCap?: number;
	cancelledOperationsCap?: number;
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
	it('exports the documented cap constants', () => {
		expect(VOICE_ENGINE_V2_QUEUED_COMMANDS_CAP).toBe(4096);
		expect(VOICE_ENGINE_V2_RESOURCE_QUEUES_CAP).toBe(256);
		expect(VOICE_ENGINE_V2_CANCELLED_OPERATIONS_CAP).toBe(4096);
	});

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

	it('rejects commands beyond resourceQueuesCap when the resource key is new', () => {
		const implementation = new DeferredAllResolveImplementation();
		const runtime = createTestRuntime(implementation, {resourceQueuesCap: 1});
		const diagnostics: Array<VoiceEngineV2RuntimeDiagnostic> = [];
		runtime.subscribeDiagnostics((diagnostic) => {
			diagnostics.push(diagnostic);
		});

		runtime.dispatch({type: 'connection.connectRequested', options: {url: 'wss://voice', token: 't'}});
		runtime.dispatch({type: 'gateway.voiceStateClearRequested', guildId: null});

		expect(runtime.activeResourceQueueCount).toBe(1);
		expect(runtime.queueFullDropTotal).toBe(1);
		expect(diagnostics).toHaveLength(1);
		const diagnostic = diagnostics[0];
		if (diagnostic === undefined) throw new Error('expected diagnostic');
		expect(diagnostic.queue).toBe('resourceQueues');
		expect(diagnostic.cap).toBe(1);
	});

	it('does not double-drop when the same resource key reappears under cap', () => {
		const implementation = new DeferredAllResolveImplementation();
		const runtime = createTestRuntime(implementation, {resourceQueuesCap: 1});

		runtime.dispatch({type: 'connection.connectRequested', options: {url: 'wss://voice', token: 't'}});
		runtime.dispatch({type: 'connection.disconnectRequested', reason: 'shutdown'});

		expect(runtime.queueFullDropTotal).toBe(0);
		expect(runtime.activeResourceQueueCount).toBe(1);
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

	it('dispatches the failure event for drops at the resource-queue cap', async () => {
		const implementation = new DeferredAllResolveImplementation();
		const runtime = createTestRuntime(implementation, {resourceQueuesCap: 1});
		const events: Array<string> = [];
		runtime.subscribe(({event}) => {
			events.push(event.type);
		});

		runtime.dispatch({type: 'connection.connectRequested', options: {url: 'wss://voice', token: 't'}});
		runtime.dispatch({type: 'gateway.voiceStateClearRequested', guildId: null});
		await waitForRuntime();

		expect(runtime.queueFullDropTotal).toBe(1);
		expect(events).toContain('gateway.voiceStateClearFailed');
		assertEventLogInvariants(runtime);
	});
});

describe('VoiceEngineV2Runtime executing-command visibility', () => {
	it('exposes the executing command per resource while it runs and clears it after completion', async () => {
		const implementation = new DeferredAllResolveImplementation();
		const runtime = createTestRuntime(implementation);

		runtime.dispatch({type: 'connection.connectRequested', options: {url: 'wss://voice', token: 't'}});
		await waitForRuntime();

		const executing = runtime.getExecutingCommand('connection');
		if (executing === null) throw new Error('expected an executing connection command');
		expect(executing.type).toBe('connection.connect');
		expect(runtime.isOperationPending(executing.operationId)).toBe(true);
		expect(runtime.getExecutingCommand('gateway')).toBeNull();

		implementation.pending[0]?.();
		await waitForRuntime();

		expect(runtime.getExecutingCommand('connection')).toBeNull();
		expect(runtime.isOperationPending(executing.operationId)).toBe(false);
	});

	it('reports queued commands as pending before the resource queue starts them', async () => {
		const implementation = new DeferredAllResolveImplementation();
		const runtime = createTestRuntime(implementation);

		runtime.dispatch({type: 'implementation.prewarmRequested'});
		runtime.dispatch({type: 'implementation.prewarmRequested'});
		const queued = runtime.commandQueue;
		expect(queued).toHaveLength(2);
		const second = queued[1];
		if (second === undefined) throw new Error('expected a second queued command');

		await waitForRuntime();

		expect(runtime.isOperationPending(second.operationId)).toBe(true);
		expect(runtime.getExecutingCommand('implementation')?.operationId).not.toBe(second.operationId);
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

	it('tombstone cancellation is O(1) and does not scan the queue', () => {
		const runtime = createTestRuntime(new DeferredAllResolveImplementation());
		const start = process.hrtime.bigint();
		for (let i = 0; i < 100_000; i += 1) {
			runtime.cancelOperation(i);
		}
		const elapsedNs = Number(process.hrtime.bigint() - start);
		expect(runtime.cancelledOperationCount).toBe(VOICE_ENGINE_V2_CANCELLED_OPERATIONS_CAP);
		expect(elapsedNs).toBeLessThan(2_000_000_000);
	});

	it('never grows the tombstone set beyond the named cap', () => {
		const runtime = createTestRuntime(new DeferredAllResolveImplementation(), {cancelledOperationsCap: 3});
		expect(runtime.cancelledOperationsCapacity).toBe(3);
		for (let i = 0; i < 10; i += 1) {
			runtime.cancelOperation(i);
			expect(runtime.cancelledOperationCount).toBeLessThanOrEqual(3);
		}
		expect(runtime.cancelledOperationCount).toBe(3);
	});

	it('evicts the oldest tombstone when the cap is reached, so the evicted operation executes', async () => {
		const implementation = new DeferredAllResolveImplementation();
		const runtime = createTestRuntime(implementation, {cancelledOperationsCap: 2});

		runtime.dispatch({type: 'implementation.prewarmRequested'});
		runtime.dispatch({type: 'implementation.prewarmRequested'});
		const queued = runtime.commandQueue;
		expect(queued).toHaveLength(2);
		const first = queued[0];
		const second = queued[1];
		if (first === undefined || second === undefined) throw new Error('expected two queued');

		runtime.cancelOperation(second.operationId);
		runtime.cancelOperation(second.operationId + 10_000);
		runtime.cancelOperation(second.operationId + 10_001);
		expect(runtime.cancelledOperationCount).toBe(2);

		await waitForRuntime();
		implementation.pending[0]?.();
		await waitForRuntime();

		const startedIds = implementation.started.map((command) => command.operationId);
		expect(startedIds).toContain(first.operationId);
		expect(startedIds).toContain(second.operationId);
	});

	it('emits a queueFull-style diagnostic when a cancellation tombstone is evicted at cap', () => {
		const runtime = createTestRuntime(new DeferredAllResolveImplementation(), {cancelledOperationsCap: 2});
		const diagnostics: Array<VoiceEngineV2RuntimeDiagnostic> = [];
		runtime.subscribeDiagnostics((diagnostic) => {
			diagnostics.push(diagnostic);
		});

		runtime.cancelOperation(11);
		runtime.cancelOperation(12);
		expect(diagnostics).toHaveLength(0);
		runtime.cancelOperation(13);

		expect(diagnostics).toHaveLength(1);
		const diagnostic = diagnostics[0];
		if (diagnostic === undefined) throw new Error('expected an eviction diagnostic');
		expect(diagnostic.kind).toBe('voiceEngineV2.queueFull');
		expect(diagnostic.queue).toBe('cancelledOperations');
		expect(diagnostic.cap).toBe(2);
		expect(diagnostic.droppedOperationId).toBe(11);
	});

	it('reports the queued command details when an evicted tombstone still targets a queued command', () => {
		const implementation = new DeferredAllResolveImplementation();
		const runtime = createTestRuntime(implementation, {cancelledOperationsCap: 1});
		const diagnostics: Array<VoiceEngineV2RuntimeDiagnostic> = [];
		runtime.subscribeDiagnostics((diagnostic) => {
			diagnostics.push(diagnostic);
		});

		runtime.dispatch({type: 'implementation.prewarmRequested'});
		const queued = runtime.commandQueue[0];
		if (queued === undefined) throw new Error('expected a queued command');
		runtime.cancelOperation(queued.operationId);
		runtime.cancelOperation(queued.operationId + 10_000);

		expect(diagnostics).toHaveLength(1);
		const diagnostic = diagnostics[0];
		if (diagnostic === undefined) throw new Error('expected an eviction diagnostic');
		expect(diagnostic.queue).toBe('cancelledOperations');
		expect(diagnostic.droppedOperationId).toBe(queued.operationId);
		expect(diagnostic.droppedCommandType).toBe('implementation.prewarm');
		expect(diagnostic.resourceKey).toBe('implementation');
	});

	it('keeps a re-cancelled operationId without consuming eviction budget', () => {
		const runtime = createTestRuntime(new DeferredAllResolveImplementation(), {cancelledOperationsCap: 2});
		runtime.cancelOperation(7);
		runtime.cancelOperation(7);
		runtime.cancelOperation(8);
		expect(runtime.cancelledOperationCount).toBe(2);
		runtime.cancelOperation(9);
		expect(runtime.cancelledOperationCount).toBe(2);
	});

	it('does not retain references via the deprecated findIndex/splice path', () => {
		const runtimeSource = VoiceEngineV2Runtime.prototype.toString();
		expect(runtimeSource).not.toMatch(/findIndex/);
		expect(runtimeSource).not.toMatch(/splice/);
	});
});

describe('VoiceEngineV2Runtime programmer error propagation', () => {
	it('classifies assertion failures and marked errors as programmer errors', () => {
		expect(isVoiceEngineV2ProgrammerError(new assert.AssertionError({message: 'invariant broken'}))).toBe(true);
		expect(isVoiceEngineV2ProgrammerError(Object.assign(new Error('boom'), {voiceEngineV2ProgrammerError: true}))).toBe(
			true,
		);
		expect(isVoiceEngineV2ProgrammerError(new Error('operational failure'))).toBe(false);
		expect(isVoiceEngineV2ProgrammerError('not an error')).toBe(false);
	});

	it('rethrows an assertion failure from completion dispatch instead of converting it to a failure event', async () => {
		const driver = new FakeVoiceEngineV2Driver();
		const runtime = createTestRuntime(new VoiceEngineV2TestImplementation(driver));
		const events: Array<string> = [];
		let triggered = false;
		runtime.subscribe(({event}) => {
			events.push(event.type);
			if (!triggered && event.type === 'connection.connectSucceeded') {
				triggered = true;
				assert.fail('synthetic reducer invariant violation');
			}
		});

		const rejections: Array<unknown> = [];
		const onUnhandledRejection = (reason: unknown) => {
			rejections.push(reason);
		};
		const previousListeners = process.listeners('unhandledRejection');
		process.removeAllListeners('unhandledRejection');
		process.on('unhandledRejection', onUnhandledRejection);
		try {
			runtime.dispatch({type: 'connection.connectRequested', options: {url: 'wss://voice', token: 't'}});
			await waitForRuntime();
			await new Promise((resolve) => setTimeout(resolve, 0));
			await new Promise((resolve) => setTimeout(resolve, 0));
		} finally {
			process.removeListener('unhandledRejection', onUnhandledRejection);
			for (const listener of previousListeners) {
				process.on('unhandledRejection', listener);
			}
		}

		expect(events).toContain('connection.connectSucceeded');
		expect(events).not.toContain('connection.connectFailed');
		expect(rejections).toHaveLength(1);
		expect(rejections[0]).toBeInstanceOf(assert.AssertionError);
	});

	it('still converts an operational listener throw into the command failure event', async () => {
		const driver = new FakeVoiceEngineV2Driver();
		const runtime = createTestRuntime(new VoiceEngineV2TestImplementation(driver));
		const events: Array<string> = [];
		let triggered = false;
		runtime.subscribe(({event}) => {
			events.push(event.type);
			if (!triggered && event.type === 'connection.connectSucceeded') {
				triggered = true;
				throw new Error('operational listener failure');
			}
		});

		runtime.dispatch({type: 'connection.connectRequested', options: {url: 'wss://voice', token: 't'}});
		await waitForRuntime();

		expect(events).toContain('connection.connectFailed');
	});
});

describe('VoiceEngineV2Runtime listener caps', () => {
	it('caps runtime listener subscriptions at the named cap', () => {
		const runtime = createTestRuntime(new DeferredAllResolveImplementation());
		for (let index = 0; index < VOICE_ENGINE_V2_LISTENERS_CAP; index += 1) {
			runtime.subscribe(() => {});
		}
		expect(() => runtime.subscribe(() => {})).toThrow(/cap/);
	});

	it('caps diagnostic listener subscriptions at the named cap', () => {
		const runtime = createTestRuntime(new DeferredAllResolveImplementation());
		for (let index = 0; index < VOICE_ENGINE_V2_DIAGNOSTIC_LISTENERS_CAP; index += 1) {
			runtime.subscribeDiagnostics(() => {});
		}
		expect(() => runtime.subscribeDiagnostics(() => {})).toThrow(/cap/);
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
