// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {isVoiceEngineV2CommandCompletionStale, transitionVoiceEngineV2} from '@fluxer/voice_engine_v2/src/core/reducer';
import {
	availableVoiceEngineV2Capabilities,
	createVoiceEngineV2InitialSnapshot,
	type VoiceEngineV2Snapshot,
	type VoiceEngineV2Transition,
} from '@fluxer/voice_engine_v2/src/core/state';
import {
	errorToVoiceEngineV2Error,
	type VoiceEngineV2CommandResult,
	type VoiceEngineV2Implementation,
} from '@fluxer/voice_engine_v2/src/implementations';
import {
	getVoiceEngineV2CommandResourceKey,
	type VoiceEngineV2Command,
} from '@fluxer/voice_engine_v2/src/protocol/commands';
import type {VoiceEngineV2Event} from '@fluxer/voice_engine_v2/src/protocol/events';
import type {
	VoiceEngineV2Capabilities,
	VoiceEngineV2Error,
	VoiceEngineV2ResourceKey,
} from '@fluxer/voice_engine_v2/src/protocol/types';
import {commandResultToEvent} from '@fluxer/voice_engine_v2/src/runtime/commandEvents';
import {
	assertEventLogRingInvariants,
	VOICE_ENGINE_V2_EVENT_LOG_CAP,
	type VoiceEngineV2EventLogEntry,
	VoiceEngineV2EventLogRing,
	type VoiceEngineV2EventLogSpillSink,
} from '@fluxer/voice_engine_v2/src/runtime/eventLogRing';
import {
	canCoalesceVoiceEngineV2Events,
	isVoiceEngineV2FrameReceivedEvent,
	VOICE_ENGINE_V2_COALESCED_TRACKS_CAP,
} from '@fluxer/voice_engine_v2/src/runtime/frameCoalescing';
import {
	createVoiceEngineV2SystemClockPort,
	createVoiceEngineV2SystemRandomPort,
	type VoiceEngineV2ClockPort,
	type VoiceEngineV2RandomPort,
} from '@fluxer/voice_engine_v2/src/runtime/platformPort';

export type {
	VoiceEngineV2EventLogEntry,
	VoiceEngineV2EventLogSpillSink,
} from '@fluxer/voice_engine_v2/src/runtime/eventLogRing';

export const VOICE_ENGINE_V2_QUEUED_COMMANDS_CAP = 4096;
export const VOICE_ENGINE_V2_RESOURCE_QUEUES_CAP = 256;
export const VOICE_ENGINE_V2_CANCELLED_OPERATIONS_CAP = 4096;
export const VOICE_ENGINE_V2_LISTENERS_CAP = 256;
export const VOICE_ENGINE_V2_DIAGNOSTIC_LISTENERS_CAP = 256;

export type VoiceEngineV2RuntimeQueueKind = 'queuedCommands' | 'resourceQueues' | 'cancelledOperations';

export interface VoiceEngineV2RuntimeDiagnostic {
	kind: 'voiceEngineV2.queueFull';
	queue: VoiceEngineV2RuntimeQueueKind;
	cap: number;
	droppedCommandType: string;
	droppedOperationId: number;
	resourceKey: VoiceEngineV2ResourceKey;
}

export type VoiceEngineV2RuntimeDiagnosticListener = (diagnostic: VoiceEngineV2RuntimeDiagnostic) => void;

export interface VoiceEngineV2RuntimeListenerPayload {
	event: VoiceEngineV2Event;
	transition: VoiceEngineV2Transition;
}

export type VoiceEngineV2RuntimeListener = (payload: VoiceEngineV2RuntimeListenerPayload) => void;

export type VoiceEngineV2RuntimeClock = VoiceEngineV2ClockPort;

export interface VoiceEngineV2RuntimeOptions {
	capabilities?: VoiceEngineV2Capabilities;
	clock?: VoiceEngineV2ClockPort;
	random?: VoiceEngineV2RandomPort;
	eventLogCap?: number;
	eventLogSpillSink: VoiceEngineV2EventLogSpillSink;
	queuedCommandsCap?: number;
	resourceQueuesCap?: number;
	cancelledOperationsCap?: number;
	verifyEventLogInvariantsOnDispatch?: boolean;
}

export function isVoiceEngineV2ProgrammerError(error: unknown): boolean {
	if (error instanceof assert.AssertionError) return true;
	if (error instanceof Error) {
		return (error as Error & {voiceEngineV2ProgrammerError?: unknown}).voiceEngineV2ProgrammerError === true;
	}
	return false;
}

export class VoiceEngineV2Runtime {
	private snapshotValue: VoiceEngineV2Snapshot;
	private readonly listeners = new Set<VoiceEngineV2RuntimeListener>();
	private readonly diagnosticListeners = new Set<VoiceEngineV2RuntimeDiagnosticListener>();
	private readonly unsubscribeImplementationEvents: () => void;
	private readonly clock: VoiceEngineV2ClockPort;
	private readonly random: VoiceEngineV2RandomPort;
	private readonly eventLogRing: VoiceEngineV2EventLogRing;
	private readonly eventLogSpillSink: VoiceEngineV2EventLogSpillSink;
	private readonly queuedCommandsById = new Map<number, VoiceEngineV2Command>();
	private readonly resourceQueues = new Map<VoiceEngineV2ResourceKey, Promise<void>>();
	private readonly executingCommandsByResource = new Map<VoiceEngineV2ResourceKey, VoiceEngineV2Command>();
	private readonly cancelledOperationIds = new Set<number>();
	private readonly queuedCommandsCap: number;
	private readonly resourceQueuesCap: number;
	private readonly cancelledOperationsCap: number;
	private readonly verifyEventLogInvariantsOnDispatch: boolean;
	private readonly coalesceCandidatesByTrack = new Map<
		string,
		{sequence: number; snapshotBefore: VoiceEngineV2Snapshot}
	>();
	private queueFullDropCount = 0;
	private coalescedEventsCountValue = 0;

	constructor(
		private readonly implementation: VoiceEngineV2Implementation,
		options: VoiceEngineV2RuntimeOptions,
	) {
		this.clock = options.clock ?? createVoiceEngineV2SystemClockPort();
		this.random = options.random ?? createVoiceEngineV2SystemRandomPort();
		const cap = options.eventLogCap ?? VOICE_ENGINE_V2_EVENT_LOG_CAP;
		assert.ok(Number.isInteger(cap), 'eventLogCap must be an integer');
		assert.ok(cap >= 1, 'eventLogCap must be >= 1');
		this.eventLogRing = new VoiceEngineV2EventLogRing(cap);
		assert.ok(
			options.eventLogSpillSink !== null && typeof options.eventLogSpillSink === 'object',
			'eventLogSpillSink is required',
		);
		assert.equal(typeof options.eventLogSpillSink.write, 'function', 'eventLogSpillSink.write must be a function');
		this.eventLogSpillSink = options.eventLogSpillSink;
		const queuedCommandsCap = options.queuedCommandsCap ?? VOICE_ENGINE_V2_QUEUED_COMMANDS_CAP;
		assert.ok(Number.isInteger(queuedCommandsCap), 'queuedCommandsCap must be an integer');
		assert.ok(queuedCommandsCap >= 1, 'queuedCommandsCap must be >= 1');
		this.queuedCommandsCap = queuedCommandsCap;
		const resourceQueuesCap = options.resourceQueuesCap ?? VOICE_ENGINE_V2_RESOURCE_QUEUES_CAP;
		assert.ok(Number.isInteger(resourceQueuesCap), 'resourceQueuesCap must be an integer');
		assert.ok(resourceQueuesCap >= 1, 'resourceQueuesCap must be >= 1');
		this.resourceQueuesCap = resourceQueuesCap;
		const cancelledOperationsCap = options.cancelledOperationsCap ?? VOICE_ENGINE_V2_CANCELLED_OPERATIONS_CAP;
		assert.ok(Number.isInteger(cancelledOperationsCap), 'cancelledOperationsCap must be an integer');
		assert.ok(cancelledOperationsCap >= 1, 'cancelledOperationsCap must be >= 1');
		this.cancelledOperationsCap = cancelledOperationsCap;
		this.verifyEventLogInvariantsOnDispatch = options.verifyEventLogInvariantsOnDispatch === true;
		this.snapshotValue = createVoiceEngineV2InitialSnapshot(
			options.capabilities ?? availableVoiceEngineV2Capabilities(),
		);
		this.unsubscribeImplementationEvents =
			implementation.subscribe?.((event) => {
				this.dispatch(event);
			}) ?? (() => {});
	}

	get snapshot(): VoiceEngineV2Snapshot {
		return this.snapshotValue;
	}

	get eventLog(): ReadonlyArray<VoiceEngineV2EventLogEntry> {
		return this.eventLogRing.snapshotEntries();
	}

	get eventLogCap(): number {
		return this.eventLogRing.cap;
	}

	get droppedEventsCount(): number {
		return this.eventLogRing.droppedEventsCount;
	}

	get evictedSequenceMin(): number | null {
		return this.eventLogRing.evictedSequenceMin;
	}

	get commandQueue(): ReadonlyArray<VoiceEngineV2Command> {
		return Array.from(this.queuedCommandsById.values());
	}

	get commandQueueSize(): number {
		return this.queuedCommandsById.size;
	}

	get platformRandom(): VoiceEngineV2RandomPort {
		return this.random;
	}

	get platformClock(): VoiceEngineV2ClockPort {
		return this.clock;
	}

	get queuedCommandsCapacity(): number {
		return this.queuedCommandsCap;
	}

	get resourceQueuesCapacity(): number {
		return this.resourceQueuesCap;
	}

	get queueFullDropTotal(): number {
		return this.queueFullDropCount;
	}

	get activeResourceQueueCount(): number {
		return this.resourceQueues.size;
	}

	getExecutingCommand(resourceKey: VoiceEngineV2ResourceKey): VoiceEngineV2Command | null {
		assert.equal(typeof resourceKey, 'string', 'getExecutingCommand requires a string resourceKey');
		assert.ok(resourceKey.length > 0, 'getExecutingCommand requires a non-empty resourceKey');
		return this.executingCommandsByResource.get(resourceKey) ?? null;
	}

	isOperationPending(operationId: number): boolean {
		assert.ok(Number.isInteger(operationId), 'isOperationPending requires an integer operationId');
		assert.ok(operationId >= 0, 'isOperationPending requires a non-negative operationId');
		if (this.queuedCommandsById.has(operationId)) return true;
		for (const command of this.executingCommandsByResource.values()) {
			if (command.operationId === operationId) return true;
		}
		return false;
	}

	subscribe(listener: VoiceEngineV2RuntimeListener): () => void {
		assert.equal(typeof listener, 'function', 'subscribe requires a function listener');
		this.listeners.add(listener);
		assert.ok(this.listeners.size <= VOICE_ENGINE_V2_LISTENERS_CAP, 'runtime listeners exceeded cap');
		return () => {
			this.listeners.delete(listener);
		};
	}

	subscribeDiagnostics(listener: VoiceEngineV2RuntimeDiagnosticListener): () => void {
		assert.equal(typeof listener, 'function', 'subscribeDiagnostics requires a function listener');
		this.diagnosticListeners.add(listener);
		assert.ok(
			this.diagnosticListeners.size <= VOICE_ENGINE_V2_DIAGNOSTIC_LISTENERS_CAP,
			'runtime diagnostic listeners exceeded cap',
		);
		return () => {
			this.diagnosticListeners.delete(listener);
		};
	}

	dispatch(event: VoiceEngineV2Event): VoiceEngineV2Transition {
		assert.ok(event !== null, 'dispatch requires a non-null event');
		assert.equal(typeof event.type, 'string', 'dispatch requires an event with a string type');
		const coalescedTransition = this.coalesceFrameReceivedDispatch(event);
		if (coalescedTransition !== null) return coalescedTransition;
		const snapshotBefore = this.snapshotValue;
		const transition = transitionVoiceEngineV2(snapshotBefore, event);
		this.snapshotValue = transition.snapshot;
		const sequence = this.appendEventLog(event, transition.commands);
		this.recordCoalesceCandidate(event, sequence, snapshotBefore);
		if (this.verifyEventLogInvariantsOnDispatch) {
			this.verifyEventLogInvariants();
		}
		this.enqueueTransitionCommands(transition.commands);
		this.notify(event, transition);
		return transition;
	}

	private coalesceFrameReceivedDispatch(event: VoiceEngineV2Event): VoiceEngineV2Transition | null {
		if (!isVoiceEngineV2FrameReceivedEvent(event)) return null;
		const candidate = this.coalesceCandidatesByTrack.get(event.frame.trackSid);
		if (candidate === undefined) return null;
		const tail = this.eventLogRing.tailEntry;
		if (tail === null) return null;
		if (tail.sequence !== candidate.sequence) return null;
		if (tail.commands.length !== 0) return null;
		assert.ok(canCoalesceVoiceEngineV2Events(tail.event, event), 'coalesce candidate must match the log tail frame');
		const transition = transitionVoiceEngineV2(candidate.snapshotBefore, event);
		this.snapshotValue = transition.snapshot;
		this.eventLogRing.replaceTail({
			sequence: tail.sequence,
			atMs: this.clock.now(),
			event,
			commands: transition.commands,
		});
		this.coalescedEventsCountValue += 1;
		assert.ok(this.coalescedEventsCountValue >= 1, 'coalesced events count must stay positive after coalescing');
		if (this.verifyEventLogInvariantsOnDispatch) {
			this.verifyEventLogInvariants();
		}
		this.enqueueTransitionCommands(transition.commands);
		this.notify(event, transition);
		return transition;
	}

	private recordCoalesceCandidate(
		event: VoiceEngineV2Event,
		sequence: number,
		snapshotBefore: VoiceEngineV2Snapshot,
	): void {
		assert.ok(sequence >= 1, 'recordCoalesceCandidate requires a positive sequence');
		if (!isVoiceEngineV2FrameReceivedEvent(event)) return;
		const trackSid = event.frame.trackSid;
		const alreadyTracked = this.coalesceCandidatesByTrack.delete(trackSid);
		if (!alreadyTracked && this.coalesceCandidatesByTrack.size >= VOICE_ENGINE_V2_COALESCED_TRACKS_CAP) {
			const oldest = this.coalesceCandidatesByTrack.keys().next();
			assert.ok(oldest.done !== true, 'coalesce candidates at cap must yield an evictable track');
			this.coalesceCandidatesByTrack.delete(oldest.value);
		}
		this.coalesceCandidatesByTrack.set(trackSid, {sequence, snapshotBefore});
		assert.ok(
			this.coalesceCandidatesByTrack.size <= VOICE_ENGINE_V2_COALESCED_TRACKS_CAP,
			'coalesce candidates exceeded cap',
		);
	}

	private enqueueTransitionCommands(commands: Array<VoiceEngineV2Command>): void {
		const commandsLimit = commands.length;
		for (let commandIndex = 0; commandIndex < commandsLimit; commandIndex += 1) {
			const command = commands[commandIndex];
			assert.ok(command !== undefined, 'command batch must not contain holes');
			this.enqueueCommand(command);
		}
	}

	get coalescedEventsCount(): number {
		assert.ok(this.coalescedEventsCountValue >= 0, 'coalesced events count cannot be negative');
		return this.coalescedEventsCountValue;
	}

	get coalesceTrackedTracksCount(): number {
		assert.ok(
			this.coalesceCandidatesByTrack.size <= VOICE_ENGINE_V2_COALESCED_TRACKS_CAP,
			'coalesce candidates exceeded cap',
		);
		return this.coalesceCandidatesByTrack.size;
	}

	verifyEventLogInvariants(): void {
		assertEventLogRingInvariants(this.eventLogRing);
		const entries = this.eventLogRing.snapshotEntries();
		assert.ok(entries.length <= this.eventLogRing.cap, 'event log size must not exceed cap');
		const droppedCount = this.eventLogRing.droppedEventsCount;
		assert.ok(droppedCount >= 0, 'dropped count must be non-negative');
		const evictedMin = this.eventLogRing.evictedSequenceMin;
		if (droppedCount === 0) {
			assert.equal(evictedMin, null, 'evicted min must be null when no drops occurred');
		}
		if (droppedCount > 0) {
			assert.ok(evictedMin !== null, 'evicted min must be set when drops occurred');
		}
	}

	dispose(): void {
		this.unsubscribeImplementationEvents();
		this.listeners.clear();
		this.diagnosticListeners.clear();
		this.coalesceCandidatesByTrack.clear();
	}

	private notify(event: VoiceEngineV2Event, transition: VoiceEngineV2Transition): void {
		for (const listener of this.listeners) {
			listener({event, transition});
		}
	}

	private appendEventLog(event: VoiceEngineV2Event, commands: Array<VoiceEngineV2Command>): number {
		const sequence = this.eventLogRing.allocateSequence();
		const entry: VoiceEngineV2EventLogEntry = {
			sequence,
			atMs: this.clock.now(),
			event,
			commands,
		};
		const evicted = this.eventLogRing.push(entry);
		if (evicted !== null) {
			this.spillEvictedEntry(evicted);
		}
		return sequence;
	}

	private spillEvictedEntry(entry: VoiceEngineV2EventLogEntry): void {
		assert.ok(entry.sequence >= 1, 'evicted entry must have a positive sequence');
		const sink = this.eventLogSpillSink;
		const writePromise = sink.write(entry);
		assert.ok(writePromise !== null, 'event log spill sink must return a promise');
		void writePromise.catch((error: unknown) => {
			const normalisedError = errorToVoiceEngineV2Error(error);
			queueMicrotask(() => {
				throw new Error(
					`voice engine v2 event log spill sink rejected sequence ${entry.sequence}: ${normalisedError.code}: ${normalisedError.message}`,
				);
			});
		});
	}

	private enqueueCommand(command: VoiceEngineV2Command): void {
		assert.ok(command !== undefined, 'enqueueCommand requires a non-undefined command');
		assert.equal(typeof command.type, 'string', 'enqueueCommand requires a string command type');
		const resourceKey = getVoiceEngineV2CommandResourceKey(command);
		if (this.queuedCommandsById.size >= this.queuedCommandsCap) {
			this.rejectCommandAtQueueCap('queuedCommands', this.queuedCommandsCap, command, resourceKey);
			return;
		}
		const alreadyTracked = this.resourceQueues.has(resourceKey);
		if (!alreadyTracked && this.resourceQueues.size >= this.resourceQueuesCap) {
			this.rejectCommandAtQueueCap('resourceQueues', this.resourceQueuesCap, command, resourceKey);
			return;
		}
		this.queuedCommandsById.set(command.operationId, command);
		assert.ok(this.queuedCommandsById.size <= this.queuedCommandsCap, 'queuedCommands push exceeded cap');
		const previous = this.resourceQueues.get(resourceKey) ?? Promise.resolve();
		const next = previous.then(
			() => this.executeQueuedCommand(command),
			() => this.executeQueuedCommand(command),
		);
		this.resourceQueues.set(resourceKey, next);
		assert.ok(this.resourceQueues.size <= this.resourceQueuesCap, 'resourceQueues set exceeded cap');
		void next.finally(() => {
			if (this.resourceQueues.get(resourceKey) === next) {
				this.resourceQueues.delete(resourceKey);
			}
		});
	}

	private async executeQueuedCommand(command: VoiceEngineV2Command): Promise<void> {
		this.consumeQueuedCommand(command);
		if (this.cancelledOperationIds.has(command.operationId)) {
			this.cancelledOperationIds.delete(command.operationId);
			return;
		}
		const resourceKey = getVoiceEngineV2CommandResourceKey(command);
		assert.ok(!this.executingCommandsByResource.has(resourceKey), 'resource queues execute one command at a time');
		this.executingCommandsByResource.set(resourceKey, command);
		assert.ok(
			this.executingCommandsByResource.size <= this.resourceQueuesCap,
			'executing commands exceeded resource queue cap',
		);
		try {
			const result = await this.executeCommand(command);
			if (isVoiceEngineV2CommandCompletionStale(this.snapshotValue, command)) {
				this.dispatch({
					type: 'command.staleCompletionRejected',
					operationId: command.operationId,
					commandType: command.type,
					resourceKey,
				});
				return;
			}
			this.dispatch(commandResultToEvent(command, result));
		} catch (error) {
			if (isVoiceEngineV2ProgrammerError(error)) throw error;
			this.dispatchTerminalFailure(command, error);
		} finally {
			if (this.executingCommandsByResource.get(resourceKey) === command) {
				this.executingCommandsByResource.delete(resourceKey);
			}
		}
	}

	private async executeCommand(command: VoiceEngineV2Command): Promise<VoiceEngineV2CommandResult> {
		try {
			return await this.implementation.execute(command);
		} catch (error) {
			return {ok: false, error: errorToVoiceEngineV2Error(error)};
		}
	}

	private consumeQueuedCommand(command: VoiceEngineV2Command): void {
		assert.ok(command !== undefined, 'consumeQueuedCommand requires a command');
		this.queuedCommandsById.delete(command.operationId);
		assert.ok(
			!this.queuedCommandsById.has(command.operationId),
			'consumeQueuedCommand postcondition: id must not remain',
		);
	}

	cancelOperation(operationId: number): void {
		assert.ok(Number.isInteger(operationId), 'cancelOperation requires an integer operationId');
		assert.ok(operationId >= 0, 'cancelOperation requires a non-negative operationId');
		if (this.cancelledOperationIds.has(operationId)) return;
		if (this.cancelledOperationIds.size >= this.cancelledOperationsCap) {
			const oldest = this.cancelledOperationIds.values().next();
			assert.ok(oldest.done !== true, 'cancelledOperationIds at cap must yield an evictable id');
			this.cancelledOperationIds.delete(oldest.value);
			this.emitCancelledOperationEvictionDiagnostic(oldest.value);
		}
		this.cancelledOperationIds.add(operationId);
		assert.ok(this.cancelledOperationIds.has(operationId), 'cancelledOperationIds insert postcondition');
		assert.ok(this.cancelledOperationIds.size <= this.cancelledOperationsCap, 'cancelledOperationIds exceeded cap');
	}

	get cancelledOperationCount(): number {
		return this.cancelledOperationIds.size;
	}

	get cancelledOperationsCapacity(): number {
		return this.cancelledOperationsCap;
	}

	private emitDiagnostic(diagnostic: VoiceEngineV2RuntimeDiagnostic): void {
		assert.equal(diagnostic.kind, 'voiceEngineV2.queueFull', 'emitDiagnostic requires a known diagnostic kind');
		assert.ok(diagnostic.cap >= 1, 'emitDiagnostic requires a positive cap');
		for (const listener of this.diagnosticListeners) {
			listener(diagnostic);
		}
	}

	private emitCancelledOperationEvictionDiagnostic(evictedOperationId: number): void {
		assert.ok(Number.isInteger(evictedOperationId), 'evicted cancellation id must be an integer');
		assert.ok(!this.cancelledOperationIds.has(evictedOperationId), 'evicted cancellation id must be removed first');
		const queuedCommand = this.queuedCommandsById.get(evictedOperationId) ?? null;
		this.emitDiagnostic({
			kind: 'voiceEngineV2.queueFull',
			queue: 'cancelledOperations',
			cap: this.cancelledOperationsCap,
			droppedCommandType: queuedCommand === null ? 'operation.cancel' : queuedCommand.type,
			droppedOperationId: evictedOperationId,
			resourceKey: queuedCommand === null ? 'lifecycle' : getVoiceEngineV2CommandResourceKey(queuedCommand),
		});
	}

	private rejectCommandAtQueueCap(
		queue: VoiceEngineV2RuntimeQueueKind,
		cap: number,
		command: VoiceEngineV2Command,
		resourceKey: VoiceEngineV2ResourceKey,
	): void {
		assert.ok(cap >= 1, 'rejectCommandAtQueueCap requires a positive cap');
		assert.equal(typeof command.type, 'string', 'rejectCommandAtQueueCap requires a command type');
		this.queueFullDropCount += 1;
		this.emitDiagnostic({
			kind: 'voiceEngineV2.queueFull',
			queue,
			cap,
			droppedCommandType: command.type,
			droppedOperationId: command.operationId,
			resourceKey,
		});
		const overflowError: VoiceEngineV2Error = {
			code: 'implementationError',
			message: `Voice engine v2 command rejected: ${queue} queue overflow at cap ${cap}`,
		};
		queueMicrotask(() => {
			if (isVoiceEngineV2CommandCompletionStale(this.snapshotValue, command)) {
				this.dispatch({
					type: 'command.staleCompletionRejected',
					operationId: command.operationId,
					commandType: command.type,
					resourceKey,
				});
				return;
			}
			this.dispatchTerminalFailure(command, overflowError);
		});
	}

	private dispatchTerminalFailure(command: VoiceEngineV2Command, error: unknown): void {
		assert.ok(command !== undefined, 'dispatchTerminalFailure requires a command');
		const normalised = errorToVoiceEngineV2Error(error);
		const failureEvent = commandResultToEvent(command, {ok: false, error: normalised});
		try {
			this.dispatch(failureEvent);
		} catch (cascadeError) {
			const cascade = errorToVoiceEngineV2Error(cascadeError);
			queueMicrotask(() => {
				throw new Error(
					`voice engine v2 terminal failure dispatch cascaded for operationId ${command.operationId} (${command.type}): ${cascade.code}: ${cascade.message}`,
				);
			});
		}
	}
}

export function assertEventLogInvariants(runtime: VoiceEngineV2Runtime): void {
	assert.ok(runtime instanceof VoiceEngineV2Runtime, 'assertEventLogInvariants requires a runtime instance');
	runtime.verifyEventLogInvariants();
}

export {commandResultToEvent} from '@fluxer/voice_engine_v2/src/runtime/commandEvents';
