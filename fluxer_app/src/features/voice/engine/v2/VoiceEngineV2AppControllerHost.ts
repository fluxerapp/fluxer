// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import {
	type VoiceEngineV2Command,
	VoiceEngineV2Controller,
	type VoiceEngineV2Error,
	type VoiceEngineV2Event,
	type VoiceEngineV2EventLogSpillSink,
	VoiceEngineV2HostPortImplementation,
	type VoiceEngineV2HostPorts,
	type VoiceEngineV2Model,
	type VoiceEngineV2OperationId,
	type VoiceEngineV2ResourceKey,
	VoiceEngineV2Runtime,
	type VoiceEngineV2RuntimeClock,
	type VoiceEngineV2RuntimeListener,
	type VoiceEngineV2Snapshot,
	type VoiceEngineV2Transition,
} from '@fluxer/voice_engine_v2';

export interface VoiceEngineV2AppControllerHostOptions {
	ports: VoiceEngineV2HostPorts;
	clock?: VoiceEngineV2RuntimeClock;
	eventLogSpillSink: VoiceEngineV2EventLogSpillSink;
	queuedCommandsCap?: number;
	resourceQueuesCap?: number;
}

export interface VoiceEngineV2AppControllerHostView {
	snapshot: VoiceEngineV2Snapshot;
	model: VoiceEngineV2Model;
}

export interface VoiceEngineV2AppControllerHostWaitOptions {
	description?: string;
	staleCompletion?: 'reject' | 'resolve';
	timeoutMs?: number;
	onCommandsPlanned?: (commands: ReadonlyArray<VoiceEngineV2Command>) => void;
}

const VOICE_ENGINE_V2_APP_COMMAND_WAIT_TIMEOUT_MS = 30_000;

const logger = new Logger('VoiceEngineV2AppControllerHost');

function operationIdOf(event: VoiceEngineV2Event): VoiceEngineV2OperationId | null {
	if ('operationId' in event && typeof event.operationId === 'number') return event.operationId;
	return null;
}

function errorOf(event: VoiceEngineV2Event): VoiceEngineV2Error | null {
	if ('error' in event && event.error) return event.error;
	return null;
}

function isFailedTerminalEvent(event: VoiceEngineV2Event): boolean {
	if (event.type === 'command.failed') return true;
	if (event.type === 'command.staleCompletionRejected') return true;
	if (event.type.endsWith('Failed')) return true;
	if (event.type.endsWith('.failed')) return true;
	return event.type === 'nativeCapture.failed' || event.type === 'permissions.failed' || event.type === 'e2ee.failed';
}

function isSucceededTerminalEvent(event: VoiceEngineV2Event): boolean {
	if (event.type === 'command.succeeded') return true;
	if (event.type.endsWith('Succeeded')) return true;
	if (event.type === 'stats.collected') return true;
	if (event.type === 'capabilities.hardwareEncoderChanged') return operationIdOf(event) !== null;
	if (event.type === 'permissions.result') return operationIdOf(event) !== null;
	if (event.type === 'devices.changed') return operationIdOf(event) !== null;
	if (event.type === 'nativeCapture.started') return true;
	if (event.type === 'nativeCapture.stopped') return operationIdOf(event) !== null;
	return event.type === 'e2ee.enabled' || event.type === 'e2ee.disabled';
}

function buildWaitFailure(event: VoiceEngineV2Event, description: string): Error {
	const error = errorOf(event);
	const message = error
		? `${description} failed: ${error.code}: ${error.message}`
		: `${description} failed: ${event.type}`;
	const failure = new Error(message);
	failure.name = 'VoiceEngineV2AppControllerHostCommandError';
	return failure;
}

function buildWaitTimeout(description: string, pending: ReadonlySet<VoiceEngineV2OperationId>): Error {
	const failure = new Error(`${description} timed out waiting for operation(s): ${Array.from(pending).join(', ')}`);
	failure.name = 'VoiceEngineV2AppControllerHostCommandTimeoutError';
	return failure;
}

export class VoiceEngineV2AppControllerHost {
	readonly controller: VoiceEngineV2Controller;
	private readonly runtime: VoiceEngineV2Runtime;
	private readonly runtimeDiagnosticsDisposer: () => void;

	constructor(options: VoiceEngineV2AppControllerHostOptions) {
		this.runtime = new VoiceEngineV2Runtime(new VoiceEngineV2HostPortImplementation(options.ports), {
			clock: options.clock,
			eventLogSpillSink: options.eventLogSpillSink,
			...(options.queuedCommandsCap !== undefined ? {queuedCommandsCap: options.queuedCommandsCap} : {}),
			...(options.resourceQueuesCap !== undefined ? {resourceQueuesCap: options.resourceQueuesCap} : {}),
		});
		this.controller = new VoiceEngineV2Controller(this.runtime);
		this.runtimeDiagnosticsDisposer = this.runtime.subscribeDiagnostics((diagnostic) => {
			logger.warn('Voice engine v2 runtime rejected a command at queue cap', {
				queue: diagnostic.queue,
				cap: diagnostic.cap,
				droppedCommandType: diagnostic.droppedCommandType,
				droppedOperationId: diagnostic.droppedOperationId,
				resourceKey: diagnostic.resourceKey,
			});
		});
	}

	get snapshot(): VoiceEngineV2Snapshot {
		return this.controller.snapshot;
	}

	get model(): VoiceEngineV2Model {
		return this.controller.model;
	}

	get view(): VoiceEngineV2AppControllerHostView {
		return {
			snapshot: this.snapshot,
			model: this.model,
		};
	}

	subscribe(listener: VoiceEngineV2RuntimeListener): () => void {
		return this.controller.subscribe(listener);
	}

	dispatch(event: VoiceEngineV2Event): VoiceEngineV2Transition {
		return this.runtime.dispatch(event);
	}

	executingCommand(resourceKey: VoiceEngineV2ResourceKey): VoiceEngineV2Command | null {
		return this.runtime.getExecutingCommand(resourceKey);
	}

	isOperationPending(operationId: VoiceEngineV2OperationId): boolean {
		return this.runtime.isOperationPending(operationId);
	}

	runAndWait(action: () => void, options: VoiceEngineV2AppControllerHostWaitOptions = {}): Promise<void> {
		const pending = new Set<VoiceEngineV2OperationId>();
		const description = options.description ?? 'voice engine v2 command';
		const staleCompletion = options.staleCompletion ?? 'reject';
		const timeoutMs = options.timeoutMs ?? VOICE_ENGINE_V2_APP_COMMAND_WAIT_TIMEOUT_MS;
		return new Promise((resolve, reject) => {
			let actionComplete = false;
			let timer: ReturnType<typeof setTimeout> | null = null;
			let unsubscribe: (() => void) | null = null;
			const cleanup = () => {
				if (timer !== null) clearTimeout(timer);
				unsubscribe?.();
			};
			const settle = (error?: Error) => {
				cleanup();
				if (error) reject(error);
				else resolve();
			};
			const plannedCommands: Array<VoiceEngineV2Command> = [];
			unsubscribe = this.subscribe(({event, transition}) => {
				if (!actionComplete) addPendingCommands(pending, plannedCommands, transition.commands);
				this.consumeTerminalEvent(event, pending, {description, staleCompletion}, settle);
			});
			timer = setTimeout(() => settle(buildWaitTimeout(description, pending)), timeoutMs);
			try {
				action();
				actionComplete = true;
				options.onCommandsPlanned?.(plannedCommands);
				if (pending.size === 0) {
					logger.info('Voice engine v2 action planned zero commands', {
						description,
						connectionStatus: this.snapshot.connection.status,
					});
					settle();
				}
			} catch (error) {
				cleanup();
				reject(error);
			}
		});
	}

	private consumeTerminalEvent(
		event: VoiceEngineV2Event,
		pending: Set<VoiceEngineV2OperationId>,
		options: Required<Pick<VoiceEngineV2AppControllerHostWaitOptions, 'description' | 'staleCompletion'>>,
		settle: (error?: Error) => void,
	): void {
		const operationId = operationIdOf(event);
		if (operationId === null || !pending.has(operationId)) return;
		if (event.type === 'command.staleCompletionRejected' && options.staleCompletion === 'resolve') {
			pending.delete(operationId);
			if (pending.size === 0) settle();
			return;
		}
		if (isFailedTerminalEvent(event)) {
			settle(buildWaitFailure(event, options.description));
			return;
		}
		if (!isSucceededTerminalEvent(event)) return;
		pending.delete(operationId);
		if (pending.size === 0) settle();
	}

	dispose(): void {
		this.runtimeDiagnosticsDisposer();
		this.controller.dispose();
	}
}

function addPendingCommands(
	pending: Set<VoiceEngineV2OperationId>,
	plannedCommands: Array<VoiceEngineV2Command>,
	commands: ReadonlyArray<VoiceEngineV2Command>,
): void {
	const commandCount = commands.length;
	for (let commandIndex = 0; commandIndex < commandCount; commandIndex += 1) {
		const command = commands[commandIndex];
		if (command !== undefined) {
			pending.add(command.operationId);
			plannedCommands.push(command);
		}
	}
}

export function createVoiceEngineV2AppControllerHost(
	options: VoiceEngineV2AppControllerHostOptions,
): VoiceEngineV2AppControllerHost {
	return new VoiceEngineV2AppControllerHost(options);
}
