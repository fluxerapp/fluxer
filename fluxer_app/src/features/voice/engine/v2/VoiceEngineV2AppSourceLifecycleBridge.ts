// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {Logger} from '@app/features/platform/utils/AppLogger';
import type {NativeScreenCaptureLifecycleMessage} from '@app/types/electron.d';
import {
	type DiagnosticsPort,
	type SourceFault,
	type SourceLifecycleClock,
	type SourceLifecycleEvent,
	SourceLifecycleRegistry,
	type SourceLifecycleState,
	type VoiceEngineV2Event,
	type VoiceEngineV2SourceLifecycleTransitionedEvent,
} from '@fluxer/voice_engine_v2';
import {createVoiceEngineV2AppDiagnosticsAdapter} from './VoiceEngineV2AppDiagnosticsAdapter';

const VALID_LIFECYCLE_KINDS: ReadonlySet<NativeScreenCaptureLifecycleMessage['kind']> = new Set([
	'error',
	'closed',
	'closed-clean',
	'stalled',
	'diagnostic',
]);

const MAX_SOURCE_ID_LENGTH = 256;
const MAX_REGISTRY_CAP = 64;
const CLOSED_CLEAN_DEDUP_WINDOW_MS = 500;

export type VoiceEngineV2AppSourceLifecycleDispatch = (event: VoiceEngineV2Event) => void;

export type VoiceEngineV2AppSourceLifecycleSubscribe = (
	callback: (message: NativeScreenCaptureLifecycleMessage) => void,
) => () => void;

export interface VoiceEngineV2AppSourceLifecycleClosedCleanInfo {
	captureId: string;
	sourceId: string;
	source: 'delegate' | 'programmatic' | 'unknown';
	atMs: number;
}

export type VoiceEngineV2AppSourceLifecycleClosedCleanCallback = (
	info: VoiceEngineV2AppSourceLifecycleClosedCleanInfo,
) => void;

export interface VoiceEngineV2AppSourceLifecycleBridgeOptions {
	dispatch: VoiceEngineV2AppSourceLifecycleDispatch;
	subscribe: VoiceEngineV2AppSourceLifecycleSubscribe;
	diagnostics?: DiagnosticsPort;
	onClosedClean?: VoiceEngineV2AppSourceLifecycleClosedCleanCallback;
	clock?: SourceLifecycleClock;
	now?: () => number;
	cap?: number;
}

export interface VoiceEngineV2AppSourceLifecycleBindOptions {
	captureId: string;
	sourceId: string;
}

export interface VoiceEngineV2AppRemoteTrackLifecycleTarget {
	on(event: 'ended', listener: () => void): void;
	off(event: 'ended', listener: () => void): void;
}

interface RegisteredBinding {
	captureId: string;
	sourceId: string;
}

function assertValidString(name: string, value: string, maxLen: number): void {
	assert.equal(typeof value, 'string', `${name} must be a string`);
	assert.ok(value.length > 0, `${name} must not be empty`);
	assert.ok(value.length <= maxLen, `${name} length must be <= ${maxLen}`);
}

function translateLifecycleEvent(kind: NativeScreenCaptureLifecycleMessage['kind']): SourceLifecycleEvent | null {
	if (kind === 'error') return {kind: 'fault', fault: 'captureDeviceLost'};
	if (kind === 'closed') return {kind: 'fault', fault: 'captureDeviceLost'};
	if (kind === 'closed-clean') return null;
	if (kind === 'stalled') return {kind: 'fault', fault: 'networkError'};
	if (kind === 'diagnostic') return null;
	return null;
}

function deriveFault(state: SourceLifecycleState): SourceFault | null {
	if (state.kind === 'reconnecting') return state.lastFault;
	if (state.kind === 'failed') return state.finalFault;
	return null;
}

function deriveAttempts(state: SourceLifecycleState): number {
	if (state.kind === 'reconnecting') return state.attempts;
	if (state.kind === 'failed') return state.totalAttempts;
	return 0;
}

function defaultNow(): number {
	const value = Date.now();
	assert.ok(Number.isFinite(value), 'Date.now must be finite');
	return value;
}

function makeBigIntClock(now: () => number): SourceLifecycleClock {
	let last = 0n;
	return {
		nowNs(): bigint {
			const ms = now();
			assert.ok(Number.isFinite(ms), 'clock now must be finite');
			const ns = ms <= 0 ? 0n : BigInt(Math.floor(ms)) * 1_000_000n;
			const next = ns < last ? last : ns;
			last = next;
			return next;
		},
	};
}

export class VoiceEngineV2AppSourceLifecycleBridge {
	private readonly dispatch: VoiceEngineV2AppSourceLifecycleDispatch;
	private readonly diagnostics: DiagnosticsPort;
	private readonly registry: SourceLifecycleRegistry;
	private readonly bindingsByCapture: Map<string, RegisteredBinding>;
	private readonly closedCleanLastAtMs: Map<string, number>;
	private readonly onClosedClean: VoiceEngineV2AppSourceLifecycleClosedCleanCallback | null;
	private readonly now: () => number;
	private readonly unsubscribe: () => void;
	private disposed = false;

	constructor(options: VoiceEngineV2AppSourceLifecycleBridgeOptions) {
		assert.ok(options, 'bridge options must not be null');
		assert.equal(typeof options.dispatch, 'function', 'dispatch must be a function');
		assert.equal(typeof options.subscribe, 'function', 'subscribe must be a function');
		const cap = options.cap ?? MAX_REGISTRY_CAP;
		assert.ok(Number.isInteger(cap), 'cap must be an integer');
		assert.ok(cap >= 1, 'cap must be >= 1');
		this.now = options.now ?? defaultNow;
		const clock = options.clock ?? makeBigIntClock(this.now);
		this.dispatch = options.dispatch;
		this.diagnostics =
			options.diagnostics ??
			createVoiceEngineV2AppDiagnosticsAdapter({logger: new Logger('VoiceEngineV2AppSourceLifecycleBridge')});
		this.registry = new SourceLifecycleRegistry(clock, cap);
		this.bindingsByCapture = new Map<string, RegisteredBinding>();
		this.closedCleanLastAtMs = new Map<string, number>();
		this.onClosedClean = options.onClosedClean ?? null;
		this.unsubscribe = options.subscribe((message) => this.handleNativeMessage(message));
		assert.equal(typeof this.unsubscribe, 'function', 'subscribe must return a disposer');
	}

	bind(options: VoiceEngineV2AppSourceLifecycleBindOptions): boolean {
		assert.ok(!this.disposed, 'bridge must not be disposed');
		assertValidString('captureId', options.captureId, MAX_SOURCE_ID_LENGTH);
		assertValidString('sourceId', options.sourceId, MAX_SOURCE_ID_LENGTH);
		if (this.bindingsByCapture.has(options.captureId)) return false;
		const registration = this.registry.register(options.sourceId);
		if (!registration.ok) return false;
		this.bindingsByCapture.set(options.captureId, {captureId: options.captureId, sourceId: options.sourceId});
		const event: VoiceEngineV2SourceLifecycleTransitionedEvent = {
			type: 'sourceLifecycle.transitioned',
			sourceId: options.sourceId,
			kind: 'active',
			since: registration.state.since,
			attempts: 0,
			fault: null,
			atMs: this.now(),
		};
		this.dispatch(event);
		return true;
	}

	unbind(captureId: string): boolean {
		assert.ok(!this.disposed, 'bridge must not be disposed');
		assertValidString('captureId', captureId, MAX_SOURCE_ID_LENGTH);
		return this.removeBinding(captureId);
	}

	private removeBinding(captureId: string): boolean {
		const binding = this.bindingsByCapture.get(captureId);
		if (!binding) return false;
		this.bindingsByCapture.delete(captureId);
		this.closedCleanLastAtMs.delete(captureId);
		const removed = this.registry.remove(binding.sourceId);
		assert.equal(removed, true, 'unbind must remove the registry entry');
		this.dispatch({type: 'sourceLifecycle.removed', sourceId: binding.sourceId});
		return true;
	}

	reportLifecycle(message: NativeScreenCaptureLifecycleMessage): boolean {
		assert.ok(!this.disposed, 'bridge must not be disposed');
		assert.ok(isRecord(message), 'reportLifecycle.message must be a record');
		const beforeBindings = this.bindingsByCapture.size;
		this.handleNativeMessage(message);
		assert.ok(this.bindingsByCapture.size <= beforeBindings, 'reportLifecycle must not add bindings');
		return true;
	}

	bindRemoteTrackLifecycle(
		track: VoiceEngineV2AppRemoteTrackLifecycleTarget,
		options: VoiceEngineV2AppSourceLifecycleBindOptions,
	): () => void {
		assert.ok(!this.disposed, 'bridge must not be disposed');
		assert.ok(track !== null && typeof track === 'object', 'track must be an object');
		assert.equal(typeof track.on, 'function', 'track.on must be a function');
		assert.equal(typeof track.off, 'function', 'track.off must be a function');
		assertValidString('captureId', options.captureId, MAX_SOURCE_ID_LENGTH);
		assertValidString('sourceId', options.sourceId, MAX_SOURCE_ID_LENGTH);
		const captureId = options.captureId;
		const sourceId = options.sourceId;
		const bound = this.bind({captureId, sourceId});
		if (!bound) return () => undefined;
		let cleanedUp = false;
		const onEnded = (): void => {
			if (cleanedUp || this.disposed) return;
			this.dispatchRemoteTrackFault(captureId, sourceId);
		};
		track.on('ended', onEnded);
		return (): void => {
			if (cleanedUp) return;
			cleanedUp = true;
			try {
				track.off('ended', onEnded);
			} catch {}
			if (!this.disposed) this.unbind(captureId);
		};
	}

	private dispatchRemoteTrackFault(captureId: string, sourceId: string): void {
		assert.ok(!this.disposed, 'bridge must not be disposed');
		assert.ok(this.bindingsByCapture.has(captureId), 'fault must target a known binding');
		const result = this.registry.dispatch(sourceId, {kind: 'fault', fault: 'networkError'});
		if (!result.ok) return;
		const event: VoiceEngineV2SourceLifecycleTransitionedEvent = {
			type: 'sourceLifecycle.transitioned',
			sourceId,
			kind: result.state.kind,
			since: result.state.since,
			attempts: deriveAttempts(result.state),
			fault: deriveFault(result.state),
			atMs: this.now(),
		};
		this.dispatch(event);
	}

	dispose(): void {
		if (this.disposed) return;
		this.disposed = true;
		for (const captureId of [...this.bindingsByCapture.keys()]) {
			this.removeBinding(captureId);
		}
		try {
			this.unsubscribe();
		} catch {}
		this.closedCleanLastAtMs.clear();
		assert.equal(this.bindingsByCapture.size, 0, 'bindings must be cleared after dispose');
	}

	private handleNativeMessage(message: NativeScreenCaptureLifecycleMessage): void {
		if (this.disposed) return;
		if (!isRecord(message)) return;
		if (typeof message.captureId !== 'string' || message.captureId.length === 0) return;
		if (message.captureId.length > MAX_SOURCE_ID_LENGTH) return;
		if (!VALID_LIFECYCLE_KINDS.has(message.kind)) return;
		const binding = this.bindingsByCapture.get(message.captureId);
		if (!binding) return;
		if (message.kind === 'diagnostic') {
			this.reportDiagnostic(binding, message);
			return;
		}
		if (message.kind === 'closed-clean') {
			this.notifyClosedClean(binding, message);
			return;
		}
		const lifecycleEvent = translateLifecycleEvent(message.kind);
		if (!lifecycleEvent) return;
		const result = this.registry.dispatch(binding.sourceId, lifecycleEvent);
		if (!result.ok) return;
		const event: VoiceEngineV2SourceLifecycleTransitionedEvent = {
			type: 'sourceLifecycle.transitioned',
			sourceId: binding.sourceId,
			kind: result.state.kind,
			since: result.state.since,
			attempts: deriveAttempts(result.state),
			fault: deriveFault(result.state),
			atMs: this.now(),
		};
		this.dispatch(event);
	}

	private notifyClosedClean(binding: RegisteredBinding, message: NativeScreenCaptureLifecycleMessage): void {
		assert.equal(message.kind, 'closed-clean', 'notifyClosedClean expects closed-clean kind');
		assert.ok(binding.captureId.length > 0, 'closed-clean binding must have captureId');
		const nowMs = this.now();
		const lastMs = this.closedCleanLastAtMs.get(binding.captureId);
		const withinWindow = lastMs !== undefined && nowMs - lastMs < CLOSED_CLEAN_DEDUP_WINDOW_MS;
		if (withinWindow) return;
		this.closedCleanLastAtMs.set(binding.captureId, nowMs);
		if (!this.onClosedClean) return;
		const source: 'delegate' | 'programmatic' | 'unknown' =
			message.source === 'delegate' || message.source === 'programmatic' ? message.source : 'unknown';
		this.onClosedClean({
			captureId: binding.captureId,
			sourceId: binding.sourceId,
			source,
			atMs: nowMs,
		});
	}

	private reportDiagnostic(binding: RegisteredBinding, message: NativeScreenCaptureLifecycleMessage): void {
		assert.equal(message.kind, 'diagnostic', 'reportDiagnostic expects diagnostic kind');
		assert.ok(binding.captureId.length > 0, 'diagnostic binding must have captureId');
		assert.ok(binding.sourceId.length > 0, 'diagnostic binding must have sourceId');
		const detail = {
			captureId: binding.captureId,
			sourceId: binding.sourceId,
			source: typeof message.source === 'string' ? message.source : 'unknown',
		};
		const diagnosticMessage =
			typeof message.message === 'string' && message.message.length > 0 ? message.message : 'native capture diagnostic';
		void this.diagnostics.log('info', 'nativeCaptureDiagnostic', diagnosticMessage, detail).catch(() => undefined);
	}
}

function isRecord(value: unknown): value is Record<string, unknown> {
	return value !== null && typeof value === 'object' && !Array.isArray(value);
}
