// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import type {VoiceEngineV2AppDiagnosticsLogger} from '@app/features/voice/engine/v2/VoiceEngineV2AppDiagnosticsAdapter';
import type {VoiceEngineV2ClockPort} from '@fluxer/voice_engine_v2/runtime';

export const LIFECYCLE_OPERATION_CAP = 4096;
export const TEARDOWN_PER_DISPOSABLE_TIMEOUT_MS = 5000;

type VoiceEngineV2AppLifecycleTimeoutHandle = number | NodeJS.Timeout;

export interface VoiceEngineV2AppLifecycleDisposable {
	readonly name: string;
	dispose(): Promise<void>;
}

export interface VoiceEngineV2AppLifecycleRegistration {
	readonly abort: AbortController;
	readonly sourceAdapter: string;
}

export interface VoiceEngineV2AppLifecycleAdapterOptions {
	readonly disposables: ReadonlyArray<VoiceEngineV2AppLifecycleDisposable>;
	readonly logger: VoiceEngineV2AppDiagnosticsLogger;
	readonly clock: VoiceEngineV2ClockPort;
	readonly teardownPerDisposableTimeoutMs?: number;
}

function buildOperatingError(method: string, code: string, reason: string): Error {
	const error = new Error(`VoiceEngineV2AppLifecycleAdapter.${method}: ${reason}`);
	error.name = 'VoiceEngineV2AppLifecycleOperatingError';
	(error as Error & {code?: string}).code = code;
	return error;
}

function isPositiveInteger(value: unknown): value is number {
	if (typeof value !== 'number') return false;
	if (!Number.isFinite(value)) return false;
	if (!Number.isInteger(value)) return false;
	return value > 0;
}

function isNonEmptyString(value: unknown): value is string {
	return typeof value === 'string' && value.length > 0;
}

export class VoiceEngineV2AppLifecycleAdapter {
	private readonly disposables: ReadonlyArray<VoiceEngineV2AppLifecycleDisposable>;
	private readonly logger: VoiceEngineV2AppDiagnosticsLogger;
	private readonly clock: VoiceEngineV2ClockPort;
	private readonly registry: Map<number, VoiceEngineV2AppLifecycleRegistration>;
	private readonly teardownTimeoutMs: number;
	private tornDown: boolean;

	constructor(options: VoiceEngineV2AppLifecycleAdapterOptions) {
		assert.ok(options !== null && typeof options === 'object', 'lifecycle adapter requires options');
		assert.ok(Array.isArray(options.disposables), 'lifecycle adapter disposables must be an array');
		assert.ok(options.disposables.length <= LIFECYCLE_OPERATION_CAP, 'lifecycle adapter disposable count exceeds cap');
		assert.ok(options.logger !== null && typeof options.logger === 'object', 'lifecycle adapter requires a logger');
		assert.ok(options.clock !== null && typeof options.clock === 'object', 'lifecycle adapter requires a clock');
		assert.equal(typeof options.clock.now, 'function', 'lifecycle adapter clock must implement now()');
		const timeoutMs = options.teardownPerDisposableTimeoutMs ?? TEARDOWN_PER_DISPOSABLE_TIMEOUT_MS;
		assert.ok(isPositiveInteger(timeoutMs), 'lifecycle adapter teardown timeout must be a positive integer');
		for (const disposable of options.disposables) {
			assert.ok(
				disposable !== null && typeof disposable === 'object',
				'lifecycle adapter disposable entry must be an object',
			);
			assert.ok(isNonEmptyString(disposable.name), 'lifecycle adapter disposable must have a non-empty name');
			assert.equal(typeof disposable.dispose, 'function', 'lifecycle adapter disposable must implement dispose()');
		}
		this.disposables = [...options.disposables];
		this.logger = options.logger;
		this.clock = options.clock;
		this.registry = new Map();
		this.teardownTimeoutMs = timeoutMs;
		this.tornDown = false;
	}

	get isTornDown(): boolean {
		return this.tornDown;
	}

	get registrySize(): number {
		return this.registry.size;
	}

	register(operationId: number, controller: AbortController, sourceAdapter: string): void {
		assert.ok(isPositiveInteger(operationId), 'lifecycle register operationId must be a positive integer');
		assert.ok(
			controller !== null && typeof controller === 'object',
			'lifecycle register controller must be an AbortController',
		);
		assert.equal(typeof controller.abort, 'function', 'lifecycle register controller must implement abort()');
		assert.ok(isNonEmptyString(sourceAdapter), 'lifecycle register sourceAdapter must be a non-empty string');
		if (this.tornDown) {
			throw buildOperatingError('register', 'lifecycleTornDown', 'cannot register after teardown');
		}
		if (this.registry.has(operationId)) {
			throw buildOperatingError(
				'register',
				'lifecycleOperationAlreadyRegistered',
				`operationId ${operationId} already registered`,
			);
		}
		if (this.registry.size >= LIFECYCLE_OPERATION_CAP) {
			throw buildOperatingError(
				'register',
				'lifecycleRegistryFull',
				`registry exceeded cap ${LIFECYCLE_OPERATION_CAP}`,
			);
		}
		this.registry.set(operationId, {abort: controller, sourceAdapter});
	}

	unregister(operationId: number): boolean {
		assert.ok(isPositiveInteger(operationId), 'lifecycle unregister operationId must be a positive integer');
		assert.ok(this.registry.size <= LIFECYCLE_OPERATION_CAP, 'lifecycle registry overflow before unregister');
		return this.registry.delete(operationId);
	}

	async cancelOperation(operationId: number, reason: string): Promise<void> {
		assert.ok(isPositiveInteger(operationId), 'lifecycle cancelOperation operationId must be a positive integer');
		assert.ok(isNonEmptyString(reason), 'lifecycle cancelOperation reason must be a non-empty string');
		const entry = this.registry.get(operationId);
		if (entry === undefined) {
			this.logger.debug({
				code: 'lifecycle.cancel.unknown',
				message: 'lifecycle cancelOperation called with unknown operationId',
				detail: {operationId, reason, registrySize: this.registry.size, tornDown: this.tornDown},
			});
			return;
		}
		this.registry.delete(operationId);
		try {
			entry.abort.abort(reason);
		} catch (error) {
			this.logger.error({
				code: 'lifecycle.cancel.abort_failed',
				message: 'lifecycle cancelOperation abort threw',
				detail: {operationId, reason, sourceAdapter: entry.sourceAdapter, error: String(error)},
			});
			return;
		}
		this.logger.info({
			code: 'lifecycle.cancelled',
			message: 'lifecycle operation cancelled',
			detail: {operationId, reason, sourceAdapter: entry.sourceAdapter},
		});
	}

	async teardown(): Promise<void> {
		assert.ok(this.disposables.length <= LIFECYCLE_OPERATION_CAP, 'lifecycle teardown disposable count exceeds cap');
		assert.ok(typeof this.clock.now === 'function', 'lifecycle teardown clock invariant violated');
		if (this.tornDown) {
			this.logger.debug({
				code: 'lifecycle.teardown.repeated',
				message: 'lifecycle teardown invoked after completion',
				detail: {disposables: this.disposables.length},
			});
			return;
		}
		this.tornDown = true;
		this.registry.clear();
		for (let index = this.disposables.length - 1; index >= 0; index -= 1) {
			const disposable = this.disposables[index];
			if (disposable === undefined) {
				continue;
			}
			await this.disposeWithTimeout(disposable);
		}
		this.logger.info({
			code: 'lifecycle.teardown.complete',
			message: 'lifecycle teardown finished',
			detail: {disposables: this.disposables.length},
		});
	}

	private async disposeWithTimeout(disposable: VoiceEngineV2AppLifecycleDisposable): Promise<void> {
		assert.ok(disposable !== null && typeof disposable === 'object', 'disposeWithTimeout requires a disposable');
		assert.equal(typeof disposable.dispose, 'function', 'disposeWithTimeout requires dispose() method');
		const startedAt = this.clock.now();
		const budgetMs = this.teardownTimeoutMs;
		let timeoutHandle: VoiceEngineV2AppLifecycleTimeoutHandle | undefined;
		const timeoutPromise = new Promise<'timeout'>((resolve) => {
			timeoutHandle = globalThis.setTimeout(() => {
				resolve('timeout');
			}, budgetMs);
		});
		try {
			const disposalPromise = Promise.resolve()
				.then(async () => {
					await disposable.dispose();
					return 'ok' as const;
				})
				.catch((error: unknown) => {
					return {error};
				});
			const outcome = await Promise.race([disposalPromise, timeoutPromise]);
			if (outcome === 'timeout') {
				const elapsedMs = this.clock.now() - startedAt;
				this.logger.error({
					code: 'lifecycle.teardown.timeout',
					message: 'lifecycle disposable exceeded per-disposable timeout',
					detail: {name: disposable.name, budgetMs, elapsedMs},
				});
				return;
			}
			if (outcome === 'ok') {
				return;
			}
			this.logger.error({
				code: 'lifecycle.teardown.error',
				message: 'lifecycle disposable threw during dispose',
				detail: {name: disposable.name, error: String((outcome as {error: unknown}).error)},
			});
		} finally {
			if (timeoutHandle !== undefined) {
				globalThis.clearTimeout(timeoutHandle);
			}
		}
	}
}

export function createVoiceEngineV2AppLifecycleAdapter(
	options: VoiceEngineV2AppLifecycleAdapterOptions,
): VoiceEngineV2AppLifecycleAdapter {
	return new VoiceEngineV2AppLifecycleAdapter(options);
}
