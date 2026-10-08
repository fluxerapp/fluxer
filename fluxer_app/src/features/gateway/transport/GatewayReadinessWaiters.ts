// SPDX-License-Identifier: AGPL-3.0-or-later

export const GATEWAY_FOREGROUND_READY_TIMEOUT_MS = 60_000;

export class GatewayReadyTimeoutError extends Error {
	constructor() {
		super(`Gateway session did not become ready within ${GATEWAY_FOREGROUND_READY_TIMEOUT_MS}ms`);
		this.name = 'GatewayReadyTimeoutError';
	}
}

interface GatewayReadyWaiter {
	readonly generation: number;
	readonly accountKey: string | null;
	readonly promise: Promise<void>;
	readonly resolve: () => void;
	readonly reject: (error: Error) => void;
	timer: number | null;
}

function createGatewayReadyWaiter(generation: number, accountKey: string | null): GatewayReadyWaiter {
	let resolvePromise: (() => void) | null = null;
	let rejectPromise: ((error: Error) => void) | null = null;
	const promise = new Promise<void>((resolve, reject) => {
		resolvePromise = () => resolve();
		rejectPromise = (error) => reject(error);
	});
	if (resolvePromise === null || rejectPromise === null) {
		throw new Error('Gateway readiness promise did not initialize synchronously');
	}
	return {
		generation,
		accountKey,
		promise,
		resolve: resolvePromise,
		reject: rejectPromise,
		timer: null,
	};
}

export type GatewayReadyTimeoutHandler = (
	generation: number,
	accountKey: string | null,
	error: GatewayReadyTimeoutError,
) => void;

export class GatewayReadinessWaiters {
	private waiter: GatewayReadyWaiter | null = null;

	constructor(private readonly handleTimeout: GatewayReadyTimeoutHandler) {}

	wait(generation: number, accountKey: string | null): Promise<void> {
		const active = this.waiter;
		if (active !== null) {
			if (active.generation !== generation || active.accountKey !== accountKey) {
				throw new Error(`Gateway readiness wait belongs to generation ${active.generation}`);
			}
			return active.promise;
		}
		const waiter = createGatewayReadyWaiter(generation, accountKey);
		waiter.timer = window.setTimeout(() => {
			waiter.timer = null;
			this.abandon(waiter);
		}, GATEWAY_FOREGROUND_READY_TIMEOUT_MS);
		this.waiter = waiter;
		return waiter.promise;
	}

	resolve(generation: number): void {
		const waiter = this.waiter;
		if (waiter === null || waiter.generation !== generation) {
			return;
		}
		this.waiter = null;
		this.clearTimer(waiter);
		waiter.resolve();
	}

	rejectAll(error: Error): void {
		const waiter = this.waiter;
		if (waiter === null) {
			return;
		}
		this.waiter = null;
		this.clearTimer(waiter);
		waiter.reject(error);
	}

	private abandon(waiter: GatewayReadyWaiter): void {
		if (this.waiter !== waiter) {
			return;
		}
		this.waiter = null;
		const error = new GatewayReadyTimeoutError();
		this.handleTimeout(waiter.generation, waiter.accountKey, error);
		waiter.reject(error);
	}

	private clearTimer(waiter: GatewayReadyWaiter): void {
		if (waiter.timer !== null) {
			clearTimeout(waiter.timer);
			waiter.timer = null;
		}
	}
}
