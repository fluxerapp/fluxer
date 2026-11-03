// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	ACCOUNT_ENGINE_AVAILABLE_RENDERER_ARG,
	ACCOUNT_ENGINE_CHANNELS,
	ACCOUNT_ENGINE_LIMITS,
	AccountEngineErrorCode,
	type AccountEngineEvent,
	type AccountEngineRendererAPI,
	type AccountEngineRequestMethod,
	type AccountEngineRequests,
} from '@fluxer/desktop_ipc/src/AccountEngineContract';

interface AccountEnginePreloadRenderer {
	invoke: (channel: string, ...args: Array<unknown>) => Promise<unknown>;
	on: (
		channel: string,
		listener: (event: {ports?: ReadonlyArray<MessagePort>}, ...args: Array<unknown>) => void,
	) => void;
}

interface AccountEnginePreloadAPI {
	readonly accountEngine: AccountEngineRendererAPI;
}

interface PendingRequest {
	readonly resolve: (value: unknown) => void;
	readonly reject: (error: AccountEngineBridgedError) => void;
	readonly timer: ReturnType<typeof setTimeout>;
}

const MAX_PENDING_REQUESTS = 512;

interface AccountEngineBridgedError {
	readonly name: 'AccountEngineRequestError';
	readonly code: AccountEngineErrorCode;
	readonly message: string;
}

function requestError(code: AccountEngineErrorCode, message: string): AccountEngineBridgedError {
	return Object.freeze({name: 'AccountEngineRequestError', code, message});
}

function isErrorCode(value: unknown): value is AccountEngineErrorCode {
	return Object.values(AccountEngineErrorCode).includes(value as AccountEngineErrorCode);
}

export function createAccountEnginePreloadAPI(
	renderer: AccountEnginePreloadRenderer,
	argv: ReadonlyArray<string> = process.argv,
): AccountEnginePreloadAPI {
	const isAvailable = argv.includes(ACCOUNT_ENGINE_AVAILABLE_RENDERER_ARG);
	const listeners = new Set<(event: AccountEngineEvent) => void>();
	const pending = new Map<number, PendingRequest>();
	let port: MessagePort | null = null;
	let portWaiters: Array<(port: MessagePort | null) => void> = [];
	let connectRequested = false;
	let nextId = 1;

	const emit = (event: AccountEngineEvent): void => {
		for (const listener of Array.from(listeners)) {
			try {
				listener(event);
			} catch {}
		}
	};

	const failPending = (code: AccountEngineErrorCode, message: string): void => {
		for (const [id, request] of pending) {
			pending.delete(id);
			clearTimeout(request.timer);
			request.reject(requestError(code, message));
		}
	};

	const handlePortMessage = (data: unknown): void => {
		if (typeof data !== 'object' || data === null) {
			return;
		}
		const message = data as Record<string, unknown>;
		if (message.kind === 'event') {
			const event = message.event as AccountEngineEvent | undefined;
			if (event != null && typeof event.type === 'string' && event.type !== 'restarted') {
				emit(event);
			}
			return;
		}
		if (message.kind !== 'response' || typeof message.id !== 'number') {
			return;
		}
		const request = pending.get(message.id);
		if (request === undefined) {
			return;
		}
		pending.delete(message.id);
		clearTimeout(request.timer);
		if (message.ok === true) {
			request.resolve(message.result ?? null);
			return;
		}
		const error = message.error as {code?: unknown; message?: unknown} | undefined;
		request.reject(
			requestError(
				isErrorCode(error?.code) ? error.code : AccountEngineErrorCode.UNAVAILABLE,
				typeof error?.message === 'string' ? error.message : 'The account engine refused the request',
			),
		);
	};

	const settleWaiters = (value: MessagePort | null): void => {
		const waiters = portWaiters;
		portWaiters = [];
		for (const waiter of waiters) {
			waiter(value);
		}
	};

	renderer.on(ACCOUNT_ENGINE_CHANNELS.port, (event) => {
		const next = event.ports?.[0];
		if (next == null) {
			return;
		}
		const replaced = port;
		port = next;
		next.onmessage = (messageEvent) => handlePortMessage(messageEvent.data);
		next.start();
		if (replaced !== null) {
			replaced.onmessage = null;
			replaced.close();
			failPending(AccountEngineErrorCode.UNAVAILABLE, 'The account engine restarted');
			emit({type: 'restarted'});
		}
		settleWaiters(next);
	});

	const awaitPort = (): Promise<MessagePort | null> => {
		if (port !== null) {
			return Promise.resolve(port);
		}
		const waiting = new Promise<MessagePort | null>((resolve) => portWaiters.push(resolve));
		if (!connectRequested) {
			connectRequested = true;
			renderer.invoke(ACCOUNT_ENGINE_CHANNELS.connect).then(
				(connected) => {
					if (connected !== true) {
						connectRequested = false;
						settleWaiters(null);
					}
				},
				() => {
					connectRequested = false;
					settleWaiters(null);
				},
			);
		}
		return waiting;
	};

	const request = async <M extends AccountEngineRequestMethod>(
		method: M,
		params: AccountEngineRequests[M]['params'],
	): Promise<AccountEngineRequests[M]['result']> => {
		if (!isAvailable) {
			throw requestError(AccountEngineErrorCode.UNAVAILABLE, 'The account engine is not available');
		}
		if (pending.size >= MAX_PENDING_REQUESTS) {
			throw requestError(AccountEngineErrorCode.BUSY, 'Too many account engine requests are waiting');
		}
		const target = await awaitPort();
		if (target === null) {
			throw requestError(AccountEngineErrorCode.UNAVAILABLE, 'The account engine is not running');
		}
		const id = nextId++;
		const timeoutMs =
			method === 'beginPromotion' ? ACCOUNT_ENGINE_LIMITS.promotionTimeoutMs : ACCOUNT_ENGINE_LIMITS.requestTimeoutMs;
		return await new Promise((resolve, reject) => {
			const timer = setTimeout(() => {
				pending.delete(id);
				reject(requestError(AccountEngineErrorCode.TIMEOUT, `${method} timed out`));
			}, timeoutMs);
			pending.set(id, {resolve: resolve as (value: unknown) => void, reject, timer});
			try {
				target.postMessage({kind: 'request', id, method, params});
			} catch (error) {
				pending.delete(id);
				clearTimeout(timer);
				reject(
					requestError(
						AccountEngineErrorCode.INVALID_REQUEST,
						error instanceof Error ? error.message : 'The request could not be sent',
					),
				);
			}
		});
	};

	return Object.freeze({
		accountEngine: Object.freeze<AccountEngineRendererAPI>({
			isAvailable,
			request,
			onEvent: (listener) => {
				listeners.add(listener);
				if (isAvailable) {
					void awaitPort();
				}
				return () => {
					listeners.delete(listener);
				};
			},
			provideAccounts: async (accounts) => {
				if (!isAvailable) {
					return false;
				}
				return (await renderer.invoke(ACCOUNT_ENGINE_CHANNELS.accounts, accounts)) === true;
			},
		}),
	});
}
