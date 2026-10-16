// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomUuid} from '@app/features/platform/utils/RandomUuid';

export const APP_STORAGE_BROADCAST_CHANNEL_NAME = 'fluxer:app-storage';
export const APP_STORAGE_SCOPE_LOCK_NAME = 'fluxer:app-storage:scope';
export const APP_STORAGE_MIGRATION_LOCK_NAME = 'fluxer:app-storage:migration';

export const AppStorageBroadcastKind = Object.freeze({
	COMMIT: 'commit',
	SCOPE_CLEARED: 'scope-cleared',
	RESET: 'reset',
} as const);

export type AppStorageBroadcastKind = (typeof AppStorageBroadcastKind)[keyof typeof AppStorageBroadcastKind];

export interface AppStorageBroadcastMessage {
	readonly clientId: string;
	readonly kind: AppStorageBroadcastKind;
	readonly scope: string;
	readonly key: string | null;
	readonly generation: number;
}

export interface AppStorageBroadcastOptions {
	readonly onCommit: (scope: string, key: string, generation: number) => void;
	readonly onScopeCleared: (scope: string) => void;
	readonly onReset: () => void;
}

interface LockManagerLike {
	request<T>(name: string, callback: () => Promise<T>): Promise<T>;
}

export class AppStorageLockUnavailableError extends Error {
	public constructor(name: string, options?: ErrorOptions) {
		super(`Web Locks could not serialise app storage operation ${name}`, options);
		this.name = 'AppStorageLockUnavailableError';
	}
}

function resolveLockManager(): LockManagerLike | null {
	const locks = (globalThis.navigator as {locks?: LockManagerLike | null} | undefined)?.locks;
	if (locks == null || typeof locks.request !== 'function') {
		return null;
	}
	return locks;
}

export async function withAppStorageLock<T>(name: string, operation: () => Promise<T>): Promise<T> {
	const locks = resolveLockManager();
	if (locks === null) {
		return operation();
	}
	let started = false;
	try {
		return await locks.request(name, () => {
			started = true;
			return operation();
		});
	} catch (error) {
		if (started) {
			throw error;
		}
		throw new AppStorageLockUnavailableError(name, {cause: error});
	}
}

function isBroadcastRecord(value: unknown): value is Record<string, unknown> {
	return typeof value === 'object' && value !== null && !Array.isArray(value);
}

export class AppStorageBroadcast {
	private readonly clientId = randomUuid();
	private channel: BroadcastChannel | null = null;
	private channelUnavailable = false;

	public constructor(private readonly options: AppStorageBroadcastOptions) {}

	public open(): void {
		this.resolveChannel();
	}

	public close(): void {
		if (this.channel === null) {
			return;
		}
		try {
			this.channel.close();
		} catch (error) {
			console.warn('[AppStorage] Failed to close the broadcast channel', error);
		}
		this.channel = null;
	}

	public publishCommit(scope: string, key: string, generation: number): void {
		this.post({kind: AppStorageBroadcastKind.COMMIT, scope, key, generation});
	}

	public publishScopeCleared(scope: string): void {
		this.post({kind: AppStorageBroadcastKind.SCOPE_CLEARED, scope, key: null, generation: 0});
	}

	public publishReset(): void {
		this.post({kind: AppStorageBroadcastKind.RESET, scope: '', key: null, generation: 0});
	}

	private resolveChannel(): BroadcastChannel | null {
		if (this.channel !== null || this.channelUnavailable) {
			return this.channel;
		}
		if (typeof globalThis.BroadcastChannel !== 'function') {
			this.channelUnavailable = true;
			return null;
		}
		try {
			const channel = new BroadcastChannel(APP_STORAGE_BROADCAST_CHANNEL_NAME);
			channel.onmessage = (event: MessageEvent<unknown>) => {
				this.receive(event.data);
			};
			this.channel = channel;
		} catch (error) {
			this.channelUnavailable = true;
			console.warn('[AppStorage] Broadcast channel unavailable, cross-tab sync is disabled', error);
		}
		return this.channel;
	}

	private post(message: Omit<AppStorageBroadcastMessage, 'clientId'>): void {
		const channel = this.resolveChannel();
		if (channel === null) {
			return;
		}
		try {
			channel.postMessage({...message, clientId: this.clientId});
		} catch (error) {
			console.warn('[AppStorage] Failed to publish a storage commit', error);
		}
	}

	private parse(value: unknown): AppStorageBroadcastMessage | null {
		if (!isBroadcastRecord(value)) {
			return null;
		}
		const {clientId, kind, scope, key, generation} = value;
		if (typeof clientId !== 'string' || clientId.length === 0 || clientId === this.clientId) {
			return null;
		}
		if (typeof scope !== 'string' || typeof generation !== 'number' || !Number.isFinite(generation)) {
			return null;
		}
		if (key !== null && typeof key !== 'string') {
			return null;
		}
		if (
			kind !== AppStorageBroadcastKind.COMMIT &&
			kind !== AppStorageBroadcastKind.SCOPE_CLEARED &&
			kind !== AppStorageBroadcastKind.RESET
		) {
			return null;
		}
		return {clientId, kind, scope, key, generation};
	}

	private receive(value: unknown): void {
		const message = this.parse(value);
		if (message === null) {
			return;
		}
		try {
			if (message.kind === AppStorageBroadcastKind.RESET) {
				this.options.onReset();
				return;
			}
			if (message.kind === AppStorageBroadcastKind.SCOPE_CLEARED) {
				this.options.onScopeCleared(message.scope);
				return;
			}
			if (message.key !== null) {
				this.options.onCommit(message.scope, message.key, message.generation);
			}
		} catch (error) {
			console.error('[AppStorage] Failed to apply a cross-tab storage change', error);
		}
	}
}
