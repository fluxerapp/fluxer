// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AppStorageWrite, PersistentStorageBackend} from '@app/features/platform/state/PersistentStorageBackend';

export const APP_STORAGE_WRITE_BATCH_SIZE = 256;

export interface PersistentStorageWriteRequest {
	readonly scope: string;
	readonly key: string;
	readonly value: string | null;
	readonly ifAbsent?: boolean;
}

export interface PersistentStorageWriteQueueOptions {
	readonly resolveBackend: () => PersistentStorageBackend;
	readonly onCommitted: (write: PersistentStorageWriteRequest, generation: number) => void;
	readonly onFailed: (write: PersistentStorageWriteRequest, error: unknown) => void;
}

interface PendingStorageWrite extends PersistentStorageWriteRequest {
	readonly generation: number;
}

interface StorageWriteGeneration {
	readonly scope: string;
	readonly key: string;
	readonly generation: number;
}

export function persistentStorageWriteIdentity(scope: string, key: string): string {
	return `${scope.length}:${scope}${key}`;
}

function toBackendWrite(write: PendingStorageWrite, value: string): AppStorageWrite {
	if (write.ifAbsent === true) {
		return {scope: write.scope, key: write.key, value, ifAbsent: true};
	}
	return {scope: write.scope, key: write.key, value};
}

export class PersistentStorageWriteQueue {
	private readonly pendingWrites = new Map<string, PendingStorageWrite>();
	private readonly inFlightWrites = new Map<string, PendingStorageWrite>();
	private readonly failedIdentities = new Set<string>();
	private readonly writeGenerations = new Map<string, StorageWriteGeneration>();
	private drainPromise: Promise<void> | null = null;

	public constructor(private readonly options: PersistentStorageWriteQueueOptions) {}

	public enqueue(requests: ReadonlyArray<PersistentStorageWriteRequest>): void {
		if (requests.length === 0) {
			return;
		}
		for (const request of requests) {
			const identity = persistentStorageWriteIdentity(request.scope, request.key);
			this.failedIdentities.delete(identity);
			const generation = this.advanceGeneration(request.scope, request.key);
			this.pendingWrites.delete(identity);
			this.pendingWrites.set(identity, {...request, generation});
		}
		this.ensureDrain();
	}

	public generation(scope: string, key: string): number {
		return this.writeGenerations.get(persistentStorageWriteIdentity(scope, key))?.generation ?? 0;
	}

	public advanceGeneration(scope: string, key: string): number {
		const identity = persistentStorageWriteIdentity(scope, key);
		const generation = (this.writeGenerations.get(identity)?.generation ?? 0) + 1;
		this.writeGenerations.set(identity, {scope, key, generation});
		return generation;
	}

	public hasOutstanding(scope: string, key: string): boolean {
		const identity = persistentStorageWriteIdentity(scope, key);
		return this.pendingWrites.has(identity) || this.inFlightWrites.has(identity);
	}

	public hasFailure(scope: string, key: string): boolean {
		return this.failedIdentities.has(persistentStorageWriteIdentity(scope, key));
	}

	public invalidateScope(scope: string): void {
		for (const [identity, write] of [...this.pendingWrites]) {
			if (write.scope === scope) {
				this.pendingWrites.delete(identity);
			}
		}
		for (const generation of [...this.writeGenerations.values()]) {
			if (generation.scope === scope) {
				this.advanceGeneration(generation.scope, generation.key);
			}
		}
		for (const identity of [...this.failedIdentities]) {
			if (this.writeGenerations.get(identity)?.scope === scope) {
				this.failedIdentities.delete(identity);
			}
		}
	}

	public invalidateAll(): void {
		this.pendingWrites.clear();
		this.failedIdentities.clear();
		for (const generation of [...this.writeGenerations.values()]) {
			this.advanceGeneration(generation.scope, generation.key);
		}
	}

	public async flush(): Promise<void> {
		while (this.drainPromise != null || this.pendingWrites.size > 0) {
			this.ensureDrain();
			await this.drainPromise;
		}
	}

	private takeBatch(): Array<PendingStorageWrite> {
		const batch: Array<PendingStorageWrite> = [];
		for (const [identity, write] of this.pendingWrites) {
			this.pendingWrites.delete(identity);
			this.inFlightWrites.set(identity, write);
			batch.push(write);
			if (batch.length === APP_STORAGE_WRITE_BATCH_SIZE) {
				break;
			}
		}
		return batch;
	}

	private settleWrite(write: PendingStorageWrite, error: unknown): void {
		const identity = persistentStorageWriteIdentity(write.scope, write.key);
		if (this.inFlightWrites.get(identity) === write) {
			this.inFlightWrites.delete(identity);
		}
		if (this.writeGenerations.get(identity)?.generation !== write.generation) {
			return;
		}
		if (error != null) {
			this.failedIdentities.add(identity);
			this.options.onFailed(write, error);
			return;
		}
		this.options.onCommitted(write, write.generation);
	}

	private async commitStoredValues(
		backend: PersistentStorageBackend,
		writes: ReadonlyArray<PendingStorageWrite>,
	): Promise<void> {
		if (writes.length === 0) {
			return;
		}
		try {
			await backend.setMany(writes.map((write) => toBackendWrite(write, write.value ?? '')));
		} catch (error) {
			for (const write of writes) {
				this.settleWrite(write, error);
			}
			return;
		}
		for (const write of writes) {
			this.settleWrite(write, null);
		}
	}

	private async commitRemovals(
		backend: PersistentStorageBackend,
		writes: ReadonlyArray<PendingStorageWrite>,
	): Promise<void> {
		for (const write of writes) {
			try {
				await backend.delete(write.scope, write.key);
				this.settleWrite(write, null);
			} catch (error) {
				this.settleWrite(write, error);
			}
		}
	}

	private async commitBatch(batch: ReadonlyArray<PendingStorageWrite>): Promise<void> {
		let backend: PersistentStorageBackend;
		try {
			backend = this.options.resolveBackend();
		} catch (error) {
			for (const write of batch) {
				this.settleWrite(write, error);
			}
			return;
		}
		const storedValues = batch.filter((write) => write.value != null);
		const removals = batch.filter((write) => write.value == null);
		await this.commitStoredValues(backend, storedValues);
		await this.commitRemovals(backend, removals);
	}

	private async drain(): Promise<void> {
		while (this.pendingWrites.size > 0) {
			await this.commitBatch(this.takeBatch());
		}
	}

	private ensureDrain(): void {
		if (this.drainPromise != null) {
			return;
		}
		const running = this.drain()
			.catch((error: unknown) => {
				for (const [identity, write] of [...this.inFlightWrites]) {
					this.inFlightWrites.delete(identity);
					this.settleWrite(write, error);
				}
			})
			.finally(() => {
				if (this.drainPromise === running) {
					this.drainPromise = null;
				}
				if (this.pendingWrites.size > 0) {
					this.ensureDrain();
				}
			});
		this.drainPromise = running;
	}
}
