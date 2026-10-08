// SPDX-License-Identifier: AGPL-3.0-or-later

import type {InMemorySnapshotStore} from '@app/features/gateway/snapshot/InMemorySnapshotStore';
import type {SnapshotEmit} from '@app/features/gateway/snapshot/SnapshotEntities';
import type {StateSyncCursor} from '@app/features/gateway/snapshot/SnapshotTypes';

export interface SnapshotWriterOptions {
	readonly storageKey: string;
	readonly store: InMemorySnapshotStore;
	readonly onInvalid: (error: Error) => void;
}

function snapshotWriterError(error: unknown): Error {
	if (error instanceof Error) {
		return error;
	}
	return new Error('Snapshot writer failed with a non-error value');
}

export class SnapshotWriter {
	private readonly storageKey: string;
	private readonly store: InMemorySnapshotStore;
	private readonly onInvalid: (error: Error) => void;
	private invalidationError: Error | null = null;
	private disposed = false;

	constructor(options: SnapshotWriterOptions) {
		this.storageKey = options.storageKey;
		this.store = options.store;
		this.onInvalid = options.onInvalid;
	}

	enqueue: SnapshotEmit = (op) => {
		if (this.disposed || this.invalidationError != null) {
			return;
		}
		try {
			this.store.apply(this.storageKey, op);
		} catch (error) {
			this.invalidate(snapshotWriterError(error));
		}
	};

	setCursor(cursor: StateSyncCursor): void {
		if (this.disposed || this.invalidationError != null) {
			return;
		}
		try {
			this.store.writeCursor(this.storageKey, cursor);
		} catch (error) {
			this.invalidate(snapshotWriterError(error));
		}
	}

	flush(): Promise<void> {
		if (this.invalidationError != null) {
			return Promise.reject(this.invalidationError);
		}
		return Promise.resolve();
	}

	dispose(): void {
		this.disposed = true;
	}

	private invalidate(error: Error): void {
		if (this.invalidationError != null) {
			return;
		}
		this.invalidationError = error;
		this.store.evict(this.storageKey);
		this.onInvalid(error);
	}
}
