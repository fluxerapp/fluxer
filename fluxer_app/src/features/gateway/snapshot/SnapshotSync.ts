// SPDX-License-Identifier: AGPL-3.0-or-later

import AccountEngine from '@app/features/account_engine/state/AccountEngine';
import type {ReadyPayload} from '@app/features/gateway/events/GatewayReady';
import {InMemorySnapshotStore} from '@app/features/gateway/snapshot/InMemorySnapshotStore';
import type {SnapshotDispatch} from '@app/features/gateway/snapshot/SnapshotDispatchEvent';
import {parseSnapshotEntity, parseSnapshotSingleton} from '@app/features/gateway/snapshot/SnapshotEntities';
import {SnapshotReducer} from '@app/features/gateway/snapshot/SnapshotReducer';
import type {
	StateSnapshotCapture,
	StateSnapshotEntries,
	StateSyncCursor,
} from '@app/features/gateway/snapshot/SnapshotTypes';
import {SnapshotWriter} from '@app/features/gateway/snapshot/SnapshotWriter';
import {Logger} from '@app/features/platform/utils/AppLogger';

interface SnapshotAccount {
	readonly writer: SnapshotWriter;
	readonly reducer: SnapshotReducer;
}

const logger = new Logger('SnapshotSync');

function snapshotSyncError(error: unknown): Error {
	if (error instanceof Error) {
		return error;
	}
	return new Error('Snapshot synchronization failed with a non-error value');
}

class SnapshotSyncOwner {
	private readonly store = new InMemorySnapshotStore();
	private readonly accounts = new Map<string, SnapshotAccount>();

	constructor(private readonly isEnabled: () => boolean) {}

	get enabled(): boolean {
		return this.isEnabled();
	}

	applyReady(storageKey: string, data: ReadyPayload): void {
		if (!this.enabled) {
			return;
		}
		try {
			this.evictOthers(storageKey);
			const account = this.accounts.get(storageKey) ?? this.createAccount(storageKey, new SnapshotReducer());
			account.writer.setCursor(account.reducer.applyReady(account.writer.enqueue, data));
		} catch (error) {
			this.invalidate(storageKey, snapshotSyncError(error));
		}
	}

	applyDispatch(storageKey: string, dispatch: SnapshotDispatch): void {
		const account = this.accounts.get(storageKey);
		if (account === undefined) {
			return;
		}
		try {
			account.reducer.applyDispatch(account.writer.enqueue, dispatch);
		} catch (error) {
			this.invalidate(storageKey, snapshotSyncError(error));
		}
	}

	adoptPromotedSnapshot(storageKey: string, selfUserId: string, capture: StateSnapshotCapture): boolean {
		if (!this.enabled) {
			return false;
		}
		this.evict(storageKey);
		this.evictOthers(storageKey);
		try {
			this.store.load(storageKey, capture);
			const reducer = new SnapshotReducer();
			reducer.importShellState({
				selfUserId,
				shellUserIds: new Set(parseSnapshotEntity(capture.entries, 'user').keys()),
				userSettings: parseSnapshotSingleton(capture.entries, 'user_settings'),
				accountMetadata: parseSnapshotSingleton(capture.entries, 'account_metadata'),
			});
			reducer.loadHeavyMapsFromEntries(capture.entries);
			this.createAccount(storageKey, reducer);
			return true;
		} catch (error) {
			this.invalidate(storageKey, snapshotSyncError(error));
			return false;
		}
	}

	readSnapshot(storageKey: string): Promise<StateSnapshotEntries> | null {
		const entries = this.store.getAll(storageKey);
		return entries == null ? null : Promise.resolve(entries);
	}

	readCursor(storageKey: string): Promise<StateSyncCursor | null> | null {
		if (!this.store.has(storageKey)) {
			return null;
		}
		return Promise.resolve(this.store.readCursor(storageKey));
	}

	captureDemotionSnapshot(storageKey: string): StateSnapshotCapture | null {
		return this.store.capture(storageKey);
	}

	flush(storageKey: string): Promise<void> {
		return this.accounts.get(storageKey)?.writer.flush() ?? Promise.resolve();
	}

	evict(storageKey: string): void {
		const account = this.accounts.get(storageKey);
		if (account !== undefined) {
			account.writer.dispose();
			this.accounts.delete(storageKey);
		}
		this.store.evict(storageKey);
	}

	private evictOthers(storageKey: string): void {
		for (const other of Array.from(this.accounts.keys())) {
			if (other !== storageKey) {
				this.evict(other);
			}
		}
	}

	private createAccount(storageKey: string, reducer: SnapshotReducer): SnapshotAccount {
		const writer = new SnapshotWriter({
			storageKey,
			store: this.store,
			onInvalid: (error) => this.invalidateAccount(storageKey, account, error),
		});
		const account: SnapshotAccount = {writer, reducer};
		this.accounts.set(storageKey, account);
		return account;
	}

	private invalidateAccount(storageKey: string, account: SnapshotAccount, error: Error): void {
		if (this.accounts.get(storageKey) !== account) {
			return;
		}
		this.invalidate(storageKey, error);
	}

	private invalidate(storageKey: string, error: Error): void {
		logger.warn('Invalidating unavailable app-shell snapshot', {storageKey, error});
		this.evict(storageKey);
	}
}

const snapshotSync = new SnapshotSyncOwner(() => AccountEngine.available);

export default snapshotSync;
