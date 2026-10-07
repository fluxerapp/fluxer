// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ReadyPayload} from '@app/features/gateway/events/GatewayReady';
import {DiskBackedSnapshotReducer} from '@app/features/gateway/snapshot/DiskBackedSnapshotReducer';
import {
	assertSnapshotEntriesCapacity,
	InMemorySnapshotStore,
} from '@app/features/gateway/snapshot/InMemorySnapshotStore';
import type {SnapshotDispatch} from '@app/features/gateway/snapshot/SnapshotDispatchEvent';
import {SnapshotReducer} from '@app/features/gateway/snapshot/SnapshotReducer';
import type {
	StateSnapshotCapture,
	StateSnapshotEntries,
	StateSyncCursor,
} from '@app/features/gateway/snapshot/SnapshotTypes';
import {SnapshotWriter} from '@app/features/gateway/snapshot/SnapshotWriter';
import {isDesktopNativeGatewayTransportAvailable} from '@app/features/gateway/transport/GatewayWireTransport';
import {Logger} from '@app/features/platform/utils/AppLogger';

export type {ReadyPayload, SnapshotDispatch};

export type SnapshotAccountMode = 'foreground' | 'background';

interface SnapshotAccount {
	mode: SnapshotAccountMode;
	writer: SnapshotWriter;
	foreground: SnapshotReducer | null;
	background: DiskBackedSnapshotReducer | null;
}

interface PreparedPromotion {
	readonly storageKey: string;
	readonly entries: StateSnapshotEntries;
	readonly reducer: SnapshotReducer;
	readonly account: SnapshotAccount;
	readonly background: DiskBackedSnapshotReducer;
}

interface CommittedPromotion {
	readonly storageKey: string;
	readonly account: SnapshotAccount;
	readonly retiredBackground: DiskBackedSnapshotReducer;
}

const logger = new Logger('SnapshotSync');
const snapshotTransportAvailable = isDesktopNativeGatewayTransportAvailable();

class SnapshotSyncOwner {
	private readonly store = new InMemorySnapshotStore();
	private readonly accounts = new Map<string, SnapshotAccount>();
	private preparedPromotion: PreparedPromotion | null = null;
	private committedPromotion: CommittedPromotion | null = null;

	applyReady(storageKey: string, data: ReadyPayload, mode: SnapshotAccountMode): void {
		if (!snapshotTransportAvailable) {
			return;
		}
		this.clearPreparedPromotion(storageKey);
		try {
			const account = this.ensureAccountForReady(storageKey, mode);
			if (account.mode === 'foreground' && account.foreground != null) {
				const cursor = account.foreground.applyReady(account.writer.enqueue, data);
				account.writer.setCursor(cursor);
				return;
			}
			account.background?.applyReady(data);
		} catch (error) {
			this.invalidateStorageKey(storageKey, snapshotSyncError(error));
		}
	}

	applyDispatch(storageKey: string, dispatch: SnapshotDispatch): void {
		if (!snapshotTransportAvailable) {
			return;
		}
		const account = this.accounts.get(storageKey);
		if (account == null) {
			return;
		}
		this.clearPreparedPromotion(storageKey);
		try {
			if (account.mode === 'foreground' && account.foreground != null) {
				account.foreground.applyDispatch(account.writer.enqueue, dispatch);
			} else {
				account.background?.applyDispatch(dispatch);
			}
		} catch (error) {
			this.invalidateStorageKey(storageKey, snapshotSyncError(error));
		}
	}

	readSnapshot(storageKey: string): Promise<StateSnapshotEntries> | null {
		if (!snapshotTransportAvailable) {
			return null;
		}
		const prepared = this.preparedPromotion;
		if (prepared?.storageKey === storageKey) {
			return Promise.resolve(prepared.entries);
		}
		const entries = this.store.getAll(storageKey);
		return entries == null ? null : Promise.resolve(entries);
	}

	readCursor(storageKey: string): Promise<StateSyncCursor | null> | null {
		if (!snapshotTransportAvailable) {
			return null;
		}
		if (!this.store.has(storageKey)) {
			return null;
		}
		return Promise.resolve(this.store.readCursor(storageKey));
	}

	captureDemotionSnapshot(storageKey: string): StateSnapshotCapture | null {
		if (!snapshotTransportAvailable) {
			return null;
		}
		return this.store.capture(storageKey);
	}

	flush(storageKey: string): Promise<void> {
		if (!snapshotTransportAvailable) {
			return Promise.resolve();
		}
		const account = this.accounts.get(storageKey);
		if (account == null) {
			return Promise.resolve();
		}
		if (account.mode === 'background' && account.background != null) {
			return account.background.flush();
		}
		return account.writer.flush();
	}

	private dispose(storageKey: string): void {
		const account = this.accounts.get(storageKey);
		if (account != null) {
			account.background?.dispose();
			account.writer.dispose();
			this.accounts.delete(storageKey);
		}
		this.clearPreparedPromotion(storageKey);
		this.clearCommittedPromotion(storageKey);
	}

	demoteForeground(storageKey: string): StateSnapshotCapture | null {
		if (!snapshotTransportAvailable) {
			return null;
		}
		const account = this.accounts.get(storageKey);
		if (account?.mode !== 'foreground' || account.foreground == null) {
			return null;
		}
		const capture = this.store.capture(storageKey);
		if (capture == null) {
			return null;
		}
		const driver = this.createBackgroundDriver(storageKey, account);
		driver.setShellState(account.foreground.exportShellState());
		account.foreground = null;
		account.background = driver;
		account.mode = 'background';
		return capture;
	}

	prepareForegroundPromotion(storageKey: string, entries: StateSnapshotEntries): boolean {
		if (!snapshotTransportAvailable) {
			return false;
		}
		if (!this.store.has(storageKey) || this.preparedPromotion !== null || this.committedPromotion !== null) {
			return false;
		}
		const account = this.accounts.get(storageKey);
		const background = account?.mode === 'background' ? account.background : null;
		if (account == null || background == null) {
			return false;
		}
		try {
			assertSnapshotEntriesCapacity(entries);
			const reducer = new SnapshotReducer();
			reducer.loadHeavyMapsFromEntries(entries);
			reducer.importShellState(background.takeShellState());
			this.preparedPromotion = {storageKey, entries, reducer, account, background};
			return true;
		} catch (error) {
			this.invalidateStorageKey(storageKey, snapshotSyncError(error));
			return false;
		}
	}

	commitForegroundPromotion(storageKey: string): boolean {
		if (!snapshotTransportAvailable) {
			return false;
		}
		const prepared = this.preparedPromotion;
		if (prepared?.storageKey !== storageKey) {
			return false;
		}
		if (!this.store.has(storageKey)) {
			this.preparedPromotion = null;
			return false;
		}
		if (
			this.committedPromotion !== null ||
			this.accounts.get(storageKey) !== prepared.account ||
			prepared.account.mode !== 'background' ||
			prepared.account.background !== prepared.background
		) {
			this.preparedPromotion = null;
			return false;
		}
		this.preparedPromotion = null;
		prepared.account.background = null;
		prepared.account.foreground = prepared.reducer;
		prepared.account.mode = 'foreground';
		this.committedPromotion = {
			storageKey,
			account: prepared.account,
			retiredBackground: prepared.background,
		};
		return true;
	}

	abortForegroundPromotion(storageKey: string): void {
		this.clearPreparedPromotion(storageKey);
		const committed = this.committedPromotion;
		if (committed?.storageKey !== storageKey) {
			return;
		}
		this.committedPromotion = null;
		committed.retiredBackground.dispose();
		if (this.accounts.get(storageKey) === committed.account) {
			committed.account.foreground = null;
			committed.account.writer.dispose();
			this.accounts.delete(storageKey);
		}
		this.store.evict(storageKey);
	}

	finalizeForegroundPromotion(storageKey: string): void {
		const committed = this.committedPromotion;
		if (
			committed?.storageKey !== storageKey ||
			this.accounts.get(storageKey) !== committed.account ||
			committed.account.mode !== 'foreground' ||
			committed.account.foreground == null ||
			committed.account.background !== null
		) {
			throw new Error(`Foreground snapshot finalization rejected ${storageKey}`);
		}
		this.committedPromotion = null;
		committed.retiredBackground.dispose();
	}

	evict(storageKey: string): void {
		this.dispose(storageKey);
		this.store.evict(storageKey);
	}

	prune(knownStorageKeys: ReadonlyArray<string>): void {
		for (const storageKey of Array.from(this.accounts.keys())) {
			if (!knownStorageKeys.includes(storageKey)) {
				this.dispose(storageKey);
			}
		}
		const preparedStorageKey = this.preparedPromotion?.storageKey;
		if (preparedStorageKey != null && !knownStorageKeys.includes(preparedStorageKey)) {
			this.preparedPromotion = null;
		}
		const committedStorageKey = this.committedPromotion?.storageKey;
		if (committedStorageKey != null && !knownStorageKeys.includes(committedStorageKey)) {
			this.clearCommittedPromotion(committedStorageKey);
		}
		this.store.prune(knownStorageKeys);
	}

	private ensureAccountForReady(storageKey: string, mode: SnapshotAccountMode): SnapshotAccount {
		const existing = this.accounts.get(storageKey);
		if (existing != null) {
			if (existing.mode !== mode) {
				this.flipAccountMode(storageKey, existing, mode);
			}
			return existing;
		}
		const account = this.createAccount(storageKey, mode, null);
		this.accounts.set(storageKey, account);
		return account;
	}

	private createAccount(
		storageKey: string,
		mode: SnapshotAccountMode,
		foregroundSeed: SnapshotReducer | null,
	): SnapshotAccount {
		let account: SnapshotAccount;
		const onInvalid = (error: Error): void => this.invalidateAccount(storageKey, account, error);
		const writer = new SnapshotWriter({storageKey, store: this.store, onInvalid});
		account = {
			mode,
			writer,
			foreground: mode === 'foreground' ? (foregroundSeed ?? new SnapshotReducer()) : null,
			background: null,
		};
		if (mode === 'background') {
			account.background = this.createBackgroundDriver(storageKey, account);
		}
		return account;
	}

	private createBackgroundDriver(storageKey: string, account: SnapshotAccount): DiskBackedSnapshotReducer {
		return new DiskBackedSnapshotReducer({
			storageKey,
			store: this.store,
			writer: account.writer,
			onInvalid: (error) => this.invalidateAccount(storageKey, account, error),
		});
	}

	private flipAccountMode(storageKey: string, account: SnapshotAccount, mode: SnapshotAccountMode): void {
		if (mode === 'foreground') {
			account.background?.dispose();
			account.background = null;
			account.foreground = new SnapshotReducer();
		} else {
			account.foreground = null;
			account.background = this.createBackgroundDriver(storageKey, account);
		}
		account.mode = mode;
	}

	private clearPreparedPromotion(storageKey: string): void {
		if (this.preparedPromotion?.storageKey === storageKey) {
			this.preparedPromotion = null;
		}
	}

	private clearCommittedPromotion(storageKey: string): void {
		if (this.committedPromotion?.storageKey === storageKey) {
			this.committedPromotion.retiredBackground.dispose();
			this.committedPromotion = null;
		}
	}

	private invalidateAccount(storageKey: string, account: SnapshotAccount, error: Error): void {
		if (this.accounts.get(storageKey) !== account) {
			return;
		}
		logger.warn('Invalidating unavailable app-shell snapshot', {storageKey, error});
		account.background?.dispose();
		account.writer.dispose();
		this.accounts.delete(storageKey);
		this.clearPreparedPromotion(storageKey);
		this.clearCommittedPromotion(storageKey);
		this.store.evict(storageKey);
	}

	private invalidateStorageKey(storageKey: string, error: Error): void {
		const account = this.accounts.get(storageKey);
		if (account != null) {
			this.invalidateAccount(storageKey, account, error);
			return;
		}
		logger.warn('Invalidating unavailable app-shell snapshot', {storageKey, error});
		this.clearPreparedPromotion(storageKey);
		this.clearCommittedPromotion(storageKey);
		this.store.evict(storageKey);
	}
}

function snapshotSyncError(error: unknown): Error {
	if (error instanceof Error) {
		return error;
	}
	return new Error('Snapshot synchronization failed with a non-error value');
}

const snapshotSync = new SnapshotSyncOwner();

export function isDesktopSnapshotAvailable(): boolean {
	return snapshotTransportAvailable;
}

export async function disposeAndEvictDesktopSnapshot(storageKey: string): Promise<void> {
	snapshotSync.evict(storageKey);
}

export async function pruneOrphanedDesktopStateStores(knownStorageKeys: ReadonlyArray<string>): Promise<void> {
	snapshotSync.prune(knownStorageKeys);
}

export default snapshotSync;
