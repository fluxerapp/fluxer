// SPDX-License-Identifier: AGPL-3.0-or-later

import {getProtectedLocalStorage} from '@app/features/platform/state/ProtectedWebStorage';
import {Logger} from '@app/features/platform/utils/AppLogger';

const logger = new Logger('AccountStorage');

export const BROWSER_ACCOUNT_FALLBACK_STORAGE_KEY = 'fluxer:accounts:fallback';

function recordUserId(value: unknown): string | null {
	if (value === null || typeof value !== 'object') {
		return null;
	}
	const userId = (value as {userId?: unknown}).userId;
	return typeof userId === 'string' && userId.length > 0 ? userId : null;
}

export class BrowserAccountFallbackStore {
	private readonly records = new Map<string, unknown>();

	constructor(private readonly storage: Storage | null = getProtectedLocalStorage()) {
		for (const value of this.readPersisted()) {
			const userId = recordUserId(value);
			if (userId !== null) {
				this.records.set(userId, value);
			}
		}
	}

	list(): Array<unknown> {
		return [...this.records.values()];
	}

	get(userId: string): unknown | null {
		return this.records.get(userId) ?? null;
	}

	put(value: unknown): void {
		const userId = recordUserId(value);
		if (userId === null) {
			throw new Error('Fallback account records need a userId');
		}
		this.records.set(userId, value);
		this.persist();
	}

	putMany(values: ReadonlyArray<unknown>): void {
		for (const value of values) {
			const userId = recordUserId(value);
			if (userId === null) {
				throw new Error('Fallback account records need a userId');
			}
			this.records.set(userId, value);
		}
		this.persist();
	}

	delete(userId: string): void {
		if (this.records.delete(userId)) {
			this.persist();
		}
	}

	private readPersisted(): Array<unknown> {
		if (this.storage === null) {
			return [];
		}
		try {
			const raw = this.storage.getItem(BROWSER_ACCOUNT_FALLBACK_STORAGE_KEY);
			if (raw === null) {
				return [];
			}
			const parsed: unknown = JSON.parse(raw);
			return Array.isArray(parsed) ? parsed : [];
		} catch (error) {
			logger.warn('Could not read the fallback account copy, starting with no accounts', error);
			return [];
		}
	}

	private persist(): void {
		if (this.storage === null) {
			return;
		}
		try {
			if (this.records.size === 0) {
				this.storage.removeItem(BROWSER_ACCOUNT_FALLBACK_STORAGE_KEY);
				return;
			}
			this.storage.setItem(BROWSER_ACCOUNT_FALLBACK_STORAGE_KEY, JSON.stringify([...this.records.values()]));
		} catch (error) {
			logger.warn('Could not persist the fallback account copy, so accounts stay in memory for this tab', error);
		}
	}
}
