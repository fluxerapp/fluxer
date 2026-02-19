/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {Logger} from '@app/lib/Logger';
import type {RuntimeConfigSnapshot} from '@app/stores/RuntimeConfigStore';
import type {LimitConfigSnapshot} from '@fluxer/limits/src/LimitTypes';

function createEmptyLimitConfig(): LimitConfigSnapshot {
	return {
		version: 1,
		traitDefinitions: [],
		rules: [],
	};
}

const logger = new Logger('AccountStorage');

const DB_NAME = 'FluxerAccounts';
const DB_VERSION = 2;
const STORE_NAME = 'accounts';

const hasIndexedDb = typeof indexedDB !== 'undefined';
const hasLocalStorage = typeof localStorage !== 'undefined';

export interface UserData {
	username: string;
	discriminator: string;
	email?: string | null;
	avatar?: string | null;
}

export interface StoredAccount {
	userId: string;
	token: string | null;
	userData?: UserData;
	localStorageData: Record<string, string>;
	managedStorageData?: Record<string, string>;
	lastActive: number;
	instance?: RuntimeConfigSnapshot;
	isValid?: boolean;
}

type IdbOpenState = 'idle' | 'opening' | 'open' | 'failed';

const MANAGED_KEY_EXACT: ReadonlySet<string> = new Set([
	'token',
	'userId',

	'runtimeConfig',
	'AccountManager',

	'token',
]);

const MANAGED_KEY_PREFIXES: ReadonlyArray<string> = ['mobx', 'mobx-persist', 'persist', 'fluxer'];

function isManagedKey(key: string): boolean {
	if (!key) {
		return false;
	}

	if (MANAGED_KEY_EXACT.has(key)) {
		return true;
	}

	for (const prefix of MANAGED_KEY_PREFIXES) {
		if (key.startsWith(prefix)) {
			return true;
		}
	}

	return false;
}

function stableNow(): number {
	return Date.now();
}

async function withTimeout<T>(promise: Promise<T>, ms: number, label: string): Promise<T> {
	let timer: NodeJS.Timeout | null = null;

	try {
		const timeout = new Promise<T>((_resolve, reject) => {
			timer = setTimeout(() => reject(new Error(`Timeout: ${label} (${ms}ms)`)), ms);
		});
		return await Promise.race([promise, timeout]);
	} finally {
		if (timer !== null) {
			clearTimeout(timer);
		}
	}
}

class AccountStorage {
	private db: IDBDatabase | null = null;
	private openPromise: Promise<IDBDatabase | null> | null = null;
	private openState: IdbOpenState = 'idle';

	private memoryStore = new Map<string, StoredAccount>();

	private storageSwapTail: Promise<void> = Promise.resolve();

	private enqueueStorageSwap(fn: () => Promise<void>): Promise<void> {
		const run = async (): Promise<void> => {
			await fn();
		};

		const next = this.storageSwapTail.then(run, run);
		this.storageSwapTail = next.then(
			() => undefined,
			() => undefined,
		);
		return next;
	}

	async init(): Promise<void> {
		if (!hasIndexedDb) {
			return;
		}

		if (this.openState === 'open') {
			return;
		}

		if (this.openState === 'opening' && this.openPromise) {
			await this.openPromise;
			return;
		}

		this.openState = 'opening';

		this.openPromise = new Promise<IDBDatabase | null>((resolve, reject) => {
			const request = indexedDB.open(DB_NAME, DB_VERSION);

			request.onerror = () => {
				this.openState = 'failed';
				logger.error('Failed to open IndexedDB', request.error);
				reject(request.error ?? new Error('IndexedDB open error'));
			};

			request.onupgradeneeded = (event) => {
				const database = (event.target as IDBOpenDBRequest).result;

				if (!database.objectStoreNames.contains(STORE_NAME)) {
					database.createObjectStore(STORE_NAME, {keyPath: 'userId'}).createIndex('lastActive', 'lastActive');
					logger.debug('Created IndexedDB object store for accounts');
				}
			};

			request.onsuccess = () => {
				this.db = request.result;
				this.openState = 'open';
				resolve(this.db);
			};
		});

		try {
			await withTimeout(this.openPromise, 5000, 'IndexedDB open');
		} finally {
			this.openPromise = null;
		}
	}

	private async ensureDb(): Promise<void> {
		if (!hasIndexedDb) {
			return;
		}

		if (!this.db) {
			try {
				await this.init();
			} catch (err) {
				logger.warn('IndexedDB init failed; using in-memory fallback', err);
			}
		}
	}

	private captureManagedStorageSnapshot(): Record<string, string> {
		if (!hasLocalStorage) {
			return {};
		}

		const snapshot: Record<string, string> = {};

		for (let i = 0; i < localStorage.length; i++) {
			const key = localStorage.key(i);
			if (!key) {
				continue;
			}

			if (!isManagedKey(key)) {
				continue;
			}

			const value = localStorage.getItem(key);
			if (value != null) {
				snapshot[key] = value;
			}
		}

		return snapshot;
	}

	private async applyManagedStorageSnapshot(snapshot: Record<string, string>): Promise<void> {
		if (!hasLocalStorage) {
			return;
		}

		const existingManagedKeys: Set<string> = new Set();

		for (let i = 0; i < localStorage.length; i++) {
			const key = localStorage.key(i);
			if (!key) {
				continue;
			}

			if (isManagedKey(key)) {
				existingManagedKeys.add(key);
			}
		}

		for (const [key, value] of Object.entries(snapshot)) {
			try {
				localStorage.setItem(key, value);
			} catch (err) {
				logger.warn(`Failed to set managed localStorage key ${key}`, err);
			}
		}

		for (const key of existingManagedKeys) {
			if (snapshot[key] === undefined) {
				try {
					localStorage.removeItem(key);
				} catch (err) {
					logger.warn(`Failed to remove managed localStorage key ${key}`, err);
				}
			}
		}
	}

	private normalizeRecord(record: StoredAccount): StoredAccount {
		const managed = record.managedStorageData ?? record.localStorageData ?? {};
		return {
			...record,
			localStorageData: managed,
			managedStorageData: managed,
		};
	}

	private cloneStorageSnapshot(snapshot: Record<string, string>): Record<string, string> {
		const safe: Record<string, string> = {};

		for (const [key, value] of Object.entries(snapshot)) {
			if (value == null) {
				continue;
			}

			safe[key] = typeof value === 'string' ? value : String(value);
		}

		return safe;
	}

	private cloneUserData(userData?: UserData): UserData | undefined {
		if (!userData) {
			return undefined;
		}

		return {...userData};
	}

	private cloneRuntimeConfig(instance?: RuntimeConfigSnapshot): RuntimeConfigSnapshot | undefined {
		if (!instance) {
			return undefined;
		}

		return {
			apiEndpoint: instance.apiEndpoint,
			apiPublicEndpoint: instance.apiPublicEndpoint,
			gatewayEndpoint: instance.gatewayEndpoint,
			mediaEndpoint: instance.mediaEndpoint,
			staticCdnEndpoint: instance.staticCdnEndpoint,
			marketingEndpoint: instance.marketingEndpoint,
			adminEndpoint: instance.adminEndpoint,
			inviteEndpoint: instance.inviteEndpoint,
			giftEndpoint: instance.giftEndpoint,
			webAppEndpoint: instance.webAppEndpoint,
			gifProvider: instance.gifProvider,
			captchaProvider: instance.captchaProvider,
			hcaptchaSiteKey: instance.hcaptchaSiteKey,
			turnstileSiteKey: instance.turnstileSiteKey,
			apiCodeVersion: instance.apiCodeVersion,
			features: {...instance.features},
			sso: instance.sso,
			publicPushVapidKey: instance.publicPushVapidKey,
			sentryDsn: instance.sentryDsn ?? '',
			limits:
				instance.limits !== undefined && instance.limits !== null
					? JSON.parse(JSON.stringify(instance.limits))
					: createEmptyLimitConfig(),
			relayDirectoryUrl: instance.relayDirectoryUrl ?? null,
		};
	}

	private sanitizeRecord(record: StoredAccount): StoredAccount {
		const managedSnapshot = this.cloneStorageSnapshot(record.managedStorageData ?? record.localStorageData ?? {});

		return {
			userId: record.userId,
			token: record.token,
			userData: this.cloneUserData(record.userData),
			localStorageData: managedSnapshot,
			managedStorageData: managedSnapshot,
			lastActive: record.lastActive,
			instance: this.cloneRuntimeConfig(record.instance),
			isValid: record.isValid,
		};
	}

	private isDataCloneError(error: unknown): boolean {
		if (!error || typeof error !== 'object') {
			return false;
		}

		const name = (error as {name?: unknown}).name;
		return name === 'DataCloneError';
	}

	private isInvalidStateError(error: unknown): boolean {
		if (!error || typeof error !== 'object') {
			return false;
		}

		const name = (error as {name?: unknown}).name;
		return name === 'InvalidStateError';
	}

	async stashAccountData(
		userId: string,
		token: string | null,
		userData?: UserData,
		instance?: RuntimeConfigSnapshot,
	): Promise<void> {
		if (!userId) {
			const error = new Error(`Invalid stashAccountData: missing userId`);
			logger.error('Invalid parameters for stashAccountData', error);
			throw error;
		}

		if (!token) {
			const error = new Error(`Invalid stashAccountData: missing token for ${userId}`);
			logger.error('Invalid parameters for stashAccountData', error);
			throw error;
		}

		await this.ensureDb();

		const managedStorageData = this.captureManagedStorageSnapshot();

		const record: StoredAccount = {
			userId,
			token,
			userData,
			localStorageData: managedStorageData,
			managedStorageData,
			lastActive: stableNow(),
			instance,
		};

		const safeRecord = this.sanitizeRecord(record);

		try {
			if (!this.db) {
				this.memoryStore.set(userId, safeRecord);
				logger.debug(`Stashed account data for ${userId} (memory fallback)`);
				return;
			}

			await withTimeout(
				new Promise<void>((resolve, reject) => {
					const tx = this.db!.transaction([STORE_NAME], 'readwrite');
					const store = tx.objectStore(STORE_NAME);

					const req = store.put(safeRecord);
					req.onsuccess = () => resolve();
					req.onerror = () => reject(req.error ?? new Error('IndexedDB put failed'));
				}),
				5000,
				'IndexedDB put account',
			);

			logger.debug(`Stashed account data for ${userId} (idb)`);
		} catch (err) {
			if (this.isDataCloneError(err)) {
				logger.warn(`DataCloneError while stashing account ${userId}; using memory store`, err);
				this.memoryStore.set(userId, safeRecord);
				logger.debug(`Stashed account data for ${userId} (memory fallback after DataCloneError)`);
				return;
			}

			if (this.isInvalidStateError(err)) {
				logger.warn(`InvalidStateError (database closing) while stashing account ${userId}; using memory store`, err);
				this.memoryStore.set(userId, safeRecord);
				logger.debug(`Stashed account data for ${userId} (memory fallback after InvalidStateError)`);
				return;
			}

			logger.error(`Failed to stash account data for ${userId}`, err);
			throw err;
		}
	}

	async restoreAccountData(userId: string): Promise<StoredAccount | null> {
		if (!userId) {
			return null;
		}

		await this.ensureDb();

		const record = await this.getRecord(userId);
		if (!record) {
			return null;
		}

		const normalized = this.normalizeRecord(record);

		await this.enqueueStorageSwap(async () => {
			await this.applyManagedStorageSnapshot(normalized.localStorageData ?? {});
		});

		await this.updateLastActive(userId);

		logger.debug(`Restored account data for ${userId}`);
		return normalized;
	}

	async getAllAccounts(): Promise<Array<StoredAccount>> {
		await this.ensureDb();

		try {
			if (!this.db) {
				return Array.from(this.memoryStore.values()).map((r) => this.normalizeRecord(r));
			}

			const records = await withTimeout(
				new Promise<Array<StoredAccount>>((resolve, reject) => {
					const tx = this.db!.transaction([STORE_NAME], 'readonly');
					const store = tx.objectStore(STORE_NAME);
					const req = store.getAll();
					req.onsuccess = () => resolve((req.result as Array<StoredAccount>) ?? []);
					req.onerror = () => reject(req.error ?? new Error('IndexedDB getAll failed'));
				}),
				5000,
				'IndexedDB getAll accounts',
			);

			return records.map((r) => this.normalizeRecord(r));
		} catch (err) {
			logger.error('Failed to fetch stored accounts', err);
			return Array.from(this.memoryStore.values()).map((r) => this.normalizeRecord(r));
		}
	}

	async deleteAccount(userId: string): Promise<void> {
		await this.ensureDb();

		if (!userId) {
			return;
		}

		try {
			if (!this.db) {
				this.memoryStore.delete(userId);
				return;
			}

			await withTimeout(
				new Promise<void>((resolve, reject) => {
					const tx = this.db!.transaction([STORE_NAME], 'readwrite');
					const store = tx.objectStore(STORE_NAME);
					const req = store.delete(userId);
					req.onsuccess = () => resolve();
					req.onerror = () => reject(req.error ?? new Error('IndexedDB delete failed'));
				}),
				5000,
				'IndexedDB delete account',
			);

			logger.debug(`Deleted account data for ${userId}`);
		} catch (err) {
			logger.error(`Failed to delete account ${userId}`, err);
			throw err;
		}
	}

	async updateAccountUserData(userId: string, userData: UserData): Promise<void> {
		await this.ensureDb();

		if (!userId) {
			return;
		}

		try {
			const record = await this.getRecord(userId);
			if (!record) {
				return;
			}

			await this.putRecord({...record, userData});
		} catch (err) {
			logger.error(`Failed to update userData for account ${userId}`, err);
		}
	}

	async updateAccountValidity(userId: string, isValid: boolean): Promise<void> {
		await this.ensureDb();

		if (!userId) {
			return;
		}

		try {
			const record = await this.getRecord(userId);
			if (!record) {
				return;
			}

			await this.putRecord({...record, isValid});
		} catch (err) {
			logger.error(`Failed to update validity for account ${userId}`, err);
		}
	}

	private async getRecord(userId: string): Promise<StoredAccount | null> {
		if (!userId) {
			return null;
		}

		if (!this.db) {
			return this.memoryStore.get(userId) ?? null;
		}

		try {
			return await withTimeout(
				new Promise<StoredAccount | null>((resolve, reject) => {
					const tx = this.db!.transaction([STORE_NAME], 'readonly');
					const store = tx.objectStore(STORE_NAME);
					const req = store.get(userId);
					req.onsuccess = () => resolve((req.result as StoredAccount | undefined) ?? null);
					req.onerror = () => reject(req.error ?? new Error('IndexedDB get failed'));
				}),
				5000,
				'IndexedDB get account',
			);
		} catch (err) {
			if (this.isInvalidStateError(err)) {
				logger.warn(`InvalidStateError (database closing) in getRecord for ${userId}; using memory store`, err);
				return this.memoryStore.get(userId) ?? null;
			}

			throw err;
		}
	}

	private async putRecord(record: StoredAccount): Promise<void> {
		const normalized = this.normalizeRecord(record);

		if (!this.db) {
			this.memoryStore.set(record.userId, normalized);
			return;
		}

		try {
			await withTimeout(
				new Promise<void>((resolve, reject) => {
					const tx = this.db!.transaction([STORE_NAME], 'readwrite');
					const store = tx.objectStore(STORE_NAME);
					const req = store.put(normalized);
					req.onsuccess = () => resolve();
					req.onerror = () => reject(req.error ?? new Error('IndexedDB put failed'));
				}),
				5000,
				'IndexedDB put account record',
			);

			this.memoryStore.set(record.userId, normalized);
		} catch (err) {
			if (this.isInvalidStateError(err)) {
				logger.warn(`InvalidStateError (database closing) in putRecord for ${record.userId}; using memory store`, err);
				this.memoryStore.set(record.userId, normalized);
				return;
			}

			throw err;
		}
	}

	private async updateLastActive(userId: string): Promise<void> {
		const record = await this.getRecord(userId);
		if (!record) {
			return;
		}

		await this.putRecord({...record, lastActive: stableNow()});
	}
}

export default new AccountStorage();
