// SPDX-License-Identifier: AGPL-3.0-or-later

import {getProtectedIndexedDB} from '@app/features/platform/state/ProtectedWebStorage';
import {Logger} from '@app/features/platform/utils/AppLogger';

const logger = new Logger('VoiceStatsDB');
const DB_NAME = 'FluxerVoiceStats';
const DB_VERSION = 1;
const STORE_NAME = 'stats';
const browserIndexedDB = getProtectedIndexedDB();

const IDB_TIMEOUT = 5000;

class VoiceStatsDB {
	private db: IDBDatabase | null = null;
	private initPromise: Promise<void> | null = null;
	private initFailed = false;

	async init(): Promise<void> {
		if (this.initFailed) return;
		if (this.initPromise) return this.initPromise;
		const openPromise = new Promise<void>((resolve, reject) => {
			if (!browserIndexedDB) {
				resolve();
				return;
			}
			const request = browserIndexedDB.open(DB_NAME, DB_VERSION);
			request.onerror = () => {
				logger.error('Failed to open IndexedDB', request.error);
				reject(request.error);
			};
			request.onblocked = () => {
				logger.warn('VoiceStatsDB: IndexedDB blocked');
				reject(new Error('IndexedDB blocked'));
			};
			request.onsuccess = () => {
				this.db = request.result;
				resolve();
			};
			request.onupgradeneeded = (event) => {
				const db = (event.target as IDBOpenDBRequest).result;
				if (!db.objectStoreNames.contains(STORE_NAME)) {
					db.createObjectStore(STORE_NAME, {keyPath: 'reportId'});
				}
			};
		});
		const timeoutPromise = new Promise<void>((_, reject) => {
			setTimeout(() => reject(new Error('VoiceStatsDB init timeout')), IDB_TIMEOUT);
		});
		this.initPromise = Promise.race([openPromise, timeoutPromise]).catch((err) => {
			logger.warn('VoiceStatsDB init failed, will use no-op mode', err);
			this.initFailed = true;
		});
		return this.initPromise;
	}

	async clear(): Promise<void> {
		await this.init();
		if (!this.db) return;
		return new Promise((resolve, reject) => {
			const transaction = this.db!.transaction([STORE_NAME], 'readwrite');
			const store = transaction.objectStore(STORE_NAME);
			const request = store.clear();
			request.onsuccess = () => resolve();
			request.onerror = () => reject(request.error);
		});
	}
}

export const voiceStatsDB = new VoiceStatsDB();
