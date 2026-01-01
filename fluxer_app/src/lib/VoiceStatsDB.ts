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

import {Logger} from '~/lib/Logger';

const logger = new Logger('VoiceStatsDB');

const DB_NAME = 'FluxerVoiceStats';
const DB_VERSION = 1;
const STORE_NAME = 'stats';

interface StatEntry {
	reportId: string;
	bytes: number;
	timestamp: number;
}

const IDB_TIMEOUT = 5000;

class VoiceStatsDB {
	private db: IDBDatabase | null = null;
	private initPromise: Promise<void> | null = null;
	private initFailed = false;

	async init(): Promise<void> {
		if (this.initFailed) return;
		if (this.initPromise) return this.initPromise;

		const openPromise = new Promise<void>((resolve, reject) => {
			if (typeof indexedDB === 'undefined') {
				resolve();
				return;
			}

			const request = indexedDB.open(DB_NAME, DB_VERSION);

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

	async set(reportId: string, bytes: number, timestamp: number): Promise<void> {
		await this.init();
		if (!this.db) return;

		return new Promise((resolve, reject) => {
			const transaction = this.db!.transaction([STORE_NAME], 'readwrite');
			const store = transaction.objectStore(STORE_NAME);
			const entry: StatEntry = {reportId, bytes, timestamp};
			const request = store.put(entry);

			request.onsuccess = () => resolve();
			request.onerror = () => reject(request.error);
		});
	}

	async get(reportId: string): Promise<{bytes: number; timestamp: number} | null> {
		await this.init();
		if (!this.db) return null;

		return new Promise((resolve, reject) => {
			const transaction = this.db!.transaction([STORE_NAME], 'readonly');
			const store = transaction.objectStore(STORE_NAME);
			const request = store.get(reportId);

			request.onsuccess = () => {
				const entry = request.result as StatEntry | undefined;
				if (entry) {
					resolve({bytes: entry.bytes, timestamp: entry.timestamp});
				} else {
					resolve(null);
				}
			};
			request.onerror = () => reject(request.error);
		});
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

	async clearOldEntries(maxAgeMs: number): Promise<void> {
		await this.init();
		if (!this.db) return;

		const cutoffTime = Date.now() - maxAgeMs;

		return new Promise((resolve, reject) => {
			const transaction = this.db!.transaction([STORE_NAME], 'readwrite');
			const store = transaction.objectStore(STORE_NAME);
			const request = store.openCursor();

			request.onsuccess = (event) => {
				const cursor = (event.target as IDBRequest<IDBCursorWithValue>).result;
				if (cursor) {
					const entry = cursor.value as StatEntry;
					if (entry.timestamp < cutoffTime) {
						cursor.delete();
					}
					cursor.continue();
				} else {
					resolve();
				}
			};
			request.onerror = () => reject(request.error);
		});
	}
}

export const voiceStatsDB = new VoiceStatsDB();
