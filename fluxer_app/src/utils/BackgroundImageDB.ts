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

const DB_NAME = 'FluxerBackgroundImages';
const DB_VERSION = 1;
const STORE_NAME = 'backgroundImages';

interface BackgroundImageData {
	id: string;
	blob: Blob;
	createdAt: number;
}

let dbInstance: IDBDatabase | null = null;

const openDB = (): Promise<IDBDatabase> => {
	return new Promise((resolve, reject) => {
		if (dbInstance) {
			resolve(dbInstance);
			return;
		}

		const request = indexedDB.open(DB_NAME, DB_VERSION);

		request.onerror = () => {
			reject(new Error('Failed to open IndexedDB'));
		};

		request.onsuccess = () => {
			dbInstance = request.result;
			resolve(dbInstance);
		};

		request.onupgradeneeded = (event) => {
			const db = (event.target as IDBOpenDBRequest).result;
			if (!db.objectStoreNames.contains(STORE_NAME)) {
				db.createObjectStore(STORE_NAME, {keyPath: 'id'});
			}
		};
	});
};

export const saveBackgroundImage = async (id: string, blob: Blob): Promise<void> => {
	const db = await openDB();
	return new Promise((resolve, reject) => {
		const transaction = db.transaction([STORE_NAME], 'readwrite');
		const store = transaction.objectStore(STORE_NAME);

		const backgroundImage: BackgroundImageData = {
			id,
			blob,
			createdAt: Date.now(),
		};

		const request = store.put(backgroundImage);

		request.onsuccess = () => {
			resolve();
		};

		request.onerror = () => {
			reject(new Error('Failed to save background image'));
		};
	});
};

const getBackgroundImage = async (id: string): Promise<BackgroundImageData | null> => {
	const db = await openDB();
	return new Promise((resolve, reject) => {
		const transaction = db.transaction([STORE_NAME], 'readonly');
		const store = transaction.objectStore(STORE_NAME);
		const request = store.get(id);

		request.onsuccess = () => {
			resolve(request.result || null);
		};

		request.onerror = () => {
			reject(new Error('Failed to get background image'));
		};
	});
};

export const deleteBackgroundImage = async (id: string): Promise<void> => {
	const db = await openDB();
	return new Promise((resolve, reject) => {
		const transaction = db.transaction([STORE_NAME], 'readwrite');
		const store = transaction.objectStore(STORE_NAME);
		const request = store.delete(id);

		request.onsuccess = () => {
			resolve();
		};

		request.onerror = () => {
			reject(new Error('Failed to delete background image'));
		};
	});
};

export const getBackgroundImageURL = async (id: string): Promise<string | null> => {
	const imageData = await getBackgroundImage(id);
	if (!imageData) return null;
	return URL.createObjectURL(imageData.blob);
};
