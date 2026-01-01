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

import type {SoundType} from './SoundUtils';

const DB_NAME = 'FluxerCustomSounds';
const DB_VERSION = 2;
const STORE_NAME = 'customSounds';
const ENTRANCE_SOUND_STORE = 'entranceSound';

export interface CustomSound {
	soundType: SoundType;
	blob: Blob;
	fileName: string;
	uploadedAt: number;
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
				db.createObjectStore(STORE_NAME, {keyPath: 'soundType'});
			}
			if (!db.objectStoreNames.contains(ENTRANCE_SOUND_STORE)) {
				db.createObjectStore(ENTRANCE_SOUND_STORE);
			}
		};
	});
};

export const saveCustomSound = async (soundType: SoundType, blob: Blob, fileName: string): Promise<void> => {
	const db = await openDB();
	return new Promise((resolve, reject) => {
		const transaction = db.transaction([STORE_NAME], 'readwrite');
		const store = transaction.objectStore(STORE_NAME);

		const customSound: CustomSound = {
			soundType,
			blob,
			fileName,
			uploadedAt: Date.now(),
		};

		const request = store.put(customSound);

		request.onsuccess = () => {
			resolve();
		};

		request.onerror = () => {
			reject(new Error('Failed to save custom sound'));
		};
	});
};

export const getCustomSound = async (soundType: SoundType): Promise<CustomSound | null> => {
	const db = await openDB();
	return new Promise((resolve, reject) => {
		const transaction = db.transaction([STORE_NAME], 'readonly');
		const store = transaction.objectStore(STORE_NAME);
		const request = store.get(soundType);

		request.onsuccess = () => {
			resolve(request.result || null);
		};

		request.onerror = () => {
			reject(new Error('Failed to get custom sound'));
		};
	});
};

export const deleteCustomSound = async (soundType: SoundType): Promise<void> => {
	const db = await openDB();
	return new Promise((resolve, reject) => {
		const transaction = db.transaction([STORE_NAME], 'readwrite');
		const store = transaction.objectStore(STORE_NAME);
		const request = store.delete(soundType);

		request.onsuccess = () => {
			resolve();
		};

		request.onerror = () => {
			reject(new Error('Failed to delete custom sound'));
		};
	});
};

export const getAllCustomSounds = async (): Promise<Array<CustomSound>> => {
	const db = await openDB();
	return new Promise((resolve, reject) => {
		const transaction = db.transaction([STORE_NAME], 'readonly');
		const store = transaction.objectStore(STORE_NAME);
		const request = store.getAll();

		request.onsuccess = () => {
			resolve(request.result || []);
		};

		request.onerror = () => {
			reject(new Error('Failed to get all custom sounds'));
		};
	});
};

const SUPPORTED_AUDIO_FORMATS = ['.mp3', '.wav', '.ogg', '.m4a', '.aac', '.flac', '.opus', '.webm'] as const;

export const SUPPORTED_MIME_TYPES = [
	'audio/mpeg',
	'audio/wav',
	'audio/ogg',
	'audio/mp4',
	'audio/aac',
	'audio/flac',
	'audio/opus',
	'audio/webm',
] as const;

const MAX_FILE_SIZE = 2 * 1024 * 1024;
const MAX_ENTRANCE_SOUND_DURATION = 5.2;

export const isValidAudioFile = (file: File): {valid: boolean; error?: string} => {
	if (file.size > MAX_FILE_SIZE) {
		return {valid: false, error: 'File size must be 2MB or less'};
	}

	const fileExtension = `.${file.name.split('.').pop()?.toLowerCase()}`;
	const isValidExtension = SUPPORTED_AUDIO_FORMATS.some((ext) => ext === fileExtension);
	const isValidMimeType = SUPPORTED_MIME_TYPES.some((mime) => file.type.startsWith(mime));

	if (!isValidExtension && !isValidMimeType) {
		return {
			valid: false,
			error: `Invalid file type. Supported formats: ${SUPPORTED_AUDIO_FORMATS.join(', ')}`,
		};
	}

	return {valid: true};
};

export const validateAudioDuration = (file: File): Promise<{valid: boolean; error?: string; duration?: number}> => {
	return new Promise((resolve) => {
		const audio = new Audio();
		const url = URL.createObjectURL(file);

		audio.onloadedmetadata = () => {
			URL.revokeObjectURL(url);
			const duration = audio.duration;

			if (duration > MAX_ENTRANCE_SOUND_DURATION) {
				resolve({
					valid: false,
					error: `Audio duration must be ${MAX_ENTRANCE_SOUND_DURATION} seconds or less`,
					duration,
				});
			} else {
				resolve({valid: true, duration});
			}
		};

		audio.onerror = () => {
			URL.revokeObjectURL(url);
			resolve({valid: false, error: 'Failed to load audio file'});
		};

		audio.src = url;
	});
};

export interface EntranceSound {
	blob: Blob;
	fileName: string;
	duration: number;
	uploadedAt: number;
}

const ENTRANCE_SOUND_KEY = 'userEntranceSound';

export const saveEntranceSound = async (blob: Blob, fileName: string, duration: number): Promise<void> => {
	const db = await openDB();
	return new Promise((resolve, reject) => {
		const transaction = db.transaction([ENTRANCE_SOUND_STORE], 'readwrite');
		const store = transaction.objectStore(ENTRANCE_SOUND_STORE);

		const entranceSound: EntranceSound = {
			blob,
			fileName,
			duration,
			uploadedAt: Date.now(),
		};

		const request = store.put(entranceSound, ENTRANCE_SOUND_KEY);

		request.onsuccess = () => {
			resolve();
		};

		request.onerror = () => {
			reject(new Error('Failed to save entrance sound'));
		};
	});
};

export const getEntranceSound = async (): Promise<EntranceSound | null> => {
	const db = await openDB();
	return new Promise((resolve, reject) => {
		const transaction = db.transaction([ENTRANCE_SOUND_STORE], 'readonly');
		const store = transaction.objectStore(ENTRANCE_SOUND_STORE);
		const request = store.get(ENTRANCE_SOUND_KEY);

		request.onsuccess = () => {
			resolve(request.result || null);
		};

		request.onerror = () => {
			reject(new Error('Failed to get entrance sound'));
		};
	});
};

export const deleteEntranceSound = async (): Promise<void> => {
	const db = await openDB();
	return new Promise((resolve, reject) => {
		const transaction = db.transaction([ENTRANCE_SOUND_STORE], 'readwrite');
		const store = transaction.objectStore(ENTRANCE_SOUND_STORE);
		const request = store.delete(ENTRANCE_SOUND_KEY);

		request.onsuccess = () => {
			resolve();
		};

		request.onerror = () => {
			reject(new Error('Failed to delete entrance sound'));
		};
	});
};
