// SPDX-License-Identifier: AGPL-3.0-or-later

import type {SoundType} from '@app/features/notification/utils/SoundUtils';
import {getProtectedIndexedDB} from '@app/features/platform/state/ProtectedWebStorage';

const DB_NAME = 'FluxerCustomSounds';
const DB_VERSION = 2;
const STORE_NAME = 'customSounds';
const LEGACY_ENTRANCE_SOUND_STORE = 'entranceSound';
const browserIndexedDB = getProtectedIndexedDB();

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
		if (!browserIndexedDB) {
			reject(new Error('IndexedDB unavailable'));
			return;
		}
		const request = browserIndexedDB.open(DB_NAME, DB_VERSION);
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
			if (!db.objectStoreNames.contains(LEGACY_ENTRANCE_SOUND_STORE)) {
				db.createObjectStore(LEGACY_ENTRANCE_SOUND_STORE);
			}
		};
	});
};

export async function saveCustomSound(soundType: SoundType, blob: Blob, fileName: string): Promise<void> {
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
}

export async function getCustomSound(soundType: SoundType): Promise<CustomSound | null> {
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
}

export async function deleteCustomSound(soundType: SoundType): Promise<void> {
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
}

export async function getAllCustomSounds(): Promise<Array<CustomSound>> {
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
}

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
const MAX_CUSTOM_SOUND_BYTES = 2 * 1024 * 1024;

export type CustomSoundValidationReason = 'too_large' | 'unsupported_type';

export function isValidAudioFile(file: File): {valid: boolean; reason?: CustomSoundValidationReason} {
	if (file.size > MAX_CUSTOM_SOUND_BYTES) {
		return {valid: false, reason: 'too_large'};
	}
	const fileExtension = `.${file.name.split('.').pop()?.toLowerCase()}`;
	const isValidExtension = SUPPORTED_AUDIO_FORMATS.some((ext) => ext === fileExtension);
	const isValidMimeType = SUPPORTED_MIME_TYPES.some((mime) => file.type.startsWith(mime));
	if (!isValidExtension && !isValidMimeType) {
		return {
			valid: false,
			reason: 'unsupported_type',
		};
	}
	return {valid: true};
}
