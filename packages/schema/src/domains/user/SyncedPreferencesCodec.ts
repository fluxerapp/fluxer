// SPDX-License-Identifier: AGPL-3.0-or-later

import {create, fromBinary, toBinary, toJson} from '@bufbuild/protobuf';
import {
	type SyncedPreferences,
	SyncedPreferencesSchema,
} from '@fluxer/schema/src/gen/fluxer/user/preferences/v1/preferences_pb';
import {base64ToUint8Array, uint8ArrayToBase64} from 'uint8array-extras';

export type {SyncedPreferences} from '@fluxer/schema/src/gen/fluxer/user/preferences/v1/preferences_pb';
export {SyncedPreferencesSchema} from '@fluxer/schema/src/gen/fluxer/user/preferences/v1/preferences_pb';

export const EMPTY_SYNCED_PREFERENCES_ENCODED = '';
export const SYNCED_PREFERENCES_MAX_BYTES = 512 * 1024;
export const SYNCED_PREFERENCES_MAX_ENCODED_LENGTH = Math.ceil(SYNCED_PREFERENCES_MAX_BYTES / 3) * 4;
const BASE64_PATTERN = /^[A-Za-z0-9+/_-]*={0,2}$/;

export function isValidSyncedPreferencesEncoding(value: string): boolean {
	if (value === '') return true;
	if (value.length > SYNCED_PREFERENCES_MAX_ENCODED_LENGTH) return false;
	return BASE64_PATTERN.test(value);
}

export function createEmptySyncedPreferences(): SyncedPreferences {
	return create(SyncedPreferencesSchema);
}

export function encodeSyncedPreferences(preferences: SyncedPreferences): string {
	const bytes = toBinary(SyncedPreferencesSchema, preferences);
	if (bytes.byteLength === 0) return EMPTY_SYNCED_PREFERENCES_ENCODED;
	return uint8ArrayToBase64(bytes);
}

export function encodedSyncedPreferencesByteLength(encoded: string): number {
	if (encoded === '') return 0;
	const padding = encoded.endsWith('==') ? 2 : encoded.endsWith('=') ? 1 : 0;
	return Math.floor((encoded.length * 3) / 4) - padding;
}

export function decodeSyncedPreferences(encoded: string | null | undefined): SyncedPreferences {
	if (!encoded) return createEmptySyncedPreferences();
	let bytes: Uint8Array;
	try {
		bytes = base64ToUint8Array(encoded);
	} catch (error) {
		throw new SyncedPreferencesDecodeError(
			error instanceof Error ? `invalid base64: ${error.message}` : 'invalid base64',
		);
	}
	if (bytes.byteLength === 0) return createEmptySyncedPreferences();
	try {
		return fromBinary(SyncedPreferencesSchema, bytes);
	} catch (error) {
		throw new SyncedPreferencesDecodeError(
			error instanceof Error ? error.message : 'invalid synced preferences protobuf',
		);
	}
}

export function decodeSyncedPreferencesLenient(encoded: string | null | undefined): SyncedPreferences {
	try {
		return decodeSyncedPreferences(encoded);
	} catch {
		return createEmptySyncedPreferences();
	}
}

export function syncedPreferencesToJson(preferences: SyncedPreferences): unknown {
	return toJson(SyncedPreferencesSchema, preferences);
}

export function isEmptySyncedPreferencesEncoded(encoded: string | null | undefined): boolean {
	return !encoded;
}

export class SyncedPreferencesDecodeError extends Error {
	constructor(message: string) {
		super(`failed to decode synced_preferences: ${message}`);
		this.name = 'SyncedPreferencesDecodeError';
	}
}
