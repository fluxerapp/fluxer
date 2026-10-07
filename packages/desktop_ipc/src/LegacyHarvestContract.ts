// SPDX-License-Identifier: AGPL-3.0-or-later

export const DESKTOP_LEGACY_HARVEST_CHANNELS = Object.freeze({
	read: 'desktop-legacy-harvest:read',
	readRawSync: 'desktop-legacy-harvest:read-raw',
	markReplanted: 'desktop-legacy-harvest:replanted',
	discard: 'desktop-legacy-harvest:discard',
	submit: 'desktop-legacy-harvest:submit',
} as const);

export const DESKTOP_LEGACY_HARVEST_SENTINEL_PATH = '/__fluxer-desktop-storage-harvest';

export const DESKTOP_LEGACY_HARVEST_MARKER_KEY = 'legacy_origin_harvest.v1';
export const DESKTOP_LEGACY_REPLANT_MARKER_KEY = 'legacy_origin_replant.v1';
export const DESKTOP_LEGACY_HARVEST_FAILURE_MARKER_KEY = 'legacy_origin_harvest.failures';
export const DESKTOP_LEGACY_HARVEST_MAX_FAILURES = 5;
export const DESKTOP_LEGACY_RAW_STORAGE_SEED_MARKER_KEY = 'fluxer:desktop:legacy-raw-storage-seed.v1';
export const DESKTOP_LEGACY_RAW_STORAGE_SEED_MARKER_VALUE = '1';
export const DESKTOP_LEGACY_HARVEST_VALUE_ENCODING_KEY = '__fluxerLegacyHarvestValue';
export const DESKTOP_LEGACY_HARVEST_VALUE_ENCODING_VERSION = 1;
export const DESKTOP_LEGACY_HARVEST_BLOB_REF_KEY = '__blobRef';
export const DESKTOP_LEGACY_HARVEST_BLOB_BYTES_KEY = 'bytes';
export const DESKTOP_LEGACY_HARVEST_BLOB_TRANSPORT_TYPE = 'blob';

export const DesktopLegacyHarvestEncodedValueType = Object.freeze({
	ARRAY_BUFFER: 'array-buffer',
	BIGINT: 'bigint',
	DATE: 'date',
	MAP: 'map',
	NUMBER: 'number',
	OBJECT: 'object',
	REGEXP: 'regexp',
	SET: 'set',
	TYPED_ARRAY: 'typed-array',
	UNDEFINED: 'undefined',
} as const);

export const DesktopLegacyHarvestEncodedNumber = Object.freeze({
	NEGATIVE_INFINITY: '-infinity',
	NEGATIVE_ZERO: '-zero',
	NOT_A_NUMBER: 'nan',
	POSITIVE_INFINITY: 'infinity',
} as const);

export const DesktopLegacyHarvestStatus = Object.freeze({
	PENDING: 'pending',
	HARVESTED: 'harvested',
	EMPTY: 'empty',
	ABANDONED: 'abandoned',
} as const);

export type DesktopLegacyHarvestStatus = (typeof DesktopLegacyHarvestStatus)[keyof typeof DesktopLegacyHarvestStatus];

const DESKTOP_LEGACY_REPLANT_MARKER_VERSION = 1;

export function desktopLegacyReplantMarker(replantedAt: number): string {
	return JSON.stringify({version: DESKTOP_LEGACY_REPLANT_MARKER_VERSION, replantedAt});
}

export function isDesktopLegacyReplantMarker(value: string | null): boolean {
	if (value === null) return false;
	let parsed: unknown;
	try {
		parsed = JSON.parse(value);
	} catch {
		return false;
	}
	if (parsed === null || typeof parsed !== 'object' || Array.isArray(parsed)) return false;
	const record = parsed as {version?: unknown; replantedAt?: unknown};
	return (
		record.version === DESKTOP_LEGACY_REPLANT_MARKER_VERSION &&
		typeof record.replantedAt === 'number' &&
		Number.isFinite(record.replantedAt) &&
		record.replantedAt > 0
	);
}

export interface DesktopLegacyHarvestRecord {
	readonly key: unknown | null;
	readonly value: unknown;
}

export interface DesktopLegacyHarvestStore {
	readonly database: string;
	readonly version: number;
	readonly store: string;
	readonly records: ReadonlyArray<DesktopLegacyHarvestRecord>;
}

export interface DesktopLegacyHarvest {
	readonly version: 1;
	readonly origin: string;
	readonly capturedAt: number;
	readonly localStorage: Readonly<Record<string, string>>;
	readonly stores: ReadonlyArray<DesktopLegacyHarvestStore>;
	readonly mediaDevices?: ReadonlyArray<DesktopLegacyHarvestMediaDevice>;
	readonly serviceWorkersUnregistered: number;
	readonly cachesDeleted: number;
	readonly truncated: ReadonlyArray<string>;
}

export interface DesktopLegacyHarvestMediaDevice {
	readonly deviceId: string;
	readonly kind: string;
	readonly label: string;
	readonly groupId?: string;
}
