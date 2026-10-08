// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	DESKTOP_LEGACY_HARVEST_CHANNELS,
	DESKTOP_LEGACY_RAW_STORAGE_SEED_MARKER_KEY,
	DESKTOP_LEGACY_RAW_STORAGE_SEED_MARKER_VALUE,
} from '@fluxer/desktop_ipc/src/LegacyHarvestContract';
import {legacyShellHintSeed} from '@fluxer/desktop_ipc/src/LegacyShellHintSeed';
import {DESKTOP_LOCAL_APP_PROTOCOL} from '@fluxer/desktop_ipc/src/LocalAppRouteContract';
import {
	GATEWAY_PREBOOT_SESSION_PRESENT,
	GATEWAY_PREBOOT_SESSION_STORAGE_KEY,
	SHELL_HINT_STORAGE_KEY,
	ZOOM_PREBOOT_MIRROR_STORAGE_KEY,
} from '@fluxer/desktop_ipc/src/StorageContract';

const LEGACY_SESSION_TOKEN_KEY = 'token';
const LEGACY_SESSION_USER_ID_KEY = 'userId';
const LEGACY_ZOOM_LEVEL_KEY = 'Accessibility:zoomLevel';
const LEGACY_ACCESSIBILITY_STORE_KEY = 'Accessibility';
const ACTIVE_ACCOUNT_KEY = 'fluxer:auth:active-account-key';
const ZOOM_PERCENT_MIN = 50;
const ZOOM_PERCENT_MAX = 200;

interface LegacyRawStorageSeedRenderer {
	sendSync: (channel: string, ...args: Array<unknown>) => unknown;
}

function isRecord(value: unknown): value is Record<string, unknown> {
	return value !== null && typeof value === 'object' && !Array.isArray(value);
}

function parseJSON(value: unknown): unknown {
	if (typeof value !== 'string') return null;
	try {
		return JSON.parse(value);
	} catch {
		return null;
	}
}

function legacyStoreZoomLevel(raw: Record<string, unknown>): unknown {
	const store = parseJSON(raw[LEGACY_ACCESSIBILITY_STORE_KEY]);
	if (!isRecord(store)) return null;
	const metadata = store.__mps__;
	if (isRecord(metadata) && metadata.version !== 1) return null;
	return store.zoomLevel;
}

function legacyZoomPercent(raw: Record<string, unknown>): string | null {
	const level = parseJSON(raw[LEGACY_ZOOM_LEVEL_KEY]) ?? legacyStoreZoomLevel(raw);
	if (typeof level !== 'number' || !Number.isFinite(level)) return null;
	return String(Math.max(ZOOM_PERCENT_MIN, Math.min(ZOOM_PERCENT_MAX, Math.round(level * 100))));
}

function seedPrebootMirrors(storage: Storage, raw: Record<string, unknown>, sessionSeeded: boolean): void {
	const zoomPercent = legacyZoomPercent(raw);
	if (zoomPercent !== null && storage.getItem(ZOOM_PREBOOT_MIRROR_STORAGE_KEY) === null) {
		storage.setItem(ZOOM_PREBOOT_MIRROR_STORAGE_KEY, zoomPercent);
	}
	if (!sessionSeeded || storage.getItem(SHELL_HINT_STORAGE_KEY) !== null) return;
	const shellHint = legacyShellHintSeed(raw, storage.getItem(ACTIVE_ACCOUNT_KEY) ?? '', Date.now());
	if (shellHint !== null) {
		storage.setItem(SHELL_HINT_STORAGE_KEY, shellHint);
	}
}

export function applyLegacyRawLocalStorage(renderer: LegacyRawStorageSeedRenderer): void {
	if (window.location.protocol !== DESKTOP_LOCAL_APP_PROTOCOL) return;
	let storage: Storage;
	try {
		storage = window.localStorage;
		if (storage.getItem(DESKTOP_LEGACY_RAW_STORAGE_SEED_MARKER_KEY) === DESKTOP_LEGACY_RAW_STORAGE_SEED_MARKER_VALUE) {
			return;
		}
	} catch {
		return;
	}
	let raw: unknown;
	try {
		raw = renderer.sendSync(DESKTOP_LEGACY_HARVEST_CHANNELS.readRawSync);
	} catch {
		return;
	}
	if (!isRecord(raw)) return;
	try {
		const seeded = new Set<string>();
		for (const [key, value] of Object.entries(raw)) {
			if (key === DESKTOP_LEGACY_RAW_STORAGE_SEED_MARKER_KEY || typeof value !== 'string') continue;
			if (storage.getItem(key) === null) {
				storage.setItem(key, value);
				seeded.add(key);
			}
		}
		const sessionSeeded =
			seeded.has(LEGACY_SESSION_TOKEN_KEY) &&
			seeded.has(LEGACY_SESSION_USER_ID_KEY) &&
			raw[LEGACY_SESSION_TOKEN_KEY] !== '' &&
			raw[LEGACY_SESSION_USER_ID_KEY] !== '' &&
			storage.getItem(GATEWAY_PREBOOT_SESSION_STORAGE_KEY) === null;
		if (sessionSeeded) {
			storage.setItem(GATEWAY_PREBOOT_SESSION_STORAGE_KEY, GATEWAY_PREBOOT_SESSION_PRESENT);
		}
		seedPrebootMirrors(storage, raw, sessionSeeded);
		storage.setItem(DESKTOP_LEGACY_RAW_STORAGE_SEED_MARKER_KEY, DESKTOP_LEGACY_RAW_STORAGE_SEED_MARKER_VALUE);
	} catch {
		return;
	}
}
