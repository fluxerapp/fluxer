// SPDX-License-Identifier: AGPL-3.0-or-later

import type {DesktopStoreMarker, DesktopStoreSkippedRecord} from '@fluxer/desktop_ipc/src/AccountContract';

export const DESKTOP_STORAGE_CHANNELS = Object.freeze({
	getStatus: 'desktop-storage:get-status',
	load: 'desktop-storage:load',
	get: 'desktop-storage:get',
	set: 'desktop-storage:set',
	delete: 'desktop-storage:delete',
	clearAllForScope: 'desktop-storage:clear-all-for-scope',
	clearAllExcept: 'desktop-storage:clear-all-except',
	setMany: 'desktop-storage:set-many',
	import: 'desktop-storage:import',
	getMarker: 'desktop-storage:get-marker',
	setMarker: 'desktop-storage:set-marker',
} as const);

export const ZOOM_PREBOOT_MIRROR_STORAGE_KEY = 'fluxer:accessibility:zoom-preboot';
export const GATEWAY_PREBOOT_SESSION_STORAGE_KEY = 'fluxer:gateway:preboot:session';
export const GATEWAY_PREBOOT_SESSION_PRESENT = '1';
export const SHELL_HINT_STORAGE_KEY = 'fluxer:ui:shell-hint';

export const DESKTOP_LEGACY_IMPORT_MARKER_KEY = 'legacy_import.v1';
export const DESKTOP_LEGACY_IMPORT_FAILURE_MARKER_KEY = 'legacy_import.failures';
export const DESKTOP_LEGACY_SESSION_MARKER_KEY = 'legacy_session.reconciled_at';
export const DESKTOP_LEGACY_AUTHORITY_MARKER_KEY = 'legacy_import.authority';
export const DESKTOP_LEGACY_AUTHORITY_MARKER_VALUE = 'committed.v1';
const DESKTOP_LEGACY_IMPORT_VERSION = 1;

export const DesktopLegacyImportPhase = Object.freeze({
	ACCOUNTS: 'accounts',
	ENTRIES: 'entries',
	SESSION: 'session',
	DONE: 'done',
} as const);

export type DesktopLegacyImportPhase = (typeof DesktopLegacyImportPhase)[keyof typeof DesktopLegacyImportPhase];

export function desktopLegacyImportMarker(phase: DesktopLegacyImportPhase, updatedAt: number): DesktopStoreMarker {
	return {
		key: DESKTOP_LEGACY_IMPORT_MARKER_KEY,
		value: JSON.stringify({version: DESKTOP_LEGACY_IMPORT_VERSION, phase, updatedAt}),
	};
}

export function readDesktopLegacyImportPhase(value: string | null): DesktopLegacyImportPhase | null {
	if (value == null) {
		return null;
	}
	let parsed: unknown;
	try {
		parsed = JSON.parse(value);
	} catch {
		return null;
	}
	if (parsed == null || typeof parsed !== 'object') {
		return null;
	}
	const {version, phase} = parsed as {version?: unknown; phase?: unknown};
	if (version !== DESKTOP_LEGACY_IMPORT_VERSION) {
		return null;
	}
	return Object.values(DesktopLegacyImportPhase).find((candidate) => candidate === phase) ?? null;
}

export type DesktopStoreQuarantineReason = 'newer' | 'schema' | 'corrupt' | 'derivation';

export interface DesktopStoreStatus {
	readonly available: boolean;
	readonly authorityExpected: boolean;
	readonly schemaVersion: number;
	readonly quarantined: boolean;
	readonly quarantineReason: DesktopStoreQuarantineReason | null;
	readonly unavailableReason: string | null;
}

export interface DesktopStorageEntry {
	readonly key: string;
	readonly value: string;
	readonly updatedAt: number;
}

export interface DesktopStorageScopedEntry extends DesktopStorageEntry {
	readonly scope: string;
}

export interface DesktopStorageWrite {
	readonly scope: string;
	readonly key: string;
	readonly value: string;
	readonly ifAbsent?: boolean;
}

export interface DesktopStorageImportRequest {
	readonly entries: ReadonlyArray<DesktopStorageScopedEntry>;
	readonly marker?: DesktopStoreMarker | null;
}

export interface DesktopStorageImportReport {
	readonly imported: number;
	readonly skipped: ReadonlyArray<DesktopStoreSkippedRecord>;
}

export interface DesktopStorageAPI {
	getStatus: () => Promise<DesktopStoreStatus>;
	load: (scope: string) => Promise<Array<DesktopStorageEntry>>;
	get: (scope: string, key: string) => Promise<DesktopStorageEntry | null>;
	set: (scope: string, key: string, value: string) => Promise<void>;
	delete: (scope: string, key: string) => Promise<void>;
	clearAllForScope: (scope: string) => Promise<void>;
	clearAllExcept: (keysToKeep: ReadonlyArray<string>) => Promise<void>;
	setMany: (writes: ReadonlyArray<DesktopStorageWrite>) => Promise<void>;
	import: (request: DesktopStorageImportRequest) => Promise<DesktopStorageImportReport>;
	getMarker: (key: string) => Promise<string | null>;
	setMarker: (key: string, value: string) => Promise<void>;
}
