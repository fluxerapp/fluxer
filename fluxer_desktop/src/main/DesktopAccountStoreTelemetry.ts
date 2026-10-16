// SPDX-License-Identifier: AGPL-3.0-or-later

import {readFileSync, renameSync, statSync, writeFileSync} from 'node:fs';
import path from 'node:path';
import {createChildLogger} from '@electron/common/Logger';

const DESKTOP_ACCOUNT_STORE_TELEMETRY_FILE_NAME = 'account-store-telemetry-v1.json';
const DESKTOP_ACCOUNT_STORE_TELEMETRY_VERSION = 1;

export const DesktopAccountStoreEvent = Object.freeze({
	MIGRATED: 'desktop.account_store.migrated',
	QUARANTINED: 'desktop.account_store.quarantined',
	FALLBACK_TO_WEB: 'desktop.account_store.fallback_to_web',
	PERMISSION_HARDENING_FAILED: 'desktop.account_store.permission_hardening_failed',
} as const);

export type DesktopAccountStoreEvent = (typeof DesktopAccountStoreEvent)[keyof typeof DesktopAccountStoreEvent];

export type DesktopAccountStoreCounts = Readonly<Record<string, number>>;

export const DesktopStorePermissionState = Object.freeze({
	HARDENED: 'hardened',
	WIDENED: 'widened',
	UNSUPPORTED: 'unsupported',
	UNKNOWN: 'unknown',
} as const);

export type DesktopStorePermissionState =
	(typeof DesktopStorePermissionState)[keyof typeof DesktopStorePermissionState];

const GROUP_AND_WORLD_BITS = 0o077;
const logger = createChildLogger('DesktopAccountStoreTelemetry');

function isFileNotFoundError(error: unknown): boolean {
	return typeof error === 'object' && error !== null && 'code' in error && error.code === 'ENOENT';
}

function desktopAccountStoreTelemetryFile(userDataPath: string): string {
	return path.join(userDataPath, DESKTOP_ACCOUNT_STORE_TELEMETRY_FILE_NAME);
}

export function readDesktopAccountStoreTelemetry(userDataPath: string): DesktopAccountStoreCounts {
	let parsed: unknown;
	try {
		parsed = JSON.parse(readFileSync(desktopAccountStoreTelemetryFile(userDataPath), 'utf8'));
	} catch (error) {
		if (!isFileNotFoundError(error)) {
			logger.warn('Failed to read account store telemetry, using empty counts', error);
		}
		return {};
	}
	if (parsed == null || typeof parsed !== 'object') {
		return {};
	}
	const {version, counts} = parsed as {version?: unknown; counts?: unknown};
	if (version !== DESKTOP_ACCOUNT_STORE_TELEMETRY_VERSION || counts == null || typeof counts !== 'object') {
		return {};
	}
	const known = new Set<string>(Object.values(DesktopAccountStoreEvent));
	const result: Record<string, number> = {};
	for (const [event, value] of Object.entries(counts as Record<string, unknown>)) {
		if (known.has(event) && typeof value === 'number' && Number.isFinite(value) && value > 0) {
			result[event] = Math.floor(value);
		}
	}
	return result;
}

export function recordDesktopAccountStoreEvent(
	userDataPath: string,
	event: DesktopAccountStoreEvent,
): DesktopAccountStoreCounts {
	const counts = {...readDesktopAccountStoreTelemetry(userDataPath)};
	counts[event] = (counts[event] ?? 0) + 1;
	const file = desktopAccountStoreTelemetryFile(userDataPath);
	const pending = `${file}.tmp`;
	try {
		writeFileSync(
			pending,
			`${JSON.stringify({version: DESKTOP_ACCOUNT_STORE_TELEMETRY_VERSION, counts, updatedAt: Date.now()})}\n`,
			{encoding: 'utf8', mode: 0o600},
		);
		renameSync(pending, file);
	} catch (error) {
		logger.warn('Failed to persist account store telemetry', {file, error});
	}
	return counts;
}

export function probeDesktopStorePermissions(storeFile: string): DesktopStorePermissionState {
	if (process.platform === 'win32') {
		return DesktopStorePermissionState.UNSUPPORTED;
	}
	try {
		const file = statSync(storeFile);
		const directory = statSync(path.dirname(storeFile));
		const widened = (file.mode & GROUP_AND_WORLD_BITS) !== 0 || (directory.mode & GROUP_AND_WORLD_BITS) !== 0;
		return widened ? DesktopStorePermissionState.WIDENED : DesktopStorePermissionState.HARDENED;
	} catch (error) {
		if (!isFileNotFoundError(error)) {
			logger.warn('Failed to inspect account store permissions', {storeFile, error});
		}
		return DesktopStorePermissionState.UNKNOWN;
	}
}

export function formatDesktopAccountStoreTelemetry(counts: DesktopAccountStoreCounts): string {
	const entries = Object.values(DesktopAccountStoreEvent).map((event) => `${event}=${counts[event] ?? 0}`);
	return entries.join(', ');
}
