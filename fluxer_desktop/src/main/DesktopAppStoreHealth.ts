// SPDX-License-Identifier: AGPL-3.0-or-later

import {readFileSync, renameSync, writeFileSync} from 'node:fs';
import path from 'node:path';

export const DESKTOP_APP_STORE_STATE_FILE_NAME = 'account-store-state-v1.json';
export const DESKTOP_APP_STORE_STATE_VERSION = 1;

export interface DesktopAppStoreState {
	readonly version: number;
	readonly firstSuccessAt: number;
	readonly lastSuccessAt: number;
	readonly lastSchemaVersion: number;
}

export const DesktopAppStoreHealth = Object.freeze({
	HEALTHY: 'healthy',
	UNPROVEN: 'unproven',
	REGRESSED: 'regressed',
} as const);

export type DesktopAppStoreHealth = (typeof DesktopAppStoreHealth)[keyof typeof DesktopAppStoreHealth];

export function desktopAppStoreStateFile(userDataPath: string): string {
	return path.join(userDataPath, DESKTOP_APP_STORE_STATE_FILE_NAME);
}

function readTimestamp(value: unknown): number | null {
	return typeof value === 'number' && Number.isFinite(value) && value > 0 ? value : null;
}

export function readDesktopAppStoreState(userDataPath: string): DesktopAppStoreState | null {
	let contents: string;
	try {
		contents = readFileSync(desktopAppStoreStateFile(userDataPath), 'utf8');
	} catch {
		return null;
	}
	let parsed: unknown;
	try {
		parsed = JSON.parse(contents);
	} catch {
		return null;
	}
	if (parsed == null || typeof parsed !== 'object') {
		return null;
	}
	const {version, firstSuccessAt, lastSuccessAt, lastSchemaVersion} = parsed as Record<string, unknown>;
	const first = readTimestamp(firstSuccessAt);
	const last = readTimestamp(lastSuccessAt);
	if (version !== DESKTOP_APP_STORE_STATE_VERSION || first === null || last === null) {
		return null;
	}
	return {
		version: DESKTOP_APP_STORE_STATE_VERSION,
		firstSuccessAt: first,
		lastSuccessAt: last,
		lastSchemaVersion:
			typeof lastSchemaVersion === 'number' && Number.isFinite(lastSchemaVersion) ? lastSchemaVersion : 0,
	};
}

export function recordDesktopAppStoreSuccess(options: {
	readonly userDataPath: string;
	readonly now: number;
	readonly schemaVersion: number;
}): DesktopAppStoreState | null {
	const previous = readDesktopAppStoreState(options.userDataPath);
	const state: DesktopAppStoreState = {
		version: DESKTOP_APP_STORE_STATE_VERSION,
		firstSuccessAt: previous?.firstSuccessAt ?? options.now,
		lastSuccessAt: options.now,
		lastSchemaVersion: options.schemaVersion,
	};
	const file = desktopAppStoreStateFile(options.userDataPath);
	const pending = `${file}.tmp`;
	try {
		writeFileSync(pending, `${JSON.stringify(state)}\n`, {encoding: 'utf8', mode: 0o600});
		renameSync(pending, file);
	} catch {
		return null;
	}
	return state;
}

export function evaluateDesktopAppStoreHealth(options: {
	readonly available: boolean;
	readonly state: DesktopAppStoreState | null;
}): DesktopAppStoreHealth {
	if (options.available) {
		return DesktopAppStoreHealth.HEALTHY;
	}
	return options.state === null ? DesktopAppStoreHealth.UNPROVEN : DesktopAppStoreHealth.REGRESSED;
}
