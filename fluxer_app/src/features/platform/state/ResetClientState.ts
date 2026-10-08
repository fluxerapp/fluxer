// SPDX-License-Identifier: AGPL-3.0-or-later

import {resolveLegacyAppStorageKey} from '@app/features/platform/state/LegacyAppStorageKeyMap';
import {LEGACY_APP_STORAGE_MIGRATION_MARKER_KEY} from '@app/features/platform/state/LegacyAppStorageMigration';
import {
	clearLegacyAccountCorpus,
	deleteAppStorageScope,
	flushAppStorageWrites,
	GLOBAL_APP_STORAGE_SCOPE,
	getAppStorageScope,
	PRESERVED_RESET_STORAGE_KEY_PREFIXES,
	PRESERVED_RESET_STORAGE_KEYS,
	RAW_MIRROR_SCOPE_PREFIX,
	resetActiveAccountScope,
	resetAppStorage,
	UNAUTHENTICATED_APP_STORAGE_SCOPE,
} from '@app/features/platform/state/PersistentStorage';
import {getPersistentStorageBackend} from '@app/features/platform/state/PersistentStorageBackend';
import {rawStorageKeys, writeRawStorageItem} from '@app/features/platform/state/PrebootMirror';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {flushPendingPersistWrites, rehydrateAllPersistentStores} from '@app/features/platform/utils/MobXPersistence';
import {DESKTOP_LEGACY_RAW_STORAGE_SEED_MARKER_KEY} from '@fluxer/desktop_ipc/src/LegacyHarvestContract';

const logger = new Logger('ResetClientState');

export const ResetClientStateReason = Object.freeze({
	LOGOUT: 'logout',
	GATEWAY_AUTH_FAILURE: 'gateway-auth-failure',
	RESET_APP_DATA: 'reset-app-data',
} as const);

export type ResetClientStateReason = (typeof ResetClientStateReason)[keyof typeof ResetClientStateReason];

export interface ResetClientStateOptions {
	readonly reason: ResetClientStateReason;
	readonly keepDrafts: boolean;
}

const UNAUTHENTICATED_RAW_MIRROR_PREFIX = `${RAW_MIRROR_SCOPE_PREFIX}${UNAUTHENTICATED_APP_STORAGE_SCOPE}::`;

function isContentStorageKey(key: string): boolean {
	return resolveLegacyAppStorageKey(key)?.row.fanout === 'content';
}

function isSignedOutContentRawName(name: string): boolean {
	return isContentStorageKey(
		name.startsWith(UNAUTHENTICATED_RAW_MIRROR_PREFIX) ? name.slice(UNAUTHENTICATED_RAW_MIRROR_PREFIX.length) : name,
	);
}

function isPreservedRawStorageName(name: string, keysToKeep: ReadonlySet<string>): boolean {
	if (keysToKeep.has(name) || PRESERVED_RESET_STORAGE_KEY_PREFIXES.some((prefix) => name.startsWith(prefix))) {
		return true;
	}
	if (!name.startsWith(RAW_MIRROR_SCOPE_PREFIX)) {
		return false;
	}
	const separator = name.lastIndexOf('::');
	return separator !== -1 && keysToKeep.has(name.slice(separator + 2));
}

async function settlePendingWrites(): Promise<void> {
	flushPendingPersistWrites();
	await flushAppStorageWrites();
}

async function clearSignedOutContent(): Promise<void> {
	for (const name of rawStorageKeys()) {
		if (isSignedOutContentRawName(name)) {
			writeRawStorageItem(name, null);
		}
	}
	const backend = getPersistentStorageBackend();
	for (const key of (await backend.load(UNAUTHENTICATED_APP_STORAGE_SCOPE)).keys()) {
		if (isContentStorageKey(key)) {
			await backend.delete(UNAUTHENTICATED_APP_STORAGE_SCOPE, key);
		}
	}
}

async function legacyCorpusIsStillNeeded(): Promise<boolean> {
	try {
		const entry = await getPersistentStorageBackend().get(
			GLOBAL_APP_STORAGE_SCOPE,
			LEGACY_APP_STORAGE_MIGRATION_MARKER_KEY,
		);
		if (entry?.value == null) {
			return false;
		}
		const marker: unknown = JSON.parse(entry.value);
		return (marker as {status?: unknown}).status !== 'complete';
	} catch (error) {
		logger.warn('Could not read the legacy migration marker, keeping the legacy corpus', error);
		return true;
	}
}

async function clearScopedStoreAndMigrationMarker(keysToKeep: ReadonlySet<string>): Promise<boolean> {
	try {
		await getPersistentStorageBackend().clearAllExcept(keysToKeep);
		return true;
	} catch (error) {
		logger.error('Could not clear the scoped store, so the marker and the legacy corpus both stay', error);
		return false;
	}
}

function clearLegacyStorageCorpus(keysToKeep: ReadonlySet<string>): void {
	for (const name of rawStorageKeys()) {
		if (isPreservedRawStorageName(name, keysToKeep)) {
			continue;
		}
		writeRawStorageItem(name, null);
	}
}

export async function resetClientState(options: ResetClientStateOptions, accountScope?: string): Promise<void> {
	const keysToKeep: ReadonlyArray<string> = options.keepDrafts ? PRESERVED_RESET_STORAGE_KEYS : [];
	await settlePendingWrites();
	switch (options.reason) {
		case ResetClientStateReason.LOGOUT:
			await deleteAppStorageScope(accountScope ?? getAppStorageScope());
			await clearSignedOutContent();
			return;
		case ResetClientStateReason.GATEWAY_AUTH_FAILURE: {
			await resetActiveAccountScope(keysToKeep);
			if (!(await legacyCorpusIsStillNeeded())) {
				const keepSet = new Set(keysToKeep);
				clearLegacyAccountCorpus((key) => keepSet.has(key));
			}
			return;
		}
		case ResetClientStateReason.RESET_APP_DATA: {
			const preserved = [...keysToKeep, DESKTOP_LEGACY_RAW_STORAGE_SEED_MARKER_KEY];
			const keepSet = new Set(preserved);
			if (!(await clearScopedStoreAndMigrationMarker(keepSet))) {
				return;
			}
			await resetAppStorage(preserved);
			clearLegacyStorageCorpus(keepSet);
			await rehydrateAllPersistentStores();
			return;
		}
	}
}
