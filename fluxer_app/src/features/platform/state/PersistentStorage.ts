// SPDX-License-Identifier: AGPL-3.0-or-later

import {DOMAIN_MIGRATION_STORAGE_KEY_PREFIXES} from '@app/features/app/domain_migration/DomainMigrationCore';
import {
	APP_STORAGE_SCOPE_LOCK_NAME,
	AppStorageBroadcast,
	withAppStorageLock,
} from '@app/features/platform/state/AppStorageBroadcast';
import {
	AppStorageKey,
	isGlobalAppStorageKey,
	LEGACY_APP_STORAGE_MIGRATION_MARKER_KEY,
} from '@app/features/platform/state/AppStorageKeys';
import {
	LEGACY_APP_STORAGE_KEY_MAP,
	resolveLegacyAppStorageKey,
} from '@app/features/platform/state/LegacyAppStorageKeyMap';
import {
	getPersistentStorageBackend,
	PersistentStorageBackendKind,
} from '@app/features/platform/state/PersistentStorageBackend';
import {PersistentStorageWriteQueue} from '@app/features/platform/state/PersistentStorageWriteQueue';
import {
	GATEWAY_PREBOOT_SESSION_STORAGE_KEY,
	isRawStorageArea,
	PREBOOT_MIRROR_KEYS,
	rawStorageKeyAt,
	rawStorageKeys,
	rawStorageKind,
	rawStorageLength,
	readRawStorageItem,
	writeRawStorageItem,
} from '@app/features/platform/state/PrebootMirror';
import {countTelemetryEvent, TelemetryEvent} from '@app/features/platform/utils/AppTelemetry';

export type StorageChangeSource = 'local' | 'external';

export interface StorageChangeEvent {
	key: string | null;
	oldValue: string | null;
	newValue: string | null;
	source: StorageChangeSource;
	storageType: 'local' | 'session' | 'memory';
}

type StorageChangeListener = (event: StorageChangeEvent) => void;

interface StorageSubscriptionOptions {
	key?: string;
	source?: StorageChangeSource | 'any';
}

interface EnhancedStorage {
	getItem(key: string): string | null;
	setItem(key: string, value: string): void;
	removeItem(key: string): void;
	clear(): void;
	key(index: number): string | null;
	readonly length: number;
	getJSON<T>(key: string, defaultValue?: T): T | null;
	setJSON<T>(key: string, value: T): void;
	keys(): Array<string>;
	subscribe(listener: StorageChangeListener, options?: StorageSubscriptionOptions): () => void;
}

export interface AppStorageScopeChangeEvent {
	readonly previousScope: string;
	readonly nextScope: string;
}

type AppStorageScopeListener = (event: AppStorageScopeChangeEvent) => PromiseLike<void> | void;

interface LoadedScopeCache {
	readonly values: Map<string, string>;
	readonly tombstones: Set<string> | null;
	readonly deletedKeys: Set<string>;
}

export const GLOBAL_APP_STORAGE_SCOPE = 'global';
export const UNAUTHENTICATED_APP_STORAGE_SCOPE = 'unauthenticated';

const APP_STORAGE_SCOPED_ENABLED = true;

export const RAW_MIRROR_SCOPE_PREFIX = 'fluxer:app-storage-mirror:';

const RAW_ONLY_APP_STORAGE_KEYS: ReadonlySet<string> = new Set([
	AppStorageKey.AUTH_SESSION_TOKEN,
	AppStorageKey.AUTH_SESSION_USER_ID,
	GATEWAY_PREBOOT_SESSION_STORAGE_KEY,
	'runtimeConfig',
	'AccountManager',
]);

const PRE_HYDRATION_APP_STORAGE_KEYS: ReadonlyArray<string> = [
	'pip_corner',
	'pip_width',
	'SoftwareEncoderWarning_neverShowAgain',
];

const RAW_MIRRORED_APP_STORAGE_KEYS: ReadonlySet<string> = new Set([
	...PREBOOT_MIRROR_KEYS,
	...PRE_HYDRATION_APP_STORAGE_KEYS,
	AppStorageKey.ACCESSIBILITY_LEGACY_STORE,
	AppStorageKey.MESSAGING_DRAFTS,
	AppStorageKey.MESSAGING_MESSAGE_EDIT,
	AppStorageKey.THEME,
]);

let activeScope: string = UNAUTHENTICATED_APP_STORAGE_SCOPE;
let scopedStorageEngaged = false;
let storageInitialized = false;
let storageMutationsBlocked = false;
let storageEventType: 'local' | 'session' | 'memory' = rawStorageKind();
let initializePromise: Promise<void> | null = null;
let transitionTail: Promise<unknown> = Promise.resolve();

const scopedCache = new Map<string, string>();
const resetTombstones = new Set<string>();
const deletedKeys = new Set<string>();
const listeners = new Set<{listener: StorageChangeListener; options: StorageSubscriptionOptions}>();
const beforeScopeChangeListeners = new Set<AppStorageScopeListener>();
const scopeChangeListeners = new Set<AppStorageScopeListener>();

function isContentAppStorageKey(key: string): boolean {
	return resolveLegacyAppStorageKey(key)?.row.fanout === 'content';
}

function isGlobalStorageKey(key: string): boolean {
	return isGlobalAppStorageKey(key) || resolveLegacyAppStorageKey(key)?.row.scope === 'global';
}

function scopedStorageActive(): boolean {
	return scopedStorageEngaged;
}

function normalizeAppStorageScope(scope: string | null | undefined): string {
	if (scope == null || scope.trim().length === 0) {
		return UNAUTHENTICATED_APP_STORAGE_SCOPE;
	}
	return scope;
}

function storageScopeForKey(key: string, scope: string = activeScope): string {
	return isGlobalStorageKey(key) ? GLOBAL_APP_STORAGE_SCOPE : scope;
}

function rawMirrorName(scope: string, key: string): string {
	if (scope === GLOBAL_APP_STORAGE_SCOPE) {
		return key;
	}
	return `${RAW_MIRROR_SCOPE_PREFIX}${scope}::${key}`;
}

const writeQueue = new PersistentStorageWriteQueue({
	resolveBackend: getPersistentStorageBackend,
	onCommitted: (write, generation) => {
		storageBroadcast.publishCommit(write.scope, write.key, generation);
	},
	onFailed: (write, error) => {
		console.error(`[AppStorage] Failed to persist "${write.key}" in scope "${write.scope}"`, error);
	},
});

const storageBroadcast = new AppStorageBroadcast({
	onCommit: (scope, key) => {
		void applyRemoteCommit(scope, key);
	},
	onScopeCleared: (scope) => {
		void applyRemoteScopeReload(scope);
	},
	onReset: () => {
		void applyRemoteScopeReload(activeScope);
	},
});

function shouldNotifyListener(
	event: StorageChangeEvent,
	subscriptionOptions: StorageSubscriptionOptions | undefined,
): boolean {
	if (!subscriptionOptions) {
		return true;
	}
	if (subscriptionOptions.key && event.key !== null && subscriptionOptions.key !== event.key) {
		return false;
	}
	if (subscriptionOptions.key && event.key === null) {
		return true;
	}
	if (
		subscriptionOptions.source &&
		subscriptionOptions.source !== 'any' &&
		subscriptionOptions.source !== event.source
	) {
		return false;
	}
	return true;
}

function notifyListeners(event: Omit<StorageChangeEvent, 'storageType'>): void {
	const fullEvent: StorageChangeEvent = {...event, storageType: storageEventType};
	for (const {listener, options} of [...listeners]) {
		if (!shouldNotifyListener(fullEvent, options)) {
			continue;
		}
		try {
			listener(fullEvent);
		} catch (error) {
			console.error('[AppStorage] A storage subscriber threw', error);
		}
	}
}

function promoteReadThroughValue(scope: string, key: string, value: string): void {
	if (RAW_ONLY_APP_STORAGE_KEYS.has(key) || isContentAppStorageKey(key)) {
		return;
	}
	if (storageMutationsBlocked || writeQueue.hasOutstanding(scope, key)) {
		return;
	}
	writeQueue.enqueue([{scope, key, value, ifAbsent: true}]);
}

function rawStorageNamesFor(key: string): Array<string> {
	const scope = storageScopeForKey(key);
	const names: Array<string> = [];
	if (RAW_MIRRORED_APP_STORAGE_KEYS.has(key) && scope !== GLOBAL_APP_STORAGE_SCOPE) {
		names.push(rawMirrorName(scope, key));
	}
	names.push(key);
	return names;
}

function hasRawStorageShadow(key: string): boolean {
	return rawStorageNamesFor(key).some((name) => readRawStorageItem(name) !== null);
}

function readThroughLegacyValue(key: string): string | null {
	if (isContentAppStorageKey(key) && scopedCache.has(LEGACY_APP_STORAGE_MIGRATION_MARKER_KEY)) {
		return null;
	}
	const scope = storageScopeForKey(key);
	const names = rawStorageNamesFor(key);
	for (const name of names) {
		const value = readRawStorageItem(name);
		if (value === null) {
			continue;
		}
		scopedCache.set(key, value);
		promoteReadThroughValue(scope, key, value);
		return value;
	}
	return null;
}

function readValue(key: string): string | null {
	if (!scopedStorageActive()) {
		return readRawStorageItem(key);
	}
	if (RAW_ONLY_APP_STORAGE_KEYS.has(key)) {
		return readRawStorageItem(key);
	}
	const cached = scopedCache.get(key);
	if (cached !== undefined) {
		return cached;
	}
	if (resetTombstones.has(key)) {
		return null;
	}
	return readThroughLegacyValue(key);
}

function persistDeletedKeys(scope: string): void {
	const names = [...deletedKeys].filter((name) => storageScopeForKey(name) === scope);
	writeQueue.enqueue([
		{scope, key: AppStorageKey.DELETED_KEYS, value: names.length === 0 ? null : JSON.stringify(names)},
	]);
}

function recordDeletedKey(scope: string, key: string): void {
	if (!hasRawStorageShadow(key)) {
		return;
	}
	resetTombstones.add(key);
	if (deletedKeys.has(key)) {
		return;
	}
	deletedKeys.add(key);
	persistDeletedKeys(scope);
}

function forgetDeletedKey(scope: string, key: string): void {
	resetTombstones.delete(key);
	if (!deletedKeys.delete(key)) {
		return;
	}
	persistDeletedKeys(scope);
}

function writeValue(key: string, value: string | null): void {
	if (!scopedStorageActive() || RAW_ONLY_APP_STORAGE_KEYS.has(key)) {
		writeRawStorageItem(key, value);
		return;
	}
	const scope = storageScopeForKey(key);
	if (value === null) {
		scopedCache.delete(key);
		recordDeletedKey(scope, key);
	} else {
		scopedCache.set(key, value);
		forgetDeletedKey(scope, key);
	}
	if (RAW_MIRRORED_APP_STORAGE_KEYS.has(key)) {
		writeRawStorageItem(rawMirrorName(scope, key), value);
	}
	writeQueue.enqueue([{scope, key, value}]);
}

function hasUnsettledWriteFailure(key: string): boolean {
	if (!scopedStorageActive() || RAW_ONLY_APP_STORAGE_KEYS.has(key)) {
		return false;
	}
	return writeQueue.hasFailure(storageScopeForKey(key), key);
}

function allKeys(): Array<string> {
	if (!scopedStorageActive()) {
		return rawStorageKeys();
	}
	const keys = [...scopedCache.keys()];
	for (const key of RAW_ONLY_APP_STORAGE_KEYS) {
		if (!scopedCache.has(key) && readRawStorageItem(key) !== null) {
			keys.push(key);
		}
	}
	return keys;
}

function collectEntries(shouldRemove: (key: string) => boolean): Array<readonly [string, string]> {
	const entries: Array<readonly [string, string]> = [];
	for (const key of allKeys()) {
		if (!shouldRemove(key)) {
			continue;
		}
		const value = readValue(key);
		if (value !== null) {
			entries.push([key, value] as const);
		}
	}
	return entries;
}

function removeEntries(entries: ReadonlyArray<readonly [string, string]>): void {
	if (entries.length === 0) {
		return;
	}
	for (const [key] of entries) {
		writeValue(key, null);
	}
	for (const [key, oldValue] of entries) {
		notifyListeners({key, oldValue, newValue: null, source: 'local'});
	}
}

function requireMutationsAvailable(): void {
	if (storageMutationsBlocked) {
		throw new Error('AppStorage cannot be mutated during a storage lifecycle transition');
	}
}

async function withMutationsBlocked<T>(operation: () => Promise<T>): Promise<T> {
	storageMutationsBlocked = true;
	try {
		return await operation();
	} finally {
		storageMutationsBlocked = false;
	}
}

function runTransition<T>(operation: () => Promise<T>): Promise<T> {
	const result = transitionTail.then(operation, operation);
	transitionTail = result.then(
		() => undefined,
		() => undefined,
	);
	return result;
}

async function runScopeListeners(
	scopeListeners: ReadonlySet<AppStorageScopeListener>,
	event: AppStorageScopeChangeEvent,
): Promise<void> {
	const outcomes = await Promise.allSettled(
		[...scopeListeners].map(async (listener) => {
			await listener(event);
		}),
	);
	for (const outcome of outcomes) {
		if (outcome.status === 'rejected') {
			console.error('[AppStorage] A scope listener failed', outcome.reason);
		}
	}
}

function accountResetTombstones(keptKeys: ReadonlySet<string>): Set<string> {
	const tombstones = new Set<string>();
	for (const row of LEGACY_APP_STORAGE_KEY_MAP) {
		if (row.kind !== 'exact' || row.scope !== 'account') {
			continue;
		}
		if (!keptKeys.has(row.key)) {
			tombstones.add(row.key);
		}
	}
	return tombstones;
}

function decodeAccountResetTombstones(value: string | null | undefined): Set<string> | null {
	if (value == null) {
		return null;
	}
	try {
		const parsed: unknown = JSON.parse(value);
		if (!Array.isArray(parsed)) {
			return null;
		}
		return accountResetTombstones(new Set(parsed.filter((name): name is string => typeof name === 'string')));
	} catch (error) {
		console.error('[AppStorage] Failed to decode the account reset record', error);
		return null;
	}
}

function decodeDeletedKeys(value: string | null | undefined): Set<string> | null {
	if (value == null) {
		return null;
	}
	try {
		const parsed: unknown = JSON.parse(value);
		if (!Array.isArray(parsed)) {
			return null;
		}
		return new Set(parsed.filter((name): name is string => typeof name === 'string'));
	} catch (error) {
		console.error('[AppStorage] Failed to decode the deleted key record', error);
		return null;
	}
}

function adoptDeletedKeys(names: ReadonlySet<string> | null, replaceExisting: boolean): void {
	if (replaceExisting) {
		deletedKeys.clear();
	}
	for (const name of names ?? []) {
		deletedKeys.add(name);
		resetTombstones.add(name);
	}
}

function adoptResetTombstones(tombstones: ReadonlySet<string> | null, replaceExisting: boolean): void {
	if (replaceExisting) {
		resetTombstones.clear();
	}
	for (const name of tombstones ?? []) {
		resetTombstones.add(name);
	}
}

async function loadCacheForScope(scope: string): Promise<LoadedScopeCache> {
	const backend = getPersistentStorageBackend();
	const values = new Map<string, string>();
	const loadedDeletedKeys = new Set<string>();
	const takeDeletedKeys = (): void => {
		for (const name of decodeDeletedKeys(values.get(AppStorageKey.DELETED_KEYS)) ?? []) {
			loadedDeletedKeys.add(name);
		}
		values.delete(AppStorageKey.DELETED_KEYS);
	};
	for (const [key, entry] of await backend.load(GLOBAL_APP_STORAGE_SCOPE)) {
		values.set(key, entry.value);
	}
	takeDeletedKeys();
	if (scope !== GLOBAL_APP_STORAGE_SCOPE) {
		for (const [key, entry] of await backend.load(scope)) {
			values.set(key, entry.value);
		}
		takeDeletedKeys();
	}
	const tombstones = decodeAccountResetTombstones(values.get(AppStorageKey.ACCOUNT_RESET_KEPT_KEYS));
	values.delete(AppStorageKey.ACCOUNT_RESET_KEPT_KEYS);
	return {values, tombstones, deletedKeys: loadedDeletedKeys};
}

function replaceScopedCache(next: ReadonlyMap<string, string>): void {
	scopedCache.clear();
	for (const [key, value] of next) {
		scopedCache.set(key, value);
	}
}

function replayRawMirror(scope: string): void {
	for (const key of RAW_MIRRORED_APP_STORAGE_KEYS) {
		if (RAW_ONLY_APP_STORAGE_KEYS.has(key)) {
			continue;
		}
		const keyScope = storageScopeForKey(key, scope);
		const value = readRawStorageItem(rawMirrorName(keyScope, key));
		if (value === null || scopedCache.get(key) === value) {
			continue;
		}
		scopedCache.set(key, value);
		writeQueue.enqueue([{scope: keyScope, key, value}]);
	}
}

function clearRawMirror(scope: string, shouldKeep: (key: string) => boolean): void {
	const clearingGlobalScope = scope === GLOBAL_APP_STORAGE_SCOPE;
	for (const key of RAW_MIRRORED_APP_STORAGE_KEYS) {
		if (shouldKeep(key) || isGlobalStorageKey(key) !== clearingGlobalScope) {
			continue;
		}
		writeRawStorageItem(rawMirrorName(scope, key), null);
	}
}

export function clearLegacyAccountCorpus(shouldKeep: (key: string) => boolean): void {
	for (const row of LEGACY_APP_STORAGE_KEY_MAP) {
		if (row.kind !== 'exact' || row.scope !== 'account') {
			continue;
		}
		if (!shouldKeep(row.key)) {
			writeRawStorageItem(row.key, null);
		}
	}
}

async function adoptRemoteAccountReset(scope: string): Promise<void> {
	try {
		const entry = await getPersistentStorageBackend().get(scope, AppStorageKey.ACCOUNT_RESET_KEPT_KEYS);
		if (scope !== activeScope) {
			return;
		}
		adoptResetTombstones(decodeAccountResetTombstones(entry?.value), false);
	} catch (error) {
		console.error('[AppStorage] Failed to apply a cross-tab account reset', error);
	}
}

async function adoptRemoteDeletedKeys(scope: string): Promise<void> {
	try {
		const entry = await getPersistentStorageBackend().get(scope, AppStorageKey.DELETED_KEYS);
		if (scope !== GLOBAL_APP_STORAGE_SCOPE && scope !== activeScope) {
			return;
		}
		adoptDeletedKeys(decodeDeletedKeys(entry?.value), false);
	} catch (error) {
		console.error('[AppStorage] Failed to apply a cross-tab deletion record', error);
	}
}

async function applyRemoteCommit(scope: string, key: string): Promise<void> {
	if (!scopedStorageActive() || (scope !== GLOBAL_APP_STORAGE_SCOPE && scope !== activeScope)) {
		return;
	}
	if (key === AppStorageKey.ACCOUNT_RESET_KEPT_KEYS) {
		await adoptRemoteAccountReset(scope);
		return;
	}
	if (key === AppStorageKey.DELETED_KEYS) {
		await adoptRemoteDeletedKeys(scope);
		return;
	}
	if (writeQueue.hasOutstanding(scope, key)) {
		return;
	}
	const generation = writeQueue.generation(scope, key);
	try {
		const entry = await getPersistentStorageBackend().get(scope, key);
		if (scope !== GLOBAL_APP_STORAGE_SCOPE && scope !== activeScope) {
			return;
		}
		if (writeQueue.hasOutstanding(scope, key) || writeQueue.generation(scope, key) !== generation) {
			return;
		}
		const oldValue = scopedCache.get(key) ?? null;
		const newValue = entry?.value ?? null;
		if (oldValue === newValue) {
			return;
		}
		if (newValue === null) {
			scopedCache.delete(key);
		} else {
			scopedCache.set(key, newValue);
		}
		notifyListeners({key, oldValue, newValue, source: 'external'});
	} catch (error) {
		console.error('[AppStorage] Failed to apply a cross-tab commit', error);
	}
}

async function applyRemoteScopeReload(scope: string): Promise<void> {
	if (!scopedStorageActive() || (scope !== GLOBAL_APP_STORAGE_SCOPE && scope !== activeScope)) {
		return;
	}
	const reloadScope = activeScope;
	try {
		const {values: loaded, tombstones, deletedKeys: loadedDeletedKeys} = await loadCacheForScope(reloadScope);
		if (reloadScope !== activeScope) {
			return;
		}
		adoptResetTombstones(tombstones, false);
		adoptDeletedKeys(loadedDeletedKeys, false);
		for (const key of new Set([...loaded.keys(), ...scopedCache.keys()])) {
			if (!writeQueue.hasOutstanding(storageScopeForKey(key), key)) {
				continue;
			}
			const pending = scopedCache.get(key);
			if (pending === undefined) {
				loaded.delete(key);
			} else {
				loaded.set(key, pending);
			}
		}
		replaceScopedCache(loaded);
		notifyListeners({key: null, oldValue: null, newValue: null, source: 'external'});
	} catch (error) {
		console.error('[AppStorage] Failed to reload the storage cache', error);
	}
}

if (typeof window !== 'undefined' && rawStorageKind() !== 'memory') {
	window.addEventListener('storage', (event: StorageEvent) => {
		if (scopedStorageActive() || !isRawStorageArea(event.storageArea)) {
			return;
		}
		notifyListeners({
			key: event.key,
			oldValue: event.oldValue,
			newValue: event.newValue,
			source: 'external',
		});
	});
}

export async function flushAppStorageWrites(): Promise<void> {
	await writeQueue.flush();
}

export function getAppStorageScope(): string {
	return activeScope;
}

export function mutationsBlocked(): boolean {
	return storageMutationsBlocked;
}

export function onBeforeAppStorageScopeChange(listener: AppStorageScopeListener): () => void {
	beforeScopeChangeListeners.add(listener);
	return () => {
		beforeScopeChangeListeners.delete(listener);
	};
}

export function subscribeAppStorageScope(listener: AppStorageScopeListener): () => void {
	scopeChangeListeners.add(listener);
	return () => {
		scopeChangeListeners.delete(listener);
	};
}

export async function initializeAppStorage(options?: {readonly scoped?: boolean}): Promise<void> {
	if (storageInitialized) {
		await flushAppStorageWrites();
		return;
	}
	if (initializePromise !== null) {
		return initializePromise;
	}
	const scoped = options?.scoped ?? APP_STORAGE_SCOPED_ENABLED;
	const completion = runTransition(async () => {
		if (storageInitialized) {
			return;
		}
		if (!scoped) {
			storageInitialized = true;
			return;
		}
		scopedStorageEngaged = true;
		try {
			storageEventType =
				getPersistentStorageBackend().kind === PersistentStorageBackendKind.MEMORY ? 'memory' : 'local';
			storageBroadcast.open();
			const loaded = await loadCacheForScope(activeScope);
			adoptResetTombstones(loaded.tombstones, true);
			adoptDeletedKeys(loaded.deletedKeys, true);
			replaceScopedCache(loaded.values);
		} catch (error) {
			console.error('[AppStorage] Failed to hydrate the scoped cache, serving the legacy corpus', error);
			replaceScopedCache(new Map());
		}
		replayRawMirror(activeScope);
		storageInitialized = true;
		notifyListeners({key: null, oldValue: null, newValue: null, source: 'external'});
	})
		.catch((error: unknown) => {
			console.error('[AppStorage] Initialization failed', error);
			storageInitialized = true;
		})
		.finally(() => {
			if (initializePromise === completion) {
				initializePromise = null;
			}
		});
	initializePromise = completion;
	return completion;
}

async function activateScopeWithinTransition(previousScope: string, nextScope: string): Promise<void> {
	const event: AppStorageScopeChangeEvent = {previousScope, nextScope};
	if (previousScope !== nextScope) {
		await runScopeListeners(beforeScopeChangeListeners, event);
	}
	await flushAppStorageWrites();
	await withMutationsBlocked(async () => {
		const loaded = await loadCacheForScope(nextScope);
		adoptResetTombstones(loaded.tombstones, previousScope !== nextScope);
		adoptDeletedKeys(loaded.deletedKeys, previousScope !== nextScope);
		activeScope = nextScope;
		replaceScopedCache(loaded.values);
		replayRawMirror(nextScope);
		storageInitialized = true;
	});
	notifyListeners({key: null, oldValue: null, newValue: null, source: 'external'});
	if (previousScope !== nextScope) {
		await runScopeListeners(scopeChangeListeners, event);
	}
}

export async function activateAppStorageScope(scope: string | null): Promise<void> {
	const nextScope = normalizeAppStorageScope(scope);
	await runTransition(async () => {
		const previousScope = activeScope;
		if (!scopedStorageActive()) {
			activeScope = nextScope;
			return;
		}
		await withAppStorageLock(APP_STORAGE_SCOPE_LOCK_NAME, () =>
			activateScopeWithinTransition(previousScope, nextScope),
		);
	}).catch((error: unknown) => {
		countTelemetryEvent(TelemetryEvent.STORAGE_SCOPE_ACTIVATION_FAILED, 1, nextScope);
		console.error('[AppStorage] Failed to activate the storage scope', error);
	});
}

export async function deleteAppStorageScope(scope: string): Promise<void> {
	const normalizedScope = normalizeAppStorageScope(scope);
	await runTransition(async () => {
		if (!scopedStorageActive()) {
			removeEntries(collectEntries(() => true));
			return;
		}
		await flushAppStorageWrites();
		const removed = await withMutationsBlocked(async (): Promise<Array<readonly [string, string]>> => {
			await getPersistentStorageBackend().clearAllForScope(normalizedScope);
			writeQueue.invalidateScope(normalizedScope);
			clearRawMirror(normalizedScope, () => false);
			if (normalizedScope !== activeScope && normalizedScope !== GLOBAL_APP_STORAGE_SCOPE) {
				return [];
			}
			const dropped = [...scopedCache].filter(([key]) => storageScopeForKey(key) === normalizedScope);
			for (const [key] of dropped) {
				scopedCache.delete(key);
			}
			return dropped;
		});
		for (const [key, oldValue] of removed) {
			notifyListeners({key, oldValue, newValue: null, source: 'local'});
		}
		storageBroadcast.publishScopeCleared(normalizedScope);
	}).catch((error: unknown) => {
		console.error('[AppStorage] Failed to delete a storage scope', error);
		throw error;
	});
}

export async function resetActiveAccountScope(keysToKeep: ReadonlyArray<string>): Promise<void> {
	const keepSet = new Set(keysToKeep);
	await runTransition(async () => {
		if (!scopedStorageActive()) {
			removeEntries(collectEntries((key) => !keepSet.has(key)));
			return;
		}
		const scope = activeScope;
		if (scope === GLOBAL_APP_STORAGE_SCOPE) {
			return;
		}
		const removed = [...scopedCache].filter(([key]) => !keepSet.has(key) && storageScopeForKey(key, scope) === scope);
		for (const [key] of removed) {
			scopedCache.delete(key);
			writeQueue.enqueue([{scope, key, value: null}]);
		}
		adoptResetTombstones(accountResetTombstones(keepSet), false);
		writeQueue.enqueue([{scope, key: AppStorageKey.ACCOUNT_RESET_KEPT_KEYS, value: JSON.stringify([...keepSet])}]);
		clearRawMirror(scope, (key) => keepSet.has(key));
		for (const [key, oldValue] of removed) {
			notifyListeners({key, oldValue, newValue: null, source: 'local'});
		}
		await flushAppStorageWrites();
	}).catch((error: unknown) => {
		console.error('[AppStorage] Failed to reset the active account scope', error);
	});
}

export async function resetAppStorage(keysToKeep: ReadonlyArray<string>): Promise<void> {
	const keepSet = new Set(keysToKeep);
	await runTransition(async () => {
		if (!scopedStorageActive()) {
			removeEntries(collectEntries((key) => !keepSet.has(key)));
			return;
		}
		await flushAppStorageWrites();
		const removed = await withMutationsBlocked(async (): Promise<Array<readonly [string, string]>> => {
			await getPersistentStorageBackend().clearAllExcept(keepSet);
			writeQueue.invalidateAll();
			clearRawMirror(activeScope, (key) => keepSet.has(key));
			clearRawMirror(GLOBAL_APP_STORAGE_SCOPE, (key) => keepSet.has(key));
			const dropped = [...scopedCache].filter(([key]) => !keepSet.has(key));
			for (const [key] of dropped) {
				scopedCache.delete(key);
			}
			return dropped;
		});
		for (const [key, oldValue] of removed) {
			notifyListeners({key, oldValue, newValue: null, source: 'local'});
		}
		storageBroadcast.publishReset();
	}).catch((error: unknown) => {
		console.error('[AppStorage] Failed to reset app storage', error);
	});
}

function createStorage(): EnhancedStorage {
	const setItemInternal = (key: string, value: string): void => {
		requireMutationsAvailable();
		const nextValue = String(value);
		const oldValue = readValue(key);
		if (oldValue === nextValue && !hasUnsettledWriteFailure(key)) {
			return;
		}
		writeValue(key, nextValue);
		if (oldValue === nextValue) {
			return;
		}
		notifyListeners({key, oldValue, newValue: nextValue, source: 'local'});
	};
	const removeItemInternal = (key: string): void => {
		requireMutationsAvailable();
		const oldValue = readValue(key);
		if (oldValue === null && !hasUnsettledWriteFailure(key)) {
			return;
		}
		writeValue(key, null);
		if (oldValue === null) {
			return;
		}
		notifyListeners({key, oldValue, newValue: null, source: 'local'});
	};
	const storage = {} as EnhancedStorage;
	Object.defineProperties(storage, {
		getItem: {
			value: (key: string) => readValue(key),
			writable: false,
			enumerable: false,
		},
		setItem: {
			value: setItemInternal,
			writable: false,
			enumerable: false,
		},
		removeItem: {
			value: removeItemInternal,
			writable: false,
			enumerable: false,
		},
		clear: {
			value: () => {
				requireMutationsAvailable();
				removeEntries(collectEntries(() => true));
			},
			writable: false,
			enumerable: false,
		},
		key: {
			value: (index: number) => {
				if (!scopedStorageActive()) {
					return rawStorageKeyAt(index);
				}
				return allKeys()[index] ?? null;
			},
			writable: false,
			enumerable: false,
		},
		length: {
			get: () => (scopedStorageActive() ? allKeys().length : rawStorageLength()),
			enumerable: false,
		},
		getJSON: {
			value: <T>(key: string, defaultValue?: T): T | null => {
				const item = readValue(key);
				if (item === null) return defaultValue === undefined ? null : defaultValue;
				try {
					return JSON.parse(item);
				} catch (e) {
					console.warn(`[AppStorage] Failed to parse JSON for key "${key}":`, e);
					return defaultValue === undefined ? null : defaultValue;
				}
			},
			writable: false,
			enumerable: false,
		},
		setJSON: {
			value: <T>(key: string, value: T) => {
				if (value === storage) {
					throw new Error('Cannot store the storage object itself');
				}
				try {
					const serialized = JSON.stringify(value);
					setItemInternal(key, serialized);
				} catch (e) {
					throw new Error(`Failed to store value for key "${key}": ${e}`);
				}
			},
			writable: false,
			enumerable: false,
		},
		keys: {
			value: allKeys,
			writable: false,
			enumerable: false,
		},
		subscribe: {
			value: (listener: StorageChangeListener, options: StorageSubscriptionOptions = {}) => {
				const subscription = {listener, options};
				listeners.add(subscription);
				return () => {
					listeners.delete(subscription);
				};
			},
			writable: false,
			enumerable: false,
		},
	});
	return storage;
}

const AppStorage = createStorage();

export default AppStorage;

export const PRESERVED_RESET_STORAGE_KEYS = [AppStorageKey.MESSAGING_DRAFTS] as const;

export const PRESERVED_RESET_STORAGE_KEY_PREFIXES = DOMAIN_MIGRATION_STORAGE_KEY_PREFIXES;
