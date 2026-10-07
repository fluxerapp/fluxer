// SPDX-License-Identifier: AGPL-3.0-or-later

import {runtimeInstanceKey} from '@app/features/app/state/InstanceSnapshotStore';
import {
	type AccountRekeyContext,
	type AccountRekeyOutcome,
	type AccountRekeyTarget,
	BrowserAccountStorageUnavailableError,
	type KeyedStoredAccount,
	type StoredAccount,
} from '@app/features/auth/state/AccountStorage';
import {accountStorageKey} from '@app/features/auth/state/AccountStorageKey';
import {BROWSER_ACCOUNT_FALLBACK_STORAGE_KEY} from '@app/features/auth/state/BrowserAccountFallbackStore';
import {APP_STORAGE_MIGRATION_LOCK_NAME, withAppStorageLock} from '@app/features/platform/state/AppStorageBroadcast';
import {
	isGlobalAppStorageKey,
	LEGACY_APP_STORAGE_MIGRATION_MARKER_KEY,
	LEGACY_SHARED_CONTENT_REVIEW_KEY,
	LegacySharedContentReviewState,
} from '@app/features/platform/state/AppStorageKeys';
import {
	isLegacyAccountSwappedKey,
	isNotMigratedLegacyKey,
	resolveLegacyAppStorageKey,
} from '@app/features/platform/state/LegacyAppStorageKeyMap';
import {
	GLOBAL_APP_STORAGE_SCOPE,
	RAW_MIRROR_SCOPE_PREFIX,
	UNAUTHENTICATED_APP_STORAGE_SCOPE,
} from '@app/features/platform/state/PersistentStorage';
import {
	AppStorageQuotaExceededError,
	type AppStorageWrite,
	type PersistentStorageBackend,
	PersistentStorageBackendKind,
} from '@app/features/platform/state/PersistentStorageBackend';
import {getProtectedLocalStorage} from '@app/features/platform/state/ProtectedWebStorage';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {DESKTOP_LEGACY_RAW_STORAGE_SEED_MARKER_KEY} from '@fluxer/desktop_ipc/src/LegacyHarvestContract';

const logger = new Logger('LegacyAppStorageMigration');

const TEXT_ENCODER = new TextEncoder();

export const LEGACY_APP_STORAGE_MIGRATION_VERSION = 1;
export {LEGACY_APP_STORAGE_MIGRATION_MARKER_KEY};
export const LEGACY_APP_STORAGE_UNCLASSIFIED_KEYS_KEY = 'fluxer:migration:unclassified-legacy-keys';

export const LegacyAppStorageMigrationStatus = Object.freeze({
	MIGRATED: 'migrated',
	PARTIAL: 'partial',
	ALREADY_DONE: 'already-done',
	SKIPPED: 'skipped',
	FAILED: 'failed',
} as const);

export type LegacyAppStorageMigrationStatus =
	(typeof LegacyAppStorageMigrationStatus)[keyof typeof LegacyAppStorageMigrationStatus];

export const LegacyAppStorageMigrationSkipReason = Object.freeze({
	MEMORY_BACKEND: 'memory-backend',
	NO_LOCAL_STORAGE: 'no-local-storage',
	NO_INSTANCE_KEY: 'no-instance-key',
	ACCOUNTS_NOT_AUTHORITATIVE: 'accounts-not-authoritative',
	SCOPE_NOT_PENDING: 'scope-not-pending',
} as const);

export type LegacyAppStorageMigrationSkipReason =
	(typeof LegacyAppStorageMigrationSkipReason)[keyof typeof LegacyAppStorageMigrationSkipReason];

export interface LegacyContentCarry {
	readonly contentScope: string | null;
	readonly sharedContentScopes: ReadonlyArray<string>;
	readonly deferredUserIds: ReadonlyArray<string>;
}

interface LegacyContentCarryAccounts {
	readonly accounts: ReadonlyArray<KeyedStoredAccount>;
	readonly deferredAccounts: ReadonlyArray<StoredAccount>;
	readonly activeScope: string;
}

export interface LegacyAppStorageMigrationPlanInput {
	readonly storage: Storage;
	readonly accounts: ReadonlyArray<KeyedStoredAccount>;
	readonly contentScope: string | null;
	readonly sharedContentScopes: ReadonlyArray<string>;
}

export interface LegacyAppStorageMigrationPlan {
	readonly scopes: ReadonlyArray<string>;
	readonly contentScope: string | null;
	readonly writes: ReadonlyArray<AppStorageWrite>;
	readonly unclassifiedKeys: ReadonlyArray<string>;
	readonly totalBytes: number;
}

export interface LegacyAppStorageMigrationLimits {
	readonly maxTotalBytes: number;
	readonly maxEntries: number;
}

export interface LegacyAppStorageMigrationAccountSource {
	migrateAccountStorageKeys(context: AccountRekeyContext): Promise<AccountRekeyOutcome>;
}

export interface LegacyAppStorageMigrationRequest {
	readonly backend: PersistentStorageBackend;
	readonly accounts: LegacyAppStorageMigrationAccountSource;
	readonly currentAccount: AccountRekeyTarget;
	readonly now: number;
	readonly limits?: LegacyAppStorageMigrationLimits;
}

export interface LegacyAppStorageMigrationResult {
	readonly status: LegacyAppStorageMigrationStatus;
	readonly reason: LegacyAppStorageMigrationSkipReason | null;
	readonly contentScope: string | null;
	readonly scopes: ReadonlyArray<string>;
	readonly pendingScopes: ReadonlyArray<string>;
	readonly deferredAccounts: number;
	readonly written: number;
	readonly unclassifiedKeys: ReadonlyArray<string>;
}

interface LegacyAppStorageMigrationMarker {
	readonly version: number;
	readonly status: 'complete' | 'partial';
	readonly pendingScopes: ReadonlyArray<string>;
	readonly deferredAccounts: number;
	readonly contentScope: string | null;
	readonly sharedContentScopes: ReadonlyArray<string>;
	readonly deferredUserIds: ReadonlyArray<string>;
}

interface RestrictedPlan {
	readonly writes: ReadonlyArray<AppStorageWrite>;
	readonly pendingScopes: ReadonlyArray<string>;
}

interface MigrationContext {
	readonly storage: Storage;
	readonly accounts: ReadonlyArray<KeyedStoredAccount>;
	readonly deferredAccounts: ReadonlyArray<StoredAccount>;
	readonly activeScope: string;
}

function byteLength(value: string): number {
	return TEXT_ENCODER.encode(value).length;
}

function writeIdentity(scope: string, key: string): string {
	return JSON.stringify([scope, key]);
}

function accountScopeForUser(
	scopesByUserId: ReadonlyMap<string, ReadonlySet<string>>,
	userId: string,
	contentScope: string | null,
): string | null {
	const scopes = scopesByUserId.get(userId);
	if (scopes === undefined) {
		return null;
	}
	if (scopes.size === 1) {
		return scopes.values().next().value ?? null;
	}
	return contentScope !== null && scopes.has(contentScope) ? contentScope : null;
}

function stageWrite(staged: Map<string, AppStorageWrite>, scope: string, key: string, value: string): void {
	staged.set(writeIdentity(scope, key), {scope, key, value, ifAbsent: true});
}

function compareWrites(left: AppStorageWrite, right: AppStorageWrite): number {
	if (left.scope !== right.scope) {
		return left.scope < right.scope ? -1 : 1;
	}
	if (left.key === right.key) {
		return 0;
	}
	return left.key < right.key ? -1 : 1;
}

function isMigrationOwnedKey(legacyKey: string): boolean {
	return (
		legacyKey === LEGACY_APP_STORAGE_MIGRATION_MARKER_KEY ||
		legacyKey === LEGACY_APP_STORAGE_UNCLASSIFIED_KEYS_KEY ||
		legacyKey === LEGACY_SHARED_CONTENT_REVIEW_KEY ||
		legacyKey === DESKTOP_LEGACY_RAW_STORAGE_SEED_MARKER_KEY ||
		legacyKey === BROWSER_ACCOUNT_FALLBACK_STORAGE_KEY ||
		legacyKey.startsWith(RAW_MIRROR_SCOPE_PREFIX)
	);
}

function isCarriedLegacyKey(legacyKey: string): boolean {
	return !isMigrationOwnedKey(legacyKey) && !isNotMigratedLegacyKey(legacyKey);
}

function readLegacyKeys(storage: Storage): Array<string> {
	const keys: Array<string> = [];
	for (let index = 0; index < storage.length; index += 1) {
		const key = storage.key(index);
		if (key != null && isCarriedLegacyKey(key)) {
			keys.push(key);
		}
	}
	return keys.sort();
}

function stageOwnedSnapshot(staged: Map<string, AppStorageWrite>, account: KeyedStoredAccount): void {
	const snapshot = account.managedStorageData ?? account.localStorageData;
	for (const legacyKey of Object.keys(snapshot).sort()) {
		const value = snapshot[legacyKey];
		if (typeof value !== 'string' || !isCarriedLegacyKey(legacyKey)) {
			continue;
		}
		const match = resolveLegacyAppStorageKey(legacyKey);
		if (match == null) {
			if (!isGlobalAppStorageKey(legacyKey)) {
				stageWrite(staged, account.storageKey, legacyKey, value);
			}
			continue;
		}
		if (match.row.scope === 'global' || (match.userId != null && match.userId !== account.userId)) {
			continue;
		}
		stageWrite(staged, account.storageKey, legacyKey, value);
	}
}

export function buildLegacyAppStorageMigrationPlan(
	input: LegacyAppStorageMigrationPlanInput,
): LegacyAppStorageMigrationPlan {
	const scopes = [UNAUTHENTICATED_APP_STORAGE_SCOPE];
	const scopesByUserId = new Map<string, Set<string>>();
	for (const account of input.accounts) {
		if (!scopes.includes(account.storageKey)) {
			scopes.push(account.storageKey);
		}
		const accountScopes = scopesByUserId.get(account.userId) ?? new Set<string>();
		accountScopes.add(account.storageKey);
		scopesByUserId.set(account.userId, accountScopes);
	}
	const contentScope =
		input.contentScope === null || scopes.includes(input.contentScope)
			? input.contentScope
			: UNAUTHENTICATED_APP_STORAGE_SCOPE;
	const ownedContentScopes = contentScope === null ? [] : [contentScope];
	const sharedContentScopes = [
		...new Set([...ownedContentScopes, ...input.sharedContentScopes.filter((scope) => scopes.includes(scope))]),
	];

	const fannedOut = new Map<string, AppStorageWrite>();
	const perAccount = new Map<string, AppStorageWrite>();
	const unclassifiedKeys: Array<string> = [];

	for (const legacyKey of readLegacyKeys(input.storage)) {
		const value = input.storage.getItem(legacyKey);
		if (value == null) {
			continue;
		}
		const match = resolveLegacyAppStorageKey(legacyKey);
		if (match == null) {
			if (!isGlobalAppStorageKey(legacyKey)) {
				unclassifiedKeys.push(legacyKey);
			}
			stageWrite(fannedOut, GLOBAL_APP_STORAGE_SCOPE, legacyKey, value);
			continue;
		}
		if (match.userId != null) {
			const scope = accountScopeForUser(scopesByUserId, match.userId, contentScope);
			if (scope != null) {
				stageWrite(perAccount, scope, legacyKey, value);
			}
			continue;
		}
		if (match.row.scope === 'global') {
			stageWrite(fannedOut, GLOBAL_APP_STORAGE_SCOPE, legacyKey, value);
			continue;
		}
		if (match.row.fanout === 'content') {
			for (const scope of isLegacyAccountSwappedKey(legacyKey) ? ownedContentScopes : sharedContentScopes) {
				stageWrite(fannedOut, scope, legacyKey, value);
			}
			continue;
		}
		for (const scope of scopes) {
			stageWrite(fannedOut, scope, legacyKey, value);
		}
	}

	for (const account of input.accounts) {
		if (account.storageKey !== contentScope) {
			stageOwnedSnapshot(perAccount, account);
		}
	}

	for (const scope of sharedContentScopes) {
		stageWrite(fannedOut, scope, LEGACY_SHARED_CONTENT_REVIEW_KEY, LegacySharedContentReviewState.PENDING);
	}

	if (unclassifiedKeys.length > 0) {
		stageWrite(
			fannedOut,
			GLOBAL_APP_STORAGE_SCOPE,
			LEGACY_APP_STORAGE_UNCLASSIFIED_KEYS_KEY,
			JSON.stringify(unclassifiedKeys),
		);
	}

	const merged = new Map(fannedOut);
	for (const [identity, write] of perAccount) {
		merged.set(identity, write);
	}
	const writes = [...merged.values()].sort(compareWrites);
	const totalBytes = writes.reduce((total, write) => total + byteLength(write.key) + byteLength(write.value), 0);
	return {scopes, contentScope, writes, unclassifiedKeys, totalBytes};
}

function carryFor(
	marker: LegacyAppStorageMigrationMarker | null,
	context: LegacyContentCarryAccounts,
): LegacyContentCarry {
	const deferredUserIds = [
		...new Set(
			context.deferredAccounts.flatMap((account) =>
				typeof account?.userId === 'string' && (marker === null || marker.deferredUserIds.includes(account.userId))
					? [account.userId]
					: [],
			),
		),
	].sort();
	if (marker === null) {
		return {
			contentScope: context.activeScope,
			sharedContentScopes: context.accounts.map((account) => account.storageKey).sort(),
			deferredUserIds,
		};
	}
	const qualifiedSince = context.accounts
		.filter((account) => marker.deferredUserIds.includes(account.userId))
		.map((account) => account.storageKey);
	return {
		contentScope: marker.contentScope,
		sharedContentScopes: [...new Set([...marker.sharedContentScopes, ...qualifiedSince])].sort(),
		deferredUserIds,
	};
}

export function resolveLegacyContentCarry(
	committedMarkers: ReadonlyArray<string | null>,
	context: LegacyContentCarryAccounts,
): LegacyContentCarry {
	for (const value of committedMarkers) {
		const marker = parseMarker(value);
		if (marker !== null) {
			return carryFor(marker, context);
		}
	}
	return carryFor(null, context);
}

export function completedLegacyAppStorageMigrationMarker(carry: LegacyContentCarry): AppStorageWrite {
	return markerWrite([], 0, carry);
}

function migrationIsComplete(pendingScopes: ReadonlyArray<string>, deferredAccounts: number): boolean {
	return pendingScopes.length === 0 && deferredAccounts === 0;
}

function markerWrite(
	pendingScopes: ReadonlyArray<string>,
	deferredAccounts: number,
	carry: LegacyContentCarry,
): AppStorageWrite {
	const marker: LegacyAppStorageMigrationMarker = {
		version: LEGACY_APP_STORAGE_MIGRATION_VERSION,
		status: migrationIsComplete(pendingScopes, deferredAccounts) ? 'complete' : 'partial',
		pendingScopes,
		deferredAccounts,
		contentScope: carry.contentScope,
		sharedContentScopes: carry.sharedContentScopes,
		deferredUserIds: carry.deferredUserIds,
	};
	return {scope: GLOBAL_APP_STORAGE_SCOPE, key: LEGACY_APP_STORAGE_MIGRATION_MARKER_KEY, value: JSON.stringify(marker)};
}

function stringList(value: unknown): Array<string> | null {
	return Array.isArray(value) ? value.filter((entry): entry is string => typeof entry === 'string') : null;
}

function parseMarker(value: string | null): LegacyAppStorageMigrationMarker | null {
	if (value == null) {
		return null;
	}
	try {
		const parsed: unknown = JSON.parse(value);
		if (parsed == null || typeof parsed !== 'object') {
			return null;
		}
		const {version, status, pendingScopes, deferredAccounts, contentScope, sharedContentScopes, deferredUserIds} =
			parsed as {
				version?: unknown;
				status?: unknown;
				pendingScopes?: unknown;
				deferredAccounts?: unknown;
				contentScope?: unknown;
				sharedContentScopes?: unknown;
				deferredUserIds?: unknown;
			};
		if (typeof version !== 'number' || !Number.isFinite(version)) {
			return null;
		}
		if (
			deferredAccounts !== undefined &&
			(typeof deferredAccounts !== 'number' || !Number.isSafeInteger(deferredAccounts) || deferredAccounts < 0)
		) {
			return null;
		}
		const owner = typeof contentScope === 'string' ? contentScope : null;
		return {
			version,
			status: status === 'partial' ? 'partial' : 'complete',
			pendingScopes: stringList(pendingScopes) ?? [],
			deferredAccounts: deferredAccounts ?? 0,
			contentScope: owner,
			sharedContentScopes: stringList(sharedContentScopes) ?? (owner === null ? [] : [owner]),
			deferredUserIds: stringList(deferredUserIds) ?? [],
		};
	} catch (error) {
		logger.warn('Discarding an unreadable legacy migration marker', error);
		return null;
	}
}

async function readMarker(backend: PersistentStorageBackend): Promise<LegacyAppStorageMigrationMarker | null> {
	const entry = await backend.get(GLOBAL_APP_STORAGE_SCOPE, LEGACY_APP_STORAGE_MIGRATION_MARKER_KEY);
	return parseMarker(entry?.value ?? null);
}

function restrictPlanToActiveScopes(plan: LegacyAppStorageMigrationPlan, activeScope: string): RestrictedPlan {
	const kept = new Set([GLOBAL_APP_STORAGE_SCOPE, UNAUTHENTICATED_APP_STORAGE_SCOPE, activeScope, plan.contentScope]);
	return {
		writes: plan.writes.filter((write) => kept.has(write.scope)),
		pendingScopes: plan.scopes.filter((scope) => !kept.has(scope)),
	};
}

function skippedResult(reason: LegacyAppStorageMigrationSkipReason): LegacyAppStorageMigrationResult {
	return {
		status: LegacyAppStorageMigrationStatus.SKIPPED,
		reason,
		contentScope: UNAUTHENTICATED_APP_STORAGE_SCOPE,
		scopes: [],
		pendingScopes: [],
		deferredAccounts: 0,
		written: 0,
		unclassifiedKeys: [],
	};
}

function failedResult(): LegacyAppStorageMigrationResult {
	return {
		status: LegacyAppStorageMigrationStatus.FAILED,
		reason: null,
		contentScope: UNAUTHENTICATED_APP_STORAGE_SCOPE,
		scopes: [],
		pendingScopes: [],
		deferredAccounts: 0,
		written: 0,
		unclassifiedKeys: [],
	};
}

async function resolveMigrationContext(
	request: LegacyAppStorageMigrationRequest,
): Promise<MigrationContext | LegacyAppStorageMigrationSkipReason> {
	const storage = getProtectedLocalStorage();
	if (storage == null) {
		return LegacyAppStorageMigrationSkipReason.NO_LOCAL_STORAGE;
	}
	const currentAccount = request.currentAccount;
	if (runtimeInstanceKey(currentAccount.instance) === null) {
		return LegacyAppStorageMigrationSkipReason.NO_INSTANCE_KEY;
	}
	const activeScope = accountStorageKey(currentAccount.userId, currentAccount.instance);
	if (activeScope === null) {
		return LegacyAppStorageMigrationSkipReason.NO_INSTANCE_KEY;
	}
	const outcome = await request.accounts.migrateAccountStorageKeys({target: currentAccount, now: request.now});
	if (outcome.source !== 'idb') {
		logger.warn('Refusing the legacy migration because the account list is not authoritative', outcome.status);
		return LegacyAppStorageMigrationSkipReason.ACCOUNTS_NOT_AUTHORITATIVE;
	}
	if (outcome.status !== 'complete' && outcome.status !== 'deferred') {
		logger.warn('Refusing the legacy migration because account qualification failed', outcome.status);
		return LegacyAppStorageMigrationSkipReason.ACCOUNTS_NOT_AUTHORITATIVE;
	}
	if (
		outcome.qualifiedRecords.some(
			(account) =>
				account.instance == null ||
				runtimeInstanceKey(account.instance) === null ||
				accountStorageKey(account.userId, account.instance) !== account.storageKey,
		)
	) {
		logger.warn('Refusing the legacy migration because a qualified account has an invalid storage identity');
		return LegacyAppStorageMigrationSkipReason.ACCOUNTS_NOT_AUTHORITATIVE;
	}
	const activeAccount = outcome.qualifiedRecords.find((account) => account.storageKey === activeScope);
	if (
		activeAccount === undefined ||
		activeAccount.userId !== currentAccount.userId ||
		activeAccount.token !== currentAccount.token
	) {
		logger.warn('Refusing the legacy migration because the active AuthSession account was not qualified exactly');
		return LegacyAppStorageMigrationSkipReason.ACCOUNTS_NOT_AUTHORITATIVE;
	}
	return {
		storage,
		accounts: outcome.qualifiedRecords,
		deferredAccounts: outcome.deferredRecords,
		activeScope,
	};
}

function planFor(context: MigrationContext, carry: LegacyContentCarry): LegacyAppStorageMigrationPlan {
	return buildLegacyAppStorageMigrationPlan({
		storage: context.storage,
		accounts: context.accounts,
		contentScope: carry.contentScope,
		sharedContentScopes: carry.sharedContentScopes,
	});
}

async function commitPlan(
	request: LegacyAppStorageMigrationRequest,
	plan: LegacyAppStorageMigrationPlan,
	activeScope: string,
	deferredAccounts: number,
	carry: LegacyContentCarry,
): Promise<LegacyAppStorageMigrationResult> {
	const commit = (attempt: RestrictedPlan): Promise<void> =>
		request.backend.setMany([
			...attempt.writes,
			markerWrite(attempt.pendingScopes, deferredAccounts, {...carry, contentScope: plan.contentScope}),
		]);
	const limits = request.limits;
	const overflows =
		limits != null && (plan.totalBytes > limits.maxTotalBytes || plan.writes.length > limits.maxEntries);
	if (overflows) {
		logger.warn(
			'Legacy migration exceeds the storage caps, seeding the active scope only',
			plan.totalBytes,
			plan.writes.length,
		);
	}
	let attempt: RestrictedPlan = overflows
		? restrictPlanToActiveScopes(plan, activeScope)
		: {writes: plan.writes, pendingScopes: []};
	try {
		await commit(attempt);
	} catch (error) {
		if (overflows || !(error instanceof AppStorageQuotaExceededError)) {
			throw error;
		}
		logger.warn('Legacy migration exceeded the storage quota, seeding the active scope only', error);
		attempt = restrictPlanToActiveScopes(plan, activeScope);
		await commit(attempt);
	}
	return {
		status: migrationIsComplete(attempt.pendingScopes, deferredAccounts)
			? LegacyAppStorageMigrationStatus.MIGRATED
			: LegacyAppStorageMigrationStatus.PARTIAL,
		reason: null,
		contentScope: plan.contentScope,
		scopes: plan.scopes,
		pendingScopes: attempt.pendingScopes,
		deferredAccounts,
		written: attempt.writes.length,
		unclassifiedKeys: plan.unclassifiedKeys,
	};
}

async function runElectedMigration(
	request: LegacyAppStorageMigrationRequest,
): Promise<LegacyAppStorageMigrationResult> {
	try {
		if (request.backend.kind !== PersistentStorageBackendKind.INDEXED_DB) {
			return skippedResult(LegacyAppStorageMigrationSkipReason.MEMORY_BACKEND);
		}
		const marker = await readMarker(request.backend);
		if (marker != null && marker.version >= LEGACY_APP_STORAGE_MIGRATION_VERSION && marker.deferredAccounts === 0) {
			return {
				status: LegacyAppStorageMigrationStatus.ALREADY_DONE,
				reason: null,
				contentScope: UNAUTHENTICATED_APP_STORAGE_SCOPE,
				scopes: [],
				pendingScopes: marker.pendingScopes,
				deferredAccounts: 0,
				written: 0,
				unclassifiedKeys: [],
			};
		}
		const context = await resolveMigrationContext(request);
		if (typeof context === 'string') {
			return skippedResult(context);
		}
		const carry = carryFor(marker, context);
		return await commitPlan(
			request,
			planFor(context, carry),
			context.activeScope,
			context.deferredAccounts.length,
			carry,
		);
	} catch (error) {
		if (error instanceof BrowserAccountStorageUnavailableError) {
			throw error;
		}
		logger.error('Legacy localStorage migration failed, so the marker was not written', error);
		return failedResult();
	}
}

async function completeElectedScope(
	request: LegacyAppStorageMigrationRequest,
	scope: string,
): Promise<LegacyAppStorageMigrationResult> {
	try {
		if (request.backend.kind !== PersistentStorageBackendKind.INDEXED_DB) {
			return skippedResult(LegacyAppStorageMigrationSkipReason.MEMORY_BACKEND);
		}
		if (scope === GLOBAL_APP_STORAGE_SCOPE || scope === UNAUTHENTICATED_APP_STORAGE_SCOPE) {
			return skippedResult(LegacyAppStorageMigrationSkipReason.SCOPE_NOT_PENDING);
		}
		const marker = await readMarker(request.backend);
		if (marker == null || marker.status !== 'partial' || !marker.pendingScopes.includes(scope)) {
			return skippedResult(LegacyAppStorageMigrationSkipReason.SCOPE_NOT_PENDING);
		}
		const context = await resolveMigrationContext(request);
		if (typeof context === 'string') {
			return skippedResult(context);
		}
		const carry = carryFor(marker, context);
		const plan = planFor(context, carry);
		const writes = plan.writes.filter((write) => write.scope === scope);
		if (writes.length === 0) {
			return skippedResult(LegacyAppStorageMigrationSkipReason.SCOPE_NOT_PENDING);
		}
		const pendingScopes = marker.pendingScopes.filter((pending) => pending !== scope);
		const deferredAccounts = context.deferredAccounts.length;
		await request.backend.setMany([...writes, markerWrite(pendingScopes, deferredAccounts, carry)]);
		return {
			status: migrationIsComplete(pendingScopes, deferredAccounts)
				? LegacyAppStorageMigrationStatus.MIGRATED
				: LegacyAppStorageMigrationStatus.PARTIAL,
			reason: null,
			contentScope: plan.contentScope,
			scopes: plan.scopes,
			pendingScopes,
			deferredAccounts,
			written: writes.length,
			unclassifiedKeys: [],
		};
	} catch (error) {
		if (error instanceof BrowserAccountStorageUnavailableError) {
			throw error;
		}
		logger.error('Completing a pending legacy migration scope failed', scope, error);
		return failedResult();
	}
}

export function runLegacyAppStorageMigration(
	request: LegacyAppStorageMigrationRequest,
): Promise<LegacyAppStorageMigrationResult> {
	return withAppStorageLock(APP_STORAGE_MIGRATION_LOCK_NAME, () => runElectedMigration(request));
}

export function completePendingLegacyAppStorageScope(
	request: LegacyAppStorageMigrationRequest,
	scope: string,
): Promise<LegacyAppStorageMigrationResult> {
	return withAppStorageLock(APP_STORAGE_MIGRATION_LOCK_NAME, () => completeElectedScope(request, scope));
}
