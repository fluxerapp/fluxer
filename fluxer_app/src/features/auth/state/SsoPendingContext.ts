// SPDX-License-Identifier: AGPL-3.0-or-later

import {type RuntimeConfigSnapshot, runtimeInstanceKey} from '@app/features/app/state/InstanceSnapshotStore';
import {safeRedirectTarget} from '@app/features/auth/utils/SafeRedirect';
import {isSsoPendingContextKey, ssoPendingContextKey} from '@app/features/platform/state/AppStorageKeys';
import AppStorage, {flushAppStorageWrites} from '@app/features/platform/state/PersistentStorage';
import {getProtectedSessionStorage} from '@app/features/platform/state/ProtectedWebStorage';
import {Logger} from '@app/features/platform/utils/AppLogger';

const PENDING_SSO_CONTEXT_TTL_MS = 30 * 60 * 1000;
const PENDING_SSO_CONTEXT_MAX_ENTRIES = 16;
const PENDING_SSO_CONTEXT_JSON_MAX_LENGTH = 262_144;
const OAUTH_STATE_MAX_LENGTH = 512;
const STORED_SSO_PENDING_CONTEXT_KEYS: ReadonlyArray<string> = ['createdAtMs', 'expiresAtMs', 'instance', 'redirectTo'];

export const LEGACY_SSO_REDIRECT_TO_STORAGE_KEY = 'fluxer:sso:redirect_to';

const logger = new Logger('SsoPendingContext');

export interface SsoPendingContext {
	readonly redirectTo: string | null;
	readonly runtimeSnapshot: RuntimeConfigSnapshot | null;
}

interface StoredSsoPendingContext extends SsoPendingContext {
	readonly createdAtMs: number;
}

interface StoredSsoPendingContextEntry {
	readonly key: string;
	readonly context: StoredSsoPendingContext;
}

function readLegacySsoRedirectTo(): string | null {
	const storage = getProtectedSessionStorage();
	if (storage === null) {
		return null;
	}
	try {
		return storage.getItem(LEGACY_SSO_REDIRECT_TO_STORAGE_KEY);
	} catch (error) {
		logger.warn('Failed to read the legacy SSO redirect target', error);
		return null;
	}
}

export function clearLegacySsoRedirectTo(): void {
	const storage = getProtectedSessionStorage();
	if (storage === null) {
		return;
	}
	try {
		storage.removeItem(LEGACY_SSO_REDIRECT_TO_STORAGE_KEY);
	} catch (error) {
		logger.warn('Failed to clear the legacy SSO redirect target', error);
	}
}

export function isValidOAuthState(state: string): boolean {
	if (state.length === 0 || state.length > OAUTH_STATE_MAX_LENGTH) {
		return false;
	}
	for (let index = 0; index < state.length; index += 1) {
		const code = state.charCodeAt(index);
		if (code < 0x21 || code > 0x7e) {
			return false;
		}
	}
	return true;
}

function isPlainRecord(value: unknown): value is Record<string, unknown> {
	return typeof value === 'object' && value != null && !Array.isArray(value);
}

function isExactRecord(value: unknown, expectedKeys: ReadonlyArray<string>): value is Record<string, unknown> {
	if (!isPlainRecord(value)) {
		return false;
	}
	if (Object.keys(value).length !== expectedKeys.length) {
		return false;
	}
	return expectedKeys.every((key) => Object.hasOwn(value, key));
}

function parseStoredSnapshot(value: unknown): RuntimeConfigSnapshot | null | undefined {
	if (value == null) {
		return null;
	}
	if (!isPlainRecord(value)) {
		return undefined;
	}
	const snapshot = value as unknown as RuntimeConfigSnapshot;
	if (typeof snapshot.apiEndpoint !== 'string' || runtimeInstanceKey(snapshot) == null) {
		return undefined;
	}
	return snapshot;
}

function parseStoredRedirect(value: unknown): string | null | undefined {
	if (value == null) {
		return null;
	}
	if (typeof value !== 'string') {
		return undefined;
	}
	const redirectTo = safeRedirectTarget(value);
	if (redirectTo == null || redirectTo !== value) {
		return undefined;
	}
	return redirectTo;
}

function parseStoredContext(raw: string, nowMs: number): StoredSsoPendingContext | null {
	if (raw.length > PENDING_SSO_CONTEXT_JSON_MAX_LENGTH) {
		return null;
	}
	let value: unknown;
	try {
		value = JSON.parse(raw);
	} catch {
		return null;
	}
	if (!isExactRecord(value, STORED_SSO_PENDING_CONTEXT_KEYS)) {
		return null;
	}
	const createdAtMs = value['createdAtMs'];
	const expiresAtMs = value['expiresAtMs'];
	if (typeof createdAtMs !== 'number' || !Number.isSafeInteger(createdAtMs) || createdAtMs <= 0) {
		return null;
	}
	if (typeof expiresAtMs !== 'number' || expiresAtMs !== createdAtMs + PENDING_SSO_CONTEXT_TTL_MS) {
		return null;
	}
	if (expiresAtMs <= nowMs) {
		return null;
	}
	const runtimeSnapshot = parseStoredSnapshot(value['instance']);
	if (runtimeSnapshot === undefined) {
		return null;
	}
	const redirectTo = parseStoredRedirect(value['redirectTo']);
	if (redirectTo === undefined) {
		return null;
	}
	return {createdAtMs, redirectTo, runtimeSnapshot};
}

function removeStoredContext(key: string): void {
	try {
		AppStorage.removeItem(key);
	} catch (error) {
		logger.warn('Failed to remove a stored SSO pending context', error);
	}
}

function compareStoredContextEntries(left: StoredSsoPendingContextEntry, right: StoredSsoPendingContextEntry): number {
	if (left.context.createdAtMs !== right.context.createdAtMs) {
		return right.context.createdAtMs - left.context.createdAtMs;
	}
	return left.key < right.key ? -1 : 1;
}

function pruneStoredContexts(retainedEntries: number, excludedKey: string | null): Map<string, SsoPendingContext> {
	const nowMs = Date.now();
	const entries: Array<StoredSsoPendingContextEntry> = [];
	for (const key of AppStorage.keys()) {
		if (!isSsoPendingContextKey(key) || key === excludedKey) {
			continue;
		}
		const raw = AppStorage.getItem(key);
		const context = raw == null ? null : parseStoredContext(raw, nowMs);
		if (context == null) {
			removeStoredContext(key);
			continue;
		}
		entries.push({key, context});
	}
	entries.sort(compareStoredContextEntries);
	for (const entry of entries.slice(retainedEntries)) {
		removeStoredContext(entry.key);
	}
	return new Map(
		entries
			.slice(0, retainedEntries)
			.map((entry) => [
				entry.key,
				{redirectTo: entry.context.redirectTo, runtimeSnapshot: entry.context.runtimeSnapshot},
			]),
	);
}

function serializeStoredContext(createdAtMs: number, context: SsoPendingContext): string {
	let redirectTo: string | null = null;
	if (context.redirectTo != null) {
		redirectTo = safeRedirectTarget(context.redirectTo);
		if (redirectTo == null) {
			throw new Error('SSO pending context received an unsafe redirect target');
		}
	}
	const serialized = JSON.stringify({
		createdAtMs,
		expiresAtMs: createdAtMs + PENDING_SSO_CONTEXT_TTL_MS,
		instance: context.runtimeSnapshot,
		redirectTo,
	});
	if (serialized.length > PENDING_SSO_CONTEXT_JSON_MAX_LENGTH) {
		throw new Error('SSO pending context exceeds its storage contract');
	}
	return serialized;
}

export async function storeSsoPendingContext(state: string, context: SsoPendingContext): Promise<void> {
	if (!isValidOAuthState(state)) {
		throw new Error('SSO start returned an invalid OAuth state');
	}
	const key = ssoPendingContextKey(state);
	const serialized = serializeStoredContext(Date.now(), context);
	pruneStoredContexts(PENDING_SSO_CONTEXT_MAX_ENTRIES - 1, key);
	AppStorage.setItem(key, serialized);
	await flushAppStorageWrites();
}

export function getSsoPendingContext(state: string): SsoPendingContext | null {
	const legacyRedirectTo = safeRedirectTarget(readLegacySsoRedirectTo());
	if (!isValidOAuthState(state)) {
		return legacyRedirectTo == null ? null : {redirectTo: legacyRedirectTo, runtimeSnapshot: null};
	}
	const context = pruneStoredContexts(PENDING_SSO_CONTEXT_MAX_ENTRIES, null).get(ssoPendingContextKey(state));
	if (context != null) {
		return context;
	}
	if (legacyRedirectTo == null) {
		return null;
	}
	return {redirectTo: legacyRedirectTo, runtimeSnapshot: null};
}

export function consumeSsoPendingContext(state: string): SsoPendingContext | null {
	const context = getSsoPendingContext(state);
	if (isValidOAuthState(state)) {
		removeStoredContext(ssoPendingContextKey(state));
	}
	clearLegacySsoRedirectTo();
	return context;
}
