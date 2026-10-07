// SPDX-License-Identifier: AGPL-3.0-or-later

import {getProtectedSessionStorage} from '@app/features/platform/state/ProtectedWebStorage';

export const DEFAULT_LAZY_MODULE_LOAD_ATTEMPTS = 3;
export const DEFAULT_LAZY_MODULE_RETRY_DELAY_MS = 250;
export const MAX_LAZY_MODULE_RETRY_DELAY_MS = 2000;

const RECOVERY_RELOAD_STORAGE_KEY = 'fluxer.lazyModuleRecoveryReload';

let recoveryReloadUsed = false;

export interface LazyModuleLoadOptions {
	attempts?: number;
	retryDelayMs?: number;
	shouldRetry?: (error: unknown) => boolean;
}

function errorText(error: unknown): string {
	if (error instanceof Error) {
		return `${error.name} ${error.message}`.toLowerCase();
	}
	return String(error).toLowerCase();
}

export function isLazyModuleLoadError(error: unknown): boolean {
	const text = errorText(error);
	return (
		text.includes('chunkloaderror') ||
		text.includes('loading chunk') ||
		text.includes('loading css chunk') ||
		text.includes('failed to fetch dynamically imported module') ||
		text.includes('error loading dynamically imported module') ||
		text.includes('importing a module script failed')
	);
}

export function isBrowserOffline(): boolean {
	return typeof navigator !== 'undefined' && navigator.onLine === false;
}

function readRecoveryReloadMark(): string | null | undefined {
	const storage = getProtectedSessionStorage();
	if (!storage) {
		return undefined;
	}
	try {
		return storage.getItem(RECOVERY_RELOAD_STORAGE_KEY);
	} catch {
		return undefined;
	}
}

function hasUsedRecoveryReload(): boolean {
	if (recoveryReloadUsed) {
		return true;
	}
	const mark = readRecoveryReloadMark();
	if (mark === undefined) {
		return true;
	}
	return mark !== null;
}

function markRecoveryReloadUsed(): boolean {
	recoveryReloadUsed = true;
	const storage = getProtectedSessionStorage();
	if (!storage) {
		return false;
	}
	try {
		storage.setItem(RECOVERY_RELOAD_STORAGE_KEY, String(Date.now()));
		return true;
	} catch {
		return false;
	}
}

export function canAttemptLazyModuleRecoveryReload(): boolean {
	if (typeof window === 'undefined') {
		return false;
	}
	if (isBrowserOffline()) {
		return false;
	}
	return !hasUsedRecoveryReload();
}

export function attemptLazyModuleRecoveryReload(): boolean {
	if (!canAttemptLazyModuleRecoveryReload()) {
		return false;
	}
	if (!markRecoveryReloadUsed()) {
		return false;
	}
	window.location.reload();
	return true;
}

function resolveAttemptCount(attempts?: number): number {
	if (attempts === undefined) {
		return DEFAULT_LAZY_MODULE_LOAD_ATTEMPTS;
	}
	return Math.max(1, Math.floor(attempts));
}

function resolveRetryDelay(baseDelayMs: number, attempt: number): number {
	return Math.min(baseDelayMs * 2 ** (attempt - 1), MAX_LAZY_MODULE_RETRY_DELAY_MS);
}

function waitForRetryDelay(delayMs: number): Promise<void> {
	if (delayMs <= 0) {
		return Promise.resolve();
	}
	return new Promise((resolve) => {
		setTimeout(resolve, delayMs);
	});
}

export async function loadLazyModule<Module>(
	load: () => Promise<Module>,
	options: LazyModuleLoadOptions = {},
): Promise<Module> {
	const attempts = resolveAttemptCount(options.attempts);
	const baseRetryDelayMs = options.retryDelayMs ?? DEFAULT_LAZY_MODULE_RETRY_DELAY_MS;
	const shouldRetry = options.shouldRetry ?? isLazyModuleLoadError;

	for (let attempt = 1; attempt <= attempts; attempt += 1) {
		try {
			return await load();
		} catch (error) {
			if (attempt >= attempts || !shouldRetry(error) || isBrowserOffline()) {
				throw error;
			}
			await waitForRetryDelay(resolveRetryDelay(baseRetryDelayMs, attempt));
		}
	}

	throw new Error('Lazy module loader exhausted without a result');
}
