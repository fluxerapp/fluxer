// SPDX-License-Identifier: AGPL-3.0-or-later

import {createRequire} from 'node:module';
import type {
	AppStoreAccount,
	AppStoreAccountCompareAndSwapRequest,
	AppStoreAccountImport,
	AppStoreAccountImportReport,
	AppStoreBinding,
	AppStoreClearStoreExceptRequest,
	AppStoreEntry,
	AppStoreEntryImport,
	AppStoreEntryImportReport,
	AppStoreInitialization,
	AppStoreKnownInstance,
	AppStoreOptions,
	AppStorePruneReport,
	AppStorePruneRequest,
} from '@fluxer/app-store';

export const APP_STORE_ADDON_PACKAGE = '@fluxer/app-store';

type AppStoreBindingConstructor = new (options: AppStoreOptions) => AppStoreBinding;

interface AppStoreEntryAddress {
	store: string;
	scope: string;
	key: string;
}

export interface AppStoreBoundary {
	readonly initialization: AppStoreInitialization;
	close: () => void;
	getMetadata: (request: {key: string}) => Promise<string | null>;
	setMetadata: (request: {key: string; value: string}) => Promise<void>;
	getAllAccounts: () => Promise<Array<AppStoreAccount>>;
	getAccount: (request: {storageKey: string}) => Promise<AppStoreAccount | null>;
	upsertAccount: (request: AppStoreAccount) => Promise<void>;
	compareAndSwapAccount: (request: AppStoreAccountCompareAndSwapRequest) => Promise<boolean>;
	deleteAccount: (request: {storageKey: string}) => Promise<void>;
	importAccounts: (request: AppStoreAccountImport) => Promise<AppStoreAccountImportReport>;
	getAllKnownInstances: () => Promise<Array<AppStoreKnownInstance>>;
	upsertKnownInstance: (request: AppStoreKnownInstance) => Promise<void>;
	deleteKnownInstance: (request: {key: string}) => Promise<void>;
	getEntries: (request: {store: string; scope: string}) => Promise<Array<AppStoreEntry>>;
	getEntry: (request: AppStoreEntryAddress) => Promise<AppStoreEntry | null>;
	setEntry: (request: AppStoreEntry) => Promise<void>;
	deleteEntry: (request: AppStoreEntryAddress) => Promise<void>;
	clearScope: (request: {scope: string}) => Promise<void>;
	clearStoreExcept: (request: AppStoreClearStoreExceptRequest) => Promise<void>;
	importEntries: (request: AppStoreEntryImport) => Promise<AppStoreEntryImportReport>;
	prune: (request: AppStorePruneRequest) => Promise<AppStorePruneReport>;
}

export type AppStoreBoundaryFactory = (options: AppStoreOptions) => AppStoreBoundary | null;

type AppStoreNativeModule = {
	AppStore: AppStoreBindingConstructor | null;
	loadError: Error | null;
};

const requireModule = createRequire(import.meta.url);

const ADDON_LOAD_FAILURE_MAX_LENGTH = 300;

let bindingCache: AppStoreBindingConstructor | null | undefined;
let bindingLoadFailure: string | null = null;

function describeAddonLoadFailure(error: unknown): string {
	if (error == null) {
		return 'the app store addon did not export a binding';
	}
	const message = error instanceof Error ? error.message : String(error);
	const reason = /^reason=(.+)$/mu.exec(message)?.[1]?.trim();
	const summary = (reason ?? message.split('\n')[0] ?? '').trim();
	if (summary.length === 0) {
		return 'the app store addon failed to load';
	}
	return summary.length > ADDON_LOAD_FAILURE_MAX_LENGTH
		? `${summary.slice(0, ADDON_LOAD_FAILURE_MAX_LENGTH - 1)}\u2026`
		: summary;
}

export function loadAppStoreBinding(): AppStoreBindingConstructor | null {
	if (bindingCache !== undefined) return bindingCache;
	try {
		const native = requireModule(APP_STORE_ADDON_PACKAGE) as AppStoreNativeModule;
		bindingCache = native.AppStore ?? null;
		bindingLoadFailure = bindingCache == null ? describeAddonLoadFailure(native.loadError) : null;
	} catch (error) {
		bindingCache = null;
		bindingLoadFailure = describeAddonLoadFailure(error);
	}
	return bindingCache;
}

export function getAppStoreLoadFailure(): string | null {
	loadAppStoreBinding();
	return bindingLoadFailure;
}

async function decode<T>(result: Promise<string>): Promise<T> {
	return JSON.parse(await result) as T;
}

export function createAppStoreBoundary(
	options: AppStoreOptions,
	binding: AppStoreBindingConstructor | null = loadAppStoreBinding(),
): AppStoreBoundary | null {
	if (!binding) return null;
	const store = new binding(options);
	return {
		initialization: JSON.parse(store.initialization) as AppStoreInitialization,
		close: () => {
			store.close();
		},
		getMetadata: ({key}) => decode<string | null>(store.getMetadata(key)),
		setMetadata: ({key, value}) => store.setMetadata(key, value),
		getAllAccounts: () => decode<Array<AppStoreAccount>>(store.getAllAccounts()),
		getAccount: ({storageKey}) => decode<AppStoreAccount | null>(store.getAccount(storageKey)),
		upsertAccount: (request) => store.upsertAccount(JSON.stringify(request)),
		compareAndSwapAccount: (request) => store.compareAndSwapAccount(JSON.stringify(request)),
		deleteAccount: ({storageKey}) => store.deleteAccount(storageKey),
		importAccounts: (request) => decode<AppStoreAccountImportReport>(store.importAccounts(JSON.stringify(request))),
		getAllKnownInstances: () => decode<Array<AppStoreKnownInstance>>(store.getAllKnownInstances()),
		upsertKnownInstance: (request) => store.upsertKnownInstance(JSON.stringify(request)),
		deleteKnownInstance: ({key}) => store.deleteKnownInstance(key),
		getEntries: ({store: entryStore, scope}) => decode<Array<AppStoreEntry>>(store.getEntries(entryStore, scope)),
		getEntry: ({store: entryStore, scope, key}) => decode<AppStoreEntry | null>(store.getEntry(entryStore, scope, key)),
		setEntry: (request) => store.setEntry(JSON.stringify(request)),
		deleteEntry: ({store: entryStore, scope, key}) => store.deleteEntry(entryStore, scope, key),
		clearScope: ({scope}) => store.clearScope(scope),
		clearStoreExcept: (request) => store.clearStoreExcept(JSON.stringify(request)),
		importEntries: (request) => decode<AppStoreEntryImportReport>(store.importEntries(JSON.stringify(request))),
		prune: (request) => decode<AppStorePruneReport>(store.prune(JSON.stringify(request))),
	};
}
