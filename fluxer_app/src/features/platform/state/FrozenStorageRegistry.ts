// SPDX-License-Identifier: AGPL-3.0-or-later

export const FrozenStorageKind = Object.freeze({
	INDEXED_DB: 'indexed-db',
	LOCAL_STORAGE: 'local-storage',
	SESSION_STORAGE: 'session-storage',
	DESKTOP_DIRECTORY: 'desktop-directory',
	DESKTOP_FILE: 'desktop-file',
} as const);

export type FrozenStorageKind = (typeof FrozenStorageKind)[keyof typeof FrozenStorageKind];

export const FrozenStorageDeletableAt = Object.freeze({
	NEVER: 'never',
	NEXT_RELEASE: 'N+1',
	RELEASE_AFTER_NEXT: 'N+2',
} as const);

export type FrozenStorageDeletableAt = (typeof FrozenStorageDeletableAt)[keyof typeof FrozenStorageDeletableAt];

export interface FrozenStorageWriter {
	readonly module: string;
	readonly marker: string;
}

export interface FrozenStorageEntry {
	readonly id: string;
	readonly kind: FrozenStorageKind;
	readonly names: ReadonlyArray<string>;
	readonly aliases: ReadonlyArray<string>;
	readonly version: number | null;
	readonly deletableAt: FrozenStorageDeletableAt;
	readonly writers: ReadonlyArray<FrozenStorageWriter>;
	readonly clearableBy: ReadonlyArray<string>;
	readonly note: string;
}

export const EVICTION_TARGET_PREFIX = 'state-cache/';

export const EVICTION_SYMBOLS: ReadonlyArray<string> = Object.freeze(['stateEvict', 'evictStateCache', 'evictScope']);

const SSO_PENDING_CONTEXT = 'fluxer_app/src/features/auth/state/SsoPendingContext.ts';
const ACCOUNT_STORAGE = 'fluxer_app/src/features/auth/state/BrowserAccountStorageRepository.ts';
const PREBOOT_MIRROR = 'fluxer_app/src/features/platform/state/PrebootMirror.ts';
const RESET_CLIENT_STATE = 'fluxer_app/src/features/platform/state/ResetClientState.ts';
const SESSION_CREDENTIAL_MIRROR = 'fluxer_app/src/features/platform/state/auth_session/SessionCredentialMirror.ts';

export const FROZEN_STORAGE_REGISTRY: ReadonlyArray<FrozenStorageEntry> = Object.freeze([
	{
		id: 'idb.fluxer-accounts',
		kind: FrozenStorageKind.INDEXED_DB,
		names: ['FluxerAccounts'],
		aliases: [],
		version: 2,
		deletableAt: FrozenStorageDeletableAt.NEVER,
		writers: [
			{module: ACCOUNT_STORAGE, marker: 'transaction.store.put(record)'},
			{module: ACCOUNT_STORAGE, marker: 'transaction.store.put(normalized)'},
		],
		clearableBy: [],
		note: 'Every stored account of every deployed user. Pinned at v2 by R1: a bump reruns onupgradeneeded against stores this release did not create.',
	},
	{
		id: 'idb.fluxer-custom-sounds',
		kind: FrozenStorageKind.INDEXED_DB,
		names: ['FluxerCustomSounds'],
		aliases: [],
		version: 2,
		deletableAt: FrozenStorageDeletableAt.NEVER,
		writers: [
			{module: 'fluxer_app/src/features/notification/utils/CustomSoundDB.ts', marker: 'store.put(customSound)'},
		],
		clearableBy: [],
		note: 'User-uploaded notification sounds. Origin-partitioned and never re-uploadable once dropped.',
	},
	{
		id: 'idb.fluxer-theme-library',
		kind: FrozenStorageKind.INDEXED_DB,
		names: ['fluxer-theme-library'],
		aliases: [],
		version: 1,
		deletableAt: FrozenStorageDeletableAt.NEVER,
		writers: [{module: 'fluxer_app/src/features/theme/utils/ThemeLibraryDb.ts', marker: 'store.put(asset)'}],
		clearableBy: [],
		note: 'Custom theme assets (binary), authored by the user and stored nowhere else. Theme definitions, enabled ids and local file metadata moved to durable AppStorage.',
	},
	{
		id: 'idb.fluxer-voice-stats',
		kind: FrozenStorageKind.INDEXED_DB,
		names: ['FluxerVoiceStats'],
		aliases: [],
		version: 1,
		deletableAt: FrozenStorageDeletableAt.NEVER,
		writers: [{module: 'fluxer_app/src/features/voice/diagnostics/VoiceStatsDB.ts', marker: 'store.put(entry)'}],
		clearableBy: [],
		note: 'Voice diagnostics history, the only record of a call that already happened.',
	},
	{
		id: 'local.session-user-id',
		kind: FrozenStorageKind.LOCAL_STORAGE,
		names: ['userId'],
		aliases: ['AuthSessionStorageKey.UserId', 'AppStorageKey.AUTH_SESSION_USER_ID'],
		version: null,
		deletableAt: FrozenStorageDeletableAt.NEVER,
		writers: [
			{module: SESSION_CREDENTIAL_MIRROR, marker: 'this.writeValue(AuthSessionStorageKey.UserId, mirror.userId)'},
			{module: SESSION_CREDENTIAL_MIRROR, marker: 'this.storage.setItem(key, value)'},
		],
		clearableBy: [SESSION_CREDENTIAL_MIRROR, RESET_CLIENT_STATE],
		note: 'Never deleted, in any release: AuthSession restores the credential mirror before an account scope is active.',
	},
	{
		id: 'local.session-token',
		kind: FrozenStorageKind.LOCAL_STORAGE,
		names: ['token'],
		aliases: ['AuthSessionStorageKey.Token', 'AppStorageKey.AUTH_SESSION_TOKEN'],
		version: null,
		deletableAt: FrozenStorageDeletableAt.NEXT_RELEASE,
		writers: [
			{module: SESSION_CREDENTIAL_MIRROR, marker: 'this.writeValue(AuthSessionStorageKey.Token, mirror.token)'},
			{module: SESSION_CREDENTIAL_MIRROR, marker: 'this.storage.setItem(key, value)'},
		],
		clearableBy: [SESSION_CREDENTIAL_MIRROR, RESET_CLIENT_STATE],
		note: 'Retained until R2 stops writing it and deletes AuthSessionStorageKey.Token.',
	},
	{
		id: 'local.gateway-preboot-session',
		kind: FrozenStorageKind.LOCAL_STORAGE,
		names: ['fluxer:gateway:preboot:session'],
		aliases: ['GATEWAY_PREBOOT_SESSION_STORAGE_KEY'],
		version: null,
		deletableAt: FrozenStorageDeletableAt.NEVER,
		writers: [{module: PREBOOT_MIRROR, marker: 'writeRawStorageItem(GATEWAY_PREBOOT_SESSION_STORAGE_KEY'}],
		clearableBy: [PREBOOT_MIRROR, RESET_CLIENT_STATE],
		note: 'The boolean marker R2 switches index.html onto. It replaces a raw token read, so it must already be written before R2 ships.',
	},
	{
		id: 'local.deployed-build-leftovers',
		kind: FrozenStorageKind.LOCAL_STORAGE,
		names: ['runtimeConfig', 'AccountManager'],
		aliases: [],
		version: null,
		deletableAt: FrozenStorageDeletableAt.NEVER,
		writers: [],
		clearableBy: [RESET_CLIENT_STATE],
		note: 'Written only by the deployed build and read by nothing here. Inert, not preserved for a downgrade, but still deployed user data that only a reset may clear.',
	},
	{
		id: 'local.frozen-preference-keys',
		kind: FrozenStorageKind.LOCAL_STORAGE,
		names: [
			'UserSettings:syncedPreferencesWire',
			'UserSettings:syncedPreferencesDirtyFields',
			'UserSettings:syncedPreferencesRecentAck',
			'PremiumCheckoutReturnIntent',
			'userEntranceSound',
			'entranceSound:guilds',
			'entranceSound:dms',
			'enabledThemeIds',
			'closeToTrayV2',
			'minimizeToTrayV2',
		],
		aliases: [],
		version: null,
		deletableAt: FrozenStorageDeletableAt.NEVER,
		writers: [
			{module: 'fluxer_app/src/features/user/state/UserSettings.ts', marker: 'UserSettings:syncedPreferencesWire'},
			{
				module: 'fluxer_app/src/features/premium/utils/PremiumCheckoutReturnIntent.ts',
				marker: 'PremiumCheckoutReturnIntent',
			},
			{module: 'fluxer_app/src/features/notification/utils/EntranceSoundScopes.ts', marker: 'userEntranceSound'},
			{module: 'fluxer_app/src/features/theme/state/ThemeLibrary.ts', marker: 'enabledThemeIds'},
			{module: 'fluxer_desktop/src/common/DesktopConfig.ts', marker: 'closeToTrayV2'},
		],
		clearableBy: [RESET_CLIENT_STATE],
		note: 'Raw keys the scoped migration deliberately never moves. Both eras read and write them in place.',
	},
	{
		id: 'local.domain-migration',
		kind: FrozenStorageKind.LOCAL_STORAGE,
		names: [
			'fluxer:domain-migration',
			'fluxer:domain-migration:device',
			'fluxer:domain-migration:notifications',
			'fluxer:domain-migration:moved-dismissed-at',
		],
		aliases: [],
		version: null,
		deletableAt: FrozenStorageDeletableAt.NEVER,
		writers: [
			{
				module: 'fluxer_app/src/features/app/domain_migration/DomainMigrationCore.ts',
				marker: 'storage?.setItem(DOMAIN_MIGRATION_MARKER_KEY',
			},
			{
				module: 'fluxer_app/src/features/app/domain_migration/DomainMigrationPreMount.ts',
				marker: 'storage?.setItem(DOMAIN_MIGRATION_DEVICE_KEY',
			},
		],
		clearableBy: [
			'fluxer_app/src/features/app/domain_migration/DomainMigrationTrigger.ts',
			'fluxer_app/src/features/app/domain_migration/DomainMovedNotice.ts',
			RESET_CLIENT_STATE,
		],
		note: 'Raw keys the domain move reads and writes in place on both origins. The scoped migration never moves them.',
	},
	{
		id: 'session.sso-redirect-to',
		kind: FrozenStorageKind.SESSION_STORAGE,
		names: ['fluxer:sso:redirect_to'],
		aliases: ['LEGACY_SSO_REDIRECT_TO_STORAGE_KEY'],
		version: null,
		deletableAt: FrozenStorageDeletableAt.NEVER,
		writers: [],
		clearableBy: [SSO_PENDING_CONTEXT],
		note: 'An SSO round trip started on the deployed build lands on this build, and the reverse. Only the module that consumes it may clear it.',
	},
	{
		id: 'desktop.voice-background-media',
		kind: FrozenStorageKind.DESKTOP_DIRECTORY,
		names: ['voice-background-media'],
		aliases: ['CACHE_DIR_NAME'],
		version: null,
		deletableAt: FrozenStorageDeletableAt.NEVER,
		writers: [
			{
				module: 'fluxer_desktop/src/main/VoiceBackgroundMediaCache.ts',
				marker: "CACHE_DIR_NAME = 'voice-background-media'",
			},
		],
		clearableBy: ['fluxer_desktop/src/main/VoiceBackgroundMediaCache.ts'],
		note: 'User-supplied voice background media. R2 re-scopes the directory per account by moving files. It is never emptied.',
	},
	{
		id: 'desktop.settings-json',
		kind: FrozenStorageKind.DESKTOP_FILE,
		names: ['settings.json'],
		aliases: ['CONFIG_FILE_NAME'],
		version: null,
		deletableAt: FrozenStorageDeletableAt.NEVER,
		writers: [{module: 'fluxer_desktop/src/common/DesktopConfig.ts', marker: "CONFIG_FILE_NAME = 'settings.json'"}],
		clearableBy: [],
		note: 'Every desktop shell preference. An old shell and a new renderer read the same file, in both skew directions.',
	},
	{
		id: 'desktop.window-state-json',
		kind: FrozenStorageKind.DESKTOP_FILE,
		names: ['window-state.json'],
		aliases: [],
		version: null,
		deletableAt: FrozenStorageDeletableAt.NEVER,
		writers: [{module: 'fluxer_desktop/src/main/Window.ts', marker: "'window-state.json'"}],
		clearableBy: [],
		note: 'Window geometry. --fluxer-reset-window-state ignores it for one launch. Nothing deletes it.',
	},
	{
		id: 'desktop.native-strings-json',
		kind: FrozenStorageKind.DESKTOP_FILE,
		names: ['native-strings.json'],
		aliases: ['STORAGE_FILE_NAME'],
		version: null,
		deletableAt: FrozenStorageDeletableAt.NEVER,
		writers: [{module: 'fluxer_desktop/src/main/MainI18n.ts', marker: "STORAGE_FILE_NAME = 'native-strings.json'"}],
		clearableBy: [],
		note: 'The tray and menu strings the shell renders before the renderer exists.',
	},
	{
		id: 'desktop.app-store-sqlite',
		kind: FrozenStorageKind.DESKTOP_FILE,
		names: ['desktop-app-store.sqlite3'],
		aliases: ['DESKTOP_APP_STORE_FILE_NAME'],
		version: null,
		deletableAt: FrozenStorageDeletableAt.NEVER,
		writers: [
			{
				module: 'fluxer_desktop/src/main/DesktopAppStorage.ts',
				marker: "DESKTOP_APP_STORE_FILE_NAME = 'desktop-app-store.sqlite3'",
			},
		],
		clearableBy: [],
		note: 'The desktop account store. A corrupt file is renamed aside and rebuilt. It is never unlinked.',
	},
]);
