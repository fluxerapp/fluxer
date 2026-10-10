// SPDX-License-Identifier: AGPL-3.0-or-later

import {AuthSessionStorageKey} from '@app/features/platform/state/auth_session/AuthSessionStorage';

export const AppStorageKey = Object.freeze({
	ACCESSIBILITY_LEGACY_STORE: 'Accessibility',
	ACCOUNT_RESET_KEPT_KEYS: 'fluxer:internal:account-reset-kept-keys',
	AUTH_ACCOUNT_KEY: AuthSessionStorageKey.ActiveAccountKey,
	AUTH_DESKTOP_CLIENT_INTRO_COMPLETED: 'fluxer:auth:desktop-client-intro:completed',
	AUTH_DESKTOP_CLIENT_PERMISSIONS_SKIPPED: 'fluxer:auth:desktop-client-intro:permissions-skipped',
	AUTH_DESKTOP_CLIENT_PREFERENCES_SEEN: 'fluxer:auth:desktop-client-intro:preferences-seen',
	AUTH_DESKTOP_CLIENT_WELCOME_SEEN: 'fluxer:auth:desktop-client-intro:welcome-seen',
	AUTH_SESSION_TOKEN: AuthSessionStorageKey.Token,
	AUTH_SESSION_USER_ID: AuthSessionStorageKey.UserId,
	BACKGROUND_ACCOUNT_PRESENCE: 'BackgroundAccountPresence',
	DELETED_KEYS: 'fluxer:internal:deleted-keys',
	MESSAGING_DRAFTS: 'Drafts',
	MESSAGING_MESSAGE_EDIT: 'MessageEdit',
	NOTIFICATION: 'Notification',
	SSO_PENDING_CONTEXT_PREFIX: 'fluxer:sso:pending-context',
	THEME: 'Theme',
	THEME_LIBRARY_ENABLED_IDS: 'fluxer:theme-library:enabled-ids',
	THEME_LIBRARY_LOCAL_FILES: 'fluxer:theme-library:local-files',
	THEME_LIBRARY_MIGRATED: 'fluxer:theme-library:migrated-to-app-storage',
	THEME_LIBRARY_THEMES: 'fluxer:theme-library:themes',
	THEME_PREBOOT_MIRROR: 'theme',
	UI_CHANNEL_PINS_POPOUT_SIZE: 'fluxer:ui:channel-pins-popout-size',
	UI_EXPRESSION_PICKER_SIZE: 'fluxer:ui:expression-picker-size',
	UI_INBOX_POPOUT_SIZE: 'fluxer:ui:inbox-popout-size',
	UI_SIDEBAR_WIDTH: 'fluxer:ui:sidebar-width',
	VOICE_SETTINGS: 'VoiceSettings',
	WINDOWS_FONT: 'WindowsFont',
} as const);

export type AppStorageKey = (typeof AppStorageKey)[keyof typeof AppStorageKey];

export const LEGACY_APP_STORAGE_MIGRATION_MARKER_KEY = 'fluxer:migration:legacy-app-storage';
export const LEGACY_SHARED_CONTENT_REVIEW_KEY = 'fluxer:migration:shared-content-review';

export const LegacySharedContentReviewState = Object.freeze({
	PENDING: 'pending',
	DONE: 'done',
} as const);

const GLOBAL_APP_STORAGE_KEY_LOOKUP: Readonly<Record<string, true>> = Object.freeze({
	[AppStorageKey.AUTH_ACCOUNT_KEY]: true,
	[AppStorageKey.AUTH_DESKTOP_CLIENT_INTRO_COMPLETED]: true,
	[AppStorageKey.AUTH_DESKTOP_CLIENT_PERMISSIONS_SKIPPED]: true,
	[AppStorageKey.AUTH_DESKTOP_CLIENT_PREFERENCES_SEEN]: true,
	[AppStorageKey.AUTH_DESKTOP_CLIENT_WELCOME_SEEN]: true,
	[AppStorageKey.AUTH_SESSION_TOKEN]: true,
	[AppStorageKey.AUTH_SESSION_USER_ID]: true,
	[AppStorageKey.BACKGROUND_ACCOUNT_PRESENCE]: true,
	[AppStorageKey.THEME_LIBRARY_ENABLED_IDS]: true,
	[AppStorageKey.THEME_LIBRARY_LOCAL_FILES]: true,
	[AppStorageKey.THEME_LIBRARY_MIGRATED]: true,
	[AppStorageKey.THEME_LIBRARY_THEMES]: true,
	[AppStorageKey.WINDOWS_FONT]: true,
});

function keyStartsWithPrefix(key: string, prefix: AppStorageKey): boolean {
	return key.startsWith(`${prefix}:`);
}

export function ssoPendingContextKey(state: string): string {
	return `${AppStorageKey.SSO_PENDING_CONTEXT_PREFIX}:${encodeURIComponent(state)}`;
}

export function isSsoPendingContextKey(key: string): boolean {
	return keyStartsWithPrefix(key, AppStorageKey.SSO_PENDING_CONTEXT_PREFIX);
}

export function isGlobalAppStorageKey(key: string): boolean {
	return Object.hasOwn(GLOBAL_APP_STORAGE_KEY_LOOKUP, key) || isSsoPendingContextKey(key);
}
