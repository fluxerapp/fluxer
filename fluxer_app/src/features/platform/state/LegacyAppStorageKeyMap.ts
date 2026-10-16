// SPDX-License-Identifier: AGPL-3.0-or-later

import {AppStorageKey} from '@app/features/platform/state/AppStorageKeys';
import {GATEWAY_PREBOOT_SESSION_STORAGE_KEY} from '@app/features/platform/state/PrebootMirror';

interface LegacyAppStorageKeyRowBase {
	readonly scope: 'global' | 'account';
	readonly fanout: 'preference' | 'content';
}

interface LegacyExactKeyRow extends LegacyAppStorageKeyRowBase {
	readonly kind: 'exact';
	readonly key: string;
}

interface LegacyUserSuffixedKeyRow extends LegacyAppStorageKeyRowBase {
	readonly kind: 'suffixed';
	readonly prefix: string;
	readonly scope: 'account';
}

interface LegacyPrefixedKeyRow extends LegacyAppStorageKeyRowBase {
	readonly kind: 'prefixed';
	readonly prefix: string;
}

export type LegacyAppStorageKeyRow = LegacyExactKeyRow | LegacyUserSuffixedKeyRow | LegacyPrefixedKeyRow;

export interface LegacyAppStorageKeyMatch {
	readonly row: LegacyAppStorageKeyRow;
	readonly userId: string | null;
}

const MOBX_PERSIST_ROWS: ReadonlyArray<LegacyAppStorageKeyRow> = [
	{kind: 'exact', key: 'Theme', scope: 'global', fanout: 'preference'},
	{kind: 'exact', key: 'Drafts', scope: 'account', fanout: 'content'},
	{kind: 'exact', key: 'SelectedGuild', scope: 'account', fanout: 'content'},
	{kind: 'exact', key: 'SelectedChannel', scope: 'account', fanout: 'content'},
	{kind: 'exact', key: 'VoiceSettings', scope: 'global', fanout: 'preference'},
	{kind: 'exact', key: 'LocalVoiceState', scope: 'global', fanout: 'preference'},
	{kind: 'exact', key: 'VoiceSessionRestore', scope: 'global', fanout: 'preference'},
	{kind: 'exact', key: 'Notification', scope: 'account', fanout: 'preference'},
	{kind: 'exact', key: 'Inbox', scope: 'account', fanout: 'content'},
	{kind: 'exact', key: 'Location', scope: 'account', fanout: 'content'},
	{kind: 'exact', key: 'GuildFolderExpanded', scope: 'account', fanout: 'preference'},
	{kind: 'exact', key: 'SearchHistory', scope: 'account', fanout: 'content'},
	{kind: 'exact', key: 'ChannelFrecencyLocal', scope: 'account', fanout: 'content'},
	{kind: 'exact', key: 'GuildMatureContentAgreeLocal', scope: 'account', fanout: 'content'},
	{kind: 'exact', key: 'ThreadPanelWidth', scope: 'account', fanout: 'content'},
	{kind: 'exact', key: 'SearchEngine', scope: 'account', fanout: 'preference'},
	{kind: 'exact', key: 'ReverseImageSearch', scope: 'account', fanout: 'preference'},
	{kind: 'exact', key: 'Slowmode', scope: 'account', fanout: 'preference'},
	{kind: 'exact', key: 'Translation', scope: 'account', fanout: 'preference'},
	{kind: 'exact', key: 'Keybind', scope: 'global', fanout: 'preference'},
	{kind: 'exact', key: 'ParticipantVolume', scope: 'global', fanout: 'preference'},
	{kind: 'exact', key: 'StreamAudioPrefs', scope: 'global', fanout: 'preference'},
	{kind: 'exact', key: 'EntranceSoundListenerPrefs', scope: 'global', fanout: 'preference'},
	{kind: 'exact', key: 'NewDeviceMonitoring', scope: 'global', fanout: 'preference'},
	{kind: 'exact', key: 'AudioVolume', scope: 'global', fanout: 'preference'},
	{kind: 'exact', key: 'VideoVolume', scope: 'global', fanout: 'preference'},
	{kind: 'exact', key: 'MobileLayout', scope: 'global', fanout: 'preference'},
	{kind: 'exact', key: 'MacPermissions', scope: 'global', fanout: 'preference'},
	{kind: 'exact', key: 'DeveloperMode', scope: 'global', fanout: 'preference'},
	{kind: 'exact', key: 'DeveloperOptions', scope: 'global', fanout: 'preference'},
	{kind: 'exact', key: 'StreamerMode', scope: 'global', fanout: 'preference'},
];

const PLAIN_ROWS: ReadonlyArray<LegacyAppStorageKeyRow> = [
	{kind: 'exact', key: 'theme', scope: 'global', fanout: 'preference'},
	{kind: 'exact', key: 'locale', scope: 'global', fanout: 'preference'},
	{kind: 'exact', key: 'debugLoggingEnabled', scope: 'global', fanout: 'preference'},
	{kind: 'exact', key: 'SkeletonLayoutMemory', scope: 'global', fanout: 'preference'},
	{kind: 'exact', key: 'MessageEdit', scope: 'account', fanout: 'content'},
	{kind: 'exact', key: 'TrustedDomain', scope: 'account', fanout: 'content'},
	{kind: 'exact', key: 'ChannelFrecencyThreadGuilds', scope: 'account', fanout: 'content'},
	{kind: 'exact', key: 'SoftwareEncoderWarning_neverShowAgain', scope: 'global', fanout: 'preference'},
	{kind: 'exact', key: 'pip_corner', scope: 'global', fanout: 'preference'},
	{kind: 'exact', key: 'pip_width', scope: 'global', fanout: 'preference'},
	{kind: 'exact', key: 'incoming_call_overlay_position', scope: 'global', fanout: 'preference'},
	{kind: 'exact', key: 'compact_voice_call_heights', scope: 'global', fanout: 'preference'},
	{kind: 'exact', key: 'member_list_default_hidden_channel_overrides', scope: 'account', fanout: 'content'},
	{kind: 'exact', key: 'AdvancedSettings:unreadBadgeCustomizationEnabled', scope: 'account', fanout: 'preference'},
	{kind: 'exact', key: 'AdvancedSettings:keepAttachmentsOnEmptyMessageEdit', scope: 'account', fanout: 'preference'},
	{kind: 'exact', key: 'AdvancedSettings:expressionCloneShortcutsEnabled', scope: 'account', fanout: 'preference'},
	{kind: 'exact', key: 'UserSettings:syncedPreferencesLocal', scope: 'account', fanout: 'preference'},
	{kind: 'exact', key: 'ThemeStudio:section', scope: 'global', fanout: 'preference'},
	{kind: 'exact', key: 'ThemeStudio:expandedGroups', scope: 'global', fanout: 'preference'},
	{kind: 'exact', key: 'ThemeStudio:librarySplit', scope: 'global', fanout: 'preference'},
	{kind: 'exact', key: 'Keybind:globalDefaultMigration:v1', scope: 'global', fanout: 'preference'},
	{kind: 'exact', key: 'Keybind:builtinDisableMarkerMigration:v1', scope: 'global', fanout: 'preference'},
	{kind: 'exact', key: 'fluxer.lastPushEndpoint', scope: 'global', fanout: 'preference'},
	{kind: 'exact', key: 'fluxer:media:volume', scope: 'global', fanout: 'preference'},
	{kind: 'exact', key: 'fluxer:media:muted', scope: 'global', fanout: 'preference'},
	{kind: 'exact', key: 'fluxer:media:playbackRate', scope: 'global', fanout: 'preference'},
	{kind: 'exact', key: 'fluxer:media_player:playback-rate', scope: 'global', fanout: 'preference'},
	{kind: 'exact', key: 'fluxer:media_caps:v2', scope: 'global', fanout: 'preference'},
	{kind: 'exact', key: 'fluxer:media_player:volume', scope: 'account', fanout: 'content'},
	{kind: 'exact', key: 'fluxer:media_player:muted', scope: 'account', fanout: 'content'},
	{kind: 'exact', key: 'fluxer:media:playbackRates', scope: 'account', fanout: 'content'},
	{kind: 'exact', key: AppStorageKey.UI_SIDEBAR_WIDTH, scope: 'account', fanout: 'content'},
	{kind: 'exact', key: AppStorageKey.UI_EXPRESSION_PICKER_SIZE, scope: 'account', fanout: 'content'},
	{kind: 'exact', key: AppStorageKey.UI_INBOX_POPOUT_SIZE, scope: 'account', fanout: 'content'},
	{kind: 'exact', key: AppStorageKey.UI_CHANNEL_PINS_POPOUT_SIZE, scope: 'account', fanout: 'content'},
	{kind: 'prefixed', prefix: 'fluxer_scheduled_maintenance_dismissed:', scope: 'account', fanout: 'content'},
];

const ACCESSIBILITY_ROWS: ReadonlyArray<LegacyAppStorageKeyRow> = [
	{kind: 'exact', key: AppStorageKey.ACCESSIBILITY_LEGACY_STORE, scope: 'account', fanout: 'preference'},
	{kind: 'exact', key: 'Accessibility:zoomLevel', scope: 'account', fanout: 'preference'},
	{kind: 'exact', key: 'Accessibility:customThemeCss', scope: 'account', fanout: 'preference'},
	{kind: 'exact', key: 'Accessibility:customThemeCssSyncAcrossDevices', scope: 'account', fanout: 'preference'},
	{kind: 'exact', key: 'Accessibility:motion', scope: 'account', fanout: 'preference'},
	{kind: 'exact', key: 'Accessibility:videoSeekPreviewThumbnails', scope: 'account', fanout: 'preference'},
	{kind: 'suffixed', prefix: 'Accessibility:showNeko', scope: 'account', fanout: 'preference'},
	{kind: 'suffixed', prefix: 'Accessibility:keepNekoStill', scope: 'account', fanout: 'preference'},
	{kind: 'suffixed', prefix: 'Accessibility:pinNekoToTextarea', scope: 'account', fanout: 'preference'},
];

export const LEGACY_APP_STORAGE_KEY_MAP: ReadonlyArray<LegacyAppStorageKeyRow> = Object.freeze([
	...MOBX_PERSIST_ROWS,
	...PLAIN_ROWS,
	...ACCESSIBILITY_ROWS,
]);

export const NOT_MIGRATED_LEGACY_KEYS: ReadonlyArray<string> = Object.freeze([
	AppStorageKey.AUTH_SESSION_TOKEN,
	AppStorageKey.AUTH_SESSION_USER_ID,
	'runtimeConfig',
	'AccountManager',
	GATEWAY_PREBOOT_SESSION_STORAGE_KEY,
	'UserSettings:syncedPreferencesWire',
	'UserSettings:syncedPreferencesDirtyFields',
	'UserSettings:syncedPreferencesRecentAck',
	'fluxer:sso:redirect_to',
	'PremiumCheckoutReturnIntent',
	'userEntranceSound',
	'entranceSound:guilds',
	'entranceSound:dms',
	'enabledThemeIds',
	'closeToTrayV2',
	'minimizeToTrayV2',
	'fluxer:domain-migration',
	'fluxer:domain-migration:device',
	'fluxer:domain-migration:notifications',
	'fluxer:domain-migration:moved-dismissed-at',
]);

const NOT_MIGRATED_LEGACY_KEY_SET: ReadonlySet<string> = new Set(NOT_MIGRATED_LEGACY_KEYS);

export function isNotMigratedLegacyKey(legacyKey: string): boolean {
	return NOT_MIGRATED_LEGACY_KEY_SET.has(legacyKey);
}

const LEGACY_ACCOUNT_SWAPPED_KEY_PATTERN = /^(?:mobx|persist|fluxer)/;

export function isLegacyAccountSwappedKey(legacyKey: string): boolean {
	return LEGACY_ACCOUNT_SWAPPED_KEY_PATTERN.test(legacyKey);
}

const EXACT_ROWS: ReadonlyMap<string, LegacyExactKeyRow> = new Map(
	LEGACY_APP_STORAGE_KEY_MAP.flatMap((row) => (row.kind === 'exact' ? [[row.key, row] as const] : [])),
);

const PATTERN_ROWS: ReadonlyArray<LegacyUserSuffixedKeyRow | LegacyPrefixedKeyRow> = LEGACY_APP_STORAGE_KEY_MAP.filter(
	(row) => row.kind !== 'exact',
);

function userIdSuffix(key: string, prefix: string): string | null {
	if (!key.startsWith(`${prefix}:`)) {
		return null;
	}
	const suffix = key.slice(prefix.length + 1);
	return suffix === '' ? null : suffix;
}

export function resolveLegacyAppStorageKey(key: string): LegacyAppStorageKeyMatch | null {
	const exact = EXACT_ROWS.get(key);
	if (exact !== undefined) {
		return {row: exact, userId: null};
	}
	for (const row of PATTERN_ROWS) {
		if (row.kind === 'suffixed') {
			if (row.prefix === key) {
				return {row, userId: null};
			}
			const userId = userIdSuffix(key, row.prefix);
			if (userId !== null) {
				return {row, userId};
			}
			continue;
		}
		if (key.startsWith(row.prefix) && key.length > row.prefix.length) {
			return {row, userId: null};
		}
	}
	return null;
}
