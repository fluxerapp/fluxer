// SPDX-License-Identifier: AGPL-3.0-or-later

import {normalizeCustomThemeCss, readLocalCustomThemeCss} from '@app/features/accessibility/state/Accessibility';
import {AppStorageKey} from '@app/features/platform/state/AppStorageKeys';
import AppStorage, {flushAppStorageWrites, getAppStorageScope} from '@app/features/platform/state/PersistentStorage';
import {getPersistentStorageBackend} from '@app/features/platform/state/PersistentStorageBackend';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {parseAccountThemeSnapshot, serializeAccountThemeSnapshot} from '@app/features/theme/state/AccountTheme';
import AccountThemeMode from '@app/features/theme/state/AccountThemeMode';
import {
	DEFAULT_INSTALLATION_THEME,
	serializeInstallationThemeSnapshot,
} from '@app/features/theme/state/InstallationTheme';
import ThemeProfile from '@app/features/theme/state/ThemeProfile';
import {getEnabledThemeIds} from '@app/features/theme/utils/ThemeLibraryDb';
import type {ThemeType} from '@fluxer/constants/src/UserConstants';

const logger = new Logger('ThemeProfileMigration');
const UNUSED_THEME_STUDIO_KEYS = ['ThemeStudio:section', 'ThemeStudio:librarySplit'];

async function writeIfAbsent(scope: string, key: string, value: string): Promise<void> {
	if (scope === getAppStorageScope()) {
		if (AppStorage.getItem(key) === null) {
			AppStorage.setItem(key, value);
		}
		return;
	}
	const backend = getPersistentStorageBackend();
	if ((await backend.get(scope, key)) === null) {
		await backend.set(scope, key, value);
	}
}

async function runMigration(accountScopes: ReadonlyArray<string>): Promise<void> {
	if (AppStorage.getItem(AppStorageKey.THEME_PROFILES_MIGRATED) !== null) {
		return;
	}
	const legacyTheme = parseAccountThemeSnapshot(AppStorage.getItem(AppStorageKey.THEME));
	if (AccountThemeMode.canUseDesktopTheme) {
		const signedIn = accountScopes.includes(getAppStorageScope());
		if (AppStorage.getItem(AppStorageKey.INSTALLATION_THEME) === null) {
			let baseTheme: ThemeType;
			if (legacyTheme === null) {
				baseTheme = DEFAULT_INSTALLATION_THEME.baseTheme;
			} else if (legacyTheme.syncAcrossDevices) {
				baseTheme = legacyTheme.serverTheme;
			} else {
				baseTheme = legacyTheme.localTheme;
			}
			const customThemeCss = signedIn ? normalizeCustomThemeCss(readLocalCustomThemeCss()) : null;
			AppStorage.setItem(
				AppStorageKey.INSTALLATION_THEME,
				serializeInstallationThemeSnapshot({baseTheme, customThemeCss}),
			);
		}
	}
	const enabledThemeIds = JSON.stringify(await getEnabledThemeIds());
	await flushAppStorageWrites();
	const scopes = new Set([getAppStorageScope(), ...accountScopes]);
	for (const scope of scopes) {
		if (legacyTheme !== null) {
			await writeIfAbsent(scope, AppStorageKey.ACCOUNT_THEME, serializeAccountThemeSnapshot(legacyTheme));
		}
		await writeIfAbsent(scope, AppStorageKey.THEME_LIBRARY_ACCOUNT_ENABLED_IDS, enabledThemeIds);
	}
	for (const key of UNUSED_THEME_STUDIO_KEYS) {
		AppStorage.removeItem(key);
	}
	AppStorage.setItem(AppStorageKey.THEME_PROFILES_MIGRATED, '1');
}

export async function migrateThemeProfiles(accountScopes: ReadonlyArray<string>): Promise<void> {
	try {
		await runMigration(accountScopes);
	} catch (error) {
		logger.error('Failed to move theme settings into each account', error);
	} finally {
		ThemeProfile.markReady();
	}
}
