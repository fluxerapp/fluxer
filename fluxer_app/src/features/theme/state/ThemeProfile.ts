// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility, {
	ACCESSIBILITY_CUSTOM_THEME_STORAGE_KEY,
	ACCESSIBILITY_CUSTOM_THEME_SYNC_STORAGE_KEY,
} from '@app/features/accessibility/state/Accessibility';
import {AppStorageKey} from '@app/features/platform/state/AppStorageKeys';
import AppStorage, {type StorageChangeEvent} from '@app/features/platform/state/PersistentStorage';
import AccountTheme from '@app/features/theme/state/AccountTheme';
import AccountThemeMode from '@app/features/theme/state/AccountThemeMode';
import InstallationTheme from '@app/features/theme/state/InstallationTheme';
import ThemeEditTarget, {type ThemeTarget} from '@app/features/theme/state/ThemeEditTarget';
import ThemeLibrary from '@app/features/theme/state/ThemeLibrary';
import ThemeSettingsState from '@app/features/theme_settings/state/ThemeSettingsState';
import type {ThemeType} from '@fluxer/constants/src/UserConstants';
import {makeAutoObservable, runInAction} from 'mobx';

const PROFILE_STORAGE_KEYS: ReadonlySet<string> = new Set([
	AppStorageKey.ACCOUNT_THEME,
	AppStorageKey.ACCOUNT_THEME_MODE,
	AppStorageKey.INSTALLATION_THEME,
	AppStorageKey.THEME_LIBRARY_ACCOUNT_ENABLED_IDS,
	ACCESSIBILITY_CUSTOM_THEME_STORAGE_KEY,
	ACCESSIBILITY_CUSTOM_THEME_SYNC_STORAGE_KEY,
]);

class ThemeProfile {
	isReady = false;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		AppStorage.subscribe(this.handleStorageChange);
	}

	get usesDesktopTheme(): boolean {
		return AccountThemeMode.usesDesktopTheme;
	}

	get activeTarget(): ThemeTarget {
		return AccountThemeMode.activeTarget;
	}

	get themePreference(): ThemeType {
		return this.themePreferenceFor(this.activeTarget);
	}

	get customThemeCss(): string | null {
		return this.customThemeCssFor(this.activeTarget);
	}

	get enabledThemeIds(): Array<string> {
		return ThemeLibrary.enabledIdsFor(this.activeTarget);
	}

	get activeThemeCss(): string {
		return ThemeLibrary.themeCssFor(this.enabledThemeIds, ThemeSettingsState.themePreview);
	}

	themePreferenceFor(target: ThemeTarget): ThemeType {
		return target === 'desktop' ? InstallationTheme.baseTheme : AccountTheme.themePreference;
	}

	customThemeCssFor(target: ThemeTarget): string | null {
		return target === 'desktop' ? InstallationTheme.customThemeCss : Accessibility.customThemeCss;
	}

	reload(): void {
		runInAction(() => {
			InstallationTheme.load();
			AccountThemeMode.load();
			AccountTheme.load();
			ThemeLibrary.loadAccountEnabledIds();
			Accessibility.loadCustomThemeCssFromStorage();
		});
	}

	markReady(): void {
		this.reload();
		this.isReady = true;
	}

	private handleStorageChange(event: StorageChangeEvent): void {
		if (event.key === null) {
			runInAction(() => {
				this.reload();
				ThemeEditTarget.reset();
			});
			return;
		}
		if (!PROFILE_STORAGE_KEYS.has(event.key)) {
			return;
		}
		if (event.source === 'external' || event.newValue === null) {
			this.reload();
		}
	}
}

export default new ThemeProfile();
