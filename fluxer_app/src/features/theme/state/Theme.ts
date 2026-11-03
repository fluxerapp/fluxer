// SPDX-License-Identifier: AGPL-3.0-or-later

import AppStorage from '@app/features/platform/state/PersistentStorage';
import type {ThemeTarget} from '@app/features/theme/state/ThemeEditTarget';
import ThemeProfile from '@app/features/theme/state/ThemeProfile';
import {getElectronAPI} from '@app/features/ui/utils/NativeUtils';
import type {LinuxAppearanceSnapshot} from '@app/types/electron.d';
import {type ThemeType, ThemeTypes} from '@fluxer/constants/src/UserConstants';
import {makeAutoObservable, reaction, runInAction} from 'mobx';

type ExplicitTheme =
	| typeof ThemeTypes.DARK
	| typeof ThemeTypes.DARK_LEGACY
	| typeof ThemeTypes.LIGHT
	| typeof ThemeTypes.COAL;

const EXPLICIT_THEMES = new Set<string>([ThemeTypes.DARK, ThemeTypes.DARK_LEGACY, ThemeTypes.LIGHT, ThemeTypes.COAL]);
const STORAGE_KEY = 'theme';

function isExplicitTheme(theme: string | null | undefined): theme is ExplicitTheme {
	return typeof theme === 'string' && EXPLICIT_THEMES.has(theme);
}

function loadThemeFromLocalStorage(): ExplicitTheme | null {
	try {
		const stored = AppStorage.getItem(STORAGE_KEY);
		return isExplicitTheme(stored) ? stored : null;
	} catch {
		return null;
	}
}

function persistThemeToLocalStorage(theme: string): void {
	try {
		if (isExplicitTheme(theme)) {
			AppStorage.setItem(STORAGE_KEY, theme);
		}
	} catch {}
}

function reportPrebootTheme(theme: string): void {
	getElectronAPI()?.setPrebootTheme?.(theme);
}

class Theme {
	systemPrefersDark = false;
	private mediaQuery: MediaQueryList | null = null;
	private localStorageSyncDisposer: (() => void) | null = null;
	private linuxPortalPrefersDark: boolean | null = null;
	private linuxPortalUnsubscribe: (() => void) | null = null;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		this.initSystemThemeDetection();
		this.initLinuxPortalThemeDetection();
		this.localStorageSyncDisposer = reaction(
			() => (ThemeProfile.isReady ? this.effectiveTheme : null),
			(theme) => {
				if (theme !== null) {
					persistThemeToLocalStorage(theme);
					reportPrebootTheme(theme);
				}
			},
			{fireImmediately: true},
		);
	}

	get effectiveTheme(): ExplicitTheme {
		if (!ThemeProfile.isReady) {
			const earlyTheme = loadThemeFromLocalStorage();
			if (earlyTheme) {
				return earlyTheme;
			}
		}
		return this.resolvePreference(ThemeProfile.themePreference);
	}

	themeFor(target: ThemeTarget): ExplicitTheme {
		return this.resolvePreference(ThemeProfile.themePreferenceFor(target));
	}

	private resolvePreference(preference: ThemeType): ExplicitTheme {
		if (preference === ThemeTypes.SYSTEM) {
			return this.systemPrefersDark ? ThemeTypes.DARK : ThemeTypes.LIGHT;
		}
		return preference;
	}

	private initSystemThemeDetection(): void {
		if (!window.matchMedia) {
			return;
		}
		this.mediaQuery = window.matchMedia('(prefers-color-scheme: dark)');
		this.systemPrefersDark = this.mediaQuery.matches;
		this.mediaQuery.addEventListener('change', this.handleSystemThemeChange);
	}

	private handleSystemThemeChange = (event: MediaQueryListEvent): void => {
		if (this.linuxPortalPrefersDark !== null) {
			return;
		}
		this.systemPrefersDark = event.matches;
	};

	private initLinuxPortalThemeDetection(): void {
		const electronApi = getElectronAPI();
		if (electronApi?.platform !== 'linux') {
			return;
		}
		if (!electronApi.getLinuxAppearance || !electronApi.onLinuxAppearanceChanged) {
			return;
		}
		this.linuxPortalUnsubscribe = electronApi.onLinuxAppearanceChanged((snapshot) => {
			this.applyLinuxPortalAppearance(snapshot);
		});
		electronApi
			.getLinuxAppearance()
			.then((snapshot) => {
				this.applyLinuxPortalAppearance(snapshot);
			})
			.catch(() => {});
	}

	private applyLinuxPortalAppearance(snapshot: LinuxAppearanceSnapshot): void {
		runInAction(() => {
			if (snapshot.colorScheme === 'prefer-dark') {
				this.linuxPortalPrefersDark = true;
				this.systemPrefersDark = true;
			} else if (snapshot.colorScheme === 'prefer-light') {
				this.linuxPortalPrefersDark = false;
				this.systemPrefersDark = false;
			} else {
				this.linuxPortalPrefersDark = null;
				if (this.mediaQuery) {
					this.systemPrefersDark = this.mediaQuery.matches;
				}
			}
		});
	}

	destroy(): void {
		if (this.mediaQuery) {
			this.mediaQuery.removeEventListener('change', this.handleSystemThemeChange);
			this.mediaQuery = null;
		}
		if (this.linuxPortalUnsubscribe) {
			this.linuxPortalUnsubscribe();
			this.linuxPortalUnsubscribe = null;
		}
		if (this.localStorageSyncDisposer) {
			this.localStorageSyncDisposer();
			this.localStorageSyncDisposer = null;
		}
	}
}

export default new Theme();
