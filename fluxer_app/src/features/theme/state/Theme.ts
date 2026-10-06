// SPDX-License-Identifier: AGPL-3.0-or-later

import AppStorage from '@app/features/platform/state/PersistentStorage';
import {makePersistent} from '@app/features/platform/utils/MobXPersistence';
import {getElectronAPI} from '@app/features/ui/utils/NativeUtils';
import type {LinuxAppearanceSnapshot} from '@app/types/electron.d';
import type {ThemeType} from '@fluxer/constants/src/UserConstants';
import {ThemeTypes} from '@fluxer/constants/src/UserConstants';
import {makeAutoObservable, reaction, runInAction} from 'mobx';

type ExplicitTheme =
	| typeof ThemeTypes.DARK
	| typeof ThemeTypes.DARK_LEGACY
	| typeof ThemeTypes.LIGHT
	| typeof ThemeTypes.COAL;

export interface ThemePreferenceSnapshot {
	syncAcrossDevices: boolean;
	localTheme: ThemeType;
	serverTheme: ThemeType;
}

const EXPLICIT_THEMES = new Set<string>([ThemeTypes.DARK, ThemeTypes.DARK_LEGACY, ThemeTypes.LIGHT, ThemeTypes.COAL]);
const VALID_THEMES = new Set<string>(Object.values(ThemeTypes));
const STORAGE_KEY = 'theme';

function isExplicitTheme(theme: string | null | undefined): theme is ExplicitTheme {
	return typeof theme === 'string' && EXPLICIT_THEMES.has(theme);
}

function isValidTheme(theme: string | null | undefined): theme is ThemeType {
	return typeof theme === 'string' && VALID_THEMES.has(theme);
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

class Theme {
	syncAcrossDevices = true;
	localTheme: ThemeType = ThemeTypes.DARK;
	serverTheme: ThemeType = ThemeTypes.DARK;
	systemPrefersDark = false;
	private _hydrated = false;
	private _pendingServerTheme: ThemeType | null = null;
	private mediaQuery: MediaQueryList | null = null;
	private localStorageSyncDisposer: (() => void) | null = null;
	private linuxPortalPrefersDark: boolean | null = null;
	private linuxPortalUnsubscribe: (() => void) | null = null;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		this.initSystemThemeDetection();
		this.initLinuxPortalThemeDetection();
		this.initPersistence();
	}

	get isHydrated(): boolean {
		return this._hydrated;
	}

	get themePreference(): ThemeType {
		return this.syncAcrossDevices ? this.serverTheme : this.localTheme;
	}

	get effectiveTheme(): ExplicitTheme {
		if (!this._hydrated) {
			const earlyTheme = loadThemeFromLocalStorage();
			if (earlyTheme) {
				return earlyTheme;
			}
		}
		const preference = this.themePreference;
		if (preference === ThemeTypes.SYSTEM) {
			return this.systemPrefersDark ? ThemeTypes.DARK : ThemeTypes.LIGHT;
		}
		return preference;
	}

	setTheme(theme: ThemeType): void {
		if (!isValidTheme(theme)) {
			return;
		}
		if (this.syncAcrossDevices) {
			this.serverTheme = theme;
		} else {
			this.localTheme = theme;
		}
	}

	setSyncAcrossDevices(sync: boolean): void {
		if (sync === this.syncAcrossDevices) {
			return;
		}
		if (sync) {
			this.syncAcrossDevices = true;
			this.localTheme = ThemeTypes.DARK;
		} else {
			const currentPreference = this.themePreference;
			this.syncAcrossDevices = false;
			this.localTheme = currentPreference;
		}
	}

	getPreferenceSnapshot(): ThemePreferenceSnapshot {
		return {
			syncAcrossDevices: this.syncAcrossDevices,
			localTheme: this.localTheme,
			serverTheme: this.serverTheme,
		};
	}

	applyPreferenceSnapshot(snapshot: ThemePreferenceSnapshot): void {
		this.syncAcrossDevices = Boolean(snapshot.syncAcrossDevices);
		this.localTheme = isValidTheme(snapshot.localTheme) ? snapshot.localTheme : ThemeTypes.DARK;
		this.serverTheme = isValidTheme(snapshot.serverTheme) ? snapshot.serverTheme : ThemeTypes.DARK;
	}

	updateServerTheme(theme: string | null | undefined): void {
		const normalized = isValidTheme(theme) ? theme : ThemeTypes.DARK;
		this.serverTheme = normalized;
		if (!this._hydrated) {
			this._pendingServerTheme = normalized;
		}
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

	private async initPersistence(): Promise<void> {
		await makePersistent(this, 'Theme', ['syncAcrossDevices', 'localTheme', 'serverTheme'], {syncAcrossTabs: true});
		runInAction(() => {
			if (this._pendingServerTheme !== null) {
				this.serverTheme = this._pendingServerTheme;
				this._pendingServerTheme = null;
			}
			this._hydrated = true;
		});
		this.localStorageSyncDisposer = reaction(
			() => this.effectiveTheme,
			(theme) => persistThemeToLocalStorage(theme),
			{fireImmediately: true},
		);
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
