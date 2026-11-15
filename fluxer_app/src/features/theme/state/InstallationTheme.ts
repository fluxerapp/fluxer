// SPDX-License-Identifier: AGPL-3.0-or-later

import {normalizeCustomThemeCss} from '@app/features/accessibility/state/Accessibility';
import {AppStorageKey} from '@app/features/platform/state/AppStorageKeys';
import AppStorage from '@app/features/platform/state/PersistentStorage';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {isThemeType} from '@app/features/theme/state/AccountTheme';
import type {ThemeType} from '@fluxer/constants/src/UserConstants';
import {ThemeTypes} from '@fluxer/constants/src/UserConstants';
import {isRecord} from '@fluxer/record_utils/src/RecordGuards';
import {makeAutoObservable} from 'mobx';

const logger = new Logger('InstallationTheme');

interface InstallationThemeSnapshot {
	readonly baseTheme: ThemeType;
	readonly customThemeCss: string | null;
}

export const DEFAULT_INSTALLATION_THEME: InstallationThemeSnapshot = Object.freeze({
	baseTheme: ThemeTypes.DARK,
	customThemeCss: null,
});

export function serializeInstallationThemeSnapshot(snapshot: InstallationThemeSnapshot): string {
	return JSON.stringify({baseTheme: snapshot.baseTheme, customThemeCss: snapshot.customThemeCss});
}

function parseInstallationThemeSnapshot(raw: string | null): InstallationThemeSnapshot | null {
	if (raw === null) {
		return null;
	}
	let parsed: unknown;
	try {
		parsed = JSON.parse(raw);
	} catch (error) {
		if (!(error instanceof SyntaxError)) {
			logger.warn('Failed to decode a stored installation theme', error);
		}
		return null;
	}
	if (!isRecord(parsed)) {
		return null;
	}
	return {
		baseTheme: isThemeType(parsed.baseTheme) ? parsed.baseTheme : DEFAULT_INSTALLATION_THEME.baseTheme,
		customThemeCss: normalizeCustomThemeCss(typeof parsed.customThemeCss === 'string' ? parsed.customThemeCss : null),
	};
}

function readStoredSnapshot(): InstallationThemeSnapshot | null {
	try {
		return parseInstallationThemeSnapshot(AppStorage.getItem(AppStorageKey.INSTALLATION_THEME));
	} catch (error) {
		logger.warn('Failed to read the installation theme', error);
		return null;
	}
}

class InstallationTheme {
	baseTheme: ThemeType = DEFAULT_INSTALLATION_THEME.baseTheme;
	customThemeCss: string | null = DEFAULT_INSTALLATION_THEME.customThemeCss;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		this.load();
	}

	load(): void {
		const snapshot = readStoredSnapshot();
		this.baseTheme = snapshot?.baseTheme ?? DEFAULT_INSTALLATION_THEME.baseTheme;
		this.customThemeCss = snapshot?.customThemeCss ?? DEFAULT_INSTALLATION_THEME.customThemeCss;
	}

	setBaseTheme(theme: ThemeType): void {
		if (!isThemeType(theme)) {
			return;
		}
		this.baseTheme = theme;
		this.persist();
	}

	setCustomThemeCss(css: string | null): void {
		this.customThemeCss = normalizeCustomThemeCss(css);
		this.persist();
	}

	private persist(): void {
		try {
			AppStorage.setItem(
				AppStorageKey.INSTALLATION_THEME,
				serializeInstallationThemeSnapshot({baseTheme: this.baseTheme, customThemeCss: this.customThemeCss}),
			);
		} catch (error) {
			logger.warn('Failed to save the installation theme', error);
		}
	}
}

export default new InstallationTheme();
