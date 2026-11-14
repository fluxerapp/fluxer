// SPDX-License-Identifier: AGPL-3.0-or-later

import {AppStorageKey} from '@app/features/platform/state/AppStorageKeys';
import AppStorage from '@app/features/platform/state/PersistentStorage';
import {Logger} from '@app/features/platform/utils/AppLogger';
import type {ThemeType} from '@fluxer/constants/src/UserConstants';
import {ThemeTypes} from '@fluxer/constants/src/UserConstants';
import {isRecord} from '@fluxer/record_utils/src/RecordGuards';
import {makeAutoObservable} from 'mobx';

const logger = new Logger('AccountTheme');

interface AccountThemeSnapshot {
	readonly syncAcrossDevices: boolean;
	readonly localTheme: ThemeType;
	readonly serverTheme: ThemeType;
}

const VALID_THEMES = new Set<string>(Object.values(ThemeTypes));

const DEFAULT_ACCOUNT_THEME: AccountThemeSnapshot = Object.freeze({
	syncAcrossDevices: true,
	localTheme: ThemeTypes.DARK,
	serverTheme: ThemeTypes.DARK,
});

export function isThemeType(theme: unknown): theme is ThemeType {
	return typeof theme === 'string' && VALID_THEMES.has(theme);
}

export function parseAccountThemeSnapshot(raw: string | null | undefined): AccountThemeSnapshot | null {
	if (raw == null) {
		return null;
	}
	let parsed: unknown;
	try {
		parsed = JSON.parse(raw);
	} catch (error) {
		if (!(error instanceof SyntaxError)) {
			logger.warn('Failed to decode a stored account theme', error);
		}
		return null;
	}
	if (!isRecord(parsed)) {
		return null;
	}
	return {
		syncAcrossDevices:
			typeof parsed.syncAcrossDevices === 'boolean'
				? parsed.syncAcrossDevices
				: DEFAULT_ACCOUNT_THEME.syncAcrossDevices,
		localTheme: isThemeType(parsed.localTheme) ? parsed.localTheme : DEFAULT_ACCOUNT_THEME.localTheme,
		serverTheme: isThemeType(parsed.serverTheme) ? parsed.serverTheme : DEFAULT_ACCOUNT_THEME.serverTheme,
	};
}

export function serializeAccountThemeSnapshot(snapshot: AccountThemeSnapshot): string {
	return JSON.stringify({
		syncAcrossDevices: snapshot.syncAcrossDevices,
		localTheme: snapshot.localTheme,
		serverTheme: snapshot.serverTheme,
	});
}

function readStoredSnapshot(): AccountThemeSnapshot {
	try {
		return parseAccountThemeSnapshot(AppStorage.getItem(AppStorageKey.ACCOUNT_THEME)) ?? DEFAULT_ACCOUNT_THEME;
	} catch (error) {
		logger.warn('Failed to read the account theme', error);
		return DEFAULT_ACCOUNT_THEME;
	}
}

class AccountTheme {
	syncAcrossDevices = DEFAULT_ACCOUNT_THEME.syncAcrossDevices;
	localTheme: ThemeType = DEFAULT_ACCOUNT_THEME.localTheme;
	serverTheme: ThemeType = DEFAULT_ACCOUNT_THEME.serverTheme;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		this.load();
	}

	get themePreference(): ThemeType {
		return this.syncAcrossDevices ? this.serverTheme : this.localTheme;
	}

	load(): void {
		const snapshot = readStoredSnapshot();
		this.syncAcrossDevices = snapshot.syncAcrossDevices;
		this.localTheme = snapshot.localTheme;
		this.serverTheme = snapshot.serverTheme;
	}

	setTheme(theme: ThemeType): void {
		if (!isThemeType(theme)) {
			return;
		}
		if (this.syncAcrossDevices) {
			this.serverTheme = theme;
		} else {
			this.localTheme = theme;
		}
		this.persist();
	}

	setSyncAcrossDevices(sync: boolean): void {
		if (sync === this.syncAcrossDevices) {
			return;
		}
		if (!sync) {
			this.localTheme = this.themePreference;
		}
		this.syncAcrossDevices = sync;
		this.persist();
	}

	updateServerTheme(theme: string | null | undefined): void {
		this.serverTheme = isThemeType(theme) ? theme : ThemeTypes.DARK;
		this.persist();
	}

	private persist(): void {
		try {
			AppStorage.setItem(
				AppStorageKey.ACCOUNT_THEME,
				serializeAccountThemeSnapshot({
					syncAcrossDevices: this.syncAcrossDevices,
					localTheme: this.localTheme,
					serverTheme: this.serverTheme,
				}),
			);
		} catch (error) {
			logger.warn('Failed to save the account theme', error);
		}
	}
}

export default new AccountTheme();
