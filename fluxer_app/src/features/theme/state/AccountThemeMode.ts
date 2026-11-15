// SPDX-License-Identifier: AGPL-3.0-or-later

import {AppStorageKey} from '@app/features/platform/state/AppStorageKeys';
import AppStorage from '@app/features/platform/state/PersistentStorage';
import {Logger} from '@app/features/platform/utils/AppLogger';
import type {ThemeTarget} from '@app/features/theme/state/ThemeEditTarget';
import {isDesktop} from '@app/features/ui/utils/NativeUtils';
import {isRecord} from '@fluxer/record_utils/src/RecordGuards';
import {makeAutoObservable} from 'mobx';

const logger = new Logger('AccountThemeMode');

function readStoredFollowDesktopTheme(): boolean {
	try {
		const raw = AppStorage.getItem(AppStorageKey.ACCOUNT_THEME_MODE);
		if (raw === null) {
			return true;
		}
		const parsed: unknown = JSON.parse(raw);
		return !(isRecord(parsed) && parsed.followDesktopTheme === false);
	} catch (error) {
		if (!(error instanceof SyntaxError)) {
			logger.warn('Failed to read the desktop theme preference', error);
		}
		return true;
	}
}

class AccountThemeMode {
	followDesktopTheme = true;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		this.load();
	}

	get canUseDesktopTheme(): boolean {
		return isDesktop();
	}

	get usesDesktopTheme(): boolean {
		return this.canUseDesktopTheme && this.followDesktopTheme;
	}

	get activeTarget(): ThemeTarget {
		return this.usesDesktopTheme ? 'desktop' : 'account';
	}

	load(): void {
		this.followDesktopTheme = readStoredFollowDesktopTheme();
	}

	setFollowDesktopTheme(value: boolean): void {
		this.followDesktopTheme = value;
		try {
			AppStorage.setItem(AppStorageKey.ACCOUNT_THEME_MODE, JSON.stringify({followDesktopTheme: value}));
		} catch (error) {
			logger.warn('Failed to save the desktop theme preference', error);
		}
	}
}

export default new AccountThemeMode();
