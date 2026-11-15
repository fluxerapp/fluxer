// SPDX-License-Identifier: AGPL-3.0-or-later

import {getNativeLocaleIdentifier} from '@app/features/platform/types/Platform';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {getElectronAPI} from '@app/features/ui/utils/NativeUtils';
import {makeAutoObservable, runInAction} from 'mobx';

const logger = new Logger('SystemLocale');

class SystemLocale {
	locale: string = getNativeLocaleIdentifier() ?? 'en-US';
	private bridgeStarted = false;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	startDesktopBridge(): void {
		if (this.bridgeStarted) return;
		const electronApi = getElectronAPI();
		if (!electronApi || typeof electronApi.getSystemLocale !== 'function') return;
		this.bridgeStarted = true;
		void electronApi
			.getSystemLocale()
			.then((locale) => {
				if (locale === '') return;
				runInAction(() => {
					this.locale = locale;
				});
			})
			.catch((error: unknown) => {
				logger.warn('Failed to read the system locale', error);
			});
	}
}

export default new SystemLocale();
