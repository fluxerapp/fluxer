// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {getElectronAPI} from '@app/features/ui/utils/NativeUtils';
import {makeAutoObservable, runInAction} from 'mobx';

const logger = new Logger('ScreenReader');

class ScreenReader {
	nativeScreenReaderActive = false;
	private bridgeStarted = false;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	get isActive(): boolean {
		return this.nativeScreenReaderActive || Accessibility.screenReaderAnnounceNewMessages;
	}

	startDesktopBridge(): void {
		if (this.bridgeStarted) return;
		const electronApi = getElectronAPI();
		if (!electronApi || typeof electronApi.getAccessibilitySupportEnabled !== 'function') return;
		this.bridgeStarted = true;
		void electronApi
			.getAccessibilitySupportEnabled()
			.then((enabled) => {
				runInAction(() => {
					this.nativeScreenReaderActive = enabled;
				});
			})
			.catch((error: unknown) => {
				logger.warn('Failed to read initial accessibility-support state', error);
			});
		electronApi.onAccessibilitySupportChanged?.((enabled) => {
			runInAction(() => {
				this.nativeScreenReaderActive = enabled;
			});
		});
	}
}

export default new ScreenReader();
