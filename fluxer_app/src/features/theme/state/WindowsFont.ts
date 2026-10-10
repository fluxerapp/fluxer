// SPDX-License-Identifier: AGPL-3.0-or-later

import {AppStorageKey} from '@app/features/platform/state/AppStorageKeys';
import {makePersistent} from '@app/features/platform/utils/MobXPersistence';
import {initializeStore} from '@app/features/platform/utils/StoreInitialization';
import {makeAutoObservable} from 'mobx';

export const WINDOWS_FONT_CHANGED_AT_MS = Date.parse('2026-10-10T00:00:00.000Z');

class WindowsFontState {
	useFluxerSans = false;
	nagbarDismissed = false;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		initializeStore(this, () => this.initPersistence());
	}

	private async initPersistence(): Promise<void> {
		await makePersistent(this, AppStorageKey.WINDOWS_FONT, ['useFluxerSans', 'nagbarDismissed'], {
			syncAcrossTabs: true,
		});
	}

	setUseFluxerSans(value: boolean): void {
		this.useFluxerSans = value;
		if (value) this.nagbarDismissed = true;
	}

	dismissNagbar(): void {
		this.nagbarDismissed = true;
	}
}

export default new WindowsFontState();
