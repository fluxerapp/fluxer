// SPDX-License-Identifier: AGPL-3.0-or-later

import {AppStorageKey} from '@app/features/platform/state/AppStorageKeys';
import {makePersistent} from '@app/features/platform/utils/MobXPersistence';
import {initializeStore} from '@app/features/platform/utils/StoreInitialization';
import {makeAutoObservable} from 'mobx';

class BackgroundAccountPresence {
	appearOffline = true;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		initializeStore(this, () => makePersistent(this, AppStorageKey.BACKGROUND_ACCOUNT_PRESENCE, ['appearOffline']));
	}

	setAppearOffline(appearOffline: boolean): void {
		this.appearOffline = appearOffline;
	}
}

export default new BackgroundAccountPresence();
