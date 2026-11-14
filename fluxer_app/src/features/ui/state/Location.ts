// SPDX-License-Identifier: AGPL-3.0-or-later

import {makePersistent} from '@app/features/platform/utils/MobXPersistence';
import {initializeStore} from '@app/features/platform/utils/StoreInitialization';
import {makeAutoObservable} from 'mobx';

class Location {
	lastLocation: string | null = null;
	isHydrated = false;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		initializeStore(this, () => this.initPersistence());
	}

	private async initPersistence(): Promise<void> {
		await makePersistent(this, 'Location', ['lastLocation']);
		this.isHydrated = true;
	}

	getLastLocation(): string | null {
		return this.lastLocation;
	}

	saveLocation(location: string): void {
		if (location && location !== this.lastLocation) {
			this.lastLocation = location;
		}
	}

	clearLastLocation(): void {
		this.lastLocation = null;
	}
}

export default new Location();
