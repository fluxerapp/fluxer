/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {makeAutoObservable} from 'mobx';
import {makePersistent} from '~/lib/MobXPersistence';

interface MobileLayoutState {
	navExpanded: boolean;
	chatExpanded: boolean;
}

class LocationStore {
	lastLocation: string | null = null;
	lastMobileLayoutState: MobileLayoutState | null = null;
	isHydrated = false;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		void this.initPersistence();
	}

	private async initPersistence(): Promise<void> {
		await makePersistent(this, 'LocationStore', ['lastLocation', 'lastMobileLayoutState']);
		this.isHydrated = true;
	}

	getLastLocation(): string | null {
		return this.lastLocation;
	}

	getLastMobileLayoutState(): MobileLayoutState | null {
		return this.lastMobileLayoutState;
	}

	saveLocation(location: string): void {
		if (location && location !== this.lastLocation) {
			this.lastLocation = location;
		}
	}

	saveMobileLayoutState(mobileLayoutState: MobileLayoutState): void {
		this.lastMobileLayoutState = mobileLayoutState;
	}

	saveLocationAndMobileState(location: string, mobileLayoutState: MobileLayoutState): void {
		this.lastLocation = location;
		this.lastMobileLayoutState = mobileLayoutState;
	}

	clearLastLocation(): void {
		this.lastLocation = null;
		this.lastMobileLayoutState = null;
	}
}

export default new LocationStore();
