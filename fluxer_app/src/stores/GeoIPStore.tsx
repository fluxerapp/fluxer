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

import {makeAutoObservable, runInAction} from 'mobx';

interface GeoIPData {
	countryCode: string;
	regionCode: string | null;
	latitude: string;
	longitude: string;
	ageRestrictedGeos: Array<{countryCode: string; regionCode: string | null}>;
	ageBlockedGeos: Array<{countryCode: string; regionCode: string | null}>;
}

class GeoIPStore {
	countryCode: string | null = null;
	regionCode: string | null = null;
	latitude: string | null = null;
	longitude: string | null = null;
	ageRestrictedGeos: Array<{countryCode: string; regionCode: string | null}> = [];
	ageBlockedGeos: Array<{countryCode: string; regionCode: string | null}> = [];
	loaded = false;
	error: string | null = null;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	async fetchGeoData(): Promise<void> {
		try {
			const response = await fetch('https://ip.fluxer.workers.dev/');
			if (!response.ok) {
				throw new Error(`Failed to fetch geo data: ${response.statusText}`);
			}

			const data: GeoIPData = await response.json();

			runInAction(() => {
				this.countryCode = data.countryCode;
				this.regionCode = data.regionCode;
				this.latitude = data.latitude;
				this.longitude = data.longitude;
				this.ageRestrictedGeos = data.ageRestrictedGeos;
				this.ageBlockedGeos = data.ageBlockedGeos;
				this.loaded = true;
				this.error = null;
			});
		} catch (error) {
			runInAction(() => {
				this.countryCode = null;
				this.regionCode = null;
				this.latitude = null;
				this.longitude = null;
				this.ageRestrictedGeos = [];
				this.ageBlockedGeos = [];
				this.loaded = true;
				this.error = error instanceof Error ? error.message : 'Unknown error';
			});

			throw error;
		}
	}

	isBlocked(): boolean {
		if (!this.countryCode) return false;

		return this.ageBlockedGeos.some((geo) => {
			if (geo.countryCode !== this.countryCode) return false;
			if (geo.regionCode === null) return true;
			return geo.regionCode === this.regionCode;
		});
	}
}

export default new GeoIPStore();
