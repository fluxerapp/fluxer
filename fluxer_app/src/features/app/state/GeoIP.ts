// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import ageGeos from '@fluxer/constants/src/AgeGeos.json';
import type {GeoEntry} from '@fluxer/instance_bootstrap/src/Types';
import {GeolocationResponse} from '@fluxer/schema/src/domains/geolocation/GeolocationSchemas';
import {makeAutoObservable, runInAction} from 'mobx';

const GEOIP_PATH = '/_geoip';
const GEOIP_ATTEMPTS = 3;
const GEOIP_ATTEMPT_TIMEOUT_MS = 2000;
const GEOIP_RETRY_DELAY_MS = 200;

const logger = new Logger('GeoIP');

interface ConnectionGeoCoordinates {
	latitude: string | null;
	longitude: string | null;
}

const UNRESOLVED_GEOIP: GeolocationResponse = {
	countryCode: null,
	regionCode: null,
	latitude: null,
	longitude: null,
	ageRestrictedGeos: ageGeos.ageRestrictedGeos,
	ageBlockedGeos: ageGeos.ageBlockedGeos,
};

function wait(ms: number): Promise<void> {
	return new Promise((resolve) => setTimeout(resolve, ms));
}

async function fetchGeoipOnce(): Promise<GeolocationResponse> {
	const controller = new AbortController();
	const timer = setTimeout(() => controller.abort(), GEOIP_ATTEMPT_TIMEOUT_MS);
	try {
		const response = await fetch(GEOIP_PATH, {
			cache: 'no-store',
			credentials: 'omit',
			headers: {Accept: 'application/json'},
			signal: controller.signal,
		});
		if (!response.ok) {
			throw new Error(`GeoIP lookup failed with status ${response.status}`);
		}
		return GeolocationResponse.parse(await response.json());
	} finally {
		clearTimeout(timer);
	}
}

async function fetchGeoip(): Promise<GeolocationResponse> {
	for (let attempt = 1; ; attempt++) {
		try {
			return await fetchGeoipOnce();
		} catch (error) {
			if (attempt >= GEOIP_ATTEMPTS) {
				logger.warn('GeoIP lookup failed, continuing without a location:', error);
				return UNRESOLVED_GEOIP;
			}
			await wait(GEOIP_RETRY_DELAY_MS * attempt);
		}
	}
}

let loading: Promise<void> | null = null;

class GeoIP {
	countryCode: string | null = UNRESOLVED_GEOIP.countryCode;
	regionCode: string | null = UNRESOLVED_GEOIP.regionCode;
	latitude: string | null = UNRESOLVED_GEOIP.latitude;
	longitude: string | null = UNRESOLVED_GEOIP.longitude;
	ageRestrictedGeos: ReadonlyArray<GeoEntry> = UNRESOLVED_GEOIP.ageRestrictedGeos;
	ageBlockedGeos: ReadonlyArray<GeoEntry> = UNRESOLVED_GEOIP.ageBlockedGeos;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	load(): Promise<void> {
		loading ??= fetchGeoip().then((data) => {
			runInAction(() => {
				this.countryCode = data.countryCode;
				this.regionCode = data.regionCode;
				this.latitude = data.latitude;
				this.longitude = data.longitude;
				this.ageRestrictedGeos = data.ageRestrictedGeos;
				this.ageBlockedGeos = data.ageBlockedGeos;
			});
		});
		return loading;
	}

	applyConnectionFallbackCoordinates(data: ConnectionGeoCoordinates): void {
		if (data.latitude === null || data.longitude === null) {
			return;
		}
		if (this.latitude !== null && this.longitude !== null) {
			return;
		}
		runInAction(() => {
			if (this.latitude === null) {
				this.latitude = data.latitude;
			}
			if (this.longitude === null) {
				this.longitude = data.longitude;
			}
		});
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

export default new GeoIP();
