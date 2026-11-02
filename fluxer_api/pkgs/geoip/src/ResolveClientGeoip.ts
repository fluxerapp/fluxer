// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GeoEntry, GeolocationResponse} from '@fluxer/instance_bootstrap/src/Types';
import {lookupGeoipByIp} from '@pkgs/geoip/src/GeoipLookup';

export async function resolveClientGeoip(
	ip: string | null,
	maxmindDbPath: string | undefined,
	agePolicy: {restricted: ReadonlyArray<GeoEntry>},
): Promise<GeolocationResponse> {
	const response: GeolocationResponse = {
		countryCode: null,
		regionCode: null,
		latitude: null,
		longitude: null,
		ageRestrictedGeos: agePolicy.restricted,
		ageBlockedGeos: [],
	};
	if (!ip) {
		return response;
	}
	const geoip = await lookupGeoipByIp(ip, maxmindDbPath);
	response.countryCode = geoip.countryCode;
	response.regionCode = geoip.regionCode ?? null;
	response.latitude = geoip.latitude != null ? geoip.latitude.toString() : null;
	response.longitude = geoip.longitude != null ? geoip.longitude.toString() : null;
	return response;
}
