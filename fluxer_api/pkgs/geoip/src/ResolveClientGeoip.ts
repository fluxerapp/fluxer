// SPDX-License-Identifier: AGPL-3.0-or-later

import {AGE_BLOCKED_GEOS, AGE_RESTRICTED_GEOS} from '@fluxer/instance_bootstrap/src/AgeGeos';
import type {GeolocationResponse} from '@fluxer/instance_bootstrap/src/Types';
import {lookupGeoipByIp} from '@pkgs/geoip/src/GeoipLookup';

export async function resolveClientGeoip(
	ip: string | null,
	maxmindDbPath: string | undefined,
): Promise<GeolocationResponse> {
	const response: GeolocationResponse = {
		countryCode: null,
		regionCode: null,
		latitude: null,
		longitude: null,
		ageRestrictedGeos: AGE_RESTRICTED_GEOS,
		ageBlockedGeos: AGE_BLOCKED_GEOS,
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
