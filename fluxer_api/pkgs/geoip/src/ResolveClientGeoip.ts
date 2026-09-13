// SPDX-License-Identifier: AGPL-3.0-or-later

import {AGE_BLOCKED_GEOS, AGE_RESTRICTED_GEOS} from '@fluxer/instance_bootstrap/src/AgeGeos';
import type {GeolocationResponse} from '@fluxer/instance_bootstrap/src/Types';
import {extractClientIp} from '@fluxer/ip_utils/src/ClientIp';
import {lookupGeoipByIp} from '@pkgs/geoip/src/GeoipLookup';

interface ResolveClientGeoipOptions {
	maxmindDbPath: string | undefined;
	trustClientIpHeader: boolean;
	clientIpHeaderName: string;
}

export async function resolveClientGeoip(
	req: Request,
	options: ResolveClientGeoipOptions,
): Promise<GeolocationResponse> {
	const response: GeolocationResponse = {
		countryCode: null,
		regionCode: null,
		latitude: null,
		longitude: null,
		ageRestrictedGeos: AGE_RESTRICTED_GEOS,
		ageBlockedGeos: AGE_BLOCKED_GEOS,
	};
	const ip = extractClientIp(req, {
		trustClientIpHeader: options.trustClientIpHeader,
		clientIpHeaderName: options.clientIpHeaderName,
	});
	if (!ip) {
		return response;
	}
	const geoip = await lookupGeoipByIp(ip, options.maxmindDbPath);
	response.countryCode = geoip.countryCode;
	response.regionCode = geoip.regionCode ?? null;
	response.latitude = geoip.latitude != null ? geoip.latitude.toString() : null;
	response.longitude = geoip.longitude != null ? geoip.longitude.toString() : null;
	return response;
}
