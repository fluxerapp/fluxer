// SPDX-License-Identifier: AGPL-3.0-or-later

import {lookupGeoip} from '../utils/IpUtils';

const KLIPY_DEFAULT_COUNTRY = 'US';

export async function resolveGifRequestCountry(req: Request): Promise<string> {
	try {
		const geoip = await lookupGeoip(req);
		return geoip.countryCode?.trim().toUpperCase() || KLIPY_DEFAULT_COUNTRY;
	} catch {
		return KLIPY_DEFAULT_COUNTRY;
	}
}
