// SPDX-License-Identifier: AGPL-3.0-or-later

import {HttpResponse, http} from 'msw';

export function createIpInfoLookupHandler() {
	return http.get('https://api.ipinfo.io/lookup/:ip', ({params}) => {
		const ip = typeof params.ip === 'string' ? params.ip : '198.51.100.1';
		return HttpResponse.json({
			ip,
			geo: {
				city: 'Ashburn',
				region: 'Virginia',
				region_code: 'VA',
				country: 'United States',
				country_code: 'US',
				continent: 'North America',
				continent_code: 'NA',
			},
			as: {
				asn: 'AS64500',
				name: 'Test ISP',
				domain: 'example.com',
				type: 'isp',
			},
			anonymous: {},
		});
	});
}
