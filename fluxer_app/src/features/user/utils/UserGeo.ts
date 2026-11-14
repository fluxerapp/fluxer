// SPDX-License-Identifier: AGPL-3.0-or-later

import {getRegionDisplayName as resolveRegionName} from '@fluxer/geo_utils/src/RegionFormatting';
import type {I18n, MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const YOUR_REGION_DESCRIPTOR = msg({
	message: 'your region',
	comment: 'Short label in the region display names. Keep it concise.',
});

const US_STATE_DESCRIPTORS: Record<string, MessageDescriptor> = {
	AL: msg({
		message: 'Alabama',
		context: 'us-state',
		comment: 'US state name, shown next to the country name in the mature content gate.',
	}),
	AK: msg({
		message: 'Alaska',
		context: 'us-state',
		comment: 'US state name, shown next to the country name in the mature content gate.',
	}),
	AZ: msg({
		message: 'Arizona',
		context: 'us-state',
		comment: 'US state name, shown next to the country name in the mature content gate.',
	}),
	AR: msg({
		message: 'Arkansas',
		context: 'us-state',
		comment: 'US state name, shown next to the country name in the mature content gate.',
	}),
	CA: msg({
		message: 'California',
		context: 'us-state',
		comment: 'US state name, shown next to the country name in the mature content gate.',
	}),
	CO: msg({
		message: 'Colorado',
		context: 'us-state',
		comment: 'US state name, shown next to the country name in the mature content gate.',
	}),
	CT: msg({
		message: 'Connecticut',
		context: 'us-state',
		comment: 'US state name, shown next to the country name in the mature content gate.',
	}),
	DE: msg({
		message: 'Delaware',
		context: 'us-state',
		comment: 'US state name, shown next to the country name in the mature content gate.',
	}),
	FL: msg({
		message: 'Florida',
		context: 'us-state',
		comment: 'US state name, shown next to the country name in the mature content gate.',
	}),
	GA: msg({
		message: 'Georgia',
		context: 'us-state',
		comment: 'US state name, shown next to the country name in the mature content gate.',
	}),
	HI: msg({
		message: 'Hawaii',
		context: 'us-state',
		comment: 'US state name, shown next to the country name in the mature content gate.',
	}),
	ID: msg({
		message: 'Idaho',
		context: 'us-state',
		comment: 'US state name, shown next to the country name in the mature content gate.',
	}),
	IL: msg({
		message: 'Illinois',
		context: 'us-state',
		comment: 'US state name, shown next to the country name in the mature content gate.',
	}),
	IN: msg({
		message: 'Indiana',
		context: 'us-state',
		comment: 'US state name, shown next to the country name in the mature content gate.',
	}),
	IA: msg({
		message: 'Iowa',
		context: 'us-state',
		comment: 'US state name, shown next to the country name in the mature content gate.',
	}),
	KS: msg({
		message: 'Kansas',
		context: 'us-state',
		comment: 'US state name, shown next to the country name in the mature content gate.',
	}),
	KY: msg({
		message: 'Kentucky',
		context: 'us-state',
		comment: 'US state name, shown next to the country name in the mature content gate.',
	}),
	LA: msg({
		message: 'Louisiana',
		context: 'us-state',
		comment: 'US state name, shown next to the country name in the mature content gate.',
	}),
	ME: msg({
		message: 'Maine',
		context: 'us-state',
		comment: 'US state name, shown next to the country name in the mature content gate.',
	}),
	MD: msg({
		message: 'Maryland',
		context: 'us-state',
		comment: 'US state name, shown next to the country name in the mature content gate.',
	}),
	MA: msg({
		message: 'Massachusetts',
		context: 'us-state',
		comment: 'US state name, shown next to the country name in the mature content gate.',
	}),
	MI: msg({
		message: 'Michigan',
		context: 'us-state',
		comment: 'US state name, shown next to the country name in the mature content gate.',
	}),
	MN: msg({
		message: 'Minnesota',
		context: 'us-state',
		comment: 'US state name, shown next to the country name in the mature content gate.',
	}),
	MS: msg({
		message: 'Mississippi',
		context: 'us-state',
		comment: 'US state name, shown next to the country name in the mature content gate.',
	}),
	MO: msg({
		message: 'Missouri',
		context: 'us-state',
		comment: 'US state name, shown next to the country name in the mature content gate.',
	}),
	MT: msg({
		message: 'Montana',
		context: 'us-state',
		comment: 'US state name, shown next to the country name in the mature content gate.',
	}),
	NE: msg({
		message: 'Nebraska',
		context: 'us-state',
		comment: 'US state name, shown next to the country name in the mature content gate.',
	}),
	NV: msg({
		message: 'Nevada',
		context: 'us-state',
		comment: 'US state name, shown next to the country name in the mature content gate.',
	}),
	NH: msg({
		message: 'New Hampshire',
		context: 'us-state',
		comment: 'US state name, shown next to the country name in the mature content gate.',
	}),
	NJ: msg({
		message: 'New Jersey',
		context: 'us-state',
		comment: 'US state name, shown next to the country name in the mature content gate.',
	}),
	NM: msg({
		message: 'New Mexico',
		context: 'us-state',
		comment: 'US state name, shown next to the country name in the mature content gate.',
	}),
	NY: msg({
		message: 'New York',
		context: 'us-state',
		comment: 'US state name, shown next to the country name in the mature content gate.',
	}),
	NC: msg({
		message: 'North Carolina',
		context: 'us-state',
		comment: 'US state name, shown next to the country name in the mature content gate.',
	}),
	ND: msg({
		message: 'North Dakota',
		context: 'us-state',
		comment: 'US state name, shown next to the country name in the mature content gate.',
	}),
	OH: msg({
		message: 'Ohio',
		context: 'us-state',
		comment: 'US state name, shown next to the country name in the mature content gate.',
	}),
	OK: msg({
		message: 'Oklahoma',
		context: 'us-state',
		comment: 'US state name, shown next to the country name in the mature content gate.',
	}),
	OR: msg({
		message: 'Oregon',
		context: 'us-state',
		comment: 'US state name, shown next to the country name in the mature content gate.',
	}),
	PA: msg({
		message: 'Pennsylvania',
		context: 'us-state',
		comment: 'US state name, shown next to the country name in the mature content gate.',
	}),
	RI: msg({
		message: 'Rhode Island',
		context: 'us-state',
		comment: 'US state name, shown next to the country name in the mature content gate.',
	}),
	SC: msg({
		message: 'South Carolina',
		context: 'us-state',
		comment: 'US state name, shown next to the country name in the mature content gate.',
	}),
	SD: msg({
		message: 'South Dakota',
		context: 'us-state',
		comment: 'US state name, shown next to the country name in the mature content gate.',
	}),
	TN: msg({
		message: 'Tennessee',
		context: 'us-state',
		comment: 'US state name, shown next to the country name in the mature content gate.',
	}),
	TX: msg({
		message: 'Texas',
		context: 'us-state',
		comment: 'US state name, shown next to the country name in the mature content gate.',
	}),
	UT: msg({
		message: 'Utah',
		context: 'us-state',
		comment: 'US state name, shown next to the country name in the mature content gate.',
	}),
	VT: msg({
		message: 'Vermont',
		context: 'us-state',
		comment: 'US state name, shown next to the country name in the mature content gate.',
	}),
	VA: msg({
		message: 'Virginia',
		context: 'us-state',
		comment: 'US state name, shown next to the country name in the mature content gate.',
	}),
	WA: msg({
		message: 'Washington',
		context: 'us-state',
		comment: 'US state name, shown next to the country name in the mature content gate.',
	}),
	WV: msg({
		message: 'West Virginia',
		context: 'us-state',
		comment: 'US state name, shown next to the country name in the mature content gate.',
	}),
	WI: msg({
		message: 'Wisconsin',
		context: 'us-state',
		comment: 'US state name, shown next to the country name in the mature content gate.',
	}),
	WY: msg({
		message: 'Wyoming',
		context: 'us-state',
		comment: 'US state name, shown next to the country name in the mature content gate.',
	}),
	DC: msg({
		message: 'District of Columbia',
		context: 'us-state',
		comment: 'US federal district name, shown next to the country name in the mature content gate.',
	}),
};

export function getRegionDisplayName(i18n: I18n, countryCode?: string, regionCode?: string): string {
	if (!countryCode) {
		return i18n._(YOUR_REGION_DESCRIPTOR);
	}
	try {
		const countryName = resolveRegionName(countryCode, {locale: i18n.locale});
		if (countryCode === 'US' && regionCode) {
			try {
				const stateName = getUSStateName(i18n, regionCode);
				return `${stateName}, ${countryName || 'United States'}`;
			} catch {
				return countryName || countryCode;
			}
		}
		return countryName || countryCode;
	} catch {
		return countryCode;
	}
}

const getUSStateName = (i18n: I18n, stateCode: string): string => {
	const descriptor = US_STATE_DESCRIPTORS[stateCode];
	return descriptor ? i18n._(descriptor) : stateCode;
};
