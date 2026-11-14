// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ReportType} from '@app/features/moderation/components/report/ReportTypes';
import type {MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const REPORT_A_MESSAGE_DESCRIPTOR = msg({
	message: 'Report a message',
	comment: 'Report flow target option. Selects reporting a specific chat message. Keep tone plain and neutral.',
});
const REPORT_A_USER_PROFILE_DESCRIPTOR = msg({
	message: 'Report a user profile',
	comment: 'Report flow target option. Selects reporting another user account or profile. Keep tone plain and neutral.',
});
const REPORT_A_COMMUNITY_DESCRIPTOR = msg({
	message: 'Report a community',
	comment:
		'Report flow target option. Selects reporting an entire community (server). Use the localized term for community, not server.',
});
const SELECT_A_COUNTRY_DESCRIPTOR = msg({
	message: 'Select a country',
	comment: 'Placeholder option label for the country dropdown in the EU illegal-content report flow.',
});
const AUSTRIA_DESCRIPTOR = msg({
	message: 'Austria',
	comment: 'Country name. EU member state in the illegal-content report country dropdown.',
});
const BELGIUM_DESCRIPTOR = msg({
	message: 'Belgium',
	comment: 'Country name. EU member state in the illegal-content report country dropdown.',
});
const BULGARIA_DESCRIPTOR = msg({
	message: 'Bulgaria',
	comment: 'Country name. EU member state in the illegal-content report country dropdown.',
});
const CROATIA_DESCRIPTOR = msg({
	message: 'Croatia',
	comment: 'Country name. EU member state in the illegal-content report country dropdown.',
});
const CYPRUS_DESCRIPTOR = msg({
	message: 'Cyprus',
	comment: 'Country name. EU member state in the illegal-content report country dropdown.',
});
const CZECH_REPUBLIC_DESCRIPTOR = msg({
	message: 'Czech Republic',
	comment: 'Country name. EU member state in the illegal-content report country dropdown.',
});
const DENMARK_DESCRIPTOR = msg({
	message: 'Denmark',
	comment: 'Country name. EU member state in the illegal-content report country dropdown.',
});
const ESTONIA_DESCRIPTOR = msg({
	message: 'Estonia',
	comment: 'Country name. EU member state in the illegal-content report country dropdown.',
});
const FINLAND_DESCRIPTOR = msg({
	message: 'Finland',
	comment: 'Country name. EU member state in the illegal-content report country dropdown.',
});
const FRANCE_DESCRIPTOR = msg({
	message: 'France',
	comment: 'Country name. EU member state in the illegal-content report country dropdown.',
});
const GERMANY_DESCRIPTOR = msg({
	message: 'Germany',
	comment: 'Country name. EU member state in the illegal-content report country dropdown.',
});
const GREECE_DESCRIPTOR = msg({
	message: 'Greece',
	comment: 'Country name. EU member state in the illegal-content report country dropdown.',
});
const HUNGARY_DESCRIPTOR = msg({
	message: 'Hungary',
	comment: 'Country name. EU member state in the illegal-content report country dropdown.',
});
const IRELAND_DESCRIPTOR = msg({
	message: 'Ireland',
	comment: 'Country name. EU member state in the illegal-content report country dropdown.',
});
const ITALY_DESCRIPTOR = msg({
	message: 'Italy',
	comment: 'Country name. EU member state in the illegal-content report country dropdown.',
});
const LATVIA_DESCRIPTOR = msg({
	message: 'Latvia',
	comment: 'Country name. EU member state in the illegal-content report country dropdown.',
});
const LITHUANIA_DESCRIPTOR = msg({
	message: 'Lithuania',
	comment: 'Country name. EU member state in the illegal-content report country dropdown.',
});
const LUXEMBOURG_DESCRIPTOR = msg({
	message: 'Luxembourg',
	comment: 'Country name. EU member state in the illegal-content report country dropdown.',
});
const MALTA_DESCRIPTOR = msg({
	message: 'Malta',
	comment: 'Country name. EU member state in the illegal-content report country dropdown.',
});
const NETHERLANDS_DESCRIPTOR = msg({
	message: 'Netherlands',
	comment: 'Country name. EU member state in the illegal-content report country dropdown.',
});
const POLAND_DESCRIPTOR = msg({
	message: 'Poland',
	comment: 'Country name. EU member state in the illegal-content report country dropdown.',
});
const PORTUGAL_DESCRIPTOR = msg({
	message: 'Portugal',
	comment: 'Country name. EU member state in the illegal-content report country dropdown.',
});
const ROMANIA_DESCRIPTOR = msg({
	message: 'Romania',
	comment: 'Country name. EU member state in the illegal-content report country dropdown.',
});
const SLOVAKIA_DESCRIPTOR = msg({
	message: 'Slovakia',
	comment: 'Country name. EU member state in the illegal-content report country dropdown.',
});
const SLOVENIA_DESCRIPTOR = msg({
	message: 'Slovenia',
	comment: 'Country name. EU member state in the illegal-content report country dropdown.',
});
const SPAIN_DESCRIPTOR = msg({
	message: 'Spain',
	comment: 'Country name. EU member state in the illegal-content report country dropdown.',
});
const SWEDEN_DESCRIPTOR = msg({
	message: 'Sweden',
	comment: 'Country name. EU member state in the illegal-content report country dropdown.',
});

export interface SelectDescriptor {
	value: string;
	label: MessageDescriptor;
}

export interface RadioDescriptor<T> {
	value: T;
	name: MessageDescriptor;
}

export const REPORT_TYPE_OPTION_DESCRIPTORS: ReadonlyArray<RadioDescriptor<ReportType>> = [
	{value: 'message', name: REPORT_A_MESSAGE_DESCRIPTOR},
	{value: 'user', name: REPORT_A_USER_PROFILE_DESCRIPTOR},
	{value: 'guild', name: REPORT_A_COMMUNITY_DESCRIPTOR},
];
export const COUNTRY_OPTIONS: ReadonlyArray<SelectDescriptor> = [
	{value: '', label: SELECT_A_COUNTRY_DESCRIPTOR},
	{value: 'AT', label: AUSTRIA_DESCRIPTOR},
	{value: 'BE', label: BELGIUM_DESCRIPTOR},
	{value: 'BG', label: BULGARIA_DESCRIPTOR},
	{value: 'HR', label: CROATIA_DESCRIPTOR},
	{value: 'CY', label: CYPRUS_DESCRIPTOR},
	{value: 'CZ', label: CZECH_REPUBLIC_DESCRIPTOR},
	{value: 'DK', label: DENMARK_DESCRIPTOR},
	{value: 'EE', label: ESTONIA_DESCRIPTOR},
	{value: 'FI', label: FINLAND_DESCRIPTOR},
	{value: 'FR', label: FRANCE_DESCRIPTOR},
	{value: 'DE', label: GERMANY_DESCRIPTOR},
	{value: 'GR', label: GREECE_DESCRIPTOR},
	{value: 'HU', label: HUNGARY_DESCRIPTOR},
	{value: 'IE', label: IRELAND_DESCRIPTOR},
	{value: 'IT', label: ITALY_DESCRIPTOR},
	{value: 'LV', label: LATVIA_DESCRIPTOR},
	{value: 'LT', label: LITHUANIA_DESCRIPTOR},
	{value: 'LU', label: LUXEMBOURG_DESCRIPTOR},
	{value: 'MT', label: MALTA_DESCRIPTOR},
	{value: 'NL', label: NETHERLANDS_DESCRIPTOR},
	{value: 'PL', label: POLAND_DESCRIPTOR},
	{value: 'PT', label: PORTUGAL_DESCRIPTOR},
	{value: 'RO', label: ROMANIA_DESCRIPTOR},
	{value: 'SK', label: SLOVAKIA_DESCRIPTOR},
	{value: 'SI', label: SLOVENIA_DESCRIPTOR},
	{value: 'ES', label: SPAIN_DESCRIPTOR},
	{value: 'SE', label: SWEDEN_DESCRIPTOR},
];
