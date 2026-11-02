// SPDX-License-Identifier: AGPL-3.0-or-later

import {OPERATOR_PASS_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {
	OPERATOR_PASS_PASSES_SECTION_DESCRIPTOR,
	OPERATOR_PASS_WALL_SECTION_DESCRIPTOR,
} from '@app/features/premium/utils/PremiumMessageDescriptors';
import type {SearchableSettingDescriptor} from '@app/features/user/components/settings_utils/search_index/SearchIndexTypes';
import {msg} from '@lingui/core/macro';

const CERTIFICATE_DESCRIPTOR = msg({
	message: 'Operator certificate',
	comment:
		'Settings search synonym for the Operator Pass settings tab. The Operator certificate is the PDF that comes with an Operator Pass. Never translate certificate as diploma, card, license or token.',
});
const OPERATORS_CHANNEL_DESCRIPTOR = msg({
	message: '#operators',
	comment: 'Settings search synonym for the Operator Pass settings tab. #operators is a channel name, keep it as is.',
});
const OPERATOR_PASS_SEARCH_DESCRIPTION_DESCRIPTOR = msg({
	message: "Your passes, your certificate and your spot on the Operators' wall",
	comment:
		"Settings search entry description for the Operator Pass settings tab. One-line summary of what the tab holds. Operators' wall is the name of a section of the homepage.",
});

export const operatorPassIndex: Array<SearchableSettingDescriptor> = [
	{
		id: 'operator-pass',
		tabType: 'operator_pass',
		label: OPERATOR_PASS_NAME,
		keywords: [
			OPERATOR_PASS_PASSES_SECTION_DESCRIPTOR,
			OPERATOR_PASS_WALL_SECTION_DESCRIPTOR,
			CERTIFICATE_DESCRIPTOR,
			OPERATORS_CHANNEL_DESCRIPTOR,
		],
		description: OPERATOR_PASS_SEARCH_DESCRIPTION_DESCRIPTOR,
	},
];
