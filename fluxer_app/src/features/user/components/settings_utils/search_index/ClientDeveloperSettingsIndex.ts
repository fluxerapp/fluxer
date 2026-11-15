// SPDX-License-Identifier: AGPL-3.0-or-later

import type {SearchableSettingDescriptor} from '@app/features/user/components/settings_utils/search_index/SearchIndexTypes';
import {DEVELOPER_MODE_DESCRIPTOR} from '@app/features/user/components/settings_utils/section_registry/SharedDescriptors';
import {DEVELOPER_DESCRIPTOR, ENABLE_DEVELOPER_MODE_DESCRIPTOR} from '@app/features/user/utils/UserMessageDescriptors';
import {msg} from '@lingui/core/macro';

const DEBUG_DESCRIPTOR = msg({
	message: 'Debug',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const COPY_IDENTIFIER_DESCRIPTOR = msg({
	message: 'Copy identifier',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});

export const clientDeveloperSettingsIndex: Array<SearchableSettingDescriptor> = [
	{
		id: 'client-developer-mode',
		tabType: 'advanced_settings',
		label: DEVELOPER_MODE_DESCRIPTOR,
		keywords: [DEVELOPER_DESCRIPTOR, DEVELOPER_MODE_DESCRIPTOR, DEBUG_DESCRIPTOR, COPY_IDENTIFIER_DESCRIPTOR],
		description: ENABLE_DEVELOPER_MODE_DESCRIPTOR,
		audience: 'advanced',
		tags: ['developer'],
	},
];
