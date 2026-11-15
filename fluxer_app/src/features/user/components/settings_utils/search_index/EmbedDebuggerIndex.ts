// SPDX-License-Identifier: AGPL-3.0-or-later

import type {SearchableSettingDescriptor} from '@app/features/user/components/settings_utils/search_index/SearchIndexTypes';
import {EMBED_DEBUGGER_DESCRIPTOR} from '@app/features/user/components/settings_utils/section_registry/SharedDescriptors';
import {DEVELOPER_DESCRIPTOR, UNFURL_DESCRIPTOR, URL_DESCRIPTOR} from '@app/features/user/utils/UserMessageDescriptors';
import {msg} from '@lingui/core/macro';

const EMBED_DESCRIPTOR = msg({
	message: 'Embed',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});

const TEST_UNFURLED_EMBEDS_DESCRIPTOR = msg({
	message: 'Test URL unfurls and rendered embeds',
	comment: 'Settings search entry description for the developer embed debugger.',
});

export const embedDebuggerIndex: Array<SearchableSettingDescriptor> = [
	{
		id: 'embed-debugger',
		tabType: 'embed_debugger',
		label: EMBED_DEBUGGER_DESCRIPTOR,
		keywords: [DEVELOPER_DESCRIPTOR, EMBED_DESCRIPTOR, UNFURL_DESCRIPTOR, URL_DESCRIPTOR],
		description: TEST_UNFURLED_EMBEDS_DESCRIPTOR,
	},
];
