// SPDX-License-Identifier: AGPL-3.0-or-later

import type {SearchableSettingDescriptor} from '@app/features/user/components/settings_utils/search_index/SearchIndexTypes';
import {
	BLOCKED_USERS_DESCRIPTOR,
	USERS_DESCRIPTOR,
} from '@app/features/user/components/settings_utils/section_registry/SharedDescriptors';
import {msg} from '@lingui/core/macro';

const BLOCKED_DESCRIPTOR = msg({
	message: 'Blocked',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const BLOCK_DESCRIPTOR = msg({
	message: 'Block',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});

const UNBLOCK_DESCRIPTOR = msg({
	message: 'Unblock',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MANAGE_BLOCKED_USERS_DESCRIPTOR = msg({
	message: 'Manage blocked users',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
export const blockedUsersIndex: Array<SearchableSettingDescriptor> = [
	{
		id: 'blocked-users',
		tabType: 'account_security',
		sectionId: 'blocked_users',
		label: BLOCKED_USERS_DESCRIPTOR,
		keywords: [BLOCKED_DESCRIPTOR, BLOCK_DESCRIPTOR, USERS_DESCRIPTOR, UNBLOCK_DESCRIPTOR],
		description: MANAGE_BLOCKED_USERS_DESCRIPTOR,
	},
];
