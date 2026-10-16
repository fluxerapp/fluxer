// SPDX-License-Identifier: AGPL-3.0-or-later

import type {SearchableSettingDescriptor} from '@app/features/user/components/settings_utils/search_index/SearchIndexTypes';
import {
	DEVICES_DESCRIPTOR,
	LINKED_DEVICES_DESCRIPTOR,
	LOGIN_DESCRIPTOR,
} from '@app/features/user/components/settings_utils/section_registry/SharedDescriptors';
import {REVOKE_DEVICE_DESCRIPTOR} from '@app/features/user/utils/UserMessageDescriptors';
import {msg} from '@lingui/core/macro';

const MY_DEVICES_DESCRIPTOR = msg({
	message: 'My devices',
	comment: 'Settings search entry label. Names the settings search entry in the settings UI.',
});
const SESSIONS_DESCRIPTOR = msg({
	message: 'Sessions',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const ACTIVE_DESCRIPTOR = msg({
	message: 'Active',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SIGNED_IN_DESCRIPTOR = msg({
	message: 'Signed in',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CURRENT_DEVICE_DESCRIPTOR = msg({
	message: 'Current device',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const OTHER_DEVICES_DESCRIPTOR = msg({
	message: 'Other devices',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});

const VIEW_DEVICES_THAT_ARE_SIGNED_IN_TO_YOUR_DESCRIPTOR = msg({
	message: 'View devices that are signed in to your account',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
const SIGN_OUT_ALL_OTHER_DEVICES_DESCRIPTOR = msg({
	message: 'Sign out all other devices',
	comment: 'Settings search entry label. Names the settings search entry in the settings UI.',
});
const SIGN_OUT_DESCRIPTOR = msg({
	message: 'Sign out',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const LOGOUT_DESCRIPTOR = msg({
	message: 'Logout',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const LOG_OUT_DESCRIPTOR = msg({
	message: 'Log out',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});

const SIGN_OUT_OTHER_DEVICES_FROM_YOUR_ACCOUNT_DESCRIPTOR = msg({
	message: 'Sign out other devices from your account',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
export const devicesIndex: Array<SearchableSettingDescriptor> = [
	{
		id: 'devices-sessions',
		tabType: 'account_security',
		sectionId: 'security',
		label: MY_DEVICES_DESCRIPTOR,
		keywords: [
			DEVICES_DESCRIPTOR,
			SESSIONS_DESCRIPTOR,
			LOGIN_DESCRIPTOR,
			ACTIVE_DESCRIPTOR,
			SIGNED_IN_DESCRIPTOR,
			CURRENT_DEVICE_DESCRIPTOR,
			OTHER_DEVICES_DESCRIPTOR,
			LINKED_DEVICES_DESCRIPTOR,
		],
		description: VIEW_DEVICES_THAT_ARE_SIGNED_IN_TO_YOUR_DESCRIPTOR,
	},
	{
		id: 'devices-logout-all',
		tabType: 'account_security',
		sectionId: 'security',
		label: SIGN_OUT_ALL_OTHER_DEVICES_DESCRIPTOR,
		keywords: [
			SIGN_OUT_DESCRIPTOR,
			LOGOUT_DESCRIPTOR,
			LOG_OUT_DESCRIPTOR,
			DEVICES_DESCRIPTOR,
			SESSIONS_DESCRIPTOR,
			REVOKE_DEVICE_DESCRIPTOR,
		],
		description: SIGN_OUT_OTHER_DEVICES_FROM_YOUR_ACCOUNT_DESCRIPTOR,
	},
];
