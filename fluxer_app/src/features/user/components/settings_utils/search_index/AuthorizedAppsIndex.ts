// SPDX-License-Identifier: AGPL-3.0-or-later

import type {SearchableSettingDescriptor} from '@app/features/user/components/settings_utils/search_index/SearchIndexTypes';
import {
	APPLICATIONS_DESCRIPTOR,
	AUTHORIZATION_DESCRIPTOR,
	AUTHORIZED_APPS_DESCRIPTOR,
	INTEGRATIONS_DESCRIPTOR,
} from '@app/features/user/components/settings_utils/section_registry/SharedDescriptors';
import {msg} from '@lingui/core/macro';

const AUTHORIZED_DESCRIPTOR = msg({
	message: 'Authorized',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const PERMISSIONS_DESCRIPTOR = msg({
	message: 'Permissions',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const THIRD_PARTY_DESCRIPTOR = msg({
	message: 'Third party',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MANAGE_AUTHORIZED_APPLICATIONS_DESCRIPTOR = msg({
	message: 'Manage authorized applications',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
export const authorizedAppsIndex: Array<SearchableSettingDescriptor> = [
	{
		id: 'authorized-apps',
		tabType: 'account_security',
		sectionId: 'security',
		label: AUTHORIZED_APPS_DESCRIPTOR,
		keywords: [
			APPLICATIONS_DESCRIPTOR,
			AUTHORIZED_DESCRIPTOR,
			AUTHORIZATION_DESCRIPTOR,
			PERMISSIONS_DESCRIPTOR,
			THIRD_PARTY_DESCRIPTOR,
			INTEGRATIONS_DESCRIPTOR,
		],
		description: MANAGE_AUTHORIZED_APPLICATIONS_DESCRIPTOR,
	},
];
