// SPDX-License-Identifier: AGPL-3.0-or-later

import {VERIFY_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {shouldShowClaimedAccountSettings} from '@app/features/user/components/settings_utils/search_index/SearchIndexHelpers';
import type {SearchableSettingDescriptor} from '@app/features/user/components/settings_utils/search_index/SearchIndexTypes';
import {msg} from '@lingui/core/macro';

const CONNECTIONS_DESCRIPTOR = msg({
	message: 'Connections',
	comment: 'Settings search entry label. Also used as a search synonym in the settings search bar.',
});
const LINKED_ACCOUNTS_DESCRIPTOR = msg({
	message: 'Linked accounts',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const LINKED_DESCRIPTOR = msg({
	message: 'Linked',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const ACCOUNTS_DESCRIPTOR = msg({
	message: 'Accounts',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const BLUESKY_DESCRIPTOR = msg({
	message: 'Bluesky',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const TRAKT_DESCRIPTOR = msg({
	message: 'Trakt',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const DOMAIN_DESCRIPTOR = msg({
	message: 'Domain',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const VERIFICATION_DESCRIPTOR = msg({
	message: 'Verification',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const EXTERNAL_DESCRIPTOR = msg({
	message: 'External',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SOCIAL_DESCRIPTOR = msg({
	message: 'Social',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const LINK_EXTERNAL_ACCOUNTS_AND_DOMAINS_TO_YOUR_PROFILE_DESCRIPTOR = msg({
	message: 'Link external accounts and domains to your profile',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
export const linkedAccountsIndex: Array<SearchableSettingDescriptor> = [
	{
		id: 'linked-accounts-connections',
		tabType: 'linked_accounts',
		label: CONNECTIONS_DESCRIPTOR,
		keywords: [
			CONNECTIONS_DESCRIPTOR,
			LINKED_ACCOUNTS_DESCRIPTOR,
			LINKED_DESCRIPTOR,
			ACCOUNTS_DESCRIPTOR,
			BLUESKY_DESCRIPTOR,
			TRAKT_DESCRIPTOR,
			DOMAIN_DESCRIPTOR,
			VERIFY_DESCRIPTOR,
			VERIFICATION_DESCRIPTOR,
			EXTERNAL_DESCRIPTOR,
			SOCIAL_DESCRIPTOR,
		],
		description: LINK_EXTERNAL_ACCOUNTS_AND_DOMAINS_TO_YOUR_PROFILE_DESCRIPTOR,
		isVisible: shouldShowClaimedAccountSettings,
	},
];
