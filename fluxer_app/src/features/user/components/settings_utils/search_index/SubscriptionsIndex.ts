// SPDX-License-Identifier: AGPL-3.0-or-later

import type {SearchableSettingDescriptor} from '@app/features/user/components/settings_utils/search_index/SearchIndexTypes';
import {PREMIUM_PRODUCT_NAME_KEYWORD} from '@app/features/user/components/settings_utils/section_registry/SharedDescriptors';
import {msg} from '@lingui/core/macro';

const SUBSCRIPTION_DESCRIPTOR = msg({
	message: 'Subscription',
	comment: 'Settings search entry label. Also used as a search synonym in the settings search bar.',
});
const UPGRADE_DESCRIPTOR = msg({
	message: 'Upgrade',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const PLAN_DESCRIPTOR = msg({
	message: 'Plan',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const BILLING_DESCRIPTOR = msg({
	message: 'Billing',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CANCEL_SUBSCRIPTION_DESCRIPTOR = msg({
	message: 'Cancel subscription',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const REACTIVATE_SUBSCRIPTION_DESCRIPTOR = msg({
	message: 'Reactivate subscription',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CUSTOMER_PORTAL_DESCRIPTOR = msg({
	message: 'Customer portal',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MANAGE_YOUR_SUBSCRIPTION_DESCRIPTOR = msg({
	message: 'Manage your subscription',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});

export const subscriptionsIndex: Array<SearchableSettingDescriptor> = [
	{
		id: 'subscriptions',
		tabType: 'subscriptions',
		label: SUBSCRIPTION_DESCRIPTOR,
		keywords: [
			PREMIUM_PRODUCT_NAME_KEYWORD,
			SUBSCRIPTION_DESCRIPTOR,
			UPGRADE_DESCRIPTOR,
			PLAN_DESCRIPTOR,
			BILLING_DESCRIPTOR,
			CANCEL_SUBSCRIPTION_DESCRIPTOR,
			REACTIVATE_SUBSCRIPTION_DESCRIPTOR,
			CUSTOMER_PORTAL_DESCRIPTOR,
		],
		description: MANAGE_YOUR_SUBSCRIPTION_DESCRIPTOR,
	},
];
