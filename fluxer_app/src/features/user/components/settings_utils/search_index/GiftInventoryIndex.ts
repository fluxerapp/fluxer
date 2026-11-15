// SPDX-License-Identifier: AGPL-3.0-or-later

import type {SearchableSettingDescriptor} from '@app/features/user/components/settings_utils/search_index/SearchIndexTypes';
import {
	CODES_DESCRIPTOR,
	PURCHASED_GIFTS_DESCRIPTOR,
	REDEEM_A_GIFT_DESCRIPTOR,
} from '@app/features/user/components/settings_utils/section_registry/SharedDescriptors';
import {GIFTS_DESCRIPTOR} from '@app/features/user/utils/UserMessageDescriptors';
import {msg} from '@lingui/core/macro';

const INVENTORY_DESCRIPTOR = msg({
	message: 'Inventory',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const REDEEM_DESCRIPTOR = msg({
	message: 'Redeem',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const GIFT_CODES_DESCRIPTOR = msg({
	message: 'Gift codes',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const GIFT_LINK_DESCRIPTOR = msg({
	message: 'Gift link',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const REDEEM_A_GIFT_CODE_FOR_YOUR_ACCOUNT_DESCRIPTOR = msg({
	message: 'Redeem a gift code for your account',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
const YOUR_PURCHASED_GIFTS_DESCRIPTOR = msg({
	message: 'Your purchased gifts',
	comment: 'Settings search entry label. Names the settings search entry in the settings UI.',
});

const GIFT_INVENTORY_DESCRIPTOR = msg({
	message: 'Gift inventory',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SHARE_GIFT_DESCRIPTOR = msg({
	message: 'Share gift',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const COPY_GIFT_LINK_DESCRIPTOR = msg({
	message: 'Copy gift link',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const REDEEM_FOR_YOURSELF_DESCRIPTOR = msg({
	message: 'Redeem for yourself',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MANAGE_PURCHASED_GIFT_CODES_AND_GIFT_URLS_DESCRIPTOR = msg({
	message: 'Manage purchased gift codes and gift URLs',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
export const giftInventoryIndex: Array<SearchableSettingDescriptor> = [
	{
		id: 'gift-inventory',
		tabType: 'gift_inventory',
		label: REDEEM_A_GIFT_DESCRIPTOR,
		keywords: [
			GIFTS_DESCRIPTOR,
			INVENTORY_DESCRIPTOR,
			CODES_DESCRIPTOR,
			REDEEM_DESCRIPTOR,
			GIFT_CODES_DESCRIPTOR,
			GIFT_LINK_DESCRIPTOR,
		],
		description: REDEEM_A_GIFT_CODE_FOR_YOUR_ACCOUNT_DESCRIPTOR,
	},
	{
		id: 'gift-purchased',
		tabType: 'gift_inventory',
		label: YOUR_PURCHASED_GIFTS_DESCRIPTOR,
		keywords: [
			PURCHASED_GIFTS_DESCRIPTOR,
			GIFT_INVENTORY_DESCRIPTOR,
			SHARE_GIFT_DESCRIPTOR,
			COPY_GIFT_LINK_DESCRIPTOR,
			REDEEM_FOR_YOURSELF_DESCRIPTOR,
		],
		description: MANAGE_PURCHASED_GIFT_CODES_AND_GIFT_URLS_DESCRIPTOR,
	},
];
