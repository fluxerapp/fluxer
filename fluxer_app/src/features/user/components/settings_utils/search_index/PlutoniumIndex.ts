// SPDX-License-Identifier: AGPL-3.0-or-later

import type {SearchableSettingDescriptor} from '@app/features/user/components/settings_utils/search_index/SearchIndexTypes';
import {
	CODES_DESCRIPTOR,
	PREMIUM_PRODUCT_NAME_KEYWORD,
} from '@app/features/user/components/settings_utils/section_registry/SharedDescriptors';
import {GIFTS_DESCRIPTOR} from '@app/features/user/utils/UserMessageDescriptors';
import {msg} from '@lingui/core/macro';

const PREMIUM_DESCRIPTOR = msg({
	message: 'Premium',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const PRICING_DESCRIPTOR = msg({
	message: 'Pricing',
	comment: 'Settings search entry label. Also used as a search synonym in the settings search bar.',
});
const MONTHLY_DESCRIPTOR = msg({
	message: 'Monthly',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const YEARLY_DESCRIPTOR = msg({
	message: 'Yearly',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const ANNUAL_DESCRIPTOR = msg({
	message: 'Annual',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CURRENCY_DESCRIPTOR = msg({
	message: 'Currency',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CHECKOUT_DESCRIPTOR = msg({
	message: 'Checkout',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const PURCHASE_DESCRIPTOR = msg({
	message: 'Purchase',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CHOOSE_A_SUBSCRIPTION_PLAN_OR_GIFT_OPTION_DESCRIPTOR = msg({
	message: 'Choose a subscription plan or gift option',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
const GIFT_DESCRIPTOR = msg({
	message: 'Gift',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const GIFT_SUBSCRIPTION_DESCRIPTOR = msg({
	message: 'Gift subscription',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MONTHLY_GIFT_DESCRIPTOR = msg({
	message: 'Monthly gift',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const YEARLY_GIFT_DESCRIPTOR = msg({
	message: 'Yearly gift',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const PURCHASE_PREMIUM_GIFTS_FOR_SOMEONE_ELSE_DESCRIPTOR = msg({
	message: 'Purchase {premiumProductName} gifts for someone else',
	comment:
		'Settings search entry description. One-line summary of what the settings search entry controls. Preserve {premiumProductName}; it is inserted by code.',
});
const FREE_VS_PREMIUM_DESCRIPTOR = msg({
	message: 'Free vs {premiumProductName}',
	comment:
		'Settings search entry label. Names the settings search entry in the settings UI. Preserve {premiumProductName}; it is inserted by code.',
});
const PERKS_DESCRIPTOR = msg({
	message: 'Perks',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const BENEFITS_DESCRIPTOR = msg({
	message: 'Benefits',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const FEATURES_DESCRIPTOR = msg({
	message: 'Features',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const COMPARISON_DESCRIPTOR = msg({
	message: 'Comparison',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const FREE_VERSUS_PREMIUM_DESCRIPTOR = msg({
	message: 'Free versus premium',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const VIEW_PREMIUM_PERKS_DESCRIPTOR = msg({
	message: 'View {premiumProductName} perks',
	comment:
		'Settings search entry description. One-line summary of what the settings search entry controls. Preserve {premiumProductName}; it is inserted by code.',
});
export const plutoniumIndex: Array<SearchableSettingDescriptor> = [
	{
		id: 'plutonium-pricing',
		tabType: 'plutonium',
		label: PRICING_DESCRIPTOR,
		keywords: [
			PRICING_DESCRIPTOR,
			MONTHLY_DESCRIPTOR,
			YEARLY_DESCRIPTOR,
			ANNUAL_DESCRIPTOR,
			CURRENCY_DESCRIPTOR,
			CHECKOUT_DESCRIPTOR,
			PURCHASE_DESCRIPTOR,
		],
		description: CHOOSE_A_SUBSCRIPTION_PLAN_OR_GIFT_OPTION_DESCRIPTOR,
	},
	{
		id: 'plutonium-gifts',
		tabType: 'plutonium',
		label: GIFTS_DESCRIPTOR,
		keywords: [
			GIFTS_DESCRIPTOR,
			GIFT_DESCRIPTOR,
			GIFT_SUBSCRIPTION_DESCRIPTOR,
			MONTHLY_GIFT_DESCRIPTOR,
			YEARLY_GIFT_DESCRIPTOR,
			CODES_DESCRIPTOR,
		],
		description: PURCHASE_PREMIUM_GIFTS_FOR_SOMEONE_ELSE_DESCRIPTOR,
	},
	{
		id: 'plutonium-perks',
		tabType: 'plutonium',
		label: FREE_VS_PREMIUM_DESCRIPTOR,
		keywords: [
			PREMIUM_PRODUCT_NAME_KEYWORD,
			PERKS_DESCRIPTOR,
			BENEFITS_DESCRIPTOR,
			FEATURES_DESCRIPTOR,
			PREMIUM_DESCRIPTOR,
			COMPARISON_DESCRIPTOR,
			FREE_VERSUS_PREMIUM_DESCRIPTOR,
		],
		description: VIEW_PREMIUM_PERKS_DESCRIPTOR,
	},
];
