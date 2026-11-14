// SPDX-License-Identifier: AGPL-3.0-or-later

import OperatorPassHoldings from '@app/features/premium/state/OperatorPassHoldings';
import {
	showsOperatorPassLabsSettings,
	showsOperatorPassWallSettings,
} from '@app/features/premium/utils/BillingTabVisibility';
import {
	OPERATOR_PASS_LABS_SECTION_DESCRIPTOR,
	OPERATOR_PASS_PASSES_SECTION_DESCRIPTOR,
	OPERATOR_PASS_WALL_SECTION_DESCRIPTOR,
	PAYMENT_METHODS_SECTION_DESCRIPTOR,
	RECEIPTS_SECTION_DESCRIPTOR,
	REFUNDS_SECTION_DESCRIPTOR,
	SUBSCRIPTION_SECTION_DESCRIPTOR,
} from '@app/features/premium/utils/PremiumMessageDescriptors';
import type {SectionDefinition} from '@app/features/user/components/settings_utils/section_registry/SectionRegistryTypes';
import {
	APPLICATIONS_DESCRIPTOR as APPLICATIONS_LIST_DESCRIPTOR,
	AUTHORIZED_APPS_DESCRIPTOR,
	BLOCKED_USERS_DESCRIPTOR,
	DATE_FORMAT_DESCRIPTOR,
	DESKTOP_WINDOW_DESCRIPTOR,
	DEVELOPER_MODE_DESCRIPTOR,
	CONNECTIONS_DESCRIPTOR as LINKED_CONNECTIONS_DESCRIPTOR,
	PURCHASED_GIFTS_DESCRIPTOR,
	REDEEM_A_GIFT_DESCRIPTOR,
	REGIONAL_FORMAT_DESCRIPTOR,
	SPELL_CHECK_DESCRIPTOR as SPELLCHECK_DESCRIPTOR,
	TIME_FORMAT_DESCRIPTOR,
} from '@app/features/user/components/settings_utils/section_registry/SharedDescriptors';
import Users from '@app/features/user/state/Users';
import {
	INTERFACE_LANGUAGE_DESCRIPTOR,
	PROFILE_BADGES_DESCRIPTOR,
	PROFILE_CUSTOMIZATION_DESCRIPTOR,
} from '@app/features/user/utils/UserMessageDescriptors';
import {msg} from '@lingui/core/macro';

const AUTHORIZED_APPLICATIONS_DESCRIPTOR = msg({
	message: 'Authorized applications',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});

const SIGNED_IN_DEVICES_DESCRIPTOR = msg({
	message: 'Signed-in devices',
	comment: 'Settings section label for account sessions and devices.',
});

const LANGUAGE_SETTINGS_DESCRIPTOR = msg({
	message: 'Language settings',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});

const UNFURL_DEBUGGER_DESCRIPTOR = msg({
	message: 'Unfurl debugger',
	comment: 'Settings section label for the embed debugger URL input.',
});

export const generalSettingsSections = [
	{
		id: 'profile-customization',
		tabType: 'my_profile',
		label: PROFILE_CUSTOMIZATION_DESCRIPTOR,
		keywords: [],
		isAdvanced: false,
	},
	{
		id: 'profile-badges',
		tabType: 'my_profile',
		label: PROFILE_BADGES_DESCRIPTOR,
		keywords: [],
		isAdvanced: false,
	},
	{
		id: 'subscription',
		tabType: 'subscriptions',
		label: SUBSCRIPTION_SECTION_DESCRIPTOR,
		keywords: [],
		isAdvanced: false,
	},
	{
		id: 'operator-pass-passes',
		tabType: 'operator_pass',
		label: OPERATOR_PASS_PASSES_SECTION_DESCRIPTOR,
		keywords: [],
		isAdvanced: false,
	},
	{
		id: 'operator-pass-wall',
		tabType: 'operator_pass',
		label: OPERATOR_PASS_WALL_SECTION_DESCRIPTOR,
		keywords: [],
		isAdvanced: false,
		isVisible: () => showsOperatorPassWallSettings(Users.currentUser, OperatorPassHoldings.passes),
	},
	{
		id: 'operator-pass-labs',
		tabType: 'operator_pass',
		label: OPERATOR_PASS_LABS_SECTION_DESCRIPTOR,
		keywords: [],
		isAdvanced: false,
		isVisible: () => showsOperatorPassLabsSettings(Users.currentUser, OperatorPassHoldings.passes),
	},
	{id: 'redeem-gift', tabType: 'gift_inventory', label: REDEEM_A_GIFT_DESCRIPTOR, keywords: [], isAdvanced: false},
	{
		id: 'purchased-gifts',
		tabType: 'gift_inventory',
		label: PURCHASED_GIFTS_DESCRIPTOR,
		keywords: [],
		isAdvanced: false,
	},
	{
		id: 'payment-methods',
		tabType: 'payments',
		label: PAYMENT_METHODS_SECTION_DESCRIPTOR,
		keywords: [],
		isAdvanced: false,
	},
	{id: 'refunds', tabType: 'payments', label: REFUNDS_SECTION_DESCRIPTOR, keywords: [], isAdvanced: false},
	{id: 'receipts', tabType: 'payments', label: RECEIPTS_SECTION_DESCRIPTOR, keywords: [], isAdvanced: false},
	{id: 'blocked-users', tabType: 'blocked_users', label: BLOCKED_USERS_DESCRIPTOR, keywords: [], isAdvanced: false},
	{
		id: 'authorized-applications',
		tabType: 'authorized_apps',
		label: AUTHORIZED_APPS_DESCRIPTOR,
		keywords: [AUTHORIZED_APPLICATIONS_DESCRIPTOR],
		isAdvanced: false,
	},
	{
		id: 'connections',
		tabType: 'linked_accounts',
		label: LINKED_CONNECTIONS_DESCRIPTOR,
		keywords: [],
		isAdvanced: false,
	},
	{id: 'signed-in-devices', tabType: 'devices', label: SIGNED_IN_DEVICES_DESCRIPTOR, keywords: [], isAdvanced: false},
	{
		id: 'language-settings',
		tabType: 'language',
		label: INTERFACE_LANGUAGE_DESCRIPTOR,
		keywords: [LANGUAGE_SETTINGS_DESCRIPTOR],
		isAdvanced: false,
	},
	{id: 'regional-format', tabType: 'language', label: REGIONAL_FORMAT_DESCRIPTOR, keywords: [], isAdvanced: false},
	{id: 'time-format', tabType: 'language', label: TIME_FORMAT_DESCRIPTOR, keywords: [], isAdvanced: false},
	{id: 'date-format', tabType: 'language', label: DATE_FORMAT_DESCRIPTOR, keywords: [], isAdvanced: false},
	{id: 'spellcheck', tabType: 'language', label: SPELLCHECK_DESCRIPTOR, keywords: [], isAdvanced: false},
	{
		id: 'desktop-window',
		tabType: 'desktop_settings',
		label: DESKTOP_WINDOW_DESCRIPTOR,
		keywords: [],
		isAdvanced: false,
	},
	{
		id: 'developer-mode',
		tabType: 'client_developer_settings',
		label: DEVELOPER_MODE_DESCRIPTOR,
		keywords: [],
		isAdvanced: false,
	},
	{
		id: 'unfurl-debugger',
		tabType: 'embed_debugger',
		label: UNFURL_DEBUGGER_DESCRIPTOR,
		keywords: [],
		isAdvanced: false,
	},
	{
		id: 'applications-list',
		tabType: 'applications',
		label: APPLICATIONS_LIST_DESCRIPTOR,
		keywords: [],
		isAdvanced: false,
	},
] as const satisfies ReadonlyArray<SectionDefinition>;
