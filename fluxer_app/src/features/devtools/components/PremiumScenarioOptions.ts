// SPDX-License-Identifier: AGPL-3.0-or-later

import {VISIONARY_LIFETIME_BADGE_LABEL} from '@app/features/app/config/I18nDisplayConstants';
import * as DeveloperOptionsCommands from '@app/features/devtools/commands/DeveloperOptionsCommands';
import type {PremiumScenarioOverride} from '@app/features/devtools/state/DeveloperOptions';
import {UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import {MS_PER_DAY} from '@fluxer/date_utils/src/DateConstants';
import type {MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const VISIONARY_DESCRIPTOR = msg({
	message: 'Visionary ({visionaryLifetimeBadgeLabel})',
	comment:
		'Developer / debug surface — keep terse and technical. Option label for the Visionary lifetime premium scenario. {visionaryLifetimeBadgeLabel} is the badge label.',
});
const SELECT_A_SCENARIO_TO_APPLY_DESCRIPTOR = msg({
	message: 'Select a scenario to apply...',
	comment: 'Placeholder option in a developer premium-state scenario selector.',
});
const FREE_USER_NO_PURCHASES_DESCRIPTOR = msg({
	message: 'Free user (no purchases)',
	comment: 'Developer premium-state scenario label.',
});
const FREE_USER_WITH_PURCHASE_HISTORY_DESCRIPTOR = msg({
	message: 'Free user (with purchase history)',
	comment: 'Developer premium-state scenario label.',
});
const ACTIVE_MONTHLY_SUBSCRIBER_DESCRIPTOR = msg({
	message: 'Active monthly subscriber',
	comment: 'Developer premium-state scenario label.',
});
const ACTIVE_MONTHLY_SUBSCRIBER_CANCELLATION_SCHEDULED_DESCRIPTOR = msg({
	message: 'Active monthly subscriber (cancellation scheduled)',
	comment: 'Developer premium-state scenario label.',
});
const ACTIVE_YEARLY_SUBSCRIBER_DESCRIPTOR = msg({
	message: 'Active yearly subscriber',
	comment: 'Developer premium-state scenario label.',
});
const ACTIVE_YEARLY_SUBSCRIBER_CANCELLATION_SCHEDULED_DESCRIPTOR = msg({
	message: 'Active yearly subscriber (cancellation scheduled)',
	comment: 'Developer premium-state scenario label.',
});
const GIFT_ACTIVE_NO_PURCHASE_HISTORY_DESCRIPTOR = msg({
	message: 'Gift: active (no purchase history)',
	comment: 'Developer premium-state scenario label for gifted premium.',
});
const GIFT_ACTIVE_HAS_PURCHASE_HISTORY_DESCRIPTOR = msg({
	message: 'Gift: active (has purchase history)',
	comment: 'Developer premium-state scenario label for gifted premium.',
});
const GIFT_EXPIRING_SOON_DESCRIPTOR = msg({
	message: 'Gift: expiring soon',
	comment: 'Developer premium-state scenario label for gifted premium.',
});
const GIFT_GRACE_PERIOD_STILL_HAVE_ACCESS_DESCRIPTOR = msg({
	message: 'Gift: grace period (still have access)',
	comment: 'Developer premium-state scenario label for gifted premium.',
});
const GIFT_EXPIRED_WITHIN_30_DAYS_DESCRIPTOR = msg({
	message: 'Gift: expired (within 30 days)',
	comment: 'Developer premium-state scenario label for gifted premium.',
});
const GRACE_PERIOD_STILL_HAVE_ACCESS_DESCRIPTOR = msg({
	message: 'Grace period (still have access)',
	comment: 'Developer premium-state scenario label.',
});
const EXPIRED_WITHIN_30_DAYS_DESCRIPTOR = msg({
	message: 'Expired (within 30 days)',
	comment: 'Developer premium-state scenario label.',
});
const EXPIRED_OVER_30_DAYS_AGO_DESCRIPTOR = msg({
	message: 'Expired (over 30 days ago)',
	comment: 'Developer premium-state scenario label.',
});
const RESET_TO_ACTUAL_VALUES_DESCRIPTOR = msg({
	message: 'Reset to actual values',
	comment: 'Developer premium-state scenario option that clears overrides.',
});

interface PremiumScenarioSelectOption {
	value: PremiumScenarioOption;
	label: MessageDescriptor;
}

export type PremiumScenarioOption = 'none' | PremiumScenarioOverride | 'reset';

export const PREMIUM_SCENARIO_OPTIONS: ReadonlyArray<PremiumScenarioSelectOption> = [
	{
		value: 'none',
		label: SELECT_A_SCENARIO_TO_APPLY_DESCRIPTOR,
	},
	{
		value: 'free_no_purchases',
		label: FREE_USER_NO_PURCHASES_DESCRIPTOR,
	},
	{
		value: 'free_with_history',
		label: FREE_USER_WITH_PURCHASE_HISTORY_DESCRIPTOR,
	},
	{
		value: 'active_monthly',
		label: ACTIVE_MONTHLY_SUBSCRIBER_DESCRIPTOR,
	},
	{
		value: 'active_monthly_cancelled',
		label: ACTIVE_MONTHLY_SUBSCRIBER_CANCELLATION_SCHEDULED_DESCRIPTOR,
	},
	{
		value: 'active_yearly',
		label: ACTIVE_YEARLY_SUBSCRIBER_DESCRIPTOR,
	},
	{
		value: 'active_yearly_cancelled',
		label: ACTIVE_YEARLY_SUBSCRIBER_CANCELLATION_SCHEDULED_DESCRIPTOR,
	},
	{
		value: 'gift_active_no_history',
		label: GIFT_ACTIVE_NO_PURCHASE_HISTORY_DESCRIPTOR,
	},
	{
		value: 'gift_active_with_history',
		label: GIFT_ACTIVE_HAS_PURCHASE_HISTORY_DESCRIPTOR,
	},
	{
		value: 'gift_expiring_soon',
		label: GIFT_EXPIRING_SOON_DESCRIPTOR,
	},
	{
		value: 'gift_grace_period_active',
		label: GIFT_GRACE_PERIOD_STILL_HAVE_ACCESS_DESCRIPTOR,
	},
	{
		value: 'gift_expired_recent',
		label: GIFT_EXPIRED_WITHIN_30_DAYS_DESCRIPTOR,
	},
	{
		value: 'grace_period_active',
		label: GRACE_PERIOD_STILL_HAVE_ACCESS_DESCRIPTOR,
	},
	{
		value: 'expired_recent',
		label: EXPIRED_WITHIN_30_DAYS_DESCRIPTOR,
	},
	{
		value: 'expired_old',
		label: EXPIRED_OVER_30_DAYS_AGO_DESCRIPTOR,
	},
	{
		value: 'visionary',
		label: {...VISIONARY_DESCRIPTOR, values: {visionaryLifetimeBadgeLabel: VISIONARY_LIFETIME_BADGE_LABEL}},
	},
	{
		value: 'reset',
		label: RESET_TO_ACTUAL_VALUES_DESCRIPTOR,
	},
];

export function resetPremiumStateOverrides(): void {
	DeveloperOptionsCommands.updateOption('premiumScenarioOverride', null);
	DeveloperOptionsCommands.updateOption('premiumTypeOverride', null);
	DeveloperOptionsCommands.updateOption('hasEverPurchasedOverride', null);
	DeveloperOptionsCommands.updateOption('premiumSinceOverride', null);
	DeveloperOptionsCommands.updateOption('premiumUntilOverride', null);
	DeveloperOptionsCommands.updateOption('premiumBillingCycleOverride', null);
	DeveloperOptionsCommands.updateOption('premiumWillCancelOverride', null);
	DeveloperOptionsCommands.updateOption('hasUnreadGiftInventoryOverride', null);
	DeveloperOptionsCommands.updateOption('unreadGiftInventoryCountOverride', null);
}

const isPremiumScenarioOverride = (scenario: PremiumScenarioOption): scenario is PremiumScenarioOverride =>
	scenario !== 'none' && scenario !== 'reset';
export const applyPremiumScenarioOption = (scenario: PremiumScenarioOption) => {
	if (scenario === 'none') return;
	const now = Date.now();
	switch (scenario) {
		case 'free_no_purchases':
			DeveloperOptionsCommands.updateOption('premiumTypeOverride', UserPremiumTypes.NONE);
			DeveloperOptionsCommands.updateOption('hasEverPurchasedOverride', false);
			DeveloperOptionsCommands.updateOption('premiumSinceOverride', null);
			DeveloperOptionsCommands.updateOption('premiumUntilOverride', null);
			DeveloperOptionsCommands.updateOption('premiumBillingCycleOverride', null);
			DeveloperOptionsCommands.updateOption('premiumWillCancelOverride', false);
			break;
		case 'free_with_history':
			DeveloperOptionsCommands.updateOption('premiumTypeOverride', UserPremiumTypes.NONE);
			DeveloperOptionsCommands.updateOption('hasEverPurchasedOverride', true);
			DeveloperOptionsCommands.updateOption('premiumSinceOverride', null);
			DeveloperOptionsCommands.updateOption('premiumUntilOverride', null);
			DeveloperOptionsCommands.updateOption('premiumBillingCycleOverride', null);
			DeveloperOptionsCommands.updateOption('premiumWillCancelOverride', false);
			break;
		case 'active_monthly':
			DeveloperOptionsCommands.updateOption('premiumTypeOverride', UserPremiumTypes.SUBSCRIPTION);
			DeveloperOptionsCommands.updateOption('hasEverPurchasedOverride', true);
			DeveloperOptionsCommands.updateOption('premiumSinceOverride', new Date(now - 15 * MS_PER_DAY));
			DeveloperOptionsCommands.updateOption('premiumUntilOverride', new Date(now + 15 * MS_PER_DAY));
			DeveloperOptionsCommands.updateOption('premiumBillingCycleOverride', 'monthly');
			DeveloperOptionsCommands.updateOption('premiumWillCancelOverride', false);
			break;
		case 'active_monthly_cancelled':
			DeveloperOptionsCommands.updateOption('premiumTypeOverride', UserPremiumTypes.SUBSCRIPTION);
			DeveloperOptionsCommands.updateOption('hasEverPurchasedOverride', true);
			DeveloperOptionsCommands.updateOption('premiumSinceOverride', new Date(now - 20 * MS_PER_DAY));
			DeveloperOptionsCommands.updateOption('premiumUntilOverride', new Date(now + 10 * MS_PER_DAY));
			DeveloperOptionsCommands.updateOption('premiumBillingCycleOverride', 'monthly');
			DeveloperOptionsCommands.updateOption('premiumWillCancelOverride', true);
			break;
		case 'active_yearly':
			DeveloperOptionsCommands.updateOption('premiumTypeOverride', UserPremiumTypes.SUBSCRIPTION);
			DeveloperOptionsCommands.updateOption('hasEverPurchasedOverride', true);
			DeveloperOptionsCommands.updateOption('premiumSinceOverride', new Date(now - 180 * MS_PER_DAY));
			DeveloperOptionsCommands.updateOption('premiumUntilOverride', new Date(now + 185 * MS_PER_DAY));
			DeveloperOptionsCommands.updateOption('premiumBillingCycleOverride', 'yearly');
			DeveloperOptionsCommands.updateOption('premiumWillCancelOverride', false);
			break;
		case 'active_yearly_cancelled':
			DeveloperOptionsCommands.updateOption('premiumTypeOverride', UserPremiumTypes.SUBSCRIPTION);
			DeveloperOptionsCommands.updateOption('hasEverPurchasedOverride', true);
			DeveloperOptionsCommands.updateOption('premiumSinceOverride', new Date(now - 250 * MS_PER_DAY));
			DeveloperOptionsCommands.updateOption('premiumUntilOverride', new Date(now + 60 * MS_PER_DAY));
			DeveloperOptionsCommands.updateOption('premiumBillingCycleOverride', 'yearly');
			DeveloperOptionsCommands.updateOption('premiumWillCancelOverride', true);
			break;
		case 'gift_active_no_history':
			DeveloperOptionsCommands.updateOption('premiumTypeOverride', UserPremiumTypes.SUBSCRIPTION);
			DeveloperOptionsCommands.updateOption('hasEverPurchasedOverride', false);
			DeveloperOptionsCommands.updateOption('premiumSinceOverride', new Date(now - 10 * MS_PER_DAY));
			DeveloperOptionsCommands.updateOption('premiumUntilOverride', new Date(now + 20 * MS_PER_DAY));
			DeveloperOptionsCommands.updateOption('premiumBillingCycleOverride', null);
			DeveloperOptionsCommands.updateOption('premiumWillCancelOverride', false);
			break;
		case 'gift_active_with_history':
			DeveloperOptionsCommands.updateOption('premiumTypeOverride', UserPremiumTypes.SUBSCRIPTION);
			DeveloperOptionsCommands.updateOption('hasEverPurchasedOverride', true);
			DeveloperOptionsCommands.updateOption('premiumSinceOverride', new Date(now - 5 * MS_PER_DAY));
			DeveloperOptionsCommands.updateOption('premiumUntilOverride', new Date(now + 60 * MS_PER_DAY));
			DeveloperOptionsCommands.updateOption('premiumBillingCycleOverride', null);
			DeveloperOptionsCommands.updateOption('premiumWillCancelOverride', false);
			break;
		case 'gift_expiring_soon':
			DeveloperOptionsCommands.updateOption('premiumTypeOverride', UserPremiumTypes.SUBSCRIPTION);
			DeveloperOptionsCommands.updateOption('hasEverPurchasedOverride', false);
			DeveloperOptionsCommands.updateOption('premiumSinceOverride', new Date(now - 25 * MS_PER_DAY));
			DeveloperOptionsCommands.updateOption('premiumUntilOverride', new Date(now + MS_PER_DAY));
			DeveloperOptionsCommands.updateOption('premiumBillingCycleOverride', null);
			DeveloperOptionsCommands.updateOption('premiumWillCancelOverride', false);
			break;
		case 'gift_grace_period_active':
			DeveloperOptionsCommands.updateOption('premiumTypeOverride', UserPremiumTypes.SUBSCRIPTION);
			DeveloperOptionsCommands.updateOption('hasEverPurchasedOverride', false);
			DeveloperOptionsCommands.updateOption('premiumSinceOverride', new Date(now - 31 * MS_PER_DAY));
			DeveloperOptionsCommands.updateOption('premiumUntilOverride', new Date(now - MS_PER_DAY));
			DeveloperOptionsCommands.updateOption('premiumBillingCycleOverride', null);
			DeveloperOptionsCommands.updateOption('premiumWillCancelOverride', false);
			break;
		case 'gift_expired_recent':
			DeveloperOptionsCommands.updateOption('premiumTypeOverride', UserPremiumTypes.NONE);
			DeveloperOptionsCommands.updateOption('hasEverPurchasedOverride', false);
			DeveloperOptionsCommands.updateOption('premiumSinceOverride', new Date(now - 35 * MS_PER_DAY));
			DeveloperOptionsCommands.updateOption('premiumUntilOverride', new Date(now - 5 * MS_PER_DAY));
			DeveloperOptionsCommands.updateOption('premiumBillingCycleOverride', null);
			DeveloperOptionsCommands.updateOption('premiumWillCancelOverride', false);
			break;
		case 'grace_period_active':
			DeveloperOptionsCommands.updateOption('premiumTypeOverride', UserPremiumTypes.SUBSCRIPTION);
			DeveloperOptionsCommands.updateOption('hasEverPurchasedOverride', true);
			DeveloperOptionsCommands.updateOption('premiumSinceOverride', new Date(now - 31 * MS_PER_DAY));
			DeveloperOptionsCommands.updateOption('premiumUntilOverride', new Date(now - MS_PER_DAY));
			DeveloperOptionsCommands.updateOption('premiumBillingCycleOverride', 'yearly');
			DeveloperOptionsCommands.updateOption('premiumWillCancelOverride', false);
			break;
		case 'expired_recent':
			DeveloperOptionsCommands.updateOption('premiumTypeOverride', UserPremiumTypes.NONE);
			DeveloperOptionsCommands.updateOption('hasEverPurchasedOverride', true);
			DeveloperOptionsCommands.updateOption('premiumSinceOverride', new Date(now - 35 * MS_PER_DAY));
			DeveloperOptionsCommands.updateOption('premiumUntilOverride', new Date(now - 5 * MS_PER_DAY));
			DeveloperOptionsCommands.updateOption('premiumBillingCycleOverride', 'yearly');
			DeveloperOptionsCommands.updateOption('premiumWillCancelOverride', false);
			break;
		case 'expired_old':
			DeveloperOptionsCommands.updateOption('premiumTypeOverride', UserPremiumTypes.NONE);
			DeveloperOptionsCommands.updateOption('hasEverPurchasedOverride', true);
			DeveloperOptionsCommands.updateOption('premiumSinceOverride', new Date(now - 65 * MS_PER_DAY));
			DeveloperOptionsCommands.updateOption('premiumUntilOverride', new Date(now - 35 * MS_PER_DAY));
			DeveloperOptionsCommands.updateOption('premiumBillingCycleOverride', 'yearly');
			DeveloperOptionsCommands.updateOption('premiumWillCancelOverride', false);
			break;
		case 'visionary':
			DeveloperOptionsCommands.updateOption('premiumTypeOverride', UserPremiumTypes.LIFETIME);
			DeveloperOptionsCommands.updateOption('hasEverPurchasedOverride', true);
			DeveloperOptionsCommands.updateOption('premiumSinceOverride', new Date(now - 365 * MS_PER_DAY));
			DeveloperOptionsCommands.updateOption('premiumUntilOverride', null);
			DeveloperOptionsCommands.updateOption('premiumBillingCycleOverride', null);
			DeveloperOptionsCommands.updateOption('premiumWillCancelOverride', false);
			break;
		case 'reset':
			resetPremiumStateOverrides();
			break;
	}
	if (isPremiumScenarioOverride(scenario)) {
		DeveloperOptionsCommands.updateOption('premiumScenarioOverride', scenario);
	}
};
