/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type {MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import * as DeveloperOptionsActionCreators from '~/actions/DeveloperOptionsActionCreators';
import {UserPremiumTypes} from '~/Constants';
import type {SelectOption} from '~/components/form/Select';

interface PremiumScenarioSelectOption extends Omit<SelectOption<PremiumScenarioOption>, 'label'> {
	label: MessageDescriptor;
}

export type PremiumScenarioOption =
	| 'none'
	| 'free_no_purchases'
	| 'free_with_history'
	| 'active_monthly'
	| 'active_monthly_cancelled'
	| 'active_yearly'
	| 'active_yearly_cancelled'
	| 'gift_active_no_history'
	| 'gift_active_with_history'
	| 'gift_expiring_soon'
	| 'gift_grace_period_active'
	| 'gift_expired_recent'
	| 'grace_period_active'
	| 'expired_recent'
	| 'expired_old'
	| 'visionary'
	| 'reset';

export const PREMIUM_SCENARIO_OPTIONS: ReadonlyArray<PremiumScenarioSelectOption> = [
	{value: 'none', label: msg`Select a scenario to apply...`},
	{value: 'free_no_purchases', label: msg`Free User (No Purchases)`},
	{value: 'free_with_history', label: msg`Free User (With Purchase History)`},
	{value: 'active_monthly', label: msg`Active Monthly Subscriber`},
	{value: 'active_monthly_cancelled', label: msg`Active Monthly Subscriber (Cancellation Scheduled)`},
	{value: 'active_yearly', label: msg`Active Yearly Subscriber`},
	{value: 'active_yearly_cancelled', label: msg`Active Yearly Subscriber (Cancellation Scheduled)`},
	{value: 'gift_active_no_history', label: msg`Gift – Active (No Purchase History)`},
	{value: 'gift_active_with_history', label: msg`Gift – Active (Has Purchase History)`},
	{value: 'gift_expiring_soon', label: msg`Gift – Expiring Soon`},
	{value: 'gift_grace_period_active', label: msg`Gift – Grace Period (Still Have Access)`},
	{value: 'gift_expired_recent', label: msg`Gift – Expired (Within 30 Days)`},
	{value: 'grace_period_active', label: msg`Grace Period (Still Have Access)`},
	{value: 'expired_recent', label: msg`Expired (Within 30 Days)`},
	{value: 'expired_old', label: msg`Expired (Over 30 Days Ago)`},
	{value: 'visionary', label: msg`Visionary (Lifetime #42)`},
	{value: 'reset', label: msg`Reset to Actual Values`},
];

export const applyPremiumScenarioOption = (scenario: PremiumScenarioOption) => {
	if (scenario === 'none') return;

	const now = Date.now();

	switch (scenario) {
		case 'free_no_purchases':
			DeveloperOptionsActionCreators.updateOption('premiumTypeOverride', UserPremiumTypes.NONE);
			DeveloperOptionsActionCreators.updateOption('hasEverPurchasedOverride', false);
			DeveloperOptionsActionCreators.updateOption('premiumSinceOverride', null);
			DeveloperOptionsActionCreators.updateOption('premiumUntilOverride', null);
			DeveloperOptionsActionCreators.updateOption('premiumBillingCycleOverride', null);
			DeveloperOptionsActionCreators.updateOption('premiumWillCancelOverride', false);
			break;
		case 'free_with_history':
			DeveloperOptionsActionCreators.updateOption('premiumTypeOverride', UserPremiumTypes.NONE);
			DeveloperOptionsActionCreators.updateOption('hasEverPurchasedOverride', true);
			DeveloperOptionsActionCreators.updateOption('premiumSinceOverride', null);
			DeveloperOptionsActionCreators.updateOption('premiumUntilOverride', null);
			DeveloperOptionsActionCreators.updateOption('premiumBillingCycleOverride', null);
			DeveloperOptionsActionCreators.updateOption('premiumWillCancelOverride', false);
			break;
		case 'active_monthly':
			DeveloperOptionsActionCreators.updateOption('premiumTypeOverride', UserPremiumTypes.SUBSCRIPTION);
			DeveloperOptionsActionCreators.updateOption('hasEverPurchasedOverride', true);
			DeveloperOptionsActionCreators.updateOption('premiumSinceOverride', new Date(now - 15 * 86400000));
			DeveloperOptionsActionCreators.updateOption('premiumUntilOverride', new Date(now + 15 * 86400000));
			DeveloperOptionsActionCreators.updateOption('premiumBillingCycleOverride', 'monthly');
			DeveloperOptionsActionCreators.updateOption('premiumWillCancelOverride', false);
			break;
		case 'active_monthly_cancelled':
			DeveloperOptionsActionCreators.updateOption('premiumTypeOverride', UserPremiumTypes.SUBSCRIPTION);
			DeveloperOptionsActionCreators.updateOption('hasEverPurchasedOverride', true);
			DeveloperOptionsActionCreators.updateOption('premiumSinceOverride', new Date(now - 20 * 86400000));
			DeveloperOptionsActionCreators.updateOption('premiumUntilOverride', new Date(now + 10 * 86400000));
			DeveloperOptionsActionCreators.updateOption('premiumBillingCycleOverride', 'monthly');
			DeveloperOptionsActionCreators.updateOption('premiumWillCancelOverride', true);
			break;
		case 'active_yearly':
			DeveloperOptionsActionCreators.updateOption('premiumTypeOverride', UserPremiumTypes.SUBSCRIPTION);
			DeveloperOptionsActionCreators.updateOption('hasEverPurchasedOverride', true);
			DeveloperOptionsActionCreators.updateOption('premiumSinceOverride', new Date(now - 180 * 86400000));
			DeveloperOptionsActionCreators.updateOption('premiumUntilOverride', new Date(now + 185 * 86400000));
			DeveloperOptionsActionCreators.updateOption('premiumBillingCycleOverride', 'yearly');
			DeveloperOptionsActionCreators.updateOption('premiumWillCancelOverride', false);
			break;
		case 'active_yearly_cancelled':
			DeveloperOptionsActionCreators.updateOption('premiumTypeOverride', UserPremiumTypes.SUBSCRIPTION);
			DeveloperOptionsActionCreators.updateOption('hasEverPurchasedOverride', true);
			DeveloperOptionsActionCreators.updateOption('premiumSinceOverride', new Date(now - 250 * 86400000));
			DeveloperOptionsActionCreators.updateOption('premiumUntilOverride', new Date(now + 60 * 86400000));
			DeveloperOptionsActionCreators.updateOption('premiumBillingCycleOverride', 'yearly');
			DeveloperOptionsActionCreators.updateOption('premiumWillCancelOverride', true);
			break;
		case 'gift_active_no_history':
			DeveloperOptionsActionCreators.updateOption('premiumTypeOverride', UserPremiumTypes.SUBSCRIPTION);
			DeveloperOptionsActionCreators.updateOption('hasEverPurchasedOverride', false);
			DeveloperOptionsActionCreators.updateOption('premiumSinceOverride', new Date(now - 10 * 86400000));
			DeveloperOptionsActionCreators.updateOption('premiumUntilOverride', new Date(now + 20 * 86400000));
			DeveloperOptionsActionCreators.updateOption('premiumBillingCycleOverride', null);
			DeveloperOptionsActionCreators.updateOption('premiumWillCancelOverride', false);
			break;
		case 'gift_active_with_history':
			DeveloperOptionsActionCreators.updateOption('premiumTypeOverride', UserPremiumTypes.SUBSCRIPTION);
			DeveloperOptionsActionCreators.updateOption('hasEverPurchasedOverride', true);
			DeveloperOptionsActionCreators.updateOption('premiumSinceOverride', new Date(now - 5 * 86400000));
			DeveloperOptionsActionCreators.updateOption('premiumUntilOverride', new Date(now + 60 * 86400000));
			DeveloperOptionsActionCreators.updateOption('premiumBillingCycleOverride', null);
			DeveloperOptionsActionCreators.updateOption('premiumWillCancelOverride', false);
			break;
		case 'gift_expiring_soon':
			DeveloperOptionsActionCreators.updateOption('premiumTypeOverride', UserPremiumTypes.SUBSCRIPTION);
			DeveloperOptionsActionCreators.updateOption('hasEverPurchasedOverride', false);
			DeveloperOptionsActionCreators.updateOption('premiumSinceOverride', new Date(now - 25 * 86400000));
			DeveloperOptionsActionCreators.updateOption('premiumUntilOverride', new Date(now + 86400000));
			DeveloperOptionsActionCreators.updateOption('premiumBillingCycleOverride', null);
			DeveloperOptionsActionCreators.updateOption('premiumWillCancelOverride', false);
			break;
		case 'gift_grace_period_active':
			DeveloperOptionsActionCreators.updateOption('premiumTypeOverride', UserPremiumTypes.SUBSCRIPTION);
			DeveloperOptionsActionCreators.updateOption('hasEverPurchasedOverride', false);
			DeveloperOptionsActionCreators.updateOption('premiumSinceOverride', new Date(now - 31 * 86400000));
			DeveloperOptionsActionCreators.updateOption('premiumUntilOverride', new Date(now - 86400000));
			DeveloperOptionsActionCreators.updateOption('premiumBillingCycleOverride', null);
			DeveloperOptionsActionCreators.updateOption('premiumWillCancelOverride', false);
			break;
		case 'gift_expired_recent':
			DeveloperOptionsActionCreators.updateOption('premiumTypeOverride', UserPremiumTypes.NONE);
			DeveloperOptionsActionCreators.updateOption('hasEverPurchasedOverride', false);
			DeveloperOptionsActionCreators.updateOption('premiumSinceOverride', new Date(now - 35 * 86400000));
			DeveloperOptionsActionCreators.updateOption('premiumUntilOverride', new Date(now - 5 * 86400000));
			DeveloperOptionsActionCreators.updateOption('premiumBillingCycleOverride', null);
			DeveloperOptionsActionCreators.updateOption('premiumWillCancelOverride', false);
			break;
		case 'grace_period_active':
			DeveloperOptionsActionCreators.updateOption('premiumTypeOverride', UserPremiumTypes.SUBSCRIPTION);
			DeveloperOptionsActionCreators.updateOption('hasEverPurchasedOverride', true);
			DeveloperOptionsActionCreators.updateOption('premiumSinceOverride', new Date(now - 31 * 86400000));
			DeveloperOptionsActionCreators.updateOption('premiumUntilOverride', new Date(now - 86400000));
			DeveloperOptionsActionCreators.updateOption('premiumBillingCycleOverride', 'yearly');
			DeveloperOptionsActionCreators.updateOption('premiumWillCancelOverride', false);
			break;
		case 'expired_recent':
			DeveloperOptionsActionCreators.updateOption('premiumTypeOverride', UserPremiumTypes.NONE);
			DeveloperOptionsActionCreators.updateOption('hasEverPurchasedOverride', true);
			DeveloperOptionsActionCreators.updateOption('premiumSinceOverride', new Date(now - 35 * 86400000));
			DeveloperOptionsActionCreators.updateOption('premiumUntilOverride', new Date(now - 5 * 86400000));
			DeveloperOptionsActionCreators.updateOption('premiumBillingCycleOverride', 'yearly');
			DeveloperOptionsActionCreators.updateOption('premiumWillCancelOverride', false);
			break;
		case 'expired_old':
			DeveloperOptionsActionCreators.updateOption('premiumTypeOverride', UserPremiumTypes.NONE);
			DeveloperOptionsActionCreators.updateOption('hasEverPurchasedOverride', true);
			DeveloperOptionsActionCreators.updateOption('premiumSinceOverride', new Date(now - 65 * 86400000));
			DeveloperOptionsActionCreators.updateOption('premiumUntilOverride', new Date(now - 35 * 86400000));
			DeveloperOptionsActionCreators.updateOption('premiumBillingCycleOverride', 'yearly');
			DeveloperOptionsActionCreators.updateOption('premiumWillCancelOverride', false);
			break;
		case 'visionary':
			DeveloperOptionsActionCreators.updateOption('premiumTypeOverride', UserPremiumTypes.LIFETIME);
			DeveloperOptionsActionCreators.updateOption('hasEverPurchasedOverride', true);
			DeveloperOptionsActionCreators.updateOption('premiumSinceOverride', new Date(now - 365 * 86400000));
			DeveloperOptionsActionCreators.updateOption('premiumUntilOverride', null);
			DeveloperOptionsActionCreators.updateOption('premiumBillingCycleOverride', null);
			DeveloperOptionsActionCreators.updateOption('premiumWillCancelOverride', false);
			break;
		case 'reset':
			DeveloperOptionsActionCreators.updateOption('premiumTypeOverride', null);
			DeveloperOptionsActionCreators.updateOption('hasEverPurchasedOverride', null);
			DeveloperOptionsActionCreators.updateOption('premiumSinceOverride', null);
			DeveloperOptionsActionCreators.updateOption('premiumUntilOverride', null);
			DeveloperOptionsActionCreators.updateOption('premiumBillingCycleOverride', null);
			DeveloperOptionsActionCreators.updateOption('premiumWillCancelOverride', null);
			break;
	}
};
