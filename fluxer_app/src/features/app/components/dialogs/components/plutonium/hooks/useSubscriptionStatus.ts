// SPDX-License-Identifier: AGPL-3.0-or-later

import DeveloperOptions from '@app/features/devtools/state/DeveloperOptions';
import type {User} from '@app/features/user/models/User';
import {UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import {MS_PER_DAY} from '@fluxer/date_utils/src/DateConstants';
import type {PremiumStateResponse} from '@fluxer/schema/src/domains/premium/PremiumSchemas';
import {useMemo} from 'react';

export interface GracePeriodInfo {
	isInGracePeriod: boolean;
	isExpired: boolean;
	graceEndDate: Date | null;
	showExpiredState: boolean;
}

export interface SubscriptionStatusInfo {
	isPremium: boolean;
	perksDisabled: boolean;
	isVisionary: boolean;
	hasEverPurchased: boolean;
	premiumWillCancel: boolean;
	billingCycle: string | null;
	actualPremiumUntil: Date | null;
	isGiftSubscription: boolean;
	gracePeriodInfo: GracePeriodInfo;
	shouldShowPremiumCard: boolean;
	shouldUseCancelQuickAction: boolean;
	shouldUseReactivateQuickAction: boolean;
	shouldUseChangePlanQuickAction: boolean;
}

const parseOptionalDate = (value: string | null | undefined): Date | null => {
	if (!value) return null;
	const date = new Date(value);
	return Number.isNaN(date.getTime()) ? null : date;
};
const hasDeveloperPremiumStateOverride = (): boolean =>
	DeveloperOptions.premiumScenarioOverride !== null ||
	DeveloperOptions.premiumTypeOverride !== null ||
	DeveloperOptions.premiumUntilOverride !== null ||
	DeveloperOptions.premiumBillingCycleOverride !== null ||
	DeveloperOptions.premiumWillCancelOverride !== null ||
	DeveloperOptions.hasEverPurchasedOverride !== null;
export const useSubscriptionStatus = (
	currentUser: User | null,
	premiumState: PremiumStateResponse | null = null,
): SubscriptionStatusInfo => {
	const actual = premiumState?.actual ?? null;
	const effective = premiumState?.effective ?? null;
	const useScenarioOverride = DeveloperOptions.premiumScenarioOverride !== null;
	const useDeveloperOverride = hasDeveloperPremiumStateOverride();
	const premiumType =
		useScenarioOverride || DeveloperOptions.premiumTypeOverride !== null
			? DeveloperOptions.premiumTypeOverride
			: (effective?.premium_type ?? currentUser?.premiumType ?? null);
	const premiumUntil = useScenarioOverride
		? DeveloperOptions.premiumUntilOverride
		: (DeveloperOptions.premiumUntilOverride ??
			parseOptionalDate(actual?.premium_until) ??
			parseOptionalDate(effective?.premium_until) ??
			currentUser?.premiumUntil ??
			null);
	const premiumGraceEndsAt = useScenarioOverride
		? null
		: (parseOptionalDate(actual?.premium_grace_ends_at) ?? currentUser?.premiumGraceEndsAt ?? null);
	const perksDisabled = effective?.premium_perks_disabled ?? currentUser?.premiumPerksDisabled ?? false;
	const hasPaidPremium = premiumType != null && premiumType > UserPremiumTypes.NONE;
	const isVisionary = premiumType === UserPremiumTypes.LIFETIME;
	const hasEverPurchased =
		useScenarioOverride || DeveloperOptions.hasEverPurchasedOverride !== null
			? (DeveloperOptions.hasEverPurchasedOverride ?? false)
			: (actual?.has_ever_purchased ?? currentUser?.hasEverPurchased ?? false);
	const premiumWillCancel =
		useScenarioOverride || DeveloperOptions.premiumWillCancelOverride !== null
			? (DeveloperOptions.premiumWillCancelOverride ?? false)
			: (actual?.premium_will_cancel ?? currentUser?.premiumWillCancel ?? false);
	const billingCycle =
		useScenarioOverride || DeveloperOptions.premiumBillingCycleOverride !== null
			? DeveloperOptions.premiumBillingCycleOverride
			: (actual?.premium_billing_cycle ?? currentUser?.premiumBillingCycle ?? null);
	const isGiftSubscription = Boolean(!billingCycle && hasPaidPremium && !isVisionary && premiumUntil);
	const gracePeriodInfo = useMemo((): GracePeriodInfo => {
		if (isVisionary) {
			return {isInGracePeriod: false, isExpired: false, graceEndDate: null, showExpiredState: false};
		}
		const explicitGraceEnd = premiumGraceEndsAt ?? null;
		const expiryDate = premiumUntil ? new Date(premiumUntil) : null;
		const graceEndDate = explicitGraceEnd ?? (expiryDate ? new Date(expiryDate.getTime() + 3 * MS_PER_DAY) : null);
		if (!graceEndDate) {
			return {isInGracePeriod: false, isExpired: false, graceEndDate: null, showExpiredState: false};
		}
		const now = new Date();
		const anchorDate = expiryDate ?? graceEndDate;
		const expiredStateEndDate = new Date(anchorDate.getTime() + 30 * MS_PER_DAY);
		const isInGracePeriod = (!expiryDate || now > expiryDate) && now <= graceEndDate;
		const isExpired = now > graceEndDate;
		const showExpiredState = isExpired && now <= expiredStateEndDate;
		return {isInGracePeriod, isExpired, graceEndDate, showExpiredState};
	}, [premiumUntil, premiumGraceEndsAt, isVisionary]);
	const {isInGracePeriod, isExpired: isFullyExpired, showExpiredState} = gracePeriodInfo;
	const isPremium = useDeveloperOverride
		? hasPaidPremium && !isFullyExpired && !perksDisabled
		: (effective?.is_premium ?? currentUser?.isPremium() ?? false);
	const shouldShowPremiumCard = hasPaidPremium || isInGracePeriod || showExpiredState;
	const shouldUseCancelQuickAction =
		hasPaidPremium && !isVisionary && !isInGracePeriod && !isFullyExpired && !premiumWillCancel && !isGiftSubscription;
	const shouldUseReactivateQuickAction =
		hasPaidPremium && premiumWillCancel && !isVisionary && !isInGracePeriod && !isFullyExpired && !isGiftSubscription;
	const shouldUseChangePlanQuickAction =
		hasPaidPremium &&
		!isVisionary &&
		!isInGracePeriod &&
		!isFullyExpired &&
		!premiumWillCancel &&
		!isGiftSubscription &&
		(billingCycle === 'monthly' || billingCycle === 'yearly');
	return {
		isPremium,
		perksDisabled,
		isVisionary,
		hasEverPurchased,
		premiumWillCancel,
		billingCycle,
		actualPremiumUntil: premiumUntil,
		isGiftSubscription,
		gracePeriodInfo,
		shouldShowPremiumCard,
		shouldUseCancelQuickAction,
		shouldUseReactivateQuickAction,
		shouldUseChangePlanQuickAction,
	};
};
