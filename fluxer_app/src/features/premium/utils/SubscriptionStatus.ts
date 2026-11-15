// SPDX-License-Identifier: AGPL-3.0-or-later

import DeveloperOptions from '@app/features/devtools/state/DeveloperOptions';
import {getPremiumGraceEndDate} from '@app/features/premium/utils/PremiumGrace';
import {
	classifyPremiumSubscription,
	getSubscriptionRenewalDate,
} from '@app/features/premium/utils/PremiumSubscriptionClassification';
import {getStoreOwnedSubscription} from '@app/features/premium/utils/PremiumUtils';
import type {User} from '@app/features/user/models/User';
import {UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import {MS_PER_DAY} from '@fluxer/date_utils/src/DateConstants';
import type {PremiumStateResponse} from '@fluxer/schema/src/domains/premium/PremiumSchemas';
import type {PremiumStoreSubscriptionState} from '@fluxer/schema/src/domains/premium/StoreBillingSchemas';

export interface GracePeriodInfo {
	isInGracePeriod: boolean;
	isExpired: boolean;
	graceEndDate: Date | null;
	showExpiredState: boolean;
	isPaymentRecovery: boolean;
}

export interface SubscriptionStatusInfo {
	isPremium: boolean;
	perksDisabled: boolean;
	isVisionary: boolean;
	hasEverPurchased: boolean;
	premiumWillCancel: boolean;
	billingCycle: string | null;
	actualPremiumUntil: Date | null;
	renewsAt: Date | null;
	isGiftSubscription: boolean;
	isGiftGrace: boolean;
	canStartSubscription: boolean;
	storeSubscription: PremiumStoreSubscriptionState | null;
	gracePeriodInfo: GracePeriodInfo;
	shouldShowPremiumCard: boolean;
	shouldUseCancelQuickAction: boolean;
	shouldUseReactivateQuickAction: boolean;
	shouldUseChangePlanQuickAction: boolean;
}

const EXPIRED_STATE_DURATION_MS = 30 * MS_PER_DAY;

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
export function getSubscriptionStatus(
	currentUser: User | null,
	premiumState: PremiumStateResponse | null,
): SubscriptionStatusInfo {
	const actual = premiumState?.actual ?? null;
	const effective = premiumState?.effective ?? null;
	const useScenarioOverride = DeveloperOptions.premiumScenarioOverride !== null;
	const useDeveloperOverride = hasDeveloperPremiumStateOverride();
	const storeSubscription = useScenarioOverride ? null : getStoreOwnedSubscription(premiumState);
	const premiumType =
		useScenarioOverride || DeveloperOptions.premiumTypeOverride !== null
			? DeveloperOptions.premiumTypeOverride
			: (actual?.premium_type ?? currentUser?.premiumType ?? null);
	const premiumUntil = useScenarioOverride
		? DeveloperOptions.premiumUntilOverride
		: (DeveloperOptions.premiumUntilOverride ??
			parseOptionalDate(actual?.premium_until) ??
			parseOptionalDate(effective?.premium_until) ??
			currentUser?.premiumUntil ??
			null);
	let premiumGraceEndsAt: Date | null = null;
	if (!useScenarioOverride) {
		if (storeSubscription) {
			premiumGraceEndsAt = parseOptionalDate(storeSubscription.grace_ends_at);
		} else {
			premiumGraceEndsAt = parseOptionalDate(actual?.premium_grace_ends_at) ?? currentUser?.premiumGraceEndsAt ?? null;
		}
	}
	const perksDisabled = effective?.premium_perks_disabled ?? currentUser?.premiumPerksDisabled ?? false;
	const hasPaidPremium = premiumType != null && premiumType > UserPremiumTypes.NONE;
	const isVisionary = premiumType === UserPremiumTypes.LIFETIME;
	const hasEverPurchased =
		useScenarioOverride || DeveloperOptions.hasEverPurchasedOverride !== null
			? (DeveloperOptions.hasEverPurchasedOverride ?? false)
			: (actual?.has_ever_purchased ?? currentUser?.hasEverPurchased ?? false);
	let premiumWillCancel: boolean;
	if (useScenarioOverride || DeveloperOptions.premiumWillCancelOverride !== null) {
		premiumWillCancel = DeveloperOptions.premiumWillCancelOverride ?? false;
	} else if (storeSubscription) {
		premiumWillCancel = !storeSubscription.will_renew;
	} else {
		premiumWillCancel = actual?.premium_will_cancel ?? currentUser?.premiumWillCancel ?? false;
	}
	let billingCycle: string | null;
	if (useScenarioOverride || DeveloperOptions.premiumBillingCycleOverride !== null) {
		billingCycle = DeveloperOptions.premiumBillingCycleOverride;
	} else if (storeSubscription) {
		billingCycle = storeSubscription.billing_cycle;
	} else {
		billingCycle = actual?.premium_billing_cycle ?? currentUser?.premiumBillingCycle ?? null;
	}
	const isStoreGrace = storeSubscription?.state === 'grace' && premiumGraceEndsAt != null;
	const gracePeriodInfo = ((): GracePeriodInfo => {
		if (isVisionary) {
			return {
				isInGracePeriod: false,
				isExpired: false,
				graceEndDate: null,
				showExpiredState: false,
				isPaymentRecovery: false,
			};
		}
		const explicitGraceEnd = premiumGraceEndsAt ?? null;
		const expiryDate = premiumUntil ? new Date(premiumUntil) : null;
		const graceEndDate = expiryDate ? getPremiumGraceEndDate(expiryDate, explicitGraceEnd) : explicitGraceEnd;
		if (!graceEndDate) {
			return {
				isInGracePeriod: false,
				isExpired: false,
				graceEndDate: null,
				showExpiredState: false,
				isPaymentRecovery: false,
			};
		}
		const now = new Date();
		const anchorDate = expiryDate ?? graceEndDate;
		const expiredStateEndDate = new Date(anchorDate.getTime() + EXPIRED_STATE_DURATION_MS);
		const isInGracePeriod = (isStoreGrace || !expiryDate || now > expiryDate) && now <= graceEndDate;
		const isExpired = now > graceEndDate;
		const showExpiredState = isExpired && now <= expiredStateEndDate;
		const isPaymentRecovery = isInGracePeriod && explicitGraceEnd != null && billingCycle != null;
		return {isInGracePeriod, isExpired, graceEndDate, showExpiredState, isPaymentRecovery};
	})();
	const {isInGracePeriod, isExpired: isFullyExpired, showExpiredState} = gracePeriodInfo;
	const billingSubscription = useScenarioOverride ? null : (premiumState?.billing.subscription ?? null);
	const {isGiftSubscription, isGiftGrace, canStartSubscription} = classifyPremiumSubscription({
		hasPaidPremium,
		isVisionary,
		hasStoreSubscription: storeSubscription != null,
		billingCycle,
		premiumUntil,
		isInGracePeriod,
		isFullyExpired,
		billingKnown: !useScenarioOverride && premiumState != null,
		storeOwnershipKnown: useScenarioOverride || premiumState != null,
		billingSubscription,
	});
	const renewsAt = storeSubscription
		? parseOptionalDate(storeSubscription.expires_at)
		: getSubscriptionRenewalDate(premiumUntil, billingSubscription);
	const isPremium = useDeveloperOverride
		? hasPaidPremium && !isFullyExpired && !perksDisabled
		: (effective?.is_premium ?? currentUser?.isPremium() ?? false);
	const shouldShowPremiumCard = hasPaidPremium || isInGracePeriod || showExpiredState;
	const shouldUseCancelQuickAction =
		hasPaidPremium &&
		!isVisionary &&
		!isInGracePeriod &&
		!isFullyExpired &&
		!premiumWillCancel &&
		!isGiftSubscription &&
		!storeSubscription;
	const shouldUseReactivateQuickAction =
		hasPaidPremium &&
		premiumWillCancel &&
		!isVisionary &&
		!isInGracePeriod &&
		!isFullyExpired &&
		!isGiftSubscription &&
		!storeSubscription;
	const shouldUseChangePlanQuickAction =
		hasPaidPremium &&
		!isVisionary &&
		!isInGracePeriod &&
		!isFullyExpired &&
		!premiumWillCancel &&
		!isGiftSubscription &&
		!storeSubscription &&
		(billingCycle === 'monthly' || billingCycle === 'yearly');
	return {
		isPremium,
		perksDisabled,
		isVisionary,
		hasEverPurchased,
		premiumWillCancel,
		billingCycle,
		actualPremiumUntil: premiumUntil,
		renewsAt,
		isGiftSubscription,
		isGiftGrace,
		canStartSubscription,
		storeSubscription,
		gracePeriodInfo,
		shouldShowPremiumCard,
		shouldUseCancelQuickAction,
		shouldUseReactivateQuickAction,
		shouldUseChangePlanQuickAction,
	};
}
