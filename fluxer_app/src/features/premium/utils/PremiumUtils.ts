// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import DeveloperOptions from '@app/features/devtools/state/DeveloperOptions';
import type {PriceIdsResponse} from '@fluxer/schema/src/domains/premium/PremiumSchemas';

export function shouldShowPremiumFeatures(): boolean {
	if (DeveloperOptions.selfHostedModeOverride) {
		return false;
	}
	return !RuntimeConfig.isSelfHosted() || RuntimeConfig.premiumEnabled;
}

export function arePremiumPurchasesAvailable(purchaseDisabled = false): boolean {
	if (!shouldShowPremiumFeatures()) {
		return false;
	}
	if (!RuntimeConfig.isSelfHosted()) {
		return true;
	}
	return RuntimeConfig.stripeEnabled && !purchaseDisabled;
}

export function canServiceStripeSubscriptions(): boolean {
	if (!shouldShowPremiumFeatures()) {
		return false;
	}
	return !RuntimeConfig.isSelfHosted() || RuntimeConfig.stripeServiceable;
}

export function areGiftPurchasesAvailable(
	priceIds: PriceIdsResponse | null | undefined,
	purchaseDisabled = false,
): boolean {
	if (!arePremiumPurchasesAvailable(purchaseDisabled)) {
		return false;
	}
	if (!RuntimeConfig.isSelfHosted()) {
		return true;
	}
	return priceIds?.gift_1_month != null || priceIds?.gift_1_year != null;
}
