// SPDX-License-Identifier: AGPL-3.0-or-later

import {APP_STORE_NAME, GOOGLE_PLAY_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {DEFAULT_PREMIUM_PRODUCT_NAME, PRODUCT_NAME} from '@app/features/app/config/ProductConstants';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import DeveloperOptions from '@app/features/devtools/state/DeveloperOptions';
import type {PremiumStateResponse, PriceIdsResponse} from '@fluxer/schema/src/domains/premium/PremiumSchemas';
import type {PremiumStoreSubscriptionState} from '@fluxer/schema/src/domains/premium/StoreBillingSchemas';

export function getPremiumProductName(): string {
	return (
		RuntimeConfig.getSnapshotOrNull()?.appPublic.branding.premium_product_name?.trim() || DEFAULT_PREMIUM_PRODUCT_NAME
	);
}

export function getPremiumProductFullName(): string {
	const productName = RuntimeConfig.getSnapshotOrNull()?.appPublic.branding.product_name?.trim() || PRODUCT_NAME;
	return `${productName} ${getPremiumProductName()}`;
}

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

export function getStoreOwnedSubscription(
	premiumState: PremiumStateResponse | null | undefined,
): PremiumStoreSubscriptionState | null {
	const store = premiumState?.store ?? null;
	return store && premiumState?.subscription_provider === store.provider ? store : null;
}

export function getStoreName(provider: PremiumStoreSubscriptionState['provider']): string {
	return provider === 'app_store' ? APP_STORE_NAME : GOOGLE_PLAY_NAME;
}
