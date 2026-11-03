// SPDX-License-Identifier: AGPL-3.0-or-later

import type {PriceIds} from '@app/features/premium/commands/PremiumCommands';
import {
	PURCHASE_BLOCKED_CLAIM_DESCRIPTOR,
	PURCHASE_BLOCKED_VERIFY_DESCRIPTOR,
} from '@app/features/premium/components/plutonium_page/PlutoniumPageMessages';
import {PAYMENT_DISPUTE_OPEN_DESCRIPTOR} from '@app/features/premium/utils/PremiumMessageDescriptors';
import {
	areGiftPurchasesAvailable,
	arePremiumPurchasesAvailable,
	canServiceStripeSubscriptions,
	getPremiumProductName,
	shouldShowPremiumFeatures,
} from '@app/features/premium/utils/PremiumUtils';
import {type PurchaseBlockedReason, PurchaseBlockedReasons} from '@fluxer/constants/src/AccountTierConstants';
import type {I18n, MessageDescriptor} from '@lingui/core';

const PURCHASE_BLOCKED_NOTES: Readonly<Record<PurchaseBlockedReason, MessageDescriptor>> = {
	[PurchaseBlockedReasons.UNCLAIMED]: PURCHASE_BLOCKED_CLAIM_DESCRIPTOR,
	[PurchaseBlockedReasons.EMAIL_UNVERIFIED]: PURCHASE_BLOCKED_VERIFY_DESCRIPTOR,
};

export function resolvePurchaseBlockedNote(
	i18n: I18n,
	blockedReason: PurchaseBlockedReason | null,
	paymentDisputeOpen: boolean,
): string | null {
	if (blockedReason !== null) {
		return i18n._(PURCHASE_BLOCKED_NOTES[blockedReason], {premiumProductName: getPremiumProductName()});
	}
	return paymentDisputeOpen ? i18n._(PAYMENT_DISPUTE_OPEN_DESCRIPTOR) : null;
}

type HeroMode = 'member-purchase' | 'member' | 'redeem-only' | 'purchase';

export interface PurchaseSurfaceInput {
	priceIds: PriceIds | null;
	userPurchaseDisabled: boolean;
	isMember: boolean;
	canStartSubscription: boolean;
	hasBillingRelationship: boolean;
}

export interface PurchaseSurface {
	purchasesAvailable: boolean;
	giftPurchasesAvailable: boolean;
	canSubscribe: boolean;
	billingUnavailable: boolean;
	heroMode: HeroMode;
	showClosing: boolean;
}

export function resolvePurchaseSurface(input: PurchaseSurfaceInput): PurchaseSurface {
	const purchasesAvailable = arePremiumPurchasesAvailable(input.userPurchaseDisabled);
	const giftPurchasesAvailable = areGiftPurchasesAvailable(input.priceIds, input.userPurchaseDisabled);
	const billingUnavailable =
		!shouldShowPremiumFeatures() ||
		(!arePremiumPurchasesAvailable() && !(input.hasBillingRelationship && canServiceStripeSubscriptions()));
	const canSubscribe = purchasesAvailable && input.canStartSubscription;
	let heroMode: HeroMode = 'purchase';
	if (input.isMember) {
		heroMode = canSubscribe ? 'member-purchase' : 'member';
	} else if (!purchasesAvailable) {
		heroMode = 'redeem-only';
	}
	return {
		purchasesAvailable,
		giftPurchasesAvailable,
		canSubscribe,
		billingUnavailable,
		heroMode,
		showClosing: canSubscribe || (input.isMember && giftPurchasesAvailable),
	};
}
