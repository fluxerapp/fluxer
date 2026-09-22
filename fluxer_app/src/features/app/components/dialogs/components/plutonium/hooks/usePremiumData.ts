// SPDX-License-Identifier: AGPL-3.0-or-later

import DeveloperOptions from '@app/features/devtools/state/DeveloperOptions';
import type {CurrentSubscriptionPrice, PriceIds} from '@app/features/premium/commands/PremiumCommands';
import {formatMinorUnitPrice} from '@app/features/premium/utils/PricingUtils';
import * as LocaleUtils from '@app/features/user/utils/LocaleUtils';
import type {PremiumStateResponse} from '@fluxer/schema/src/domains/premium/PremiumSchemas';
import {useMemo} from 'react';

const PRICE_PLACEHOLDER = '...';
const formatPriceLabel = (
	amountMinor: number | null | undefined,
	currency: string | null | undefined,
	locale: string,
): string => {
	const formatted = formatMinorUnitPrice(amountMinor, currency, locale);
	return formatted == null ? PRICE_PLACEHOLDER : formatted;
};
const formatOptionalPriceLabel = (
	amountMinor: number | null | undefined,
	currency: string | null | undefined,
	locale: string,
): string | null => formatMinorUnitPrice(amountMinor, currency, locale);
const hasDeveloperPremiumStateOverride = (): boolean =>
	DeveloperOptions.premiumScenarioOverride !== null ||
	DeveloperOptions.premiumTypeOverride !== null ||
	DeveloperOptions.premiumUntilOverride !== null ||
	DeveloperOptions.premiumBillingCycleOverride !== null ||
	DeveloperOptions.premiumWillCancelOverride !== null ||
	DeveloperOptions.hasEverPurchasedOverride !== null;

export interface PremiumData {
	priceIds: PriceIds | null;
	monthlyPrice: string;
	yearlyPrice: string;
	giftMonthlyPrice: string;
	giftYearlyPrice: string;
	currentSubscriptionPrice: CurrentSubscriptionPrice | null;
	currentSubscriptionPriceLabel: string | null;
	currentSubscriptionListPriceLabel: string | null;
	isCurrentSubscriptionGrandfathered: boolean;
}

export const usePremiumData = ({
	premiumState = null,
}: {
	premiumState?: PremiumStateResponse | null;
} = {}): PremiumData => {
	const locale = LocaleUtils.getCurrentLocale();
	const priceIds = premiumState?.pricing.localized ?? null;
	const monthlyPrice = useMemo(() => {
		return formatPriceLabel(priceIds?.monthly_amount_minor, priceIds?.currency, locale);
	}, [locale, priceIds?.currency, priceIds?.monthly_amount_minor]);
	const yearlyPrice = useMemo(() => {
		return formatPriceLabel(priceIds?.yearly_amount_minor, priceIds?.currency, locale);
	}, [locale, priceIds?.currency, priceIds?.yearly_amount_minor]);
	const giftMonthlyPrice = useMemo(() => {
		return formatPriceLabel(priceIds?.gift_1_month_amount_minor, priceIds?.gift_currency, locale);
	}, [locale, priceIds?.gift_1_month_amount_minor, priceIds?.gift_currency]);
	const giftYearlyPrice = useMemo(() => {
		return formatPriceLabel(priceIds?.gift_1_year_amount_minor, priceIds?.gift_currency, locale);
	}, [locale, priceIds?.gift_1_year_amount_minor, priceIds?.gift_currency]);
	const currentSubscriptionPrice: CurrentSubscriptionPrice | null = hasDeveloperPremiumStateOverride()
		? null
		: (premiumState?.billing.current_subscription_price ?? null);
	const currentSubscriptionPriceLabel = useMemo(() => {
		if (!currentSubscriptionPrice) return null;
		return formatOptionalPriceLabel(currentSubscriptionPrice.amount_minor, currentSubscriptionPrice.currency, locale);
	}, [currentSubscriptionPrice, locale]);
	const currentSubscriptionListPriceLabel = useMemo(() => {
		if (!currentSubscriptionPrice || currentSubscriptionPrice.list_amount_minor == null) return null;
		return formatOptionalPriceLabel(
			currentSubscriptionPrice.list_amount_minor,
			currentSubscriptionPrice.currency,
			locale,
		);
	}, [currentSubscriptionPrice, locale]);
	return {
		priceIds,
		monthlyPrice,
		yearlyPrice,
		giftMonthlyPrice,
		giftYearlyPrice,
		currentSubscriptionPrice,
		currentSubscriptionPriceLabel,
		currentSubscriptionListPriceLabel,
		isCurrentSubscriptionGrandfathered:
			currentSubscriptionPrice?.is_grandfathered === true &&
			(currentSubscriptionPrice.list_amount_minor == null ||
				currentSubscriptionPrice.list_amount_minor > currentSubscriptionPrice.amount_minor),
	};
};
