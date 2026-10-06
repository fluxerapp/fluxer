// SPDX-License-Identifier: AGPL-3.0-or-later

import type {PremiumStateResponse} from '@fluxer/schema/src/domains/premium/PremiumSchemas';
import type {MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';

export const PRICE_ANNOUNCEMENT_MESSAGE_DESCRIPTOR = msg({
	message:
		'We lowered the price of {premiumProductName} in Brazil. It is now {monthlyPrice} a month or {yearlyPrice} a year.',
	comment:
		'Nagbar body announcing a price cut to people in Brazil who do not have a paid subscription. {premiumProductName} is the short paid product name. {monthlyPrice} and {yearlyPrice} are currency amounts already formatted and localized by code; never write an amount into the translation. Keep the tone calm and plain.',
});
export const LEGACY_PRICE_OPT_IN_MONTHLY_MESSAGE_DESCRIPTOR = msg({
	message:
		'{premiumProductName} costs less now. Switch from {currentPrice} to {newPrice} a month starting {effectiveDate}, and nothing is charged today.',
	comment:
		'Nagbar body shown to monthly subscribers who are still billed the older, higher price after a price cut. {premiumProductName} is the short paid product name. {currentPrice} and {newPrice} are monthly currency amounts already formatted and localized by code, and {effectiveDate} is a date already formatted and localized by code; never write an amount or a date into the translation. Keep the tone calm and plain.',
});
export const LEGACY_PRICE_OPT_IN_YEARLY_MESSAGE_DESCRIPTOR = msg({
	message:
		'{premiumProductName} costs less now. Switch from {currentPrice} to {newPrice} a year starting {effectiveDate}, and nothing is charged today.',
	comment:
		'Nagbar body shown to yearly subscribers who are still billed the older, higher price after a price cut. {premiumProductName} is the short paid product name. {currentPrice} and {newPrice} are yearly currency amounts already formatted and localized by code, and {effectiveDate} is a date already formatted and localized by code; never write an amount or a date into the translation. Keep the tone calm and plain.',
});

interface PriceAnnouncementCampaign {
	readonly id: string;
	readonly currency: 'BRL';
	readonly monthlyCeilingMinor: number;
	readonly yearlyCeilingMinor: number;
	readonly announcementMessage: MessageDescriptor;
	readonly optInMonthlyMessage: MessageDescriptor;
	readonly optInYearlyMessage: MessageDescriptor;
}

const BRAZIL_PRICE_CUT_CAMPAIGN: PriceAnnouncementCampaign = {
	id: 'brl-price-cut',
	currency: 'BRL',
	monthlyCeilingMinor: 1890,
	yearlyCeilingMinor: 18900,
	announcementMessage: PRICE_ANNOUNCEMENT_MESSAGE_DESCRIPTOR,
	optInMonthlyMessage: LEGACY_PRICE_OPT_IN_MONTHLY_MESSAGE_DESCRIPTOR,
	optInYearlyMessage: LEGACY_PRICE_OPT_IN_YEARLY_MESSAGE_DESCRIPTOR,
};

export interface ResolvedPriceAnnouncementCampaign {
	readonly campaign: PriceAnnouncementCampaign;
	readonly currency: 'BRL';
	readonly monthlyAmountMinor: number;
	readonly yearlyAmountMinor: number;
}

export function resolvePriceAnnouncementCampaign(
	premiumState: PremiumStateResponse | null,
): ResolvedPriceAnnouncementCampaign | null {
	if (premiumState == null) return null;
	if (premiumState.effective.self_hosted || premiumState.effective.premium_purchase_disabled) return null;
	const campaign = BRAZIL_PRICE_CUT_CAMPAIGN;
	const localized = premiumState.pricing.localized;
	if (localized == null || localized.currency !== campaign.currency) return null;
	const monthlyAmountMinor = localized.monthly_amount_minor;
	const yearlyAmountMinor = localized.yearly_amount_minor;
	if (monthlyAmountMinor == null || yearlyAmountMinor == null) return null;
	if (monthlyAmountMinor > campaign.monthlyCeilingMinor || yearlyAmountMinor > campaign.yearlyCeilingMinor) return null;
	return {campaign, currency: campaign.currency, monthlyAmountMinor, yearlyAmountMinor};
}
