// SPDX-License-Identifier: AGPL-3.0-or-later

import type {DonationCurrency} from '@fluxer/schema/src/domains/donation/DonationSchemas';

export interface IDonationService {
	requestMagicLink(email: string, locale?: string | null): Promise<void>;
	validateMagicLinkToken(token: string): Promise<{
		email: string;
		stripeCustomerId: string | null;
	}>;
	redeemMagicLinkToken(token: string): Promise<string | null>;
	createDonationCheckout(params: {
		email: string;
		amountCents: number;
		currency: DonationCurrency;
		interval: 'month' | 'year' | null;
		isBusiness?: boolean;
		locale?: string | null;
	}): Promise<string>;
}
