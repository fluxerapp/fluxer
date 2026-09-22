// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IDonationService} from '@app/api/donation/IDonationService';
import type {DonationCheckoutService} from '@app/api/donation/services/DonationCheckoutService';
import type {DonationMagicLinkService} from '@app/api/donation/services/DonationMagicLinkService';
import type {DonationCurrency} from '@fluxer/schema/src/domains/donation/DonationSchemas';

export class DonationService implements IDonationService {
	constructor(
		private magicLinkService: DonationMagicLinkService,
		private checkoutService: DonationCheckoutService,
	) {}

	async requestMagicLink(email: string, locale: string | null = null): Promise<void> {
		return this.magicLinkService.sendMagicLink(email, locale);
	}

	async validateMagicLinkToken(token: string): Promise<{
		email: string;
		stripeCustomerId: string | null;
	}> {
		return this.magicLinkService.validateToken(token);
	}

	async redeemMagicLinkToken(token: string): Promise<string | null> {
		const {stripeCustomerId} = await this.magicLinkService.validateToken(token);
		if (!stripeCustomerId) {
			return null;
		}
		const portalUrl = await this.checkoutService.createPortalSession(stripeCustomerId);
		if (!portalUrl) {
			return null;
		}
		await this.magicLinkService.consumeToken(token);
		return portalUrl;
	}

	async createDonationCheckout(params: {
		email: string;
		amountCents: number;
		currency: DonationCurrency;
		interval: 'month' | 'year' | null;
		isBusiness?: boolean;
		locale?: string | null;
	}): Promise<string> {
		return this.checkoutService.createCheckout(params);
	}
}
