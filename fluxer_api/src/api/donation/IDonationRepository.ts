// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Donor} from '@app/api/donation/models/Donor';
import type {DonorMagicLinkToken} from '@app/api/donation/models/DonorMagicLinkToken';

export abstract class IDonationRepository {
	abstract findDonorByEmail(email: string): Promise<Donor | null>;

	abstract findDonorByStripeCustomerId(customerId: string): Promise<Donor | null>;

	abstract findDonorByStripeSubscriptionId(subscriptionId: string): Promise<Donor | null>;

	abstract upsertDonor(donor: Donor): Promise<void>;

	abstract createDonor(data: {
		email: string;
		stripeCustomerId: string | null;
		businessName?: string | null;
		taxId?: string | null;
		taxIdType?: string | null;
		stripeSubscriptionId: string | null;
		subscriptionAmountCents: number | null;
		subscriptionCurrency: string | null;
		subscriptionInterval: string | null;
		subscriptionCurrentPeriodEnd: Date | null;
		subscriptionCancelAt?: Date | null;
	}): Promise<Donor>;

	abstract updateDonorSubscription(
		email: string,
		data: {
			stripeCustomerId: string | null;
			businessName?: string | null;
			taxId?: string | null;
			taxIdType?: string | null;
			stripeSubscriptionId: string | null;
			subscriptionAmountCents: number | null;
			subscriptionCurrency: string | null;
			subscriptionInterval: string | null;
			subscriptionCurrentPeriodEnd: Date | null;
			subscriptionCancelAt?: Date | null;
		},
	): Promise<Donor | null>;

	abstract cancelDonorSubscription(email: string): Promise<void>;

	abstract createMagicLinkToken(token: DonorMagicLinkToken): Promise<void>;

	abstract findMagicLinkToken(token: string): Promise<DonorMagicLinkToken | null>;

	abstract markMagicLinkTokenUsed(token: string, usedAt: Date): Promise<void>;

	abstract invalidateTokensForEmail(email: string): Promise<void>;
}
