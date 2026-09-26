// SPDX-License-Identifier: AGPL-3.0-or-later

import {BatchBuilder, deleteOneOrMany, fetchMany, fetchOne, upsertOne} from '@app/api/database/CassandraQueryExecution';
import {Db, type PatchObject} from '@app/api/database/CassandraTypes';
import {executeVersionedUpdate} from '@app/api/database/CassandraVersionedUpdate';
import type {
	DonorByStripeCustomerIdRow,
	DonorByStripeSubscriptionIdRow,
	DonorMagicLinkTokenByEmailRow,
	DonorMagicLinkTokenRow,
	DonorRow,
} from '@app/api/database/types/DonationTypes';
import {
	DonorMagicLinkTokens,
	DonorMagicLinkTokensByEmail,
	Donors,
	DonorsByStripeCustomerId,
	DonorsByStripeSubscriptionId,
} from '@app/api/donation/DonationTables';
import {IDonationRepository} from '@app/api/donation/IDonationRepository';
import {Donor} from '@app/api/donation/models/Donor';
import {DonorMagicLinkToken} from '@app/api/donation/models/DonorMagicLinkToken';

const FETCH_DONOR_BY_EMAIL_QUERY = Donors.selectCql({
	where: Donors.where.eq('email'),
	limit: 1,
});
const FETCH_DONOR_BY_STRIPE_CUSTOMER_ID_QUERY = DonorsByStripeCustomerId.selectCql({
	columns: ['email'],
	where: DonorsByStripeCustomerId.where.eq('stripe_customer_id'),
	limit: 1,
});
const FETCH_DONOR_BY_STRIPE_SUBSCRIPTION_ID_QUERY = DonorsByStripeSubscriptionId.selectCql({
	columns: ['email'],
	where: DonorsByStripeSubscriptionId.where.eq('stripe_subscription_id'),
	limit: 1,
});
const FETCH_MAGIC_LINK_TOKEN_QUERY = DonorMagicLinkTokens.selectCql({
	where: DonorMagicLinkTokens.where.eq('token_'),
	limit: 1,
});
const FETCH_MAGIC_LINK_TOKENS_BY_EMAIL_QUERY = DonorMagicLinkTokensByEmail.selectCql({
	columns: ['donor_email', 'token_'],
	where: DonorMagicLinkTokensByEmail.where.eq('donor_email'),
});

export class DonationRepository extends IDonationRepository {
	async findDonorByEmail(email: string): Promise<Donor | null> {
		const row = await fetchOne<DonorRow>(FETCH_DONOR_BY_EMAIL_QUERY, {email});
		return row ? new Donor(row) : null;
	}

	async findDonorByStripeCustomerId(customerId: string): Promise<Donor | null> {
		const mapping = await fetchOne<DonorByStripeCustomerIdRow>(FETCH_DONOR_BY_STRIPE_CUSTOMER_ID_QUERY, {
			stripe_customer_id: customerId,
		});
		if (!mapping) return null;
		return this.findDonorByEmail(mapping.email);
	}

	async findDonorByStripeSubscriptionId(subscriptionId: string): Promise<Donor | null> {
		const mapping = await fetchOne<DonorByStripeSubscriptionIdRow>(FETCH_DONOR_BY_STRIPE_SUBSCRIPTION_ID_QUERY, {
			stripe_subscription_id: subscriptionId,
		});
		if (!mapping) return null;
		return this.findDonorByEmail(mapping.email);
	}

	async upsertDonor(donor: Donor): Promise<void> {
		const row = donor.toRow();
		const batch = new BatchBuilder();
		batch.addPrepared(Donors.upsertAll(row));
		if (row.stripe_customer_id) {
			batch.addPrepared(
				DonorsByStripeCustomerId.upsertAll({
					stripe_customer_id: row.stripe_customer_id,
					email: row.email,
				}),
			);
		}
		if (row.stripe_subscription_id) {
			batch.addPrepared(
				DonorsByStripeSubscriptionId.upsertAll({
					stripe_subscription_id: row.stripe_subscription_id,
					email: row.email,
				}),
			);
		}
		await batch.execute();
	}

	async createDonor(data: {
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
		subscriptionStatus?: string | null;
	}): Promise<Donor> {
		const now = new Date();
		const donorRow: DonorRow = {
			email: data.email,
			stripe_customer_id: data.stripeCustomerId,
			business_name: data.businessName ?? null,
			tax_id: data.taxId ?? null,
			tax_id_type: data.taxIdType ?? null,
			stripe_subscription_id: data.stripeSubscriptionId,
			subscription_amount_cents: data.subscriptionAmountCents,
			subscription_currency: data.subscriptionCurrency,
			subscription_interval: data.subscriptionInterval,
			subscription_current_period_end: data.subscriptionCurrentPeriodEnd,
			subscription_cancel_at: data.subscriptionCancelAt ?? null,
			subscription_status: data.subscriptionStatus ?? null,
			created_at: now,
			updated_at: now,
			version: 1,
		};
		const batch = new BatchBuilder();
		batch.addPrepared(Donors.upsertAll(donorRow));
		if (donorRow.stripe_customer_id) {
			batch.addPrepared(
				DonorsByStripeCustomerId.upsertAll({
					stripe_customer_id: donorRow.stripe_customer_id,
					email: donorRow.email,
				}),
			);
		}
		if (donorRow.stripe_subscription_id) {
			batch.addPrepared(
				DonorsByStripeSubscriptionId.upsertAll({
					stripe_subscription_id: donorRow.stripe_subscription_id,
					email: donorRow.email,
				}),
			);
		}
		await batch.execute();
		return new Donor(donorRow);
	}

	async updateDonorSubscription(
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
			subscriptionStatus?: string | null;
		},
	): Promise<Donor | null> {
		await executeVersionedUpdate(
			() => fetchOne<DonorRow>(FETCH_DONOR_BY_EMAIL_QUERY, {email}),
			() => {
				const patch: PatchObject = {
					stripe_customer_id: Db.set(data.stripeCustomerId),
					stripe_subscription_id: Db.set(data.stripeSubscriptionId),
					subscription_amount_cents: Db.set(data.subscriptionAmountCents),
					subscription_currency: Db.set(data.subscriptionCurrency),
					subscription_interval: Db.set(data.subscriptionInterval),
					subscription_current_period_end: Db.set(data.subscriptionCurrentPeriodEnd),
					subscription_cancel_at: Db.set(data.subscriptionCancelAt ?? null),
					subscription_status: Db.set(data.subscriptionStatus ?? null),
					updated_at: Db.set(new Date()),
				};
				if (data.businessName !== undefined) {
					patch.business_name = Db.set(data.businessName);
				}
				if (data.taxId !== undefined) {
					patch.tax_id = Db.set(data.taxId);
				}
				if (data.taxIdType !== undefined) {
					patch.tax_id_type = Db.set(data.taxIdType);
				}
				return {
					pk: {email},
					patch,
				};
			},
			Donors,
		);
		const batch = new BatchBuilder();
		if (data.stripeCustomerId) {
			batch.addPrepared(
				DonorsByStripeCustomerId.upsertAll({
					stripe_customer_id: data.stripeCustomerId,
					email,
				}),
			);
		}
		if (data.stripeSubscriptionId) {
			batch.addPrepared(
				DonorsByStripeSubscriptionId.upsertAll({
					stripe_subscription_id: data.stripeSubscriptionId,
					email,
				}),
			);
		}
		await batch.execute();
		return this.findDonorByEmail(email);
	}

	async updateDonorCustomerDetails(
		email: string,
		data: {
			stripeCustomerId: string | null;
			businessName?: string | null;
			taxId?: string | null;
			taxIdType?: string | null;
		},
	): Promise<Donor | null> {
		await executeVersionedUpdate(
			() => fetchOne<DonorRow>(FETCH_DONOR_BY_EMAIL_QUERY, {email}),
			() => {
				const patch: PatchObject = {
					stripe_customer_id: Db.set(data.stripeCustomerId),
					updated_at: Db.set(new Date()),
				};
				if (data.businessName !== undefined) {
					patch.business_name = Db.set(data.businessName);
				}
				if (data.taxId !== undefined) {
					patch.tax_id = Db.set(data.taxId);
				}
				if (data.taxIdType !== undefined) {
					patch.tax_id_type = Db.set(data.taxIdType);
				}
				return {
					pk: {email},
					patch,
				};
			},
			Donors,
		);
		if (data.stripeCustomerId) {
			await upsertOne(
				DonorsByStripeCustomerId.upsertAll({
					stripe_customer_id: data.stripeCustomerId,
					email,
				}),
			);
		}
		return this.findDonorByEmail(email);
	}

	async linkDonorStripeCustomer(email: string, stripeCustomerId: string): Promise<void> {
		await upsertOne(
			DonorsByStripeCustomerId.upsertAll({
				stripe_customer_id: stripeCustomerId,
				email,
			}),
		);
	}

	async clearDonorStripeCustomer(stripeCustomerId: string): Promise<void> {
		const donor = await this.findDonorByStripeCustomerId(stripeCustomerId);
		if (!donor) {
			return;
		}
		if (donor.stripeCustomerId === stripeCustomerId) {
			await executeVersionedUpdate(
				() => fetchOne<DonorRow>(FETCH_DONOR_BY_EMAIL_QUERY, {email: donor.email}),
				() => ({
					pk: {email: donor.email},
					patch: {
						stripe_customer_id: Db.clear(),
						updated_at: Db.set(new Date()),
					},
				}),
				Donors,
			);
		}
		await deleteOneOrMany(
			DonorsByStripeCustomerId.deleteByPk({
				stripe_customer_id: stripeCustomerId,
				email: donor.email,
			}),
		);
	}

	async cancelDonorSubscription(email: string): Promise<void> {
		const current = await this.findDonorByEmail(email);
		if (!current) {
			return;
		}
		const oldSubscriptionId = current.stripeSubscriptionId;
		await executeVersionedUpdate(
			() => fetchOne<DonorRow>(FETCH_DONOR_BY_EMAIL_QUERY, {email}),
			() => ({
				pk: {email},
				patch: {
					stripe_subscription_id: Db.clear(),
					subscription_amount_cents: Db.clear(),
					subscription_currency: Db.clear(),
					subscription_interval: Db.clear(),
					subscription_current_period_end: Db.clear(),
					subscription_cancel_at: Db.clear(),
					subscription_status: Db.clear(),
					updated_at: Db.set(new Date()),
				},
			}),
			Donors,
		);
		if (oldSubscriptionId) {
			await deleteOneOrMany(
				DonorsByStripeSubscriptionId.deleteByPk({
					stripe_subscription_id: oldSubscriptionId,
					email,
				}),
			);
		}
	}

	async deleteDonorSubscriptionMapping(stripeSubscriptionId: string, email: string): Promise<void> {
		await deleteOneOrMany(
			DonorsByStripeSubscriptionId.deleteByPk({
				stripe_subscription_id: stripeSubscriptionId,
				email,
			}),
		);
	}

	async createMagicLinkToken(token: DonorMagicLinkToken): Promise<void> {
		const row = token.toRow();
		const batch = new BatchBuilder();
		batch.addPrepared(DonorMagicLinkTokens.upsertAll(row));
		batch.addPrepared(
			DonorMagicLinkTokensByEmail.upsertAll({
				donor_email: row.donor_email,
				token_: row.token_,
			}),
		);
		await batch.execute();
	}

	async findMagicLinkToken(token: string): Promise<DonorMagicLinkToken | null> {
		const row = await fetchOne<DonorMagicLinkTokenRow>(FETCH_MAGIC_LINK_TOKEN_QUERY, {token_: token});
		return row ? new DonorMagicLinkToken(row) : null;
	}

	async markMagicLinkTokenUsed(token: string, usedAt: Date): Promise<void> {
		await fetchOne(
			DonorMagicLinkTokens.patchByPk(
				{token_: token},
				{
					used_at: Db.set(usedAt),
				},
			),
		);
	}

	async invalidateTokensForEmail(email: string): Promise<void> {
		const tokenRefs = await fetchMany<DonorMagicLinkTokenByEmailRow>(FETCH_MAGIC_LINK_TOKENS_BY_EMAIL_QUERY, {
			donor_email: email,
		});
		if (tokenRefs.length === 0) return;
		const batch = new BatchBuilder();
		for (const ref of tokenRefs) {
			batch.addPrepared(DonorMagicLinkTokens.deleteByPk({token_: ref.token_}));
			batch.addPrepared(
				DonorMagicLinkTokensByEmail.deleteByPk({
					donor_email: ref.donor_email,
					token_: ref.token_,
				}),
			);
		}
		await batch.execute();
	}
}
