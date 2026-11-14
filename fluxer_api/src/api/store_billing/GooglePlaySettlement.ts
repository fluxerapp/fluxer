// SPDX-License-Identifier: AGPL-3.0-or-later

import type {StorePurchaseRow} from '@app/api/database/types/StoreBillingTypes';
import {Logger} from '@app/api/Logger';
import type {User} from '@app/api/models/User';
import {isGooglePlayApiError} from '@app/api/store_billing/google_play/GooglePlayApiError';
import type {GooglePlayDeveloperApiClient} from '@app/api/store_billing/google_play/GooglePlayDeveloperApiClient';
import type {StoreBillingRepository} from '@app/api/store_billing/StoreBillingRepository';
import {
	LIFETIME_REFUND_REASON,
	PAID_SUBSCRIPTION_STATES,
	redactStoreKey,
} from '@app/api/store_billing/StoreBillingTypes';
import {UserPremiumTypes} from '@fluxer/constants/src/UserConstants';

export class GooglePlaySettlement {
	constructor(
		private readonly repository: StoreBillingRepository,
		private readonly googlePlayClient: GooglePlayDeveloperApiClient,
	) {}

	async settleSubscription(row: StorePurchaseRow, owner: User | null): Promise<StorePurchaseRow> {
		if (row.kind !== 'subscription' || row.user_id === null || !owner) {
			return row;
		}
		if (owner.premiumType === UserPremiumTypes.LIFETIME) {
			return this.refundForLifetimeUser(row);
		}
		if (row.acknowledged || !PAID_SUBSCRIPTION_STATES.has(row.state)) {
			return row;
		}
		let acknowledged = false;
		try {
			await this.googlePlayClient.acknowledgeSubscription(row.app_id, row.product_id, row.store_reference);
			acknowledged = true;
		} catch (error) {
			if (isGooglePlayApiError(error) && error.status === 400) {
				try {
					const purchase = await this.googlePlayClient.getSubscription(row.app_id, row.store_reference);
					acknowledged = purchase.acknowledgementState === 'ACKNOWLEDGEMENT_STATE_ACKNOWLEDGED';
				} catch (readError) {
					Logger.warn(
						{error: readError, storeKey: redactStoreKey(row.store_key)},
						'Failed to re-read a Google Play subscription',
					);
				}
			} else {
				Logger.warn(
					{error, storeKey: redactStoreKey(row.store_key)},
					'Failed to acknowledge a Google Play subscription',
				);
			}
		}
		if (!acknowledged) {
			return row;
		}
		const updated = await this.repository.updatePurchase(row, {acknowledged: true});
		return updated ?? row;
	}

	async consumeProduct(row: StorePurchaseRow): Promise<boolean> {
		try {
			await this.googlePlayClient.consumeProduct(row.app_id, row.product_id, row.store_reference);
			return true;
		} catch (error) {
			if (isGooglePlayApiError(error) && error.status === 400) {
				try {
					const purchase = await this.googlePlayClient.getProduct(row.app_id, row.store_reference);
					const lineItem = (purchase.productLineItem ?? []).find((item) => item.productId === row.product_id);
					return lineItem?.productOfferDetails?.consumptionState === 'CONSUMPTION_STATE_CONSUMED';
				} catch (readError) {
					Logger.warn(
						{error: readError, storeKey: redactStoreKey(row.store_key)},
						'Failed to re-read a Google Play product',
					);
					return false;
				}
			}
			Logger.warn({error, storeKey: redactStoreKey(row.store_key)}, 'Failed to consume a Google Play gift purchase');
			return false;
		}
	}

	async refundUnsupportedGiftQuantity(row: StorePurchaseRow): Promise<StorePurchaseRow> {
		if (row.provider !== 'google_play' || row.revoked_at || !row.latest_transaction_id) {
			return row;
		}
		try {
			await this.googlePlayClient.refundOrder(row.app_id, row.latest_transaction_id);
		} catch (error) {
			Logger.warn(
				{error, storeKey: redactStoreKey(row.store_key)},
				'Failed to refund a Google Play gift bought in a quantity above one',
			);
			return row;
		}
		Logger.info(
			{storeKey: redactStoreKey(row.store_key)},
			'Refunded a Google Play gift bought in a quantity above one',
		);
		const updated = await this.repository.updatePurchase(row, {revoked_at: new Date()});
		return updated ?? row;
	}

	private async refundForLifetimeUser(row: StorePurchaseRow): Promise<StorePurchaseRow> {
		if (
			row.revocation_reason === LIFETIME_REFUND_REASON ||
			!PAID_SUBSCRIPTION_STATES.has(row.state) ||
			!row.latest_transaction_id
		) {
			return row;
		}
		try {
			await this.googlePlayClient.refundOrder(row.app_id, row.latest_transaction_id);
		} catch (error) {
			Logger.warn(
				{error, storeKey: redactStoreKey(row.store_key)},
				'Failed to refund a Google Play purchase for a lifetime user',
			);
			return row;
		}
		Logger.info(
			{storeKey: redactStoreKey(row.store_key)},
			'Refunded a Google Play subscription bought by a lifetime user',
		);
		const updated = await this.repository.updatePurchase(row, {
			revocation_reason: LIFETIME_REFUND_REASON,
			revoked_at: new Date(),
			entitled: false,
		});
		return updated ?? row;
	}
}
