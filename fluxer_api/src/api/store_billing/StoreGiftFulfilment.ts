// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import type {StorePurchaseRow} from '@app/api/database/types/StoreBillingTypes';
import {Logger} from '@app/api/Logger';
import {type GiftCode, mapGiftDurationMonthsToFields} from '@app/api/models/GiftCode';
import type {GooglePlaySettlement} from '@app/api/store_billing/GooglePlaySettlement';
import {STORE_PRODUCT_SLOTS} from '@app/api/store_billing/StoreBillingConfig';
import {selectActiveStoreSubscription} from '@app/api/store_billing/StoreBillingMappers';
import type {StoreBillingRepository} from '@app/api/store_billing/StoreBillingRepository';
import {redactStoreKey, type StoreSlot, UNSUPPORTED_QUANTITY_REASON} from '@app/api/store_billing/StoreBillingTypes';
import type {StoreEntitlementWriter} from '@app/api/store_billing/StoreEntitlementWriter';
import type {StripeGiftReversalHandler} from '@app/api/stripe/services/StripeGiftReversalHandler';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import * as RandomUtils from '@app/api/utils/RandomUtils';

const PURCHASE_WRITE_ATTEMPTS = 4;
const GIFT_REFUND_REVERSAL_REASON = 'store_refund';

interface StoreGiftFulfilmentDeps {
	repository: StoreBillingRepository;
	userRepository: UserRepository;
	giftReversalHandler: StripeGiftReversalHandler;
	googlePlaySettlement: GooglePlaySettlement;
	entitlementWriter: StoreEntitlementWriter;
}

export class StoreGiftFulfilment {
	constructor(private readonly deps: StoreGiftFulfilmentDeps) {}

	async settleGift(row: StorePurchaseRow): Promise<StorePurchaseRow> {
		if (row.state === 'refunded') {
			if (row.gift_code) {
				await this.reverseStoreGift(row.gift_code, row);
			}
			return row;
		}
		if (row.revocation_reason === UNSUPPORTED_QUANTITY_REASON) {
			return this.deps.googlePlaySettlement.refundUnsupportedGiftQuantity(row);
		}
		if (!row.entitled || row.user_id === null || (row.state !== 'purchased' && row.state !== 'fulfilled')) {
			return row;
		}
		let current = row;
		if (!current.gift_code) {
			const candidate = await this.generateUniqueGiftCode();
			await this.deps.repository.setGiftCode(current.store_key, candidate);
			const reloaded = await this.deps.repository.findPurchase(current.store_key);
			if (!reloaded?.gift_code) {
				return current;
			}
			current = reloaded;
		}
		const giftCode = current.gift_code;
		if (!giftCode || current.user_id === null) {
			return current;
		}
		const existingGift = await this.deps.userRepository.findGiftCode(giftCode);
		if (!existingGift) {
			await this.createStoreGiftCode(giftCode, current.user_id, current.slot);
		} else if (existingGift.revokedAt) {
			await this.restoreStoreGift(existingGift, current);
		}
		let acknowledged = current.acknowledged;
		if (current.provider === 'google_play' && !acknowledged) {
			acknowledged = await this.deps.googlePlaySettlement.consumeProduct(current);
		}
		if (current.state !== 'fulfilled' || acknowledged !== current.acknowledged) {
			for (let attempt = 0; attempt < PURCHASE_WRITE_ATTEMPTS; attempt++) {
				const updated = await this.deps.repository.updatePurchase(current, {state: 'fulfilled', acknowledged});
				if (updated) {
					return updated;
				}
				const reloaded = await this.deps.repository.findPurchase(current.store_key);
				if (!reloaded) {
					break;
				}
				current = reloaded;
			}
		}
		return current;
	}

	private async createStoreGiftCode(code: string, purchaserId: UserID, slot: StoreSlot): Promise<void> {
		const duration = mapGiftDurationMonthsToFields(STORE_PRODUCT_SLOTS[slot].durationMonths);
		await this.deps.userRepository.createGiftCode({
			code,
			duration_months: null,
			duration_type: duration.durationType,
			duration_quantity: duration.durationQuantity,
			created_at: new Date(),
			created_by_user_id: purchaserId,
			redeemed_at: null,
			redeemed_by_user_id: null,
			stripe_payment_intent_id: null,
			visionary_sequence_number: null,
			checkout_session_id: null,
			version: 1,
		});
		const purchaser = await this.deps.userRepository.findUnique(purchaserId);
		if (!purchaser) {
			return;
		}
		await this.deps.entitlementWriter.patchUserAndDispatch(purchaser, {
			gift_inventory_server_seq: (purchaser.giftInventoryServerSeq ?? 0) + 1,
			...(purchaser.hasEverPurchased ? {} : {has_ever_purchased: true}),
		});
		Logger.info({purchaserId: purchaserId.toString(), slot}, 'Minted a gift code from a store purchase');
	}

	private async reverseStoreGift(code: string, row: StorePurchaseRow): Promise<void> {
		const gift = await this.deps.userRepository.findGiftCode(code);
		if (!gift || gift.revokedAt) {
			return;
		}
		const redeemerId = gift.redeemedByUserId;
		if (redeemerId !== null) {
			await this.reverseRedeemedStoreGift(gift, redeemerId);
		}
		await this.deps.userRepository.revokeGiftCode(code);
		Logger.info(
			{storeKey: redactStoreKey(row.store_key), redeemerId: redeemerId?.toString() ?? null},
			'Revoked a gift code after a store refund',
		);
	}

	private async reverseRedeemedStoreGift(gift: GiftCode, redeemerId: UserID): Promise<void> {
		const redeemerRows = await this.deps.repository.listPurchasesForUser(redeemerId);
		if (!selectActiveStoreSubscription(redeemerRows, new Date())) {
			await this.deps.giftReversalHandler.handleGiftPremiumReversal(gift, {reason: GIFT_REFUND_REVERSAL_REASON});
			return;
		}
		const redeemer = await this.deps.userRepository.findUnique(redeemerId);
		if (!redeemer) {
			return;
		}
		await this.deps.giftReversalHandler.reverseStackedGift(redeemer, gift);
		await this.deps.entitlementWriter.applyEntitlement(redeemerId, []);
	}

	private async restoreStoreGift(gift: GiftCode, row: StorePurchaseRow): Promise<void> {
		const redeemerId = gift.redeemedByUserId;
		if (redeemerId !== null) {
			await this.deps.giftReversalHandler.restoreReversedGift(gift);
			await this.deps.entitlementWriter.applyEntitlement(redeemerId, []);
		}
		await this.deps.userRepository.unrevokeGiftCode(gift.code);
		Logger.info(
			{storeKey: redactStoreKey(row.store_key), redeemerId: redeemerId?.toString() ?? null},
			'Reinstated a gift code after a store refund was reversed',
		);
	}

	private async generateUniqueGiftCode(): Promise<string> {
		for (;;) {
			const code = RandomUtils.randomString(32);
			if (!(await this.deps.userRepository.findGiftCode(code))) {
				return code;
			}
		}
	}
}
