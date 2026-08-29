// SPDX-License-Identifier: AGPL-3.0-or-later

import {UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import {CannotRedeemPlutoniumWithVisionaryError} from '@fluxer/errors/src/domains/payment/CannotRedeemPlutoniumWithVisionaryError';
import {GiftCodeAlreadyRedeemedError} from '@fluxer/errors/src/domains/payment/GiftCodeAlreadyRedeemedError';
import {NoActiveSubscriptionError} from '@fluxer/errors/src/domains/payment/NoActiveSubscriptionError';
import {StripeError} from '@fluxer/errors/src/domains/payment/StripeError';
import {StripeGiftRedemptionInProgressError} from '@fluxer/errors/src/domains/payment/StripeGiftRedemptionInProgressError';
import {UnknownGiftCodeError} from '@fluxer/errors/src/domains/payment/UnknownGiftCodeError';
import {UnknownUserError} from '@fluxer/errors/src/domains/user/UnknownUserError';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import {seconds} from 'itty-time';
import type Stripe from 'stripe';
import type {UserID} from '../../BrandedTypes';
import {createUserID} from '../../BrandedTypes';
import type {UserRow} from '../../database/types/UserTypes';
import type {IGatewayService} from '../../infrastructure/IGatewayService';
import {Logger} from '../../Logger';
import {getBillingRepository} from '../../middleware/ServiceRegistry';
import {type GiftCode, mapGiftDurationMonthsToFields} from '../../models/GiftCode';
import type {User} from '../../models/User';
import type {IUserRepository} from '../../user/IUserRepository';
import {mapUserToPrivateResponse} from '../../user/UserMappers';
import * as RandomUtils from '../../utils/RandomUtils';
import type {ProductInfo} from '../ProductRegistry';
import type {StripeCheckoutService} from './StripeCheckoutService';
import type {StripePremiumService} from './StripePremiumService';
import type {StripeSubscriptionService} from './StripeSubscriptionService';

export class StripeGiftService {
	constructor(
		private stripe: Stripe | null,
		private userRepository: IUserRepository,
		private cacheService: ICacheService,
		private gatewayService: IGatewayService,
		private checkoutService: StripeCheckoutService,
		private premiumService: StripePremiumService,
		private subscriptionService: StripeSubscriptionService,
	) {}

	async getGiftCode(code: string): Promise<GiftCode> {
		const giftCode = await this.userRepository.findGiftCode(code);
		if (!giftCode || giftCode.revokedAt) {
			throw new UnknownGiftCodeError();
		}
		return giftCode;
	}

	async redeemGiftCode(userId: UserID, code: string): Promise<void> {
		const lockKey = `gift_redeem_lock:${code}`;
		const appliedKey = `gift_redeem_applied:${code}`;
		Logger.debug({userId, giftCode: code, lockKey, appliedKey}, 'Starting gift code redemption');
		const hasAppliedSentinel = await this.cacheService.get<boolean>(appliedKey);
		if (hasAppliedSentinel) {
			Logger.debug({userId, giftCode: code, appliedKey}, 'Gift redemption blocked by applied sentinel');
			throw new GiftCodeAlreadyRedeemedError();
		}
		const lockToken = await this.cacheService.acquireLock(lockKey, seconds('2 minutes'));
		if (!lockToken) {
			Logger.debug({userId, giftCode: code, lockKey}, 'Gift redemption blocked by concurrent redemption lock');
			throw new StripeGiftRedemptionInProgressError();
		}
		try {
			const giftCode = await this.userRepository.findGiftCode(code);
			if (!giftCode || giftCode.revokedAt) {
				Logger.debug({userId, giftCode: code}, 'Gift code not found during redemption');
				throw new UnknownGiftCodeError();
			}
			Logger.debug(
				{
					userId,
					giftCode: code,
					durationType: giftCode.durationType,
					durationQuantity: giftCode.durationQuantity,
					redeemedByUserId: giftCode.redeemedByUserId,
				},
				'Loaded gift code for redemption',
			);
			if (giftCode.redeemedByUserId) {
				await this.cacheService.set(appliedKey, true, seconds('365 days'));
				Logger.debug(
					{
						userId,
						giftCode: code,
						appliedKey,
						redeemedByUserId: giftCode.redeemedByUserId,
					},
					'Gift code already redeemed in database; applied sentinel written',
				);
				throw new GiftCodeAlreadyRedeemedError();
			}
			const redeemedGiftCacheKey = `redeemed_gift_codes:${code}`;
			if (await this.cacheService.get<boolean>(redeemedGiftCacheKey)) {
				await this.cacheService.set(appliedKey, true, seconds('365 days'));
				Logger.debug(
					{userId, giftCode: code, redeemedGiftCacheKey, appliedKey},
					'Gift code already redeemed in cache; applied sentinel written',
				);
				throw new GiftCodeAlreadyRedeemedError();
			}
			const user = await this.userRepository.findUnique(userId);
			if (!user) {
				Logger.error({userId, giftCode: code}, 'Redeemer user not found during gift redemption');
				throw new UnknownUserError();
			}
			Logger.debug(
				{
					userId,
					giftCode: code,
					premiumType: user.premiumType,
					premiumUntil: user.premiumUntil,
					stripeSubscriptionId: user.stripeSubscriptionId,
					stripeCustomerId: user.stripeCustomerId,
					premiumBillingCycle: user.premiumBillingCycle,
				},
				'Loaded redeemer state for gift redemption',
			);
			this.checkoutService.validateUserCanPurchase(user);
			Logger.debug({userId, giftCode: code}, 'Redeemer passed gift purchase validation');
			if (user.premiumType === UserPremiumTypes.LIFETIME) {
				Logger.debug({userId, giftCode: code}, 'Rejecting redemption for lifetime user');
				throw new CannotRedeemPlutoniumWithVisionaryError();
			}
			await this.userRepository.redeemGiftCode(code, userId);
			Logger.debug({userId, giftCode: code}, 'Applied gift redemption row update');
			try {
				const premiumType = giftCode.durationQuantity === 0 ? UserPremiumTypes.LIFETIME : UserPremiumTypes.SUBSCRIPTION;
				Logger.debug({userId, giftCode: code, premiumType}, 'Computed premium type from gift duration');
				if (premiumType === UserPremiumTypes.LIFETIME && user.stripeSubscriptionId && this.stripe) {
					Logger.debug(
						{
							userId,
							giftCode: code,
							stripeSubscriptionId: user.stripeSubscriptionId,
						},
						'Cancelling active Stripe subscription before lifetime grant',
					);
					await this.cancelStripeSubscriptionImmediately(user);
				}
				let stackedOntoStripeSubscription = false;
				if (premiumType === UserPremiumTypes.SUBSCRIPTION) {
					stackedOntoStripeSubscription = await this.tryStackSubscriptionGiftOntoStripeSubscription(
						user,
						giftCode,
						code,
					);
				}
				Logger.debug(
					{
						userId,
						giftCode: code,
						stackedOntoStripeSubscription,
					},
					'Gift redemption stacking decision completed',
				);
				if (stackedOntoStripeSubscription) {
					Logger.debug(
						{
							userId,
							giftCode: code,
							durationType: giftCode.durationType,
							durationQuantity: giftCode.durationQuantity,
						},
						'Recording gift extension in gift bucket alongside Stripe trial stacking',
					);
					await this.premiumService.extendPremiumByGift(
						userId,
						premiumType,
						giftCode.durationType,
						giftCode.durationQuantity,
						true,
					);
				} else if (premiumType === UserPremiumTypes.LIFETIME && giftCode.visionarySequenceNumber != null) {
					const GIFT_CODE_SENTINEL_USER_ID = createUserID(-1n);
					Logger.debug(
						{
							userId,
							giftCode: code,
							visionarySequenceNumber: giftCode.visionarySequenceNumber,
						},
						'Applying lifetime gift through visionary-slot transfer flow',
					);
					await this.userRepository.unreserveVisionarySlot(
						giftCode.visionarySequenceNumber,
						GIFT_CODE_SENTINEL_USER_ID,
					);
					await this.premiumService.grantPremiumFromGiftWithDuration(
						userId,
						premiumType,
						giftCode.durationType,
						giftCode.durationQuantity,
						giftCode.visionarySequenceNumber,
					);
					await this.userRepository.reserveVisionarySlot(giftCode.visionarySequenceNumber, userId);
				} else {
					Logger.debug(
						{
							userId,
							giftCode: code,
							durationType: giftCode.durationType,
							durationQuantity: giftCode.durationQuantity,
							premiumType,
						},
						'Applying gift premium via user premium field grant',
					);
					await this.premiumService.extendPremiumByGift(
						userId,
						premiumType,
						giftCode.durationType,
						giftCode.durationQuantity,
						false,
					);
				}
				Logger.debug({userId, giftCode: code}, 'Completed entitlement grant path for redeemed gift');
			} catch (entitlementError: unknown) {
				try {
					await this.userRepository.unredeemGiftCode(code, userId);
					Logger.warn(
						{userId, giftCode: code, error: entitlementError},
						'Rolled back gift code redemption after entitlement grant failure',
					);
				} catch (rollbackError: unknown) {
					Logger.error(
						{userId, giftCode: code, entitlementError, rollbackError},
						'Failed to roll back gift code redemption after entitlement grant failure',
					);
				}
				throw entitlementError;
			}
			await this.cacheService.set(`redeemed_gift_codes:${code}`, true, seconds('5 minutes'));
			await this.cacheService.set(appliedKey, true, seconds('365 days'));
			Logger.debug(
				{
					userId,
					giftCode: code,
					redeemedGiftCacheKey: `redeemed_gift_codes:${code}`,
					appliedKey,
				},
				'Gift redemption cache sentinels written',
			);
			Logger.debug(
				{
					userId,
					giftCode: code,
					durationType: giftCode.durationType,
					durationQuantity: giftCode.durationQuantity,
				},
				'Gift code redeemed',
			);
		} finally {
			await this.cacheService.releaseLock(lockKey, lockToken);
			Logger.debug({userId, giftCode: code, lockKey}, 'Gift redemption lock released');
		}
	}

	async getUserGifts(userId: UserID): Promise<Array<GiftCode>> {
		const gifts = await this.userRepository.findGiftCodesByCreator(userId);
		const redeemedGracePeriodMs = 7 * 24 * 60 * 60 * 1000;
		const cutoff = Date.now() - redeemedGracePeriodMs;
		return gifts
			.filter((gift) => gift.revokedAt === null)
			.filter((gift) => gift.redeemedAt === null || gift.redeemedAt.getTime() > cutoff)
			.sort((a, b) => b.createdAt.getTime() - a.createdAt.getTime());
	}

	async prepareGiftCode(
		checkoutSessionId: string,
		purchaser: User,
		productInfo: ProductInfo,
		paymentIntentId: string | null,
	): Promise<string> {
		const payment = await this.userRepository.getPaymentByCheckoutSession(checkoutSessionId);
		if (!payment) {
			Logger.error({checkoutSessionId}, 'Payment not found for gift code creation');
			throw new StripeError('Payment not found for gift code creation');
		}
		if (payment.giftCode) {
			Logger.debug({checkoutSessionId, code: payment.giftCode}, 'Gift code already exists for checkout session');
			return payment.giftCode;
		}
		if (paymentIntentId) {
			const existingGift = await this.userRepository.findGiftCodeByPaymentIntent(paymentIntentId);
			if (existingGift) {
				await this.userRepository.linkGiftCodeToCheckoutSession(existingGift.code, checkoutSessionId);
				Logger.warn(
					{checkoutSessionId, paymentIntentId, code: existingGift.code},
					'Reused existing gift code for checkout session',
				);
				return existingGift.code;
			}
		}
		const lockKey = `gift_prepare_lock:${checkoutSessionId}`;
		const lockToken = await this.cacheService.acquireLock(lockKey, seconds('30 seconds'));
		if (!lockToken) {
			const freshPayment = await this.userRepository.getPaymentByCheckoutSession(checkoutSessionId);
			if (freshPayment?.giftCode) {
				return freshPayment.giftCode;
			}
			throw new StripeError('Failed to acquire gift code preparation lock');
		}
		try {
			const freshPayment = await this.userRepository.getPaymentByCheckoutSession(checkoutSessionId);
			if (freshPayment?.giftCode) {
				return freshPayment.giftCode;
			}
			const code = await this.generateUniqueGiftCode();
			const duration = mapGiftDurationMonthsToFields(productInfo.durationMonths);
			await this.userRepository.createGiftCode({
				code,
				duration_months: null,
				duration_type: duration.durationType,
				duration_quantity: duration.durationQuantity,
				created_at: new Date(),
				created_by_user_id: purchaser.id,
				redeemed_at: null,
				redeemed_by_user_id: null,
				stripe_payment_intent_id: paymentIntentId,
				visionary_sequence_number: null,
				checkout_session_id: checkoutSessionId,
				version: 1,
			});
			await this.userRepository.linkGiftCodeToCheckoutSession(code, checkoutSessionId);
			Logger.debug(
				{code, purchaserId: purchaser.id, durationMonths: productInfo.durationMonths, productType: productInfo.type},
				'Gift code prepared',
			);
			return code;
		} finally {
			try {
				await this.cacheService.releaseLock(lockKey, lockToken);
			} catch (error) {
				Logger.error({error, checkoutSessionId, lockKey}, 'Failed to release gift code preparation lock');
			}
		}
	}

	async finaliseGiftCode(purchaserId: UserID): Promise<void> {
		const currentUser = await this.userRepository.findUnique(purchaserId);
		if (!currentUser) {
			Logger.error({userId: purchaserId}, 'Purchaser not found for gift finalisation');
			return;
		}
		const updatedUser = await this.userRepository.patchUpsert(
			purchaserId,
			{
				gift_inventory_server_seq: (currentUser.giftInventoryServerSeq ?? 0) + 1,
			},
			currentUser.toRow(),
		);
		await this.dispatchUser(updatedUser);
	}

	private async generateUniqueGiftCode(): Promise<string> {
		let code: string;
		let exists = true;
		while (exists) {
			code = RandomUtils.randomString(32);
			const existing = await this.userRepository.findGiftCode(code);
			exists = !!existing;
		}
		return code!;
	}

	private async tryStackSubscriptionGiftOntoStripeSubscription(
		user: User,
		giftCode: GiftCode,
		code: string,
	): Promise<boolean> {
		const hasCurrentSubscriptionState = this.hasCurrentSubscriptionState(user);
		if (!hasCurrentSubscriptionState || !user.stripeSubscriptionId || !this.stripe) {
			Logger.debug(
				{
					userId: user.id,
					giftCode: code,
					hasCurrentSubscriptionState,
					hasStripeSubscriptionId: Boolean(user.stripeSubscriptionId),
					hasStripeClient: Boolean(this.stripe),
				},
				'Skipping Stripe subscription stacking for gift redemption',
			);
			return false;
		}
		Logger.debug(
			{
				userId: user.id,
				giftCode: code,
				stripeSubscriptionId: user.stripeSubscriptionId,
				durationType: giftCode.durationType,
				durationQuantity: giftCode.durationQuantity,
			},
			'Attempting to stack gift duration onto active Stripe subscription',
		);
		try {
			await this.subscriptionService.extendSubscriptionWithGiftTrialDuration(
				user,
				giftCode.durationType,
				giftCode.durationQuantity,
				code,
			);
			Logger.debug({userId: user.id, giftCode: code}, 'Stacked gift duration onto Stripe subscription');
			return true;
		} catch (error: unknown) {
			if (!this.shouldFallbackToPremiumFieldGrant(error)) {
				Logger.error(
					{
						userId: user.id,
						giftCode: code,
						error,
					},
					'Stripe stacking failed and is not eligible for fallback',
				);
				throw error;
			}
			Logger.warn(
				{
					userId: user.id,
					giftCode: code,
					error,
				},
				'Stripe stacking failed; falling back to premium field grant',
			);
			await this.clearStripeSubscriptionIdentity(user);
			Logger.warn(
				{
					userId: user.id,
					giftCode: code,
					stripeSubscriptionId: user.stripeSubscriptionId,
				},
				'Falling back to premium-field gift grant after missing or inactive Stripe subscription',
			);
			return false;
		}
	}

	private hasCurrentSubscriptionState(user: User): boolean {
		if (user.premiumType !== UserPremiumTypes.SUBSCRIPTION) {
			return false;
		}
		return user.premiumUntil === null || user.premiumUntil.getTime() > Date.now();
	}

	private shouldFallbackToPremiumFieldGrant(error: unknown): boolean {
		if (error instanceof NoActiveSubscriptionError) {
			return true;
		}
		if (!(error instanceof StripeError)) {
			return false;
		}
		const detail = error.messageVariables?.detail;
		if (typeof detail !== 'string') {
			return false;
		}
		const normalisedDetail = detail.toLowerCase();
		return (
			normalisedDetail.includes('no such subscription') ||
			normalisedDetail.includes('no active subscription') ||
			normalisedDetail.includes('cannot update a canceled subscription') ||
			normalisedDetail.includes('cannot update a cancelled subscription') ||
			normalisedDetail.includes('canceled subscription') ||
			normalisedDetail.includes('cancelled subscription') ||
			normalisedDetail.includes('has no trial_end or current_period_end')
		);
	}

	private async clearStripeSubscriptionIdentity(user: User): Promise<void> {
		const patch: Partial<UserRow> = {};
		if (user.stripeSubscriptionId !== null) {
			patch.stripe_subscription_id = null;
		}
		if (user.premiumBillingCycle !== null) {
			patch.premium_billing_cycle = null;
		}
		if (user.premiumWillCancel) {
			patch.premium_will_cancel = false;
		}
		if (Object.keys(patch).length === 0) {
			Logger.debug({userId: user.id}, 'Stripe identity already clear; no patch required');
			return;
		}
		Logger.debug({userId: user.id, patch}, 'Clearing stale Stripe identity before premium field fallback');
		const updatedUser = await this.userRepository.patchUpsert(user.id, patch, user.toRow());
		await this.dispatchUser(updatedUser);
	}

	private async cancelStripeSubscriptionImmediately(user: User): Promise<void> {
		if (!this.stripe) {
			throw new StripeError('Stripe client not available for immediate cancellation');
		}
		if (!user.stripeSubscriptionId) {
			throw new StripeError('User missing subscription id for immediate cancellation');
		}
		const canceledSubscription = await this.stripe.subscriptions.cancel(user.stripeSubscriptionId, {
			invoice_now: false,
			prorate: false,
		});
		try {
			await getBillingRepository().subscriptions.upsertFromStripe(canceledSubscription, {
				knownUserId: user.id,
				snapshotCapturedAt: new Date(),
			});
		} catch (mirrorErr) {
			Logger.error(
				{mirrorErr, subId: canceledSubscription.id},
				'Mirror upsert failed after Stripe write; reconciler will heal',
			);
		}
		const updatedUser = await this.userRepository.patchUpsert(
			user.id,
			{
				stripe_subscription_id: null,
				premium_billing_cycle: null,
				premium_will_cancel: false,
			},
			user.toRow(),
		);
		await this.dispatchUser(updatedUser);
		Logger.debug({userId: user.id}, 'Canceled active subscription due to lifetime grant');
	}

	private async dispatchUser(user: User): Promise<void> {
		await this.gatewayService.dispatchPresence({
			userId: user.id,
			event: 'USER_UPDATE',
			data: mapUserToPrivateResponse(user),
		});
	}
}
