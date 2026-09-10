// SPDX-License-Identifier: AGPL-3.0-or-later

import {DeletionReasons} from '@fluxer/constants/src/Core';
import {PremiumFlags, UserFlags} from '@fluxer/constants/src/UserConstants';
import {StripeError} from '@fluxer/errors/src/domains/payment/StripeError';
import type {IEmailService} from '@pkgs/email/src/IEmailService';
import type Stripe from 'stripe';
import type {UserRow} from '../../database/types/UserTypes';
import type {IDonationRepository} from '../../donation/IDonationRepository';
import type {IGatewayService} from '../../infrastructure/IGatewayService';
import type {KVAccountDeletionQueueService} from '../../infrastructure/KVAccountDeletionQueueService';
import type {UserCacheService} from '../../infrastructure/UserCacheService';
import {Logger} from '../../Logger';
import {getBillingRepository} from '../../middleware/ServiceRegistry';
import type {GiftCode} from '../../models/GiftCode';
import type {User} from '../../models/User';
import type {IUserRepository} from '../../user/IUserRepository';
import {clearPendingDeletion} from '../../user/services/PendingDeletionCoordinator';
import {mapUserToPrivateResponse} from '../../user/UserMappers';
import {extractId} from '../StripeUtils';
import type {StripeGiftReversalHandler} from './StripeGiftReversalHandler';
import type {StripePaymentFraudService} from './StripePaymentFraudService';

export const REFUND_ALLOWANCE_CLAIM_PREFIX = 'refund-allowance';

export class StripeDisputeWebhookHandler {
	constructor(
		private userRepository: IUserRepository,
		private userCacheService: UserCacheService,
		private emailService: IEmailService,
		private gatewayService: IGatewayService,
		private donationRepository: IDonationRepository,
		private kvDeletionQueue: KVAccountDeletionQueueService,
		private giftReversalHandler: StripeGiftReversalHandler,
		private paymentFraudService: StripePaymentFraudService,
	) {}

	async handleChargebackCreated(dispute: Stripe.Dispute): Promise<void> {
		await this.paymentFraudService.handleFraudulentDispute(dispute);
		const paymentIntentId = extractId(dispute.payment_intent);
		if (!paymentIntentId) {
			Logger.error({dispute}, 'Chargeback missing payment intent');
			throw new StripeError('Chargeback missing payment intent');
		}
		const giftCode = await this.userRepository.findGiftCodeByPaymentIntent(paymentIntentId);
		if (giftCode) {
			await this.handleGiftChargeback(giftCode, dispute);
			return;
		}
		const payment = await this.userRepository.getPaymentByPaymentIntent(paymentIntentId);
		if (!payment) {
			Logger.error({paymentIntentId}, 'No payment found for chargeback');
			throw new StripeError('No payment found for chargeback');
		}
		await this.paymentFraudService.enforceAccountFraudAction({
			userId: payment.userId,
			source: 'chargeback',
			signalId: dispute.id,
			chargeId: extractId(dispute.charge),
			paymentIntentId,
			customerId: null,
			fraudType: null,
		});
	}

	async handleChargebackClosed(dispute: Stripe.Dispute): Promise<void> {
		if (dispute.status !== 'won') {
			return;
		}
		const paymentIntentId = extractId(dispute.payment_intent);
		if (!paymentIntentId) {
			throw new StripeError('Chargeback withdrawal missing payment intent');
		}
		const payment = await this.userRepository.getPaymentByPaymentIntent(paymentIntentId);
		if (!payment) {
			throw new StripeError('No payment found for chargeback withdrawal');
		}
		const user = await this.userRepository.findUnique(payment.userId);
		if (!user) {
			throw new StripeError('User not found for chargeback withdrawal');
		}
		if (user.flags & UserFlags.DELETED && user.deletionReasonCode === DeletionReasons.BILLING_DISPUTE_OR_ABUSE) {
			await clearPendingDeletion({
				userId: payment.userId,
				pendingDeletionAt: user.pendingDeletionAt,
				userRepository: this.userRepository,
				deletionQueue: this.kvDeletionQueue,
			});
			const updatedUser = await this.userRepository.patchUpsert(
				payment.userId,
				{
					flags: user.flags & ~UserFlags.DELETED,
					pending_deletion_at: null,
					deletion_reason_code: null,
					deletion_public_reason: null,
					deletion_audit_log_reason: null,
					first_refund_at: user.firstRefundAt || new Date(),
				},
				user.toRow(),
			);
			await this.userCacheService.setUserPartialResponseFromUser(updatedUser);
			if (updatedUser.email) {
				await this.emailService.sendUnbanNotification(
					updatedUser.email,
					updatedUser.username,
					'chargeback withdrawal',
					updatedUser.locale,
				);
			}
			Logger.debug(
				{userId: payment.userId},
				'User unsuspended after chargeback withdrawal - 30 day self-serve refund cooldown applied',
			);
		}
	}

	async handleRefund(charge: Stripe.Charge): Promise<void> {
		const paymentIntentId = extractId(charge.payment_intent);
		if (!paymentIntentId) {
			Logger.error({chargeId: charge.id}, 'Refund missing payment intent');
			throw new StripeError('Refund missing payment intent');
		}
		const giftCode = await this.userRepository.findGiftCodeByPaymentIntent(paymentIntentId);
		if (giftCode) {
			if (giftCode.redeemedByUserId) {
				await this.giftReversalHandler.handleGiftPremiumReversal(giftCode, {
					reason: 'gift_refund',
					chargeId: charge.id,
				});
			} else if (!giftCode.revokedAt) {
				await this.userRepository.revokeGiftCode(giftCode.code);
				Logger.debug({giftCode: giftCode.code, chargeId: charge.id}, 'Revoked unredeemed gift code after refund');
			}
			return;
		}
		const payment = await this.userRepository.getPaymentByPaymentIntent(paymentIntentId);
		let user: User;
		if (payment) {
			const foundUser = await this.userRepository.findUnique(payment.userId);
			if (!foundUser) {
				Logger.error({userId: payment.userId, chargeId: charge.id}, 'User not found for refund');
				throw new StripeError('User not found for refund');
			}
			await this.userRepository.updatePayment({
				...payment.toRow(),
				status: 'refunded',
			});
			user = foundUser;
		} else {
			const customerId = extractId(charge.customer);
			if (!customerId) {
				Logger.error(
					{paymentIntentId, chargeId: charge.id},
					'No payment found for refund and charge has no customer ID',
				);
				throw new StripeError('No payment found for refund');
			}
			const donor = await this.donationRepository.findDonorByStripeCustomerId(customerId);
			if (donor) {
				Logger.info({customerId, chargeId: charge.id}, 'Refund for donation customer - no premium action required');
				return;
			}
			const foundUser = await this.userRepository.findByStripeCustomerId(customerId);
			if (!foundUser) {
				Logger.error({customerId, paymentIntentId, chargeId: charge.id}, 'No user found for refund by customer ID');
				throw new StripeError('No user found for refund');
			}
			Logger.debug(
				{userId: foundUser.id, paymentIntentId, chargeId: charge.id},
				'Processing refund via customer ID (payment intent not indexed)',
			);
			user = foundUser;
		}
		const claimKeys = await this.resolveRefundAllowanceClaimKeys(charge);
		if (claimKeys.length === 0) {
			Logger.debug(
				{userId: user.id, chargeId: charge.id, paymentIntentId},
				'Refund was issued by Fluxer - not counted against the user refund allowance',
			);
			return;
		}
		const claimedKeys: Array<string> = [];
		let alreadyCounted = false;
		for (const claimKey of claimKeys) {
			const claim = await getBillingRepository().webhookEvents.tryClaim(claimKey);
			if (claim === 'claimed') {
				claimedKeys.push(claimKey);
			} else {
				alreadyCounted = true;
			}
		}
		if (alreadyCounted) {
			for (const claimKey of claimedKeys) {
				await getBillingRepository().webhookEvents.markProcessed(claimKey);
			}
			Logger.debug(
				{userId: user.id, chargeId: charge.id, paymentIntentId, claimKeys},
				'Refund already counted against the user refund allowance',
			);
			return;
		}
		const isFirstRefund = !user.firstRefundAt;
		const patch: Partial<UserRow> = isFirstRefund
			? {first_refund_at: new Date()}
			: {premium_flags: user.premiumFlags | PremiumFlags.PURCHASE_DISABLED};
		let updatedUser: User;
		try {
			updatedUser = await this.userRepository.patchUpsert(user.id, patch, user.toRow());
		} catch (error) {
			for (const claimKey of claimedKeys) {
				await getBillingRepository().webhookEvents.releaseClaim(claimKey);
			}
			throw error;
		}
		for (const claimKey of claimedKeys) {
			await getBillingRepository().webhookEvents.markProcessed(claimKey);
		}
		await this.dispatchUser(updatedUser);
		Logger.debug(
			{userId: user.id, chargeId: charge.id, paymentIntentId},
			isFirstRefund
				? 'First refund recorded - 30 day self-serve refund cooldown applied'
				: 'Second refund recorded - permanent purchase block applied',
		);
	}

	private async resolveRefundAllowanceClaimKeys(charge: Stripe.Charge): Promise<Array<string>> {
		const chargeClaimKey = `${REFUND_ALLOWANCE_CLAIM_PREFIX}:${charge.id}`;
		const inlined = charge.refunds?.data ?? [];
		const refunds = (
			inlined.length > 0
				? inlined.map((refund) => ({
						id: refund.id,
						createdAtMs: refund.created * 1000,
						status: refund.status,
						rejectionReason: refund.metadata?.rejection_reason ?? null,
					}))
				: (await getBillingRepository().refunds.listByCharge(charge.id)).map((row) => ({
						id: row.provider_id,
						createdAtMs: row.stripe_created_at?.getTime() ?? 0,
						status: row.status,
						rejectionReason: row.metadata?.get('rejection_reason') ?? null,
					}))
		).filter((refund) => refund.status !== 'failed' && refund.status !== 'canceled');
		if (refunds.length === 0) {
			Logger.warn(
				{chargeId: charge.id},
				'No refund records found for refunded charge; counting the charge once against the user refund allowance',
			);
			return [chargeClaimKey];
		}
		const customerRefunds = refunds
			.filter((refund) => refund.rejectionReason === null)
			.sort((left, right) => left.createdAtMs - right.createdAtMs || left.id.localeCompare(right.id));
		const earliest = customerRefunds[0];
		const latest = customerRefunds[customerRefunds.length - 1];
		if (!earliest || !latest) {
			return [];
		}
		const latestClaimKey = `${REFUND_ALLOWANCE_CLAIM_PREFIX}:${latest.id}`;
		return latest.id === earliest.id ? [chargeClaimKey, latestClaimKey] : [latestClaimKey];
	}

	private async handleGiftChargeback(giftCode: GiftCode, dispute: Stripe.Dispute): Promise<void> {
		if (giftCode.redeemedByUserId) {
			await this.giftReversalHandler.handleGiftPremiumReversal(giftCode, {reason: 'gift_chargeback'});
			const redeemer = await this.userRepository.findUnique(giftCode.redeemedByUserId);
			if (redeemer?.email) {
				await this.emailService.sendGiftChargebackNotification(redeemer.email, redeemer.username, redeemer.locale);
			}
			Logger.debug(
				{giftCode: giftCode.code, redeemerId: giftCode.redeemedByUserId},
				'Premium revoked due to gift chargeback',
			);
		} else if (!giftCode.revokedAt) {
			await this.userRepository.revokeGiftCode(giftCode.code);
			Logger.debug(
				{giftCode: giftCode.code, chargeId: extractId(dispute.charge)},
				'Revoked unredeemed gift code after chargeback',
			);
		}
		await this.paymentFraudService.enforceAccountFraudAction({
			userId: giftCode.createdByUserId,
			source: 'chargeback',
			signalId: extractId(dispute.charge) ?? dispute.id,
			chargeId: extractId(dispute.charge),
			paymentIntentId: giftCode.stripePaymentIntentId,
			customerId: null,
			fraudType: null,
		});
	}

	private async dispatchUser(user: User): Promise<void> {
		await this.gatewayService.dispatchPresence({
			userId: user.id,
			event: 'USER_UPDATE',
			data: mapUserToPrivateResponse(user),
		});
	}
}
