// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import {getWithdrawalPeriodEndsAt} from '@app/api/billing/WithdrawalPeriod';
import {SYSTEM_USER_ID} from '@app/api/constants/Core';
import type {BillingChargeRow} from '@app/api/database/types/BillingTypes';
import type {OperatorPassPurchaseRow} from '@app/api/database/types/OperatorPassTypes';
import type {PaymentDisputeReviewRow} from '@app/api/database/types/PaymentDisputeTypes';
import type {DonationRepository} from '@app/api/donation/DonationRepository';
import type {EnforcementDecisionService} from '@app/api/enforcement/EnforcementDecisionService';
import type {KVAccountDeletionQueueService} from '@app/api/infrastructure/KVAccountDeletionQueueService';
import type {UserCacheService} from '@app/api/infrastructure/UserCacheService';
import {Logger} from '@app/api/Logger';
import {getBillingRepository} from '@app/api/middleware/ServiceRegistry';
import type {GiftCode} from '@app/api/models/GiftCode';
import type {Payment} from '@app/api/models/Payment';
import type {OperatorPassFulfilmentService} from '@app/api/operator_pass/OperatorPassFulfilmentService';
import {followOperatorPassWallStanding} from '@app/api/operator_pass/OperatorPassWallStanding';
import {
	type PaymentDisputeEnforcementService,
	shouldCloseAfterPaymentDispute,
} from '@app/api/payment_disputes/PaymentDisputeEnforcement';
import {
	getOperatorPassDisputeSubject,
	type PaymentDisputeReviewService,
	type PaymentDisputeSubject,
} from '@app/api/payment_disputes/PaymentDisputeReviews';
import {getBillingBranding} from '@app/api/stripe/BillingBranding';
import {extractId} from '@app/api/stripe/StripeUtils';
import type {StripeGiftReversalHandler} from '@app/api/stripe/services/StripeGiftReversalHandler';
import type {StripePaymentFraudService} from '@app/api/stripe/services/StripePaymentFraudService';
import type {StripeSubscriptionService} from '@app/api/stripe/services/StripeSubscriptionService';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {clearPendingDeletion} from '@app/api/user/services/PendingDeletionCoordinator';
import {DeletionReasons} from '@fluxer/constants/src/Core';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {StripeError} from '@fluxer/errors/src/domains/payment/StripeError';
import type {BillingRecurringPlan, IEmailService} from '@pkgs/email/src/IEmailService';
import type Stripe from 'stripe';

type PlutoniumDisputeTarget =
	| {kind: 'gift'; purchaserId: UserID; giftCode: GiftCode; payment: Payment | null}
	| {kind: 'purchase'; purchaserId: UserID; payment: Payment | null};

export class StripeDisputeWebhookHandler {
	constructor(
		private userRepository: UserRepository,
		private userCacheService: UserCacheService,
		private emailService: IEmailService,
		private donationRepository: DonationRepository,
		private kvDeletionQueue: KVAccountDeletionQueueService,
		private giftReversalHandler: StripeGiftReversalHandler,
		private paymentFraudService: StripePaymentFraudService,
		private enforcement: EnforcementDecisionService,
		private subscriptionService: Pick<
			StripeSubscriptionService,
			'cancelSubscriptionAtPeriodEnd' | 'cancelSubscriptionImmediately'
		>,
		private disputeReviews: PaymentDisputeReviewService,
		private disputeEnforcement: PaymentDisputeEnforcementService,
		private operatorPassFulfilment: OperatorPassFulfilmentService | null = null,
	) {}

	async handleChargebackCreated(dispute: Stripe.Dispute): Promise<void> {
		const paymentIntentId = extractId(dispute.payment_intent);
		const operatorPassPurchase = paymentIntentId ? await this.findOperatorPassPurchase(paymentIntentId) : null;
		await this.paymentFraudService.handleFraudulentDispute(dispute);
		if (!paymentIntentId) {
			Logger.error({dispute}, 'Chargeback missing payment intent');
			throw new StripeError('Chargeback missing payment intent');
		}
		if (operatorPassPurchase && this.operatorPassFulfilment) {
			await this.operatorPassFulfilment.handleDisputeCreated(operatorPassPurchase, dispute);
			const current = (await this.findOperatorPassPurchase(paymentIntentId)) ?? operatorPassPurchase;
			await this.disputeReviews.openReview(await this.buildOperatorPassSubject(current, dispute), dispute);
			return;
		}
		const target = await this.resolvePlutoniumTarget(dispute, paymentIntentId);
		if (!target) {
			return;
		}
		await this.disputeReviews.openReview(await this.buildPlutoniumSubject(target, dispute, paymentIntentId), dispute);
		Logger.info(
			{disputeId: dispute.id, paymentIntentId, purchaserId: target.purchaserId, kind: target.kind},
			'Plutonium payment dispute opened; purchases are blocked until it closes',
		);
	}

	async handleChargebackClosed(dispute: Stripe.Dispute): Promise<void> {
		const paymentIntentId = extractId(dispute.payment_intent);
		if (!paymentIntentId) {
			throw new StripeError('Chargeback withdrawal missing payment intent');
		}
		const operatorPassPurchase = await this.findOperatorPassPurchase(paymentIntentId);
		if (operatorPassPurchase && this.operatorPassFulfilment) {
			if (dispute.status === 'lost') {
				await this.operatorPassFulfilment.handleDisputeLost(operatorPassPurchase, dispute);
			} else {
				await this.operatorPassFulfilment.handleDisputeReleased(operatorPassPurchase, dispute);
			}
			const current = (await this.findOperatorPassPurchase(paymentIntentId)) ?? operatorPassPurchase;
			const review = await this.disputeReviews.closeReview(
				await this.buildOperatorPassSubject(current, dispute),
				dispute,
			);
			await this.enforceAfterClose(review);
			return;
		}
		const target = await this.resolvePlutoniumTarget(dispute, paymentIntentId);
		if (!target) {
			return;
		}
		if (dispute.status === 'lost') {
			await this.endWhatTheDisputedPaymentBought(target, dispute);
		} else {
			await this.cancelAutomaticDisputeDeletion(target.purchaserId);
			if (target.payment) {
				await this.enforcement.reverseAutomatedPurchaseLimits(
					target.purchaserId,
					target.payment.createdAt,
					`Stripe dispute ${dispute.id} was won`,
				);
			}
			Logger.info(
				{disputeId: dispute.id, paymentIntentId, status: dispute.status},
				'Plutonium payment dispute closed without the money being returned',
			);
		}
		const review = await this.disputeReviews.closeReview(
			await this.buildPlutoniumSubject(target, dispute, paymentIntentId),
			dispute,
		);
		await this.enforceAfterClose(review);
	}

	async handleDisputeUpdated(dispute: Stripe.Dispute): Promise<void> {
		await this.disputeReviews.refreshReview(dispute);
	}

	private async enforceAfterClose(review: PaymentDisputeReviewRow): Promise<void> {
		if (!shouldCloseAfterPaymentDispute(review)) {
			return;
		}
		const user = await this.userRepository.findUnique(review.purchaser_id);
		if (user?.stripeSubscriptionId && !user.premiumWillCancel) {
			try {
				await this.subscriptionService.cancelSubscriptionAtPeriodEnd(user.id, 'payment_dispute');
			} catch (error) {
				Logger.error(
					{error, disputeId: review.dispute_id, userId: user.id},
					'Failed to stop the subscription of an account closed after a dispute',
				);
			}
		}
		await this.disputeEnforcement.closeAccount(review);
	}

	private async buildOperatorPassSubject(
		purchase: OperatorPassPurchaseRow,
		dispute: Stripe.Dispute,
	): Promise<PaymentDisputeSubject> {
		const holderRemovals = this.operatorPassFulfilment
			? await this.operatorPassFulfilment.repository.listRemovalHistory(purchase.purchaser_id)
			: [];
		return getOperatorPassDisputeSubject(purchase, dispute, holderRemovals);
	}

	private async resolvePlutoniumTarget(
		dispute: Stripe.Dispute,
		paymentIntentId: string,
	): Promise<PlutoniumDisputeTarget | null> {
		const payment = await this.userRepository.getPaymentByPaymentIntent(paymentIntentId);
		const giftCode =
			(await this.userRepository.findGiftCodeByPaymentIntent(paymentIntentId)) ??
			(payment?.giftCode ? await this.userRepository.findGiftCode(payment.giftCode) : null);
		if (giftCode) {
			return {kind: 'gift', purchaserId: giftCode.createdByUserId, giftCode, payment};
		}
		if (payment) {
			return {kind: 'purchase', purchaserId: payment.userId, payment};
		}
		const customerId = await this.resolveDisputeCustomerId(dispute);
		if (customerId) {
			const donor = await this.donationRepository.findDonorByStripeCustomerId(customerId);
			if (donor) {
				Logger.info(
					{paymentIntentId, disputeId: dispute.id, email: donor.email},
					'Dispute on a donation payment - no account action',
				);
				return null;
			}
			const user = await this.userRepository.findByStripeCustomerId(customerId);
			if (user) {
				return {kind: 'purchase', purchaserId: user.id, payment: null};
			}
		}
		Logger.error({paymentIntentId, disputeId: dispute.id}, 'No payment found for chargeback');
		throw new StripeError('No payment found for chargeback');
	}

	private async findPlutoniumInvoiceNumber(
		invoiceId: string | null,
		charge: BillingChargeRow | null,
	): Promise<string | null> {
		const invoice = invoiceId ? await getBillingRepository().invoices.findById(invoiceId) : null;
		return invoice?.number ?? charge?.receipt_number ?? null;
	}

	private async buildPlutoniumSubject(
		target: PlutoniumDisputeTarget,
		dispute: Stripe.Dispute,
		paymentIntentId: string,
	): Promise<PaymentDisputeSubject> {
		const chargeId = extractId(dispute.charge);
		const charge = chargeId ? await getBillingRepository().charges.findById(chargeId) : null;
		const payment = target.payment;
		const purchasedAt =
			charge?.stripe_created_at ??
			payment?.completedAt ??
			payment?.createdAt ??
			(target.kind === 'gift' ? target.giftCode.createdAt : null);
		const disputedAt = new Date(dispute.created * 1000);
		return {
			product: 'plutonium',
			orderId:
				payment?.checkoutSessionId ??
				(target.kind === 'gift' ? target.giftCode.checkoutSessionId : null) ??
				payment?.invoiceId ??
				paymentIntentId,
			invoiceNumber: await this.findPlutoniumInvoiceNumber(charge?.invoice_id ?? payment?.invoiceId ?? null, charge),
			purchaserId: target.purchaserId,
			isGift: target.kind === 'gift' || payment?.isGift === true,
			paymentIntentId,
			amountMinor: dispute.amount ?? payment?.amountCents ?? 0,
			currency: dispute.currency ?? payment?.currency ?? 'usd',
			disputedAt,
			purchasedAt,
			perksStartedAt: this.plutoniumProvidedAt(target, purchasedAt),
			withdrawalEndsAt: purchasedAt ? getWithdrawalPeriodEndsAt(purchasedAt) : null,
			withdrawnAt: null,
			refundedAt: await this.findFirstRefundAt(paymentIntentId, payment, disputedAt),
			perksRemovedAt: null,
		};
	}

	private plutoniumProvidedAt(target: PlutoniumDisputeTarget, purchasedAt: Date | null): Date | null {
		if (target.kind === 'gift') {
			return target.giftCode.createdAt;
		}
		return target.payment?.isGift ? null : purchasedAt;
	}

	private async findFirstRefundAt(
		paymentIntentId: string,
		payment: Payment | null,
		disputedAt: Date,
	): Promise<Date | null> {
		const refunds = await getBillingRepository().refunds.listByPaymentIntent(paymentIntentId);
		const times = refunds
			.filter((refund) => refund.status === 'succeeded' || refund.status === 'pending')
			.map((refund) => refund.stripe_created_at?.getTime() ?? disputedAt.getTime());
		if (times.length > 0) {
			return new Date(Math.min(...times));
		}
		return payment?.status === 'refunded' ? disputedAt : null;
	}

	private async endWhatTheDisputedPaymentBought(
		target: PlutoniumDisputeTarget,
		dispute: Stripe.Dispute,
	): Promise<void> {
		if (target.kind === 'gift') {
			await this.takeBackGift(target.giftCode, dispute);
			return;
		}
		const payment = target.payment;
		if (payment && !payment.subscriptionId && !payment.productType?.includes('subscription')) {
			return;
		}
		const user = await this.userRepository.findUnique(target.purchaserId);
		if (!user?.stripeSubscriptionId) {
			return;
		}
		if (payment?.subscriptionId && payment.subscriptionId !== user.stripeSubscriptionId) {
			return;
		}
		await this.subscriptionService.cancelSubscriptionImmediately(
			user.id,
			'payment_dispute_lost',
			user.stripeSubscriptionId,
		);
		Logger.info(
			{disputeId: dispute.id, userId: user.id, subscriptionId: user.stripeSubscriptionId},
			'Plutonium ended right away after a dispute returned the money',
		);
	}

	private async sendGiftRemoved(redeemerId: UserID, plan: BillingRecurringPlan | null): Promise<void> {
		const redeemer = await this.userRepository.findUnique(redeemerId);
		if (!redeemer?.email) {
			return;
		}
		const billingResumed = plan ? {premiumName: (await getBillingBranding()).premiumName, plan} : null;
		await this.emailService.send('gift_chargeback_notification', redeemer.email, {billingResumed}, redeemer.locale);
	}

	private async takeBackGift(giftCode: GiftCode, dispute: Stripe.Dispute): Promise<void> {
		if (giftCode.redeemedByUserId) {
			const outcome = await this.giftReversalHandler.handleGiftPremiumReversal(giftCode, {reason: 'gift_chargeback'});
			await this.sendGiftRemoved(giftCode.redeemedByUserId, outcome.billingResumed);
			Logger.debug(
				{giftCode: giftCode.code, redeemerId: giftCode.redeemedByUserId},
				'Premium taken back after a lost gift dispute',
			);
		} else if (!giftCode.revokedAt) {
			await this.userRepository.revokeGiftCode(giftCode.code);
			Logger.debug(
				{giftCode: giftCode.code, chargeId: extractId(dispute.charge)},
				'Revoked unredeemed gift code after a lost dispute',
			);
		}
	}

	private async cancelAutomaticDisputeDeletion(userId: UserID): Promise<void> {
		const user = await this.userRepository.findUnique(userId);
		if (
			!user ||
			!(user.flags & UserFlags.DELETED) ||
			user.deletionReasonCode !== DeletionReasons.BILLING_DISPUTE_OR_ABUSE ||
			(user.deletionScheduledBy !== null && user.deletionScheduledBy !== SYSTEM_USER_ID) ||
			user.enforcementDecisionId !== null
		) {
			return;
		}
		const updatedUser = await this.userRepository.updateDeletionSchedule(user, {
			flags: user.flags & ~UserFlags.DELETED,
			pending_deletion_at: null,
			deletion_reason_code: null,
			deletion_public_reason: null,
			deletion_audit_log_reason: null,
		});
		await followOperatorPassWallStanding(user, updatedUser);
		await clearPendingDeletion({userId, deletionQueue: this.kvDeletionQueue});
		await this.userCacheService.setUserPartialResponseFromUser(updatedUser);
		if (updatedUser.email) {
			await this.emailService.send('account_deletion_cancelled', updatedUser.email, {}, updatedUser.locale);
		}
		Logger.info({userId}, 'Cancelled an automatic deletion scheduled for a payment dispute');
	}

	private async findOperatorPassPurchase(paymentIntentId: string): Promise<OperatorPassPurchaseRow | null> {
		return (await this.operatorPassFulfilment?.findPurchaseByPaymentIntent(paymentIntentId)) ?? null;
	}

	private async resolveDisputeCustomerId(dispute: Stripe.Dispute): Promise<string | null> {
		const expandedCharge = typeof dispute.charge === 'object' ? dispute.charge : null;
		if (expandedCharge) {
			const expandedCustomerId = extractId(expandedCharge.customer);
			if (expandedCustomerId) {
				return expandedCustomerId;
			}
		}
		const chargeId = extractId(dispute.charge);
		if (!chargeId) {
			return null;
		}
		const mirroredCharge = await getBillingRepository().charges.findById(chargeId);
		return mirroredCharge?.customer_id ?? null;
	}

	async handleRefund(charge: Stripe.Charge): Promise<void> {
		const paymentIntentId = extractId(charge.payment_intent);
		if (!paymentIntentId) {
			Logger.error({chargeId: charge.id}, 'Refund missing payment intent');
			throw new StripeError('Refund missing payment intent');
		}
		const operatorPassPurchase = await this.findOperatorPassPurchase(paymentIntentId);
		if (operatorPassPurchase && this.operatorPassFulfilment) {
			await this.operatorPassFulfilment.handleChargeRefunded(operatorPassPurchase, charge);
			return;
		}
		const giftCode = await this.userRepository.findGiftCodeByPaymentIntent(paymentIntentId);
		if (giftCode) {
			if (giftCode.redeemedByUserId) {
				const outcome = await this.giftReversalHandler.handleGiftPremiumReversal(giftCode, {
					reason: 'gift_refund',
					chargeId: charge.id,
				});
				if (outcome.billingResumed) {
					await this.sendGiftRemoved(giftCode.redeemedByUserId, outcome.billingResumed);
				}
			} else if (!giftCode.revokedAt) {
				await this.userRepository.revokeGiftCode(giftCode.code);
				Logger.debug({giftCode: giftCode.code, chargeId: charge.id}, 'Revoked unredeemed gift code after refund');
			}
			return;
		}
		const payment = await this.userRepository.getPaymentByPaymentIntent(paymentIntentId);
		if (payment) {
			await this.userRepository.updatePayment({
				...payment.toRow(),
				status: 'refunded',
			});
		}
	}
}
