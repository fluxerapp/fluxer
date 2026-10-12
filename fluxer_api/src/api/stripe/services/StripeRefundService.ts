// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID, type UserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import type {UserRow} from '@app/api/database/types/UserTypes';
import {Logger} from '@app/api/Logger';
import {getBillingRepository} from '@app/api/middleware/ServiceRegistry';
import type {User} from '@app/api/models/User';
import {mirrorBestEffort} from '@app/api/stripe/StripeMirror';
import {extractId} from '@app/api/stripe/StripeUtils';
import {
	buildRefundEligibility,
	refundCooldownExpiresAt,
	refundWindowExpiresAt,
	SELF_SERVE_REFUND_WINDOW_DAYS,
	shouldBlockFurtherPurchases,
} from '@app/api/stripe/services/RefundAllowance';
import {REFUND_ALLOWANCE_CLAIM_PREFIX} from '@app/api/stripe/services/StripeDisputeWebhookHandler';
import type {StripeSubscriptionService} from '@app/api/stripe/services/StripeSubscriptionService';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {PremiumFlags} from '@fluxer/constants/src/UserConstants';
import {FeatureNotAvailableSelfHostedError} from '@fluxer/errors/src/domains/core/FeatureNotAvailableSelfHostedError';
import {StripeError} from '@fluxer/errors/src/domains/payment/StripeError';
import {StripeNoPurchaseHistoryError} from '@fluxer/errors/src/domains/payment/StripeNoPurchaseHistoryError';
import {StripePaymentNotAvailableError} from '@fluxer/errors/src/domains/payment/StripePaymentNotAvailableError';
import {StripeRefundCooldownActiveError} from '@fluxer/errors/src/domains/payment/StripeRefundCooldownActiveError';
import {StripeRefundOutsideWindowError} from '@fluxer/errors/src/domains/payment/StripeRefundOutsideWindowError';
import {UnknownUserError} from '@fluxer/errors/src/domains/user/UnknownUserError';
import type {
	SelfServeRefundEligibilityResponse,
	SelfServeRefundResponse,
} from '@fluxer/schema/src/domains/premium/PremiumSchemas';
import type Stripe from 'stripe';

const PIX_EXACT_AMOUNT_REFUND_SHORTFALL_CENTS = 1;

interface RefundTarget {
	invoice: Stripe.Invoice;
	invoiceId: string;
	amountPaidCents: number;
	currency: string;
	chargeId: string | null;
	paymentIntentId: string | null;
	paidAt: Date;
	subscriptionId: string | null;
}

type StripeInvoiceWithPayments = Stripe.Invoice & {
	customer?: string | Stripe.Customer | null;
	payments?: {
		data?: Array<{
			payment?: {
				charge?: string | Stripe.Charge | null;
				payment_intent?: string | Stripe.PaymentIntent | null;
			} | null;
			status?: string | null;
			status_transitions?: {
				paid_at?: number | null;
			} | null;
		}>;
	} | null;
};

function resolveInvoiceSubscriptionId(invoice: Stripe.Invoice): string | null {
	return extractId(invoice.parent?.subscription_details?.subscription ?? null);
}

function getInvoicePaymentRef(invoice: Stripe.Invoice): {
	chargeId: string | null;
	paymentIntentId: string | null;
	paidAt: Date | null;
} | null {
	const candidates = (invoice as StripeInvoiceWithPayments).payments?.data ?? [];
	const preferred =
		candidates.find((c) => c.status === 'paid' && (c.payment?.payment_intent || c.payment?.charge)) ??
		candidates.find((c) => c.payment?.payment_intent || c.payment?.charge) ??
		null;
	if (!preferred) {
		return null;
	}
	const paymentIntentId = extractId(preferred.payment?.payment_intent);
	let chargeId = extractId(preferred.payment?.charge);
	if (!chargeId && preferred.payment?.payment_intent && typeof preferred.payment.payment_intent !== 'string') {
		chargeId = extractId(preferred.payment.payment_intent.latest_charge);
	}
	const paidAtSec = preferred.status_transitions?.paid_at ?? null;
	return {
		chargeId,
		paymentIntentId,
		paidAt: paidAtSec ? new Date(paidAtSec * 1000) : null,
	};
}

export class StripeRefundService {
	constructor(
		private readonly stripe: Stripe | null,
		private readonly userRepository: UserRepository,
		private readonly subscriptionService: StripeSubscriptionService,
	) {}

	private ensureStripe(): Stripe {
		if (Config.instance.selfHosted) {
			throw new FeatureNotAvailableSelfHostedError();
		}
		if (!this.stripe) {
			throw new StripePaymentNotAvailableError();
		}
		return this.stripe;
	}

	private async getRequiredUser(userId: UserID): Promise<User> {
		const user = await this.userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}
		return user;
	}

	private async resolveLatestRefundTarget(user: User): Promise<RefundTarget | null> {
		if (!this.stripe || !user.stripeCustomerId) {
			return null;
		}
		try {
			const list = await this.stripe.invoices.list({
				customer: user.stripeCustomerId,
				limit: 5,
				expand: ['data.payments.data.payment'],
			});
			for (const invoice of list.data) {
				if (!invoice.id || invoice.status !== 'paid' || invoice.amount_paid <= 0) {
					continue;
				}
				const ref = getInvoicePaymentRef(invoice);
				if (!ref || (!ref.paymentIntentId && !ref.chargeId)) {
					continue;
				}
				const paidAt = ref.paidAt ?? new Date(invoice.created * 1000);
				return {
					invoice,
					invoiceId: invoice.id,
					amountPaidCents: invoice.amount_paid,
					currency: invoice.currency ?? 'usd',
					chargeId: ref.chargeId,
					paymentIntentId: ref.paymentIntentId,
					paidAt,
					subscriptionId: resolveInvoiceSubscriptionId(invoice),
				};
			}
		} catch (error) {
			Logger.warn({error, userId: user.id.toString()}, 'Failed to list invoices for self-serve refund');
		}
		return null;
	}

	async getEligibility(userId: UserID): Promise<SelfServeRefundEligibilityResponse> {
		if (Config.instance.selfHosted || !this.stripe) return buildRefundEligibility({reason: 'feature_unavailable'});
		const user = await this.getRequiredUser(userId);
		const target = await this.resolveLatestRefundTarget(user);
		return buildRefundEligibility({target, cooldownExpiresAt: refundCooldownExpiresAt(user)});
	}

	private async resolveChargePaymentMethodType(stripe: Stripe, target: RefundTarget): Promise<string | null> {
		try {
			if (target.chargeId) {
				const charge = await stripe.charges.retrieve(target.chargeId);
				return charge.payment_method_details?.type ?? null;
			}
			if (target.paymentIntentId) {
				const paymentIntent = await stripe.paymentIntents.retrieve(target.paymentIntentId, {
					expand: ['latest_charge'],
				});
				const latestCharge = paymentIntent.latest_charge;
				if (latestCharge && typeof latestCharge !== 'string') {
					return latestCharge.payment_method_details?.type ?? null;
				}
			}
		} catch (error) {
			Logger.warn(
				{error, chargeId: target.chargeId, paymentIntentId: target.paymentIntentId},
				'Failed to resolve payment method type for self-serve refund',
			);
			throw new StripeError('Could not determine the payment method for this refund');
		}
		return null;
	}

	private async countPriorTerminalFailures(invoiceId: string): Promise<number> {
		const priorRefunds = await getBillingRepository().refunds.listByInvoice(invoiceId);
		return priorRefunds.filter((r) => r.status === 'failed' || r.status === 'canceled').length;
	}

	private async finalizeIfSucceeded(refund: Stripe.Refund): Promise<void> {
		if (refund.status !== 'succeeded' || refund.metadata?.refund_kind !== 'self_serve') {
			return;
		}
		const userIdRaw = refund.metadata.user_id;
		if (!userIdRaw) {
			return;
		}
		let userId: UserID;
		try {
			userId = createUserID(BigInt(userIdRaw));
		} catch {
			return;
		}
		const user = await this.userRepository.findUnique(userId);
		if (!user) {
			return;
		}
		const subscriptionId = refund.metadata.subscription_id;
		if (subscriptionId) {
			try {
				await this.subscriptionService.cancelSubscriptionImmediately(user.id, 'self_serve_refund', subscriptionId);
			} catch (error) {
				Logger.error(
					{error, userId: user.id.toString(), subscriptionId},
					'Self-serve refund confirmed but subscription cancellation failed; the subscription is still active and will keep billing until it is cancelled manually',
				);
			}
		}
		const claimKey = `${REFUND_ALLOWANCE_CLAIM_PREFIX}:${refund.id}`;
		const claim = await getBillingRepository().webhookEvents.tryClaim(claimKey);
		if (claim === 'in_flight') {
			Logger.debug(
				{userId: user.id.toString(), refundId: refund.id, claim},
				'Self-serve refund allowance is being counted by another delivery',
			);
			return;
		}
		const isFirstRefund = !user.firstRefundAt;
		let patch: Partial<UserRow>;
		let countedRefundIds: Array<string> = [];
		if (isFirstRefund) {
			patch = {first_refund_at: new Date()};
		} else {
			const outcome = await shouldBlockFurtherPurchases(user, this.userRepository);
			countedRefundIds = outcome.countedRefundIds;
			if (!outcome.blocked) {
				await getBillingRepository().webhookEvents.markProcessed(claimKey);
				Logger.info(
					{userId: user.id.toString(), refundId: refund.id, countedRefundIds},
					'Self-serve refund redelivered after the allowance claim expired; not counting it twice',
				);
				return;
			}
			patch = {premium_flags: user.premiumFlags | PremiumFlags.PURCHASE_DISABLED};
		}
		try {
			await this.userRepository.patchUpsert(user.id, patch, user.toRow());
		} catch (error) {
			await getBillingRepository().webhookEvents.releaseClaim(claimKey);
			throw error;
		}
		await getBillingRepository().webhookEvents.markProcessed(claimKey);
		Logger.info(
			{
				userId: user.id.toString(),
				refundId: refund.id,
				subscriptionId: subscriptionId || null,
				isFirstRefund,
				countedRefundIds,
			},
			'Self-serve refund confirmed succeeded; refund allowance and cancellation finalized',
		);
	}

	async handleRefundWebhookEvent(refund: Stripe.Refund): Promise<void> {
		await this.finalizeIfSucceeded(refund);
	}

	async refundLatestPurchase(userId: UserID): Promise<SelfServeRefundResponse> {
		const stripe = this.ensureStripe();
		const user = await this.getRequiredUser(userId);
		const target = await this.resolveLatestRefundTarget(user);
		if (!target) {
			throw new StripeNoPurchaseHistoryError();
		}
		const now = Date.now();
		if (refundWindowExpiresAt(target.paidAt).getTime() <= now) {
			throw new StripeRefundOutsideWindowError();
		}
		if (refundCooldownExpiresAt(user)) {
			throw new StripeRefundCooldownActiveError();
		}
		const priorFailures = await this.countPriorTerminalFailures(target.invoiceId);
		const idempotencyKey = [
			'self-serve-refund',
			user.id.toString(),
			target.invoiceId,
			target.paymentIntentId ?? target.chargeId,
			...(priorFailures > 0 ? [`retry-${priorFailures}`] : []),
		].join(':');
		const paymentMethodType = await this.resolveChargePaymentMethodType(stripe, target);
		const isPix = paymentMethodType === 'pix';
		const requestedAmountCents =
			isPix && target.amountPaidCents > PIX_EXACT_AMOUNT_REFUND_SHORTFALL_CENTS
				? target.amountPaidCents - PIX_EXACT_AMOUNT_REFUND_SHORTFALL_CENTS
				: target.amountPaidCents;
		let refund: Stripe.Response<Stripe.Refund>;
		try {
			refund = await stripe.refunds.create(
				{
					...(target.paymentIntentId ? {payment_intent: target.paymentIntentId} : {charge: target.chargeId!}),
					amount: requestedAmountCents,
					reason: 'requested_by_customer',
					metadata: {
						user_id: user.id.toString(),
						invoice_id: target.invoiceId,
						refund_kind: 'self_serve',
						refund_window_days: String(SELF_SERVE_REFUND_WINDOW_DAYS),
						...(target.subscriptionId ? {subscription_id: target.subscriptionId} : {}),
					},
				},
				{idempotencyKey},
			);
		} catch (error) {
			Logger.warn(
				{error, userId: user.id.toString(), invoiceId: target.invoiceId},
				'Self-serve refund failed at Stripe',
			);
			throw new StripeError(error instanceof Error ? error.message : 'Failed to refund latest purchase');
		}
		await mirrorBestEffort(
			() =>
				getBillingRepository().refunds.upsertFromStripe(refund, {
					invoiceId: target.invoiceId,
					customerId: user.stripeCustomerId ?? undefined,
					userId: user.id,
				}),
			{refundId: refund.id},
			'Mirror upsert failed after Stripe write; reconciler will heal',
		);
		await this.finalizeIfSucceeded(refund);
		const succeeded = refund.status === 'succeeded';
		Logger.info(
			{
				userId: user.id.toString(),
				invoiceId: target.invoiceId,
				refundId: refund.id,
				status: refund.status,
				amountCents: refund.amount,
				subscriptionId: target.subscriptionId,
			},
			succeeded ? 'Self-serve refund issued' : 'Self-serve refund created; awaiting confirmation from provider',
		);
		return {
			invoice_id: target.invoiceId,
			payment_intent_id: target.paymentIntentId,
			charge_id: target.chargeId,
			refund_id: refund.id,
			refunded_amount_cents: succeeded ? refund.amount : 0,
			invoice_amount_paid_cents: target.amountPaidCents,
			currency: target.currency,
			subscription_id: succeeded ? target.subscriptionId : null,
			status: refund.status ?? 'pending',
		};
	}
}
