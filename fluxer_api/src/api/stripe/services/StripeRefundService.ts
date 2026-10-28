// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID, type UserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {Logger} from '@app/api/Logger';
import {getBillingRepository} from '@app/api/middleware/ServiceRegistry';
import type {User} from '@app/api/models/User';
import {
	type GooglePlayRefundTarget,
	isGooglePlaySelfServiceRefundEnabled,
} from '@app/api/store_billing/GooglePlayRefundTarget';
import type {StoreEntitlementService} from '@app/api/store_billing/StoreEntitlementService';
import {mirrorBestEffort} from '@app/api/stripe/StripeMirror';
import {getInvoicePaymentRef} from '@app/api/stripe/StripeSubscriptionPeriod';
import {extractId} from '@app/api/stripe/StripeUtils';
import {
	buildRefundEligibility,
	googlePlayRefundEligibilityTarget,
	latestRefundTarget,
	type RefundEligibilityTarget,
	refundCooldownExpiresAt,
	refundWindowExpiresAt,
	SELF_SERVE_REFUND_WINDOW_DAYS,
} from '@app/api/stripe/services/RefundAllowance';
import type {StripeSubscriptionService} from '@app/api/stripe/services/StripeSubscriptionService';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {FeatureNotAvailableSelfHostedError} from '@fluxer/errors/src/domains/core/FeatureNotAvailableSelfHostedError';
import {StoreBillingUnavailableError} from '@fluxer/errors/src/domains/payment/StoreBillingUnavailableError';
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
	provider: 'stripe';
	invoice: Stripe.Invoice;
	invoiceId: string;
	amountPaidCents: number;
	currency: string;
	chargeId: string | null;
	paymentIntentId: string | null;
	paidAt: Date;
	subscriptionId: string | null;
}

type GooglePlayLatestTarget = RefundEligibilityTarget & {
	provider: 'google_play';
	paidAt: Date;
	googlePlay: GooglePlayRefundTarget;
};

type LatestRefundTarget = RefundTarget | GooglePlayLatestTarget;

function resolveInvoiceSubscriptionId(invoice: Stripe.Invoice): string | null {
	return extractId(invoice.parent?.subscription_details?.subscription ?? null);
}

export class StripeRefundService {
	constructor(
		private readonly stripe: Stripe | null,
		private readonly userRepository: UserRepository,
		private readonly subscriptionService: StripeSubscriptionService,
		private readonly storeEntitlementService: StoreEntitlementService | null,
	) {}

	private isAvailable(): boolean {
		return !Config.instance.selfHosted && (this.stripe !== null || this.isGooglePlayAvailable());
	}

	private isGooglePlayAvailable(): boolean {
		return this.storeEntitlementService !== null && isGooglePlaySelfServiceRefundEnabled();
	}

	private ensureAvailable(): void {
		if (Config.instance.selfHosted) {
			throw new FeatureNotAvailableSelfHostedError();
		}
		if (!this.isAvailable()) {
			throw new StripePaymentNotAvailableError();
		}
	}

	private async getRequiredUser(userId: UserID): Promise<User> {
		const user = await this.userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}
		return user;
	}

	private async resolveLatestRefundTarget(user: User): Promise<LatestRefundTarget | null> {
		const [stripeTarget, googlePlayTarget] = await Promise.all([
			this.resolveLatestStripeTarget(user),
			this.resolveLatestGooglePlayTarget(user),
		]);
		return latestRefundTarget<LatestRefundTarget>(stripeTarget, googlePlayTarget);
	}

	private async resolveLatestGooglePlayTarget(user: User): Promise<GooglePlayLatestTarget | null> {
		if (!this.storeEntitlementService || !this.isGooglePlayAvailable()) {
			return null;
		}
		const target = await this.storeEntitlementService.findGooglePlayRefundTarget(user.id);
		return target
			? {
					...googlePlayRefundEligibilityTarget(target),
					provider: 'google_play',
					paidAt: target.paidAt,
					googlePlay: target,
				}
			: null;
	}

	private async resolveLatestStripeTarget(user: User): Promise<RefundTarget | null> {
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
					provider: 'stripe',
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
		if (!this.isAvailable()) return buildRefundEligibility({reason: 'feature_unavailable'});
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
		if (!(await this.startRefundCooldown(user, new Date(refund.created * 1000)))) {
			return;
		}
		Logger.info(
			{userId: user.id.toString(), refundId: refund.id, subscriptionId: subscriptionId || null},
			'Self-serve refund confirmed succeeded; refund cooldown started',
		);
	}

	private async startRefundCooldown(user: User, refundedAt: Date): Promise<boolean> {
		if (user.lastRefundAt && user.lastRefundAt.getTime() >= refundedAt.getTime()) {
			return false;
		}
		await this.userRepository.patchUpsert(user.id, {first_refund_at: refundedAt}, user.toRow());
		return true;
	}

	private async refundGooglePlayPurchase(user: User, target: GooglePlayLatestTarget): Promise<SelfServeRefundResponse> {
		let refunded: boolean;
		try {
			refunded = await this.storeEntitlementService!.refundGooglePlayPurchase(user.id, target.googlePlay);
		} catch (error) {
			Logger.warn(
				{error, userId: user.id.toString(), orderId: target.invoiceId},
				'Self-serve refund failed at Google Play',
			);
			throw new StoreBillingUnavailableError();
		}
		if (!refunded) {
			throw new StripeNoPurchaseHistoryError();
		}
		await this.startRefundCooldown(await this.getRequiredUser(user.id), new Date());
		Logger.info(
			{userId: user.id.toString(), orderId: target.invoiceId, subscriptionId: target.subscriptionId},
			'Self-serve refund issued through Google Play',
		);
		return {
			invoice_id: target.invoiceId,
			payment_intent_id: null,
			charge_id: null,
			refund_id: null,
			refunded_amount_cents: null,
			invoice_amount_paid_cents: null,
			currency: null,
			subscription_id: target.subscriptionId,
			status: 'succeeded',
			provider: 'google_play',
		};
	}

	async handleRefundWebhookEvent(refund: Stripe.Refund): Promise<void> {
		await this.finalizeIfSucceeded(refund);
	}

	async refundLatestPurchase(userId: UserID): Promise<SelfServeRefundResponse> {
		this.ensureAvailable();
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
		if (target.provider === 'google_play') {
			return this.refundGooglePlayPurchase(user, target);
		}
		const stripe = this.stripe!;
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
			provider: 'stripe',
		};
	}
}
