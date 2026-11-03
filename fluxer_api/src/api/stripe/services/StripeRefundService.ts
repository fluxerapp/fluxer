// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID, type UserID} from '@app/api/BrandedTypes';
import {alertSupport} from '@app/api/billing/BillingSupportAlert';
import {Config} from '@app/api/Config';
import {Logger} from '@app/api/Logger';
import {getBillingRepository, getKVClient, getWorkerService} from '@app/api/middleware/ServiceRegistry';
import type {User} from '@app/api/models/User';
import {
	type GooglePlayRefundTarget,
	isGooglePlaySelfServiceRefundEnabled,
} from '@app/api/store_billing/GooglePlayRefundTarget';
import type {StoreEntitlementService} from '@app/api/store_billing/StoreEntitlementService';
import {mirrorBestEffort} from '@app/api/stripe/StripeMirror';
import {getInvoicePaymentRef} from '@app/api/stripe/StripeSubscriptionPeriod';
import {extractId} from '@app/api/stripe/StripeUtils';
import {type BillingEmailJob, enqueueBillingEmail} from '@app/api/stripe/services/BillingEmailService';
import {primaryInvoiceLine} from '@app/api/stripe/services/BillingInvoiceDetails';
import {addCalendarMonths} from '@app/api/stripe/services/BillingNoticeSchedule';
import {
	isProRataWithdrawal,
	opensPlutoniumWithdrawalPeriod,
	type PlutoniumWithdrawalPeriod,
	type PlutoniumWithdrawalRule,
	plutoniumWithdrawalPeriod,
	proRataWithdrawalRefundMinor,
} from '@app/api/stripe/services/PlutoniumWithdrawal';
import {
	buildRefundEligibility,
	combinedRefundTarget,
	googlePlayRefundEligibilityTarget,
	isInsideRefundWindow,
	latestRefundTarget,
	type RefundEligibilityTarget,
	refundCooldownExpiresAt,
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
	PlutoniumWithdrawalResponse,
	SelfServeRefundEligibilityResponse,
	SelfServeRefundResponse,
} from '@fluxer/schema/src/domains/premium/PremiumSchemas';
import {seconds} from 'itty-time';
import type Stripe from 'stripe';

const PIX_EXACT_AMOUNT_REFUND_SHORTFALL_CENTS = 1;
const WITHDRAWAL_RECORD_KEY_PREFIX = 'plutonium_withdrawal:';
const WITHDRAWAL_RECORD_TTL_SECONDS = seconds('60 days');

function withdrawalRecordKey(userId: UserID, purchaseId: string): string {
	return `${WITHDRAWAL_RECORD_KEY_PREFIX}${userId.toString()}:${purchaseId}`;
}

const WITHDRAWAL_STUCK_AFTER_MS = seconds('2 minutes') * 1000;

interface StoredWithdrawal {
	received_at: string;
	pending: SelfServeRefundResponse;
	response?: SelfServeRefundResponse;
	receipt?: BillingEmailJob;
}

async function sendWithdrawalReceipt(receipt: BillingEmailJob, userId: string, purchaseId: string): Promise<void> {
	try {
		await enqueueBillingEmail(getWorkerService(), receipt);
	} catch (error) {
		Logger.error({error, userId, purchaseId}, 'Could not queue the receipt for a Plutonium withdrawal');
		await alertSupport({
			dedupeKey: `plutonium-withdrawal-receipt-queue:${purchaseId}`,
			subject: `Withdrawal receipt not queued for ${purchaseId}`,
			summary:
				'A buyer withdrew from a Plutonium purchase, but the confirmation email could not be queued. Send the buyer the withdrawal confirmation with the date and time we received it.',
			facts: {userId, error},
		});
	}
}

async function storedWithdrawal(record: string, userId: string, purchaseId: string): Promise<SelfServeRefundResponse> {
	const stored = JSON.parse(record) as StoredWithdrawal;
	if (stored.response) {
		if (stored.receipt) {
			await sendWithdrawalReceipt(stored.receipt, userId, purchaseId);
		}
		return stored.response;
	}
	await alertStuckWithdrawal(stored, userId, purchaseId);
	return stored.pending;
}

async function alertStuckWithdrawal(stored: StoredWithdrawal, userId: string, purchaseId: string): Promise<void> {
	if (Date.now() - new Date(stored.received_at).getTime() < WITHDRAWAL_STUCK_AFTER_MS) {
		return;
	}
	Logger.error({userId, purchaseId}, 'A Plutonium withdrawal was received but never finished');
	await alertSupport({
		dedupeKey: `plutonium-withdrawal-stuck:${userId}:${purchaseId}`,
		subject: `Plutonium withdrawal ${purchaseId} never finished`,
		summary: `A buyer withdrew from a Plutonium purchase at ${stored.received_at}, but the refund and the confirmation email never ran. The withdrawal stands. Check Stripe for a refund, refund what is missing, end what the purchase bought and send the buyer the confirmation.`,
		facts: {userId, amountMinor: stored.pending.invoice_amount_paid_cents, currency: stored.pending.currency},
	});
}

function stripePaymentSource(id: string | undefined): {invoiceId?: string; checkoutSessionId?: string} {
	if (id?.startsWith('in_')) return {invoiceId: id};
	if (id?.startsWith('cs_')) return {checkoutSessionId: id};
	return {};
}

interface RefundTarget {
	provider: 'stripe';
	invoiceId: string;
	amountPaidCents: number;
	currency: string;
	chargeId: string | null;
	paymentIntentId: string | null;
	paidAt: Date;
	subscriptionId: string | null;
	billingReason: string | null;
	priceId: string | null;
	servicePeriodEnd: Date | null;
	metadata: Readonly<Record<string, string>> | null;
}

type RefundKind = 'self_serve' | 'withdrawal';

interface WithdrawablePurchase {
	response: PlutoniumWithdrawalResponse;
	targets: Array<RefundTarget>;
	rule: PlutoniumWithdrawalRule;
}

interface WithdrawalRefund {
	target: RefundTarget;
	amountMinor: number;
}

const WITHDRAWAL_QUOTE_MAX_AGE_MS = seconds('1 day') * 1000;

type GooglePlayLatestTarget = RefundEligibilityTarget & {
	provider: 'google_play';
	paidAt: Date;
	googlePlay: GooglePlayRefundTarget;
};

type LatestRefundTarget = RefundTarget | GooglePlayLatestTarget;

interface ResolvedRefundTargets {
	latest: LatestRefundTarget | null;
	stripe: Array<RefundTarget>;
	googlePlay: GooglePlayLatestTarget | null;
}

interface IssuedRefund {
	target: RefundTarget;
	refund: Stripe.Refund;
}

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

	private async resolveRefundTargets(user: User): Promise<ResolvedRefundTargets> {
		const [stripeTargets, googlePlayTarget] = await Promise.all([
			this.resolveStripeTargets(user),
			this.resolveLatestGooglePlayTarget(user),
		]);
		return {
			latest: latestRefundTarget<LatestRefundTarget>(latestRefundTarget(...stripeTargets), googlePlayTarget),
			stripe: stripeTargets,
			googlePlay: googlePlayTarget,
		};
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

	private async resolveStripeTargets(user: User): Promise<Array<RefundTarget>> {
		if (!this.stripe || !user.stripeCustomerId) {
			return [];
		}
		try {
			const list = await this.stripe.invoices.list({
				customer: user.stripeCustomerId,
				limit: 5,
				expand: ['data.payments.data.payment'],
			});
			const targets: Array<RefundTarget> = [];
			for (const invoice of list.data) {
				if (!invoice.id || invoice.status !== 'paid' || invoice.amount_paid <= 0) {
					continue;
				}
				const ref = getInvoicePaymentRef(invoice);
				if (!ref || (!ref.paymentIntentId && !ref.chargeId)) {
					continue;
				}
				const line = primaryInvoiceLine(invoice);
				targets.push({
					provider: 'stripe',
					invoiceId: invoice.id,
					amountPaidCents: invoice.amount_paid,
					currency: invoice.currency ?? 'usd',
					chargeId: ref.chargeId,
					paymentIntentId: ref.paymentIntentId,
					paidAt: ref.paidAt ?? new Date(invoice.created * 1000),
					subscriptionId: resolveInvoiceSubscriptionId(invoice),
					billingReason: invoice.billing_reason ?? null,
					priceId: extractId(line?.pricing?.price_details?.price ?? null),
					servicePeriodEnd: line?.period?.end ? new Date(line.period.end * 1000) : null,
					metadata: invoice.parent?.subscription_details?.metadata ?? null,
				});
			}
			const gifts = await this.giftTargets(user);
			const giftPayments = new Set(gifts.map((gift) => gift.paymentIntentId));
			return [...targets.filter((target) => !giftPayments.has(target.paymentIntentId)), ...gifts].sort(
				(left, right) => right.paidAt.getTime() - left.paidAt.getTime(),
			);
		} catch (error) {
			Logger.warn({error, userId: user.id.toString()}, 'Failed to list invoices for self-serve refund');
		}
		return [];
	}

	private async giftTargets(user: User): Promise<Array<RefundTarget>> {
		return (await this.userRepository.findPaymentsByUserId(user.id))
			.filter((payment) => payment.isGift && payment.status === 'completed' && payment.paymentIntentId)
			.map((payment) => ({
				provider: 'stripe' as const,
				invoiceId: payment.checkoutSessionId,
				amountPaidCents: payment.amountCents,
				currency: payment.currency,
				chargeId: null,
				paymentIntentId: payment.paymentIntentId,
				paidAt: payment.completedAt ?? payment.createdAt,
				subscriptionId: null,
				billingReason: null,
				priceId: payment.priceId,
				servicePeriodEnd: null,
				metadata: null,
			}));
	}

	async getEligibility(userId: UserID): Promise<SelfServeRefundEligibilityResponse> {
		if (!this.isAvailable()) return buildRefundEligibility({reason: 'feature_unavailable'});
		const user = await this.getRequiredUser(userId);
		const {latest, stripe} = await this.resolveRefundTargets(user);
		const now = Date.now();
		const latestStripeInWindow = latestRefundTarget(
			...stripe.filter((target) => isInsideRefundWindow(target.paidAt, now)),
		);
		const target = latestStripeInWindow ? combinedRefundTarget(latestStripeInWindow, stripe, now) : latest;
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
		const kind = refund.metadata?.refund_kind;
		if (!refund.metadata || refund.status !== 'succeeded' || (kind !== 'self_serve' && kind !== 'withdrawal')) {
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
		if (subscriptionId && user.stripeSubscriptionId) {
			try {
				await this.subscriptionService.cancelSubscriptionImmediately(
					user.id,
					kind === 'withdrawal' ? 'withdrawal' : 'self_serve_refund',
					subscriptionId,
				);
			} catch (error) {
				Logger.error(
					{error, userId: user.id.toString(), subscriptionId},
					'Self-serve refund confirmed but subscription cancellation failed; the subscription is still active and will keep billing until it is cancelled manually',
				);
			}
		}
		if (kind === 'withdrawal' || !(await this.startRefundCooldown(user, new Date(refund.created * 1000)))) {
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

	async handleRefundWebhookEvent(refund: Stripe.Refund, eventType: string): Promise<void> {
		if (refund.status === 'failed' || refund.status === 'canceled') {
			const announcesEnd =
				(refund.status === 'failed' && eventType === 'refund.failed') ||
				(refund.status === 'canceled' && eventType === 'refund.updated');
			const kind = refund.metadata?.refund_kind;
			if (!refund.metadata || !announcesEnd || (kind !== 'self_serve' && kind !== 'withdrawal')) {
				return;
			}
			Logger.error(
				{
					refundId: refund.id,
					userId: refund.metadata.user_id ?? null,
					invoiceId: refund.metadata.invoice_id ?? null,
					amount: refund.amount,
					currency: refund.currency,
					status: refund.status,
					failureReason: refund.failure_reason ?? null,
				},
				'A self-serve refund did not reach the buyer; support has to send the money another way',
			);
			const outcome = refund.status === 'canceled' ? 'was canceled' : 'failed';
			const label = kind === 'withdrawal' ? 'Plutonium withdrawal refund' : 'Self-serve refund';
			await alertSupport({
				dedupeKey: `self-serve-refund:${refund.id}`,
				subject: `${label} ${refund.id} ${outcome}`,
				summary: `A ${label.toLowerCase()} ${outcome} at Stripe, so the money did not reach the buyer. Send the money another way and tell the buyer.`,
				facts: {
					refundId: refund.id,
					...stripePaymentSource(refund.metadata.invoice_id),
					paymentIntentId: extractId(refund.payment_intent ?? null),
					userId: refund.metadata.user_id ?? null,
					amountMinor: refund.amount,
					currency: refund.currency,
					error: refund.failure_reason ?? null,
				},
			});
			return;
		}
		await this.finalizeIfSucceeded(refund);
	}

	async refundRecentPurchases(userId: UserID): Promise<SelfServeRefundResponse> {
		this.ensureAvailable();
		const user = await this.getRequiredUser(userId);
		const {latest, stripe, googlePlay} = await this.resolveRefundTargets(user);
		if (!latest) {
			throw new StripeNoPurchaseHistoryError();
		}
		const now = Date.now();
		if (!isInsideRefundWindow(latest.paidAt, now)) {
			throw new StripeRefundOutsideWindowError();
		}
		if (refundCooldownExpiresAt(user)) {
			throw new StripeRefundCooldownActiveError();
		}
		const targets = await this.unrefunded(stripe.filter((target) => isInsideRefundWindow(target.paidAt, now)));
		const googlePlayInWindow = googlePlay && isInsideRefundWindow(googlePlay.paidAt, now) ? googlePlay : null;
		if (targets.length === 0) {
			if (!googlePlayInWindow) {
				throw new StripeRefundOutsideWindowError();
			}
			return this.refundGooglePlayPurchase(user, googlePlayInWindow);
		}
		const issued: Array<IssuedRefund> = [];
		const failed: Array<{target: RefundTarget; error: unknown}> = [];
		for (const target of targets) {
			try {
				issued.push({target, refund: await this.refundStripeTarget(user, target, 'self_serve')});
			} catch (error) {
				failed.push({target, error});
				Logger.error(
					{error, userId: user.id.toString(), invoiceId: target.invoiceId, refunded: issued.length},
					'One payment of a self-serve refund failed at Stripe; support has to refund it another way',
				);
			}
		}
		const lastError = failed.at(-1)?.error;
		if (issued.length === 0) {
			throw lastError instanceof StripeError ? lastError : new StripeError('Failed to refund recent purchases');
		}
		for (const {target, error} of failed) {
			await alertSupport({
				dedupeKey: `self-serve-refund-payment:${target.invoiceId}`,
				subject: `Self-serve refund missed payment ${target.invoiceId}`,
				summary:
					'A self-serve refund refunded some payments, but Stripe refused one of them. Refund that payment another way and tell the buyer.',
				facts: {
					...stripePaymentSource(target.invoiceId),
					paymentIntentId: target.paymentIntentId,
					userId: user.id,
					amountMinor: target.amountPaidCents,
					currency: target.currency,
					error,
				},
			});
		}
		for (const {refund} of issued) {
			await this.finalizeIfSucceeded(refund);
		}
		if (googlePlayInWindow) {
			try {
				await this.refundGooglePlayPurchase(user, googlePlayInWindow);
			} catch (error) {
				Logger.error(
					{error, userId: user.id.toString(), orderId: googlePlayInWindow.invoiceId},
					'The Google Play part of a self-serve refund failed; support has to refund it another way',
				);
				await alertSupport({
					dedupeKey: `self-serve-refund-google-play:${googlePlayInWindow.invoiceId}`,
					subject: 'Google Play part of a self-serve refund failed',
					summary:
						'A self-serve refund refunded the Stripe payments, but the Google Play purchase was not refunded. Refund it in the Google Play Console and tell the buyer.',
					facts: {
						googlePlayOrderId: googlePlayInWindow.invoiceId,
						userId: user.id,
						amountMinor: googlePlayInWindow.amountPaidCents,
						currency: googlePlayInWindow.currency,
						error,
					},
				});
			}
		}
		const latestIssued = issued[0];
		const sameCurrency = issued.every(({target}) => target.currency === latestIssued.target.currency);
		const counted = sameCurrency ? issued : [latestIssued];
		const succeeded = issued.filter(({refund}) => refund.status === 'succeeded');
		return {
			invoice_id: latestIssued.target.invoiceId,
			payment_intent_id: latestIssued.target.paymentIntentId,
			charge_id: latestIssued.target.chargeId,
			refund_id: latestIssued.refund.id,
			refunded_amount_cents: counted
				.filter(({refund}) => refund.status === 'succeeded')
				.reduce((sum, {refund}) => sum + refund.amount, 0),
			invoice_amount_paid_cents: counted.reduce((sum, {target}) => sum + target.amountPaidCents, 0),
			currency: latestIssued.target.currency,
			subscription_id: succeeded.find(({target}) => target.subscriptionId)?.target.subscriptionId ?? null,
			status: refundBatchStatus(issued.map(({refund}) => refund.status ?? 'pending')),
			provider: 'stripe',
		};
	}

	async getWithdrawals(userId: UserID): Promise<Array<PlutoniumWithdrawalResponse>> {
		if (Config.instance.selfHosted || !this.stripe) return [];
		const user = await this.getRequiredUser(userId);
		return (await this.listWithdrawablePurchases(user, new Date())).map((purchase) => purchase.response);
	}

	async withdraw({
		userId,
		purchaseId,
		name,
		receivedAt,
		quotedAt,
	}: {
		userId: UserID;
		purchaseId: string;
		name: string;
		receivedAt: Date;
		quotedAt: Date | null;
	}): Promise<SelfServeRefundResponse> {
		this.ensureAvailable();
		if (!this.stripe) {
			throw new StripePaymentNotAvailableError();
		}
		const user = await this.getRequiredUser(userId);
		const kv = getKVClient();
		const recordKey = withdrawalRecordKey(user.id, purchaseId);
		const existing = await kv.get(recordKey);
		if (existing) {
			return storedWithdrawal(existing, user.id.toString(), purchaseId);
		}
		const purchase = (await this.listWithdrawablePurchases(user, receivedAt)).find(
			(candidate) => candidate.response.id === purchaseId,
		);
		if (!purchase) {
			throw new StripeRefundOutsideWindowError();
		}
		const {response} = purchase;
		const pending: SelfServeRefundResponse = {
			invoice_id: purchase.targets[0].invoiceId,
			payment_intent_id: null,
			charge_id: null,
			refund_id: null,
			refunded_amount_cents: 0,
			invoice_amount_paid_cents: purchase.targets.reduce((sum, target) => sum + target.amountPaidCents, 0),
			currency: response.currency,
			subscription_id: response.kind === 'subscription' ? response.id : null,
			status: 'pending',
			provider: 'stripe',
		};
		const record = JSON.stringify({received_at: receivedAt.toISOString(), pending} satisfies StoredWithdrawal);
		if (!(await kv.setnx(recordKey, record, WITHDRAWAL_RECORD_TTL_SECONDS))) {
			return storedWithdrawal((await kv.get(recordKey)) ?? record, user.id.toString(), purchaseId);
		}
		const refunds = (await this.withdrawalRefunds(purchase, honouredQuoteTime(purchase, receivedAt, quotedAt))).filter(
			({amountMinor}) => amountMinor > 0,
		);
		const issued: Array<IssuedRefund> = [];
		for (const {target, amountMinor} of refunds) {
			try {
				issued.push({
					target,
					refund: await this.refundStripeTarget(user, target, 'withdrawal', receivedAt, amountMinor),
				});
			} catch (error) {
				Logger.error(
					{error, userId: user.id.toString(), invoiceId: target.invoiceId, purchaseId},
					'A Plutonium withdrawal refund failed at Stripe; support has to refund it another way',
				);
				await alertSupport({
					dedupeKey: `plutonium-withdrawal-payment:${target.invoiceId}`,
					subject: `Plutonium withdrawal refund missed payment ${target.invoiceId}`,
					summary: `A buyer withdrew from a Plutonium purchase, but Stripe refused the refund of this payment. The withdrawal stands. Refund ${amountMinor} (minor units) another way ${purchase.rule === 'brazil' ? 'right away, because this is a Brazilian purchase' : 'within 14 days'}, and tell the buyer.`,
					facts: {
						...stripePaymentSource(target.invoiceId),
						paymentIntentId: target.paymentIntentId,
						userId: user.id,
						amountMinor,
						currency: target.currency,
						error,
					},
				});
			}
		}
		if (response.kind === 'subscription' && response.id === user.stripeSubscriptionId) {
			try {
				await this.subscriptionService.cancelSubscriptionImmediately(user.id, 'withdrawal', response.id);
			} catch (error) {
				Logger.error(
					{error, userId: user.id.toString(), subscriptionId: response.id},
					'A Plutonium withdrawal could not end the subscription; it keeps billing until someone cancels it',
				);
				await alertSupport({
					dedupeKey: `plutonium-withdrawal-cancel:${response.id}`,
					subject: `Plutonium withdrawal did not end subscription ${response.id}`,
					summary:
						'A buyer withdrew from a Plutonium subscription, but the subscription could not be canceled. Cancel it now in Stripe so it does not renew.',
					facts: {subscriptionId: response.id, userId: user.id, error},
				});
			}
		}
		const refunded = issued.length === refunds.length;
		const refundMinor = refunds.reduce((sum, {amountMinor}) => sum + amountMinor, 0);
		const latestIssued = issued[0];
		const result: SelfServeRefundResponse = {
			invoice_id: latestIssued?.target.invoiceId ?? purchase.targets[0].invoiceId,
			payment_intent_id: latestIssued?.target.paymentIntentId ?? null,
			charge_id: latestIssued?.target.chargeId ?? null,
			refund_id: latestIssued?.refund.id ?? null,
			refunded_amount_cents: issued
				.filter(({refund}) => refund.status === 'succeeded')
				.reduce((sum, {refund}) => sum + refund.amount, 0),
			invoice_amount_paid_cents: purchase.targets.reduce((sum, target) => sum + target.amountPaidCents, 0),
			currency: response.currency,
			subscription_id: response.kind === 'subscription' ? response.id : null,
			status: refunded ? refundBatchStatus(issued.map(({refund}) => refund.status ?? 'pending')) : 'failed',
			provider: 'stripe',
		};
		const receipt: BillingEmailJob = {
			kind: 'plutonium_withdrawal',
			id: purchaseId,
			userId: user.id.toString(),
			name,
			receivedAt: receivedAt.toISOString(),
			purchasedAt: response.purchased_at,
			gift: response.kind === 'gift',
			amountMinor: refundMinor,
			currency: response.currency,
			refund: refunded ? 'started' : 'manual',
			brazil: purchase.rule === 'brazil',
			proRata: refunds.some(({target, amountMinor}) => amountMinor < target.amountPaidCents),
		};
		await kv.setex(
			recordKey,
			WITHDRAWAL_RECORD_TTL_SECONDS,
			JSON.stringify({
				received_at: receivedAt.toISOString(),
				pending,
				response: result,
				receipt,
			} satisfies StoredWithdrawal),
		);
		await sendWithdrawalReceipt(receipt, user.id.toString(), purchaseId);
		Logger.info(
			{userId: user.id.toString(), purchaseId, kind: response.kind, rule: purchase.rule, refunds: issued.length},
			'Plutonium purchase withdrawn',
		);
		return result;
	}

	private async listWithdrawablePurchases(user: User, now: Date): Promise<Array<WithdrawablePurchase>> {
		const targets = await this.resolveStripeTargets(user);
		const anchors = new Map<string, {anchor: RefundTarget; period: PlutoniumWithdrawalPeriod}>();
		for (const [index, target] of targets.entries()) {
			const gift = target.invoiceId.startsWith('cs_');
			const key = gift ? target.invoiceId : target.subscriptionId;
			if (!key) {
				continue;
			}
			const period = plutoniumWithdrawalPeriod(await this.withdrawalMetadata(target), target.paidAt);
			if (!period || period.endsAt.getTime() <= now.getTime()) {
				continue;
			}
			const previous = targets.slice(index + 1).find((older) => older.subscriptionId === target.subscriptionId);
			if (!gift && !opensPlutoniumWithdrawalPeriod(period.rule, target, previous?.priceId ?? null)) {
				continue;
			}
			anchors.set(key, {anchor: target, period});
		}
		const purchases: Array<WithdrawablePurchase> = [];
		for (const [id, {anchor, period}] of anchors) {
			const gift = anchor.invoiceId.startsWith('cs_');
			const paid = await this.unrefunded(
				gift
					? [anchor]
					: targets.filter(
							(target) =>
								target.subscriptionId === id &&
								target.currency === anchor.currency &&
								target.paidAt.getTime() >= anchor.paidAt.getTime(),
						),
			);
			if (paid.length === 0) {
				continue;
			}
			const purchase: WithdrawablePurchase = {
				targets: paid,
				rule: period.rule,
				response: {
					id,
					kind: gift ? 'gift' : 'subscription',
					amount_minor: 0,
					paid_minor: paid.reduce((sum, target) => sum + target.amountPaidCents, 0),
					quoted_at: now.toISOString(),
					currency: anchor.currency,
					purchased_at: anchor.paidAt.toISOString(),
					withdrawal_ends_at: period.endsAt.toISOString(),
					withdrawal_ends_on: period.endsOn,
				},
			};
			purchase.response.amount_minor = (await this.withdrawalRefunds(purchase, now)).reduce(
				(sum, {amountMinor}) => sum + amountMinor,
				0,
			);
			purchases.push(purchase);
		}
		if (purchases.length === 0) {
			return purchases;
		}
		const recorded = await getKVClient().mget(
			...purchases.map((purchase) => withdrawalRecordKey(user.id, purchase.response.id)),
		);
		for (const [index, record] of recorded.entries()) {
			const stored = record ? (JSON.parse(record) as StoredWithdrawal) : null;
			if (stored && !stored.response) {
				await alertStuckWithdrawal(stored, user.id.toString(), purchases[index].response.id);
			}
		}
		return purchases.filter((_, index) => recorded[index] === null);
	}

	private async withdrawalRefunds(purchase: WithdrawablePurchase, at: Date): Promise<Array<WithdrawalRefund>> {
		const refunds: Array<WithdrawalRefund> = [];
		for (const target of purchase.targets) {
			const proRata = purchase.rule !== 'brazil' && isProRataWithdrawal(await this.withdrawalMetadata(target));
			refunds.push({
				target,
				amountMinor: proRata
					? proRataWithdrawalRefundMinor(target.amountPaidCents, await this.serviceUsage(target), at)
					: target.amountPaidCents,
			});
		}
		return refunds;
	}

	private async serviceUsage(target: RefundTarget): Promise<{startedAt: Date; endsAt: Date} | null> {
		if (!target.invoiceId.startsWith('cs_')) {
			return target.servicePeriodEnd && target.servicePeriodEnd > target.paidAt
				? {startedAt: target.paidAt, endsAt: target.servicePeriodEnd}
				: null;
		}
		const gift = target.paymentIntentId
			? await this.userRepository.findGiftCodeByPaymentIntent(target.paymentIntentId)
			: null;
		if (!gift?.redeemedAt || !gift.durationMonths) {
			return null;
		}
		const redeemedAtUnix = Math.floor(gift.redeemedAt.getTime() / 1000);
		return {
			startedAt: gift.redeemedAt,
			endsAt: new Date(addCalendarMonths(redeemedAtUnix, gift.durationMonths) * 1000),
		};
	}

	private async withdrawalMetadata(target: RefundTarget): Promise<Readonly<Record<string, string>> | null> {
		if (target.metadata || !target.paymentIntentId || !this.stripe) {
			return target.metadata;
		}
		try {
			return (await this.stripe.paymentIntents.retrieve(target.paymentIntentId)).metadata;
		} catch (error) {
			Logger.warn(
				{error, paymentIntentId: target.paymentIntentId},
				'Could not read the checkout details of a Plutonium gift for the withdrawal function',
			);
			return null;
		}
	}

	private async unrefunded(targets: Array<RefundTarget>): Promise<Array<RefundTarget>> {
		const open: Array<RefundTarget> = [];
		for (const target of targets) {
			const refunds = target.paymentIntentId
				? await getBillingRepository().refunds.listByPaymentIntent(target.paymentIntentId)
				: await getBillingRepository().refunds.listByInvoice(target.invoiceId);
			if (!refunds.some((refund) => refund.status !== 'failed' && refund.status !== 'canceled')) {
				open.push(target);
			}
		}
		return open;
	}

	private async refundStripeTarget(
		user: User,
		target: RefundTarget,
		kind: RefundKind,
		withdrawalReceivedAt: Date | null = null,
		withdrawalAmountMinor: number | null = null,
	): Promise<Stripe.Refund> {
		const stripe = this.stripe!;
		const priorFailures = await this.countPriorTerminalFailures(target.invoiceId);
		const idempotencyKey = [
			kind === 'withdrawal' ? 'plutonium-withdrawal' : 'self-serve-refund',
			user.id.toString(),
			target.invoiceId,
			target.paymentIntentId ?? target.chargeId,
			...(priorFailures > 0 ? [`retry-${priorFailures}`] : []),
		].join(':');
		const isPix = kind === 'self_serve' && (await this.resolveChargePaymentMethodType(stripe, target)) === 'pix';
		const requestedAmountCents =
			withdrawalAmountMinor ??
			(isPix && target.amountPaidCents > PIX_EXACT_AMOUNT_REFUND_SHORTFALL_CENTS
				? target.amountPaidCents - PIX_EXACT_AMOUNT_REFUND_SHORTFALL_CENTS
				: target.amountPaidCents);
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
						refund_kind: kind,
						...(withdrawalReceivedAt
							? {withdrawal_received_at: withdrawalReceivedAt.toISOString()}
							: {refund_window_days: String(SELF_SERVE_REFUND_WINDOW_DAYS)}),
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
			throw new StripeError(error instanceof Error ? error.message : 'Failed to refund a recent purchase');
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
		Logger.info(
			{
				userId: user.id.toString(),
				invoiceId: target.invoiceId,
				refundId: refund.id,
				status: refund.status,
				amountCents: refund.amount,
				subscriptionId: target.subscriptionId,
			},
			refund.status === 'succeeded'
				? 'Self-serve refund issued'
				: 'Self-serve refund created; awaiting confirmation from provider',
		);
		return refund;
	}
}

function honouredQuoteTime(purchase: WithdrawablePurchase, receivedAt: Date, quotedAt: Date | null): Date {
	if (
		purchase.rule === 'brazil' ||
		!quotedAt ||
		quotedAt.getTime() > receivedAt.getTime() ||
		receivedAt.getTime() - quotedAt.getTime() > WITHDRAWAL_QUOTE_MAX_AGE_MS ||
		quotedAt.getTime() < new Date(purchase.response.purchased_at).getTime()
	) {
		return receivedAt;
	}
	return quotedAt;
}

function refundBatchStatus(statuses: ReadonlyArray<string>): string {
	if (statuses.every((status) => status === 'succeeded')) return 'succeeded';
	return statuses.find((status) => status === 'failed' || status === 'canceled') ?? 'pending';
}
