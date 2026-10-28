// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AdminRepository} from '@app/api/admin/AdminRepository';
import {AdminAuditService} from '@app/api/admin/services/AdminAuditService';
import type {BillingRepository} from '@app/api/billing/repositories/BillingRepository';
import type {DonationRepository} from '@app/api/donation/DonationRepository';
import type {EnforcementDecisionService} from '@app/api/enforcement/EnforcementDecisionService';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {ISnowflakeService} from '@app/api/infrastructure/ISnowflakeService';
import type {KVAccountDeletionQueueService} from '@app/api/infrastructure/KVAccountDeletionQueueService';
import type {PremiumStateReconciliationQueueService} from '@app/api/infrastructure/PremiumStateReconciliationQueueService';
import type {UserCacheService} from '@app/api/infrastructure/UserCacheService';
import {Logger} from '@app/api/Logger';
import {getWorkerService} from '@app/api/middleware/ServiceRegistry';
import {OAuth2TokenRepository} from '@app/api/oauth/repositories/OAuth2TokenRepository';
import type {OperatorPassFulfilmentService} from '@app/api/operator_pass/OperatorPassFulfilmentService';
import {PaymentDisputeEnforcementService} from '@app/api/payment_disputes/PaymentDisputeEnforcement';
import {PaymentDisputeRepository} from '@app/api/payment_disputes/PaymentDisputeRepository';
import {PaymentDisputeReviewService} from '@app/api/payment_disputes/PaymentDisputeReviews';
import type {StoreEntitlementService} from '@app/api/store_billing/StoreEntitlementService';
import {getAcceptedWebhookSecrets} from '@app/api/stripe/BillingConfigCache';
import type {ProductRegistry} from '@app/api/stripe/ProductRegistry';
import {mirrorBestEffort} from '@app/api/stripe/StripeMirror';
import {type BillingEmailJob, enqueueBillingEmail, isRecentRefund} from '@app/api/stripe/services/BillingEmailService';
import {subscriptionBillingProduct} from '@app/api/stripe/services/BillingInvoiceDetails';
import {
	CLEARED_CANCEL_METADATA,
	freshCancelSource,
	subscriptionCancelling,
} from '@app/api/stripe/services/BillingSubscriptionDetails';
import {StripeCheckoutWebhookHandler} from '@app/api/stripe/services/StripeCheckoutWebhookHandler';
import {StripeDisputeWebhookHandler} from '@app/api/stripe/services/StripeDisputeWebhookHandler';
import {StripeGiftReversalHandler} from '@app/api/stripe/services/StripeGiftReversalHandler';
import type {StripeGiftService} from '@app/api/stripe/services/StripeGiftService';
import {StripePaymentFraudService} from '@app/api/stripe/services/StripePaymentFraudService';
import type {StripePremiumService} from '@app/api/stripe/services/StripePremiumService';
import type {StripeRefundService} from '@app/api/stripe/services/StripeRefundService';
import {StripeSubscriptionReconciler} from '@app/api/stripe/services/StripeSubscriptionReconciler';
import type {StripeSubscriptionService} from '@app/api/stripe/services/StripeSubscriptionService';
import {StripeSubscriptionWebhookHandler} from '@app/api/stripe/services/StripeSubscriptionWebhookHandler';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {StripeError} from '@fluxer/errors/src/domains/payment/StripeError';
import {StripeWebhookNotAvailableError} from '@fluxer/errors/src/domains/payment/StripeWebhookNotAvailableError';
import {StripeWebhookSignatureInvalidError} from '@fluxer/errors/src/domains/payment/StripeWebhookSignatureInvalidError';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import type {IEmailService} from '@pkgs/email/src/IEmailService';
import type Stripe from 'stripe';

interface HandleWebhookParams {
	body: string;
	signature: string;
}

export class StripeWebhookService {
	private checkoutHandler: StripeCheckoutWebhookHandler;
	private subscriptionHandler: StripeSubscriptionWebhookHandler;
	private disputeHandler: StripeDisputeWebhookHandler;
	private paymentFraudService: StripePaymentFraudService;

	constructor(
		private stripe: Stripe | null,
		userRepository: UserRepository,
		userCacheService: UserCacheService,
		subscriptionService: StripeSubscriptionService,
		emailService: IEmailService,
		gatewayService: IGatewayService,
		private productRegistry: ProductRegistry,
		cacheService: ICacheService,
		giftService: StripeGiftService,
		premiumService: StripePremiumService,
		private donationRepository: DonationRepository,
		kvDeletionQueue: KVAccountDeletionQueueService,
		premiumStateReconciliationQueueService: PremiumStateReconciliationQueueService,
		adminRepository: AdminRepository,
		snowflakeService: ISnowflakeService,
		private billingRepository: BillingRepository,
		private refundService: StripeRefundService,
		enforcementService: EnforcementDecisionService,
		storeEntitlementService: StoreEntitlementService | null = null,
		operatorPassFulfilment: OperatorPassFulfilmentService | null = null,
	) {
		const auditService = new AdminAuditService(adminRepository, snowflakeService);
		const oauth2Tokens = new OAuth2TokenRepository();
		const disputeRepository = new PaymentDisputeRepository();
		const disputeEnforcement = new PaymentDisputeEnforcementService({
			repository: disputeRepository,
			userRepository,
			userCacheService,
			emailService,
			cacheService,
			gatewayService,
			auditService,
			kvDeletionQueue,
			oauth2Tokens,
			systemDm: null,
			enforcement: enforcementService,
		});
		this.checkoutHandler = new StripeCheckoutWebhookHandler(
			stripe,
			userRepository,
			emailService,
			gatewayService,
			productRegistry,
			cacheService,
			giftService,
			premiumService,
			donationRepository,
			operatorPassFulfilment,
		);
		const reconciler = new StripeSubscriptionReconciler(stripe, userRepository, productRegistry);
		const giftReversalHandler = new StripeGiftReversalHandler(
			userRepository,
			gatewayService,
			premiumStateReconciliationQueueService,
			storeEntitlementService,
		);
		this.paymentFraudService = new StripePaymentFraudService({
			stripe,
			userRepository,
			gatewayService,
			donationRepository,
			cacheService,
			auditService,
			enforcement: enforcementService,
			operatorPassFulfilment,
		});
		this.subscriptionHandler = new StripeSubscriptionWebhookHandler(
			userRepository,
			gatewayService,
			premiumService,
			donationRepository,
			premiumStateReconciliationQueueService,
			reconciler,
			billingRepository,
			storeEntitlementService,
		);
		this.disputeHandler = new StripeDisputeWebhookHandler(
			userRepository,
			userCacheService,
			emailService,
			donationRepository,
			kvDeletionQueue,
			giftReversalHandler,
			this.paymentFraudService,
			enforcementService,
			subscriptionService,
			new PaymentDisputeReviewService(disputeRepository, auditService, emailService),
			disputeEnforcement,
			operatorPassFulfilment,
		);
	}

	private constructVerifiedEvent(
		stripe: Stripe,
		body: string,
		signature: string,
		webhookSecrets: Array<string>,
	): Stripe.Event {
		const SEVEN_DAYS_SECONDS = 7 * 24 * 60 * 60;
		let lastError: unknown = null;
		for (const webhookSecret of webhookSecrets) {
			try {
				return stripe.webhooks.constructEvent(body, signature, webhookSecret, SEVEN_DAYS_SECONDS);
			} catch (error: unknown) {
				lastError = error;
			}
		}
		Logger.error({error: lastError}, 'Invalid webhook signature');
		throw new StripeWebhookSignatureInvalidError();
	}

	async handleWebhook({body, signature}: HandleWebhookParams): Promise<void> {
		const webhookSecrets = getAcceptedWebhookSecrets();
		if (!this.stripe || webhookSecrets.length === 0) {
			throw new StripeWebhookNotAvailableError();
		}
		const event = this.constructVerifiedEvent(this.stripe, body, signature, webhookSecrets);
		Logger.debug({eventType: event.type, eventId: event.id}, 'Processing Stripe webhook');
		const claim = await this.billingRepository.webhookEvents.tryClaim(event.id);
		if (claim === 'already_processed') {
			Logger.debug({eventId: event.id, eventType: event.type}, 'Stripe webhook already processed (durable claim)');
			return;
		}
		if (claim === 'in_flight') {
			Logger.warn({eventId: event.id, eventType: event.type}, 'Stripe webhook in-flight; will retry via worker queue');
			throw new StripeError('Stripe webhook event in-flight, retry');
		}
		try {
			await this.dispatchWebhookEvent(event);
			await this.billingRepository.webhookEvents.markProcessed(event.id);
		} catch (err) {
			await this.billingRepository.webhookEvents.releaseClaim(event.id);
			throw err;
		}
	}

	private async dispatchWebhookEvent(event: Stripe.Event): Promise<void> {
		switch (event.type) {
			case 'checkout.session.completed': {
				const checkoutSession = event.data.object as Stripe.Checkout.Session;
				await this.checkoutHandler.handleCheckoutSessionCompleted(checkoutSession);
				break;
			}
			case 'checkout.session.async_payment_succeeded': {
				const cs = event.data.object as Stripe.Checkout.Session;
				await this.checkoutHandler.handleAsyncPaymentSucceeded(cs);
				break;
			}
			case 'checkout.session.expired': {
				await this.checkoutHandler.handleCheckoutSessionExpired(event.data.object as Stripe.Checkout.Session);
				break;
			}
			case 'checkout.session.async_payment_failed': {
				const cs = event.data.object as Stripe.Checkout.Session;
				await this.checkoutHandler.handleAsyncPaymentFailed(cs);
				break;
			}
			case 'invoice.paid':
			case 'invoice.payment_succeeded': {
				const inv = event.data.object as Stripe.Invoice;
				await this.mirrorInvoice(event, inv);
				await this.subscriptionHandler.handleInvoicePaymentSucceeded(event.id, inv);
				await this.queueBillingEmail(inv.id ? {kind: 'invoice_paid', id: inv.id} : null);
				break;
			}
			case 'invoice.payment_failed': {
				const inv = event.data.object as Stripe.Invoice;
				await this.mirrorInvoice(event, inv);
				await this.subscriptionHandler.handleInvoicePaymentFailed(inv);
				await this.queueInvoicePaymentProblem(inv, false);
				break;
			}
			case 'invoice.payment_action_required': {
				const inv = event.data.object as Stripe.Invoice;
				await this.mirrorInvoice(event, inv);
				await this.subscriptionHandler.handleInvoicePaymentActionRequired(inv);
				await this.queueInvoicePaymentProblem(inv, true);
				break;
			}
			case 'invoice.finalization_failed': {
				const inv = event.data.object as Stripe.Invoice;
				await this.mirrorInvoice(event, inv);
				await this.subscriptionHandler.handleInvoiceFinalizationFailed(inv);
				break;
			}
			case 'invoice.updated': {
				const inv = event.data.object as Stripe.Invoice;
				await this.mirrorInvoice(event, inv);
				await this.subscriptionHandler.handleInvoiceUpdated(inv);
				break;
			}
			case 'customer.subscription.created':
			case 'customer.subscription.updated':
			case 'customer.subscription.pending_update_applied':
			case 'customer.subscription.pending_update_expired': {
				const sub = event.data.object as Stripe.Subscription;
				const subKnownUserId = await this.resolveKnownUserIdForSubscription(sub);
				const snapshotCapturedAt = new Date(event.created * 1000);
				await this.safeMirrorUpsert(event, () =>
					this.billingRepository.subscriptions.upsertFromStripe(sub, {
						knownUserId: subKnownUserId ?? undefined,
						snapshotCapturedAt,
					}),
				);
				await this.subscriptionHandler.handleSubscriptionUpdated(sub, snapshotCapturedAt);
				if (event.type === 'customer.subscription.updated') {
					await this.queuePortalCancellation(event, sub);
					await this.clearWithdrawnCancelSource(event, sub);
				}
				break;
			}
			case 'customer.subscription.deleted': {
				const sub = event.data.object as Stripe.Subscription;
				const subKnownUserId = await this.resolveKnownUserIdForSubscription(sub);
				await this.safeMirrorUpsert(event, () =>
					this.billingRepository.subscriptions.upsertFromStripe(sub, {
						knownUserId: subKnownUserId ?? undefined,
						snapshotCapturedAt: new Date(event.created * 1000),
					}),
				);
				await this.subscriptionHandler.handleSubscriptionDeleted(sub);
				await this.queueEndedUnpaid(sub);
				break;
			}
			case 'charge.dispute.created': {
				const d = event.data.object as Stripe.Dispute;
				await this.disputeHandler.handleChargebackCreated(d);
				break;
			}
			case 'charge.dispute.closed': {
				const d = event.data.object as Stripe.Dispute;
				await this.disputeHandler.handleChargebackClosed(d);
				break;
			}
			case 'radar.early_fraud_warning.created':
			case 'radar.early_fraud_warning.updated':
				await this.paymentFraudService.handleEarlyFraudWarning(event.data.object as Stripe.Radar.EarlyFraudWarning);
				break;
			case 'charge.refunded': {
				const c = event.data.object as Stripe.Charge;
				await this.safeMirrorUpsert(event, () => this.billingRepository.charges.upsertFromStripe(c));
				const refunds = await this.listChargeRefunds(c);
				for (const r of refunds) {
					await this.safeMirrorUpsert(event, () =>
						this.billingRepository.refunds.upsertFromStripe(r, {
							customerId: typeof c.customer === 'string' ? c.customer : (c.customer?.id ?? undefined),
							livemode: event.livemode,
						}),
					);
				}
				await this.disputeHandler.handleRefund(c);
				for (const r of refunds.filter((refund) => isRecentRefund(refund, event.created))) {
					await this.queueBillingEmail({kind: 'refund', id: r.id});
				}
				break;
			}
			case 'customer.created':
			case 'customer.updated': {
				const cust = event.data.object as Stripe.Customer;
				await this.safeMirrorUpsert(event, () => this.billingRepository.customers.upsertFromStripe(cust));
				break;
			}
			case 'customer.deleted': {
				const cust = event.data.object as Stripe.Customer | Stripe.DeletedCustomer;
				await this.safeMirrorUpsert(event, () => this.billingRepository.customers.upsertFromStripe(cust));
				await this.safeMirrorUpsert(event, () => this.billingRepository.customers.markDeleted(cust.id, new Date()));
				await this.donationRepository.clearDonorStripeCustomer(cust.id);
				break;
			}
			case 'price.created':
			case 'price.updated':
			case 'price.deleted': {
				const p = event.data.object as Stripe.Price;
				await this.safeMirrorUpsert(event, () => this.billingRepository.prices.upsertFromStripe(p));
				break;
			}
			case 'payment_method.attached':
			case 'payment_method.updated':
			case 'payment_method.automatically_updated': {
				const pm = event.data.object as Stripe.PaymentMethod;
				await this.safeMirrorUpsert(event, () => this.billingRepository.paymentMethods.upsertFromStripe(pm));
				break;
			}
			case 'payment_method.detached': {
				const pm = event.data.object as Stripe.PaymentMethod;
				await this.safeMirrorUpsert(event, () => this.billingRepository.paymentMethods.markDetached(pm.id, new Date()));
				break;
			}
			case 'mandate.updated': {
				await this.handleMandateUpdated(event.data.object as Stripe.Mandate);
				break;
			}
			case 'payment_intent.created':
			case 'payment_intent.processing':
			case 'payment_intent.succeeded':
			case 'payment_intent.payment_failed':
			case 'payment_intent.canceled':
			case 'payment_intent.requires_action': {
				const pi = event.data.object as Stripe.PaymentIntent;
				await this.safeMirrorUpsert(event, () => this.billingRepository.paymentIntents.upsertFromStripe(pi));
				break;
			}
			case 'charge.succeeded':
			case 'charge.updated':
			case 'charge.failed':
			case 'charge.captured': {
				const c = event.data.object as Stripe.Charge;
				await this.safeMirrorUpsert(event, () => this.billingRepository.charges.upsertFromStripe(c));
				break;
			}
			case 'refund.created':
			case 'refund.updated':
			case 'refund.failed': {
				const r = event.data.object as Stripe.Refund;
				const customerId = await this.resolveRefundCustomerId(r);
				await this.safeMirrorUpsert(event, () =>
					this.billingRepository.refunds.upsertFromStripe(r, {
						customerId: customerId ?? undefined,
						livemode: event.livemode,
					}),
				);
				await this.refundService.handleRefundWebhookEvent(r);
				await this.queueBillingEmail({kind: 'refund', id: r.id});
				break;
			}
			case 'invoice.created':
			case 'invoice.finalized':
			case 'invoice.voided':
			case 'invoice.marked_uncollectible': {
				const inv = event.data.object as Stripe.Invoice;
				await this.mirrorInvoice(event, inv);
				break;
			}
			case 'credit_note.created': {
				const note = event.data.object as Stripe.CreditNote;
				if (note.customer_balance_transaction == null) {
					Logger.debug({creditNoteId: note.id}, 'Credit note did not add account credit; no email needed');
					break;
				}
				await this.queueBillingEmail({kind: 'credit_note', id: note.id});
				break;
			}
			case 'charge.dispute.updated': {
				const d = event.data.object as Stripe.Dispute;
				await this.disputeHandler.handleDisputeUpdated(d);
				break;
			}
			default: {
				Logger.debug({eventType: event.type, eventId: event.id}, 'Stripe webhook event type not handled');
			}
		}
	}

	private async queueBillingEmail(job: BillingEmailJob | null): Promise<void> {
		if (job) {
			await enqueueBillingEmail(getWorkerService(), job);
		}
	}

	private async queuePortalCancellation(event: Stripe.Event, subscription: Stripe.Subscription): Promise<void> {
		const previous = (event.data.previous_attributes ?? {}) as Partial<Stripe.Subscription>;
		const scheduled =
			(previous.cancel_at_period_end === false && subscription.cancel_at_period_end) ||
			('cancel_at' in previous && previous.cancel_at === null && subscription.cancel_at != null);
		if (
			!scheduled ||
			freshCancelSource(subscription, event.created) ||
			!subscriptionBillingProduct(subscription, this.productRegistry)
		) {
			return;
		}
		await this.queueBillingEmail({
			kind: 'subscription_cancel_scheduled',
			id: subscription.id,
			requestedAt: event.created,
		});
	}

	private async clearWithdrawnCancelSource(event: Stripe.Event, subscription: Stripe.Subscription): Promise<void> {
		const previous = (event.data.previous_attributes ?? {}) as Partial<Stripe.Subscription>;
		const withdrawn =
			(previous.cancel_at_period_end === true && !subscription.cancel_at_period_end) ||
			(previous.cancel_at != null && subscription.cancel_at == null);
		if (
			!withdrawn ||
			subscriptionCancelling(subscription, null) ||
			!this.stripe ||
			!subscription.metadata?.fluxer_cancel_source
		) {
			return;
		}
		try {
			await this.stripe.subscriptions.update(subscription.id, {metadata: CLEARED_CANCEL_METADATA});
		} catch (error) {
			Logger.warn({error, subscriptionId: subscription.id}, 'Could not clear the cancellation source after a resume');
		}
	}

	private async queueEndedUnpaid(subscription: Stripe.Subscription): Promise<void> {
		const reason = subscription.cancellation_details?.reason;
		if (reason === 'canceled_by_retention_policy') {
			Logger.info({subscriptionId: subscription.id}, 'Subscription ended by the retention policy; no email sent');
			return;
		}
		if (reason === 'payment_failed' && subscriptionBillingProduct(subscription, this.productRegistry) === 'plutonium') {
			await this.queueBillingEmail({kind: 'subscription_ended_unpaid', id: subscription.id});
		}
	}

	private async queueInvoicePaymentProblem(invoice: Stripe.Invoice, actionRequired: boolean): Promise<void> {
		await this.queueBillingEmail(
			invoice.id
				? {kind: 'invoice_payment_problem', id: invoice.id, paymentAttempt: invoice.attempt_count, actionRequired}
				: null,
		);
	}

	private async handleMandateUpdated(mandate: Stripe.Mandate): Promise<void> {
		if (mandate.status !== 'inactive') {
			return;
		}
		const paymentMethodId =
			typeof mandate.payment_method === 'string' ? mandate.payment_method : (mandate.payment_method?.id ?? null);
		Logger.warn(
			{mandateId: mandate.id, paymentMethodId, status: mandate.status},
			'Stripe mandate is no longer active; recurring payments on this payment method will fail',
		);
	}

	private async resolveRefundCustomerId(refund: Stripe.Refund): Promise<string | null> {
		const chargeId = typeof refund.charge === 'string' ? refund.charge : (refund.charge?.id ?? null);
		if (chargeId !== null) {
			const chargeRow = await this.billingRepository.charges.findById(chargeId);
			if (chargeRow?.customer_id != null) {
				return chargeRow.customer_id;
			}
		}
		const paymentIntentId =
			typeof refund.payment_intent === 'string' ? refund.payment_intent : (refund.payment_intent?.id ?? null);
		if (paymentIntentId !== null) {
			const paymentIntentRow = await this.billingRepository.paymentIntents.findById(paymentIntentId);
			if (paymentIntentRow?.customer_id != null) {
				return paymentIntentRow.customer_id;
			}
		}
		return null;
	}

	private async listChargeRefunds(charge: Stripe.Charge): Promise<Array<Stripe.Refund>> {
		const inlined = charge.refunds?.data ?? [];
		if (inlined.length > 0 || charge.id == null || this.stripe == null) {
			return inlined;
		}
		try {
			const listed = await this.stripe.refunds.list({charge: charge.id, limit: 100});
			return listed.data;
		} catch (listErr) {
			Logger.warn({listErr, chargeId: charge.id}, 'Failed to list refunds for charge; skipping refund mirror');
			return [];
		}
	}

	private async mirrorInvoice(event: Stripe.Event, inv: Stripe.Invoice): Promise<void> {
		let hydrated = inv;
		if (inv.payments === undefined && inv.id != null && this.stripe != null) {
			try {
				hydrated = await this.stripe.invoices.retrieve(inv.id, {expand: ['payments.data.payment']});
			} catch (hydrateErr) {
				Logger.warn(
					{hydrateErr, eventId: event.id, invoiceId: inv.id},
					'Failed to hydrate invoice payments; mirroring invoice without payment rows',
				);
			}
		}
		await this.safeMirrorUpsert(event, () => this.billingRepository.invoices.upsertFromStripe(hydrated));
	}

	private async safeMirrorUpsert(event: Stripe.Event, fn: () => Promise<unknown>): Promise<void> {
		await mirrorBestEffort(
			() => fn(),
			{eventId: event.id, eventType: event.type},
			'Mirror upsert failed; continuing with downstream handler',
		);
	}

	private async resolveKnownUserIdForSubscription(sub: Stripe.Subscription): Promise<bigint | null> {
		const metadataUserId = sub.metadata?.user_id ?? sub.metadata?.userId;
		if (typeof metadataUserId === 'string' && metadataUserId.length > 0) {
			try {
				return BigInt(metadataUserId);
			} catch {}
		}
		const customerId = typeof sub.customer === 'string' ? sub.customer : (sub.customer?.id ?? null);
		if (!customerId) {
			return null;
		}
		try {
			const customerRow = await this.billingRepository.customers.findById(customerId);
			return customerRow?.user_id ?? null;
		} catch (err) {
			Logger.warn({err, subscriptionId: sub.id, customerId}, 'Customer reverse-lookup failed for subscription mirror');
			return null;
		}
	}
}
