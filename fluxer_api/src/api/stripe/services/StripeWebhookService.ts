// SPDX-License-Identifier: AGPL-3.0-or-later

import {StripeError} from '@fluxer/errors/src/domains/payment/StripeError';
import {StripeWebhookNotAvailableError} from '@fluxer/errors/src/domains/payment/StripeWebhookNotAvailableError';
import {StripeWebhookSignatureInvalidError} from '@fluxer/errors/src/domains/payment/StripeWebhookSignatureInvalidError';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import type {IEmailService} from '@pkgs/email/src/IEmailService';
import type Stripe from 'stripe';
import type {AdminRepository} from '../../admin/AdminRepository';
import {AdminAuditService} from '../../admin/services/AdminAuditService';
import type {ISessionTerminator} from '../../auth/ISessionTerminator';
import type {BillingRepository} from '../../billing/repositories/BillingRepository';
import {Config} from '../../Config';
import type {IDonationRepository} from '../../donation/IDonationRepository';
import type {IGatewayService} from '../../infrastructure/IGatewayService';
import type {ISnowflakeService} from '../../infrastructure/ISnowflakeService';
import type {KVAccountDeletionQueueService} from '../../infrastructure/KVAccountDeletionQueueService';
import type {PremiumStateReconciliationQueueService} from '../../infrastructure/PremiumStateReconciliationQueueService';
import type {UserCacheService} from '../../infrastructure/UserCacheService';
import {Logger} from '../../Logger';
import type {IUserRepository} from '../../user/IUserRepository';
import type {ProductRegistry} from '../ProductRegistry';
import type {AgeVerificationService} from './AgeVerificationService';
import type {StripeCheckoutService} from './StripeCheckoutService';
import {StripeCheckoutWebhookHandler} from './StripeCheckoutWebhookHandler';
import {StripeDisputeWebhookHandler} from './StripeDisputeWebhookHandler';
import {StripeGiftReversalHandler} from './StripeGiftReversalHandler';
import type {StripeGiftService} from './StripeGiftService';
import {StripePaymentFraudService} from './StripePaymentFraudService';
import type {StripePremiumService} from './StripePremiumService';
import type {StripeRefundService} from './StripeRefundService';
import {StripeSubscriptionReconciler} from './StripeSubscriptionReconciler';
import {StripeSubscriptionWebhookHandler} from './StripeSubscriptionWebhookHandler';

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
		private checkoutService: StripeCheckoutService,
		userRepository: IUserRepository,
		userCacheService: UserCacheService,
		sessionTerminator: ISessionTerminator,
		emailService: IEmailService,
		gatewayService: IGatewayService,
		productRegistry: ProductRegistry,
		cacheService: ICacheService,
		giftService: StripeGiftService,
		premiumService: StripePremiumService,
		donationRepository: IDonationRepository,
		kvDeletionQueue: KVAccountDeletionQueueService,
		premiumStateReconciliationQueueService: PremiumStateReconciliationQueueService,
		private ageVerificationService: AgeVerificationService | null,
		adminRepository: AdminRepository,
		snowflakeService: ISnowflakeService,
		private billingRepository: BillingRepository,
		private refundService: StripeRefundService,
	) {
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
		);
		const reconciler = new StripeSubscriptionReconciler(stripe, userRepository, productRegistry);
		const giftReversalHandler = new StripeGiftReversalHandler(
			userRepository,
			gatewayService,
			premiumService,
			premiumStateReconciliationQueueService,
		);
		const auditService = new AdminAuditService(adminRepository, snowflakeService);
		this.paymentFraudService = new StripePaymentFraudService({
			stripe,
			userRepository,
			userCacheService,
			sessionTerminator,
			emailService,
			gatewayService,
			donationRepository,
			cacheService,
			auditService,
			kvDeletionQueue,
		});
		this.subscriptionHandler = new StripeSubscriptionWebhookHandler(
			userRepository,
			gatewayService,
			premiumService,
			donationRepository,
			premiumStateReconciliationQueueService,
			reconciler,
			billingRepository,
		);
		this.disputeHandler = new StripeDisputeWebhookHandler(
			userRepository,
			userCacheService,
			emailService,
			gatewayService,
			donationRepository,
			kvDeletionQueue,
			giftReversalHandler,
			this.paymentFraudService,
		);
	}

	async handleWebhook({body, signature}: HandleWebhookParams): Promise<void> {
		if (!this.stripe || !Config.stripe.webhookSecret) {
			throw new StripeWebhookNotAvailableError();
		}
		let event: Stripe.Event;
		try {
			const SEVEN_DAYS_SECONDS = 7 * 24 * 60 * 60;
			event = this.stripe.webhooks.constructEvent(body, signature, Config.stripe.webhookSecret, SEVEN_DAYS_SECONDS);
		} catch (error: unknown) {
			Logger.error({error}, 'Invalid webhook signature');
			throw new StripeWebhookSignatureInvalidError();
		}
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
				await this.safeMirrorUpsert(event, () =>
					this.billingRepository.checkoutSessions.upsertFromStripe(checkoutSession, {eventCreated: event.created}),
				);
				if (checkoutSession.metadata?.verification_type === 'uk_age_verification' && this.ageVerificationService) {
					await this.ageVerificationService.completeVerification(checkoutSession);
					break;
				}
				if (checkoutSession.metadata?.setup_type === 'localized_card_preapproval') {
					await this.checkoutService.completeLocalizedCardPreapproval(checkoutSession);
					break;
				}
				await this.checkoutHandler.handleCheckoutSessionCompleted(checkoutSession);
				break;
			}
			case 'checkout.session.async_payment_succeeded': {
				const cs = event.data.object as Stripe.Checkout.Session;
				await this.safeMirrorUpsert(event, () =>
					this.billingRepository.checkoutSessions.upsertFromStripe(cs, {eventCreated: event.created}),
				);
				await this.checkoutHandler.handleAsyncPaymentSucceeded(cs);
				break;
			}
			case 'checkout.session.async_payment_failed': {
				const cs = event.data.object as Stripe.Checkout.Session;
				await this.safeMirrorUpsert(event, () =>
					this.billingRepository.checkoutSessions.upsertFromStripe(cs, {eventCreated: event.created}),
				);
				await this.checkoutHandler.handleAsyncPaymentFailed(cs);
				break;
			}
			case 'invoice.paid':
			case 'invoice.payment_succeeded': {
				const inv = event.data.object as Stripe.Invoice;
				await this.mirrorInvoice(event, inv);
				await this.subscriptionHandler.handleInvoicePaymentSucceeded(event.id, inv);
				break;
			}
			case 'invoice.payment_failed': {
				const inv = event.data.object as Stripe.Invoice;
				await this.mirrorInvoice(event, inv);
				await this.subscriptionHandler.handleInvoicePaymentFailed(inv);
				break;
			}
			case 'invoice.payment_action_required': {
				const inv = event.data.object as Stripe.Invoice;
				await this.mirrorInvoice(event, inv);
				await this.subscriptionHandler.handleInvoicePaymentActionRequired(inv);
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
				break;
			}
			case 'charge.dispute.created': {
				const d = event.data.object as Stripe.Dispute;
				await this.safeMirrorUpsert(event, () => this.billingRepository.disputes.upsertFromStripe(d));
				await this.disputeHandler.handleChargebackCreated(d);
				break;
			}
			case 'charge.dispute.closed': {
				const d = event.data.object as Stripe.Dispute;
				await this.safeMirrorUpsert(event, () => this.billingRepository.disputes.upsertFromStripe(d));
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
				break;
			}
			case 'product.created':
			case 'product.updated':
			case 'product.deleted': {
				const p = event.data.object as Stripe.Product;
				await this.safeMirrorUpsert(event, () => this.billingRepository.products.upsertFromStripe(p));
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
			case 'checkout.session.expired': {
				const cs = event.data.object as Stripe.Checkout.Session;
				await this.safeMirrorUpsert(event, () =>
					this.billingRepository.checkoutSessions.upsertFromStripe(cs, {eventCreated: event.created}),
				);
				break;
			}
			case 'charge.dispute.updated':
			case 'charge.dispute.funds_reinstated':
			case 'charge.dispute.funds_withdrawn': {
				const d = event.data.object as Stripe.Dispute;
				await this.safeMirrorUpsert(event, () => this.billingRepository.disputes.upsertFromStripe(d));
				break;
			}
			default: {
				Logger.debug({eventType: event.type, eventId: event.id}, 'Stripe webhook event type not handled');
			}
		}
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
		try {
			await fn();
		} catch (mirrorErr) {
			Logger.error(
				{mirrorErr, eventId: event.id, eventType: event.type},
				'Mirror upsert failed; continuing with downstream handler',
			);
		}
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
