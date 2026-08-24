// SPDX-License-Identifier: AGPL-3.0-or-later

import {UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import {StripeError} from '@fluxer/errors/src/domains/payment/StripeError';
import type Stripe from 'stripe';
import type {UserID} from '../../BrandedTypes';
import type {BillingRepository} from '../../billing/repositories/BillingRepository';
import {nextVersion} from '../../database/CassandraTypes';
import type {UserRow} from '../../database/types/UserTypes';
import type {IDonationRepository} from '../../donation/IDonationRepository';
import {Donor} from '../../donation/models/Donor';
import type {IGatewayService} from '../../infrastructure/IGatewayService';
import type {PremiumStateReconciliationQueueService} from '../../infrastructure/PremiumStateReconciliationQueueService';
import {Logger} from '../../Logger';
import type {User} from '../../models/User';
import type {IUserRepository} from '../../user/IUserRepository';
import {PaymentRepository} from '../../user/repositories/PaymentRepository';
import {PREMIUM_GRACE_PERIOD_MS} from '../../user/UserHelpers';
import {mapUserToPrivateResponse} from '../../user/UserMappers';
import type {ProductInfo} from '../ProductRegistry';
import {
	canProvisionPremiumFromSubscriptionStatus,
	getPremiumWillCancelFromSubscription,
	shouldTreatInvoiceCollectionIssueAsAccessChange,
	shouldTreatInvoicePaymentFailureAsAccessChange,
	shouldTreatInvoiceUpdatedAsCollectionIssue,
} from '../StripeSubscriptionAccessPolicy';
import {
	getInvoiceLatestLinePeriodEnd,
	getPrimarySubscriptionItem,
	getSubscriptionItemPeriodEnd,
	getSubscriptionPremiumPeriodEnd,
	getSubscriptionStartDate,
} from '../StripeSubscriptionPeriod';
import {extractId} from '../StripeUtils';
import type {StripePremiumService} from './StripePremiumService';
import type {StripeSubscriptionReconciler} from './StripeSubscriptionReconciler';

export class StripeSubscriptionWebhookHandler {
	private readonly paymentRepository = new PaymentRepository();

	constructor(
		private userRepository: IUserRepository,
		private gatewayService: IGatewayService,
		private premiumService: StripePremiumService,
		private donationRepository: IDonationRepository,
		private premiumStateReconciliationQueueService: PremiumStateReconciliationQueueService,
		private reconciler: StripeSubscriptionReconciler,
		private billingRepository: BillingRepository,
	) {}

	async handleInvoicePaymentSucceeded(eventId: string, invoice: Stripe.Invoice): Promise<void> {
		if (invoice.billing_reason === 'subscription_create') {
			Logger.debug({invoiceId: invoice.id}, 'Skipping first invoice - handled by checkout.session.completed');
			return;
		}
		const subscriptionId = this.reconciler.getSubscriptionIdFromInvoice(invoice);
		if (!subscriptionId) {
			const billingReason = invoice.billing_reason ?? null;
			const isSubscriptionInvoice = typeof billingReason === 'string' && billingReason.startsWith('subscription');
			if (!isSubscriptionInvoice) {
				Logger.debug({invoiceId: invoice.id, billingReason}, 'Skipping invoice payment without subscription context');
				return;
			}
			Logger.error({invoiceId: invoice.id, billingReason}, 'No subscription ID found in subscription invoice');
			throw new StripeError('Invoice missing subscription id');
		}
		if (this.isSubscriptionUpdateInvoice(invoice)) {
			Logger.debug(
				{
					invoiceId: invoice.id,
					eventId,
					subscriptionId,
					billingReason: invoice.billing_reason,
					amountPaid: invoice.amount_paid,
					amountDue: invoice.amount_due,
					total: invoice.total,
				},
				'Skipping subscription_update invoice; premium_until is managed by customer.subscription.updated webhook',
			);
			return;
		}
		const renewalContext = await this.reconciler.resolveInvoiceRenewalContext(subscriptionId, invoice);
		const renewalUserBeforeGrant = await this.userRepository.findUnique(renewalContext.userId);
		if (!renewalUserBeforeGrant) {
			Logger.error(
				{invoiceId: invoice.id, eventId, subscriptionId, userId: renewalContext.userId},
				'User not found for invoice renewal',
			);
			throw new StripeError('User not found for invoice renewal');
		}
		const subscriptionPayment = await this.paymentRepository.getSubscriptionInfo(subscriptionId);
		const checkoutSessionId = subscriptionPayment?.checkout_session_id ?? null;
		const existingPayment = checkoutSessionId
			? await this.paymentRepository.getPaymentByCheckoutSession(checkoutSessionId)
			: null;
		if (invoice.id && existingPayment?.invoiceId === invoice.id) {
			Logger.debug(
				{invoiceId: invoice.id, eventId, subscriptionId, userId: renewalContext.userId},
				'Skipping already-applied invoice renewal',
			);
			return;
		}
		const subscriptionSnapshot = await this.reconciler.getSubscriptionSnapshotById(subscriptionId);
		const premiumSinceAnchor = subscriptionSnapshot ? getSubscriptionStartDate(subscriptionSnapshot) : null;
		const renewalPeriodEnd =
			(subscriptionSnapshot ? getSubscriptionPremiumPeriodEnd(subscriptionSnapshot) : null) ??
			getInvoiceLatestLinePeriodEnd(invoice);
		if (!renewalPeriodEnd) {
			Logger.error(
				{invoiceId: invoice.id, eventId, subscriptionId, userId: renewalContext.userId},
				'Invoice renewal missing both subscription snapshot and invoice period_end',
			);
			throw new StripeError('Invoice renewal missing period end');
		}
		let recoveredFromPartialFailure = false;
		try {
			await this.premiumService.setPremiumFromSubscriptionPeriod(
				renewalContext.userId,
				renewalContext.productInfo.premiumType,
				renewalPeriodEnd,
				renewalContext.productInfo.billingCycle || null,
				true,
				premiumSinceAnchor,
			);
		} catch (error) {
			const latestUser = await this.userRepository.findUnique(renewalContext.userId);
			if (!this.didRenewalApply(renewalUserBeforeGrant, latestUser, renewalPeriodEnd)) {
				throw error;
			}
			recoveredFromPartialFailure = true;
			Logger.warn(
				{invoiceId: invoice.id, eventId, subscriptionId, userId: renewalContext.userId},
				'Invoice renewal grant threw after state update; treating as already applied for idempotency',
			);
		}
		if (invoice.id && checkoutSessionId) {
			await this.paymentRepository.updatePayment({
				checkout_session_id: checkoutSessionId,
				invoice_id: invoice.id,
			});
		}
		if (subscriptionSnapshot) {
			await this.mirrorSubscriptionSnapshot(subscriptionSnapshot, renewalContext.userId, 'invoice_payment_succeeded');
			await this.reconcilePaidSubscriptionState(
				renewalContext.userId,
				subscriptionSnapshot,
				renewalContext.productInfo,
			);
		}
		await this.enqueuePremiumStateReconciliation(renewalContext.userId, {
			reason: 'invoice_payment_succeeded',
			subscriptionId,
		});
		Logger.debug(
			{
				userId: renewalContext.userId,
				invoiceId: invoice.id,
				eventId,
				subscriptionId,
				durationMonths: renewalContext.productInfo.durationMonths,
				reconciledFromStripe: renewalContext.reconciledFromStripe,
				recoveredFromPartialFailure,
			},
			'Subscription renewed from invoice payment',
		);
	}

	private isSubscriptionUpdateInvoice(invoice: Stripe.Invoice): boolean {
		return invoice.billing_reason === 'subscription_update';
	}

	private async reconcilePaidSubscriptionState(
		userId: UserID,
		subscription: Stripe.Subscription,
		productInfo: ProductInfo,
	): Promise<void> {
		const user = await this.userRepository.findUnique(userId);
		if (!user) {
			Logger.error({userId, subscriptionId: subscription.id}, 'User not found for paid subscription reconciliation');
			throw new StripeError('User not found for paid subscription reconciliation');
		}
		const premiumSince = getSubscriptionStartDate(subscription);
		const premiumUntil = getSubscriptionPremiumPeriodEnd(subscription);
		const willCancel = getPremiumWillCancelFromSubscription(subscription);
		const customerId = extractId(subscription.customer);
		const billingCycle = productInfo.billingCycle || this.reconciler.getBillingCycleFromSubscription(subscription);
		const patch: Partial<UserRow> = {};
		if (user.premiumType !== productInfo.premiumType) {
			patch.premium_type = productInfo.premiumType;
		}
		if (!user.premiumSince || user.premiumSince.getTime() > premiumSince.getTime()) {
			patch.premium_since = premiumSince;
		}
		if (premiumUntil && user.premiumUntil?.getTime() !== premiumUntil.getTime()) {
			patch.premium_until = premiumUntil;
		}
		if (user.premiumWillCancel !== willCancel) {
			patch.premium_will_cancel = willCancel;
		}
		if (user.premiumGraceEndsAt) {
			patch.premium_grace_ends_at = null;
		}
		if (billingCycle && user.premiumBillingCycle !== billingCycle) {
			patch.premium_billing_cycle = billingCycle;
		}
		if (user.stripeSubscriptionId !== subscription.id) {
			patch.stripe_subscription_id = subscription.id;
		}
		if (customerId && user.stripeCustomerId !== customerId) {
			patch.stripe_customer_id = customerId;
		}
		if (Object.keys(patch).length === 0) {
			return;
		}
		const updatedUser = await this.userRepository.patchUpsert(user.id, patch, user.toRow());
		await this.dispatchUser(updatedUser);
	}

	async handleInvoicePaymentFailed(invoice: Stripe.Invoice): Promise<void> {
		if (!shouldTreatInvoicePaymentFailureAsAccessChange(invoice)) {
			Logger.debug(
				{
					invoiceId: invoice.id,
					billingReason: invoice.billing_reason,
					subscriptionId: this.reconciler.getSubscriptionIdFromInvoice(invoice),
				},
				'Ignoring invoice.payment_failed that does not change recurring access state',
			);
			return;
		}
		await this.handleInvoiceCollectionIssue(invoice, {
			reason: 'invoice_payment_failed',
			logMessage: 'Recorded recurring invoice payment failure and disabled subscription grace period',
		});
	}

	async handleInvoicePaymentActionRequired(invoice: Stripe.Invoice): Promise<void> {
		if (!shouldTreatInvoiceCollectionIssueAsAccessChange(invoice)) {
			Logger.debug(
				{
					invoiceId: invoice.id,
					billingReason: invoice.billing_reason,
					subscriptionId: this.reconciler.getSubscriptionIdFromInvoice(invoice),
				},
				'Ignoring invoice.payment_action_required that does not change recurring access state',
			);
			return;
		}
		await this.handleInvoiceCollectionIssue(invoice, {
			reason: 'invoice_payment_action_required',
			logMessage: 'Recorded recurring invoice payment action requirement and disabled subscription grace period',
		});
	}

	async handleInvoiceFinalizationFailed(invoice: Stripe.Invoice): Promise<void> {
		if (!shouldTreatInvoiceCollectionIssueAsAccessChange(invoice)) {
			Logger.debug(
				{
					invoiceId: invoice.id,
					billingReason: invoice.billing_reason,
					subscriptionId: this.reconciler.getSubscriptionIdFromInvoice(invoice),
				},
				'Ignoring invoice.finalization_failed that does not change recurring access state',
			);
			return;
		}
		await this.handleInvoiceCollectionIssue(invoice, {
			reason: 'invoice_finalization_failed',
			logMessage: 'Recorded recurring invoice finalization failure and disabled subscription grace period',
		});
	}

	async handleInvoiceUpdated(invoice: Stripe.Invoice): Promise<void> {
		if (!shouldTreatInvoiceUpdatedAsCollectionIssue(invoice)) {
			Logger.debug(
				{
					invoiceId: invoice.id,
					billingReason: invoice.billing_reason,
					status: invoice.status,
					attempted: invoice.attempted,
					attemptCount: invoice.attempt_count,
					nextPaymentAttempt: invoice.next_payment_attempt,
				},
				'Ignoring invoice.updated that does not indicate a recurring collection issue',
			);
			return;
		}
		await this.handleInvoiceCollectionIssue(invoice, {
			reason: 'invoice_updated_collection_issue',
			logMessage:
				'Recorded recurring invoice collection issue from invoice.updated and disabled subscription grace period',
		});
	}

	private async handleInvoiceCollectionIssue(
		invoice: Stripe.Invoice,
		context: {
			reason: string;
			logMessage: string;
		},
	): Promise<void> {
		const subscriptionId = this.reconciler.getSubscriptionIdFromInvoice(invoice);
		const targetUser = await this.reconciler.resolveUserForInvoiceReconciliation(invoice, {
			subscriptionId,
			reason: context.reason,
		});
		if (!targetUser) {
			Logger.warn(
				{
					invoiceId: invoice.id,
					subscriptionId,
					customerId: this.reconciler.getCustomerIdFromInvoice(invoice),
					billingReason: invoice.billing_reason,
					reason: context.reason,
				},
				'Unable to resolve user for invoice collection issue access update',
			);
			return;
		}
		const updatedUser = await this.markSubscriptionAsGraceDisabled(targetUser, {
			subscriptionId,
			customerId: this.reconciler.getCustomerIdFromInvoice(invoice),
			failedInvoiceServicePeriod: this.getInvoiceServicePeriod(invoice),
		});
		await this.enqueuePremiumStateReconciliation(updatedUser.id, {
			reason: context.reason,
			subscriptionId: subscriptionId ?? undefined,
		});
		Logger.info(
			{
				userId: updatedUser.id,
				invoiceId: invoice.id,
				subscriptionId,
				billingReason: invoice.billing_reason,
				attemptCount: invoice.attempt_count,
				status: invoice.status,
				nextPaymentAttempt: invoice.next_payment_attempt,
				reason: context.reason,
			},
			context.logMessage,
		);
	}

	async handleSubscriptionUpdated(subscription: Stripe.Subscription, snapshotCapturedAt?: Date): Promise<void> {
		Logger.debug(
			{
				subscriptionId: subscription.id,
				customerId: extractId(subscription.customer),
				status: subscription.status,
				cancelAt: subscription.cancel_at,
				cancelAtPeriodEnd: subscription.cancel_at_period_end,
				trialEnd: subscription.trial_end,
			},
			'Processing Stripe subscription.updated webhook',
		);
		const donor = await this.donationRepository.findDonorByStripeSubscriptionId(subscription.id);
		if (donor) {
			Logger.debug(
				{subscriptionId: subscription.id, donorEmail: donor.email},
				'Routing subscription update to donor flow',
			);
			await this.handleDonationSubscriptionUpdated(subscription, donor);
			return;
		}
		let canonicalSubscription = subscription;
		const subscriptionInfo = await this.userRepository.getSubscriptionInfo(subscription.id);
		Logger.debug(
			{
				subscriptionId: subscription.id,
				hasSubscriptionInfo: Boolean(subscriptionInfo),
				mappedUserId: subscriptionInfo?.user_id,
			},
			'Loaded subscription mapping for subscription update',
		);
		let targetUser: User | null = null;
		if (subscriptionInfo) {
			targetUser = await this.userRepository.findUnique(subscriptionInfo.user_id);
		}
		if (targetUser && this.reconciler.hasSubscriptionIdentityMismatch(targetUser, canonicalSubscription)) {
			Logger.warn(
				{
					subscriptionId: subscription.id,
					userId: targetUser.id,
					userStripeSubscriptionId: targetUser.stripeSubscriptionId,
					userStripeCustomerId: targetUser.stripeCustomerId,
					webhookCustomerId: extractId(canonicalSubscription.customer),
				},
				'Subscription update mapping user does not match webhook identity; reconciling from Stripe',
			);
			targetUser = null;
		}
		if (!targetUser) {
			canonicalSubscription = await this.reconciler.getLatestSubscriptionSnapshot(subscription);
			Logger.debug(
				{
					subscriptionId: subscription.id,
					reconciledSubscriptionId: canonicalSubscription.id,
					reconciledCustomerId: extractId(canonicalSubscription.customer),
					reconciledTrialEnd: canonicalSubscription.trial_end,
				},
				'Resolved canonical subscription snapshot for update reconciliation',
			);
			targetUser = await this.reconciler.resolveUserForSubscriptionReconciliation(canonicalSubscription, {
				allowStripeFetch: false,
				reason: subscriptionInfo ? 'subscription_update_identity_mismatch' : 'subscription_update_missing_mapping',
			});
		}
		if (!targetUser) {
			Logger.error({subscriptionId: subscription.id}, 'No user found for subscription update');
			throw new StripeError('No subscription info found for subscription update');
		}
		targetUser = await this.reconciler.reconcileUserWithSubscriptionState(targetUser, canonicalSubscription);
		await this.mirrorSubscriptionSnapshot(
			canonicalSubscription,
			targetUser.id,
			'subscription_updated',
			snapshotCapturedAt,
		);
		Logger.debug(
			{
				subscriptionId: canonicalSubscription.id,
				userId: targetUser.id,
				userStripeSubscriptionId: targetUser.stripeSubscriptionId,
				userStripeCustomerId: targetUser.stripeCustomerId,
			},
			'Reconciled user Stripe identity from subscription update',
		);
		type SubscriptionWithPendingUpdate = Stripe.Subscription & {
			pending_update?: {
				expires_at?: number | null;
			} | null;
		};
		const pendingUpdate = (canonicalSubscription as SubscriptionWithPendingUpdate).pending_update ?? null;
		if (pendingUpdate) {
			Logger.warn(
				{
					subscriptionId: canonicalSubscription.id,
					userId: targetUser.id,
					status: canonicalSubscription.status,
					pendingUpdateExpiresAt: pendingUpdate.expires_at ?? null,
				},
				'Subscription update contains pending_update; keeping current subscription state and enqueuing reconciliation',
			);
			await this.enqueuePremiumStateReconciliation(targetUser.id, {
				reason: 'subscription_updated_pending_update',
				subscriptionId: canonicalSubscription.id,
			});
		}
		const willCancel = getPremiumWillCancelFromSubscription(canonicalSubscription);
		if (!canProvisionPremiumFromSubscriptionStatus(canonicalSubscription.status)) {
			const updatedUser = await this.markSubscriptionAsGraceDisabled(targetUser, {
				subscriptionId: canonicalSubscription.id,
				customerId: extractId(canonicalSubscription.customer),
				failedInvoiceServicePeriod: null,
			});
			await this.enqueuePremiumStateReconciliation(updatedUser.id, {
				reason: 'subscription_updated_non_provisionable',
				subscriptionId: canonicalSubscription.id,
			});
			Logger.info(
				{
					userId: updatedUser.id,
					subscriptionId: canonicalSubscription.id,
					status: canonicalSubscription.status,
					willCancel,
				},
				'Subscription updated in non-provisionable state; preserved local expiry and disabled grace period',
			);
			return;
		}
		let computedPremiumUntil = getSubscriptionPremiumPeriodEnd(canonicalSubscription);
		Logger.debug(
			{
				subscriptionId: canonicalSubscription.id,
				userId: targetUser.id,
				willCancel,
				computedPremiumUntil,
			},
			'Computed premium_until from initial subscription snapshot',
		);
		if (!computedPremiumUntil) {
			canonicalSubscription = await this.reconciler.getLatestSubscriptionSnapshot(canonicalSubscription);
			computedPremiumUntil = getSubscriptionPremiumPeriodEnd(canonicalSubscription);
			Logger.debug(
				{
					subscriptionId: canonicalSubscription.id,
					userId: targetUser.id,
					computedPremiumUntil,
				},
				'Recomputed premium_until after refreshing subscription snapshot',
			);
		}
		if (!computedPremiumUntil) {
			Logger.error({subscriptionId: subscription.id}, 'Subscription update missing period end');
			throw new StripeError('Subscription update missing period end');
		}
		const result = await this.userRepository.updateSubscriptionStatus(targetUser.id, {
			premiumWillCancel: willCancel,
			computedPremiumUntil,
		});
		if (result.finalVersion === null) {
			Logger.error(
				{subscriptionId: subscription.id, userId: targetUser.id},
				'Failed to update subscription status after retries',
			);
			throw new StripeError('Failed to update subscription status');
		}
		const updatedUser = await this.userRepository.findUnique(targetUser.id);
		if (!updatedUser) {
			Logger.error({subscriptionId: subscription.id, userId: targetUser.id}, 'Updated user not found');
			throw new StripeError('Updated user not found for subscription update');
		}
		await this.dispatchUser(updatedUser);
		await this.enqueuePremiumStateReconciliation(updatedUser.id, {
			reason: 'subscription_updated',
			subscriptionId: canonicalSubscription.id,
		});
		Logger.debug(
			{
				userId: targetUser.id,
				subscriptionId: subscription.id,
				willCancel,
				computedPremiumUntil,
				status: canonicalSubscription.status,
			},
			'Subscription updated (preserved gifted extension)',
		);
	}

	private didRenewalApply(initialUser: User, latestUser: User | null, targetPremiumUntil: Date): boolean {
		if (!latestUser) {
			return false;
		}
		const latestUntil = latestUser.premiumUntil?.getTime() ?? 0;
		const initialUntil = initialUser.premiumUntil?.getTime() ?? 0;
		return latestUntil >= targetPremiumUntil.getTime() && latestUntil > initialUntil;
	}

	async handleSubscriptionDeleted(subscription: Stripe.Subscription): Promise<void> {
		const donor = await this.donationRepository.findDonorByStripeSubscriptionId(subscription.id);
		if (donor) {
			const updatedDonor = new Donor({
				...donor.toRow(),
				stripe_subscription_id: null,
				subscription_amount_cents: null,
				subscription_currency: null,
				subscription_interval: null,
				subscription_current_period_end: null,
				subscription_cancel_at: null,
				updated_at: new Date(),
				version: nextVersion(donor.version),
			});
			await this.donationRepository.upsertDonor(updatedDonor);
			Logger.info({email: donor.email, subscriptionId: subscription.id}, 'Donation subscription deleted');
			return;
		}
		const info = await this.userRepository.getSubscriptionInfo(subscription.id);
		let targetUser: User | null = null;
		if (info) {
			targetUser = await this.userRepository.findUnique(info.user_id);
		}
		if (targetUser && this.reconciler.hasSubscriptionIdentityMismatch(targetUser, subscription)) {
			Logger.warn(
				{
					subscriptionId: subscription.id,
					userId: targetUser.id,
					userStripeSubscriptionId: targetUser.stripeSubscriptionId,
					userStripeCustomerId: targetUser.stripeCustomerId,
					webhookCustomerId: extractId(subscription.customer),
				},
				'Subscription delete mapping user does not match webhook identity; reconciling from Stripe',
			);
			targetUser = null;
		}
		if (!targetUser) {
			targetUser = await this.reconciler.resolveUserForSubscriptionReconciliation(subscription, {
				allowStripeFetch: true,
				reason: info ? 'subscription_delete_identity_mismatch' : 'subscription_delete_missing_mapping',
			});
		}
		if (!targetUser) {
			Logger.error({subscriptionId: subscription.id}, 'Subscription delete missing subscription info');
			throw new StripeError('Subscription delete missing subscription info');
		}
		const updates: Partial<UserRow> = {
			premium_will_cancel: false,
			stripe_subscription_id: null,
			premium_billing_cycle: null,
		};
		if (targetUser.premiumType === UserPremiumTypes.SUBSCRIPTION) {
			const subscriptionEndedAt = subscription.ended_at ? new Date(subscription.ended_at * 1000) : new Date();
			const alreadyAppliedEarlyCancellation =
				targetUser.premiumUntil?.getTime() === subscriptionEndedAt.getTime() &&
				targetUser.premiumGraceEndsAt?.getTime() === subscriptionEndedAt.getTime();
			if (!alreadyAppliedEarlyCancellation) {
				const cancelledBeforePeriodEnd =
					targetUser.premiumUntil != null && subscriptionEndedAt.getTime() < targetUser.premiumUntil.getTime();
				if (cancelledBeforePeriodEnd) {
					updates.premium_until = subscriptionEndedAt;
					updates.premium_grace_ends_at = subscriptionEndedAt;
				} else {
					updates.premium_grace_ends_at = new Date(subscriptionEndedAt.getTime() + PREMIUM_GRACE_PERIOD_MS);
				}
			}
		}
		const updatedUser = await this.userRepository.patchUpsert(targetUser.id, updates, targetUser.toRow());
		await this.dispatchUser(updatedUser);
		await this.enqueuePremiumStateReconciliation(updatedUser.id, {
			reason: 'subscription_deleted',
			subscriptionId: subscription.id,
		});
	}

	private async handleDonationSubscriptionUpdated(subscription: Stripe.Subscription, donor: Donor): Promise<void> {
		const item = getPrimarySubscriptionItem(subscription);
		if (!item?.price?.recurring || item.price.unit_amount == null || !item.price.currency) {
			Logger.error({subscriptionId: subscription.id}, 'Donation subscription update missing pricing details');
			throw new StripeError('Donation subscription update missing pricing details');
		}
		const currentPeriodEnd = getSubscriptionItemPeriodEnd(item);
		if (!currentPeriodEnd) {
			Logger.error({subscriptionId: subscription.id}, 'Donation subscription update missing period end');
			throw new StripeError('Donation subscription update missing period end');
		}
		const amountCents = item.price.unit_amount;
		const currency = item.price.currency;
		const interval = item.price.recurring.interval;
		const cancelAt = subscription.cancel_at ? new Date(subscription.cancel_at * 1000) : null;
		await this.donationRepository.updateDonorSubscription(donor.email, {
			stripeCustomerId: donor.stripeCustomerId,
			stripeSubscriptionId: subscription.id,
			subscriptionAmountCents: amountCents,
			subscriptionCurrency: currency,
			subscriptionInterval: interval,
			subscriptionCurrentPeriodEnd: currentPeriodEnd,
			subscriptionCancelAt: cancelAt,
		});
		Logger.debug(
			{
				email: donor.email,
				subscriptionId: subscription.id,
				currentPeriodEnd,
				status: subscription.status,
			},
			'Donation subscription updated',
		);
	}

	private async markSubscriptionAsGraceDisabled(
		user: User,
		context: {
			subscriptionId: string | null;
			customerId: string | null;
			failedInvoiceServicePeriod: {
				start: Date;
				end: Date;
			} | null;
		},
	): Promise<User> {
		const patch: Partial<UserRow> = {};
		if (context.subscriptionId && user.stripeSubscriptionId !== context.subscriptionId) {
			patch.stripe_subscription_id = context.subscriptionId;
		}
		if (context.customerId && user.stripeCustomerId !== context.customerId) {
			patch.stripe_customer_id = context.customerId;
		}
		if (user.premiumType === UserPremiumTypes.SUBSCRIPTION && user.premiumWillCancel !== true) {
			patch.premium_will_cancel = true;
		}
		if (
			user.premiumType === UserPremiumTypes.SUBSCRIPTION &&
			user.premiumUntil &&
			context.failedInvoiceServicePeriod &&
			user.premiumUntil.getTime() > context.failedInvoiceServicePeriod.start.getTime() &&
			user.premiumUntil.getTime() <= context.failedInvoiceServicePeriod.end.getTime()
		) {
			patch.premium_until = context.failedInvoiceServicePeriod.start;
		}
		if (Object.keys(patch).length === 0) {
			return user;
		}
		const updatedUser = await this.userRepository.patchUpsert(user.id, patch, user.toRow());
		await this.dispatchUser(updatedUser);
		return updatedUser;
	}

	private getInvoiceServicePeriod(invoice: Stripe.Invoice): {
		start: Date;
		end: Date;
	} | null {
		if (!invoice.lines?.data?.length) {
			return null;
		}
		let periodStart: number | null = null;
		let periodEnd: number | null = null;
		for (const line of invoice.lines.data) {
			if (!line.period?.start || !line.period?.end) {
				continue;
			}
			if (periodStart == null || line.period.start < periodStart) {
				periodStart = line.period.start;
			}
			if (periodEnd == null || line.period.end > periodEnd) {
				periodEnd = line.period.end;
			}
		}
		if (periodStart == null || periodEnd == null) {
			return null;
		}
		return {
			start: new Date(periodStart * 1000),
			end: new Date(periodEnd * 1000),
		};
	}

	private async mirrorSubscriptionSnapshot(
		subscription: Stripe.Subscription,
		userId: UserID,
		reason: string,
		snapshotCapturedAt?: Date,
	): Promise<void> {
		try {
			await this.billingRepository.subscriptions.upsertFromStripe(subscription, {
				knownUserId: userId,
				snapshotCapturedAt,
			});
		} catch (error) {
			Logger.error(
				{error, userId: userId.toString(), subscriptionId: subscription.id, reason},
				'Failed to refresh billing subscription mirror from Stripe snapshot',
			);
		}
	}

	private async dispatchUser(user: User): Promise<void> {
		await this.gatewayService.dispatchPresence({
			userId: user.id,
			event: 'USER_UPDATE',
			data: mapUserToPrivateResponse(user),
		});
	}

	private async enqueuePremiumStateReconciliation(
		userId: UserID,
		context: {
			reason: string;
			subscriptionId?: string;
		},
	): Promise<void> {
		try {
			await this.premiumStateReconciliationQueueService.enqueueUser(userId);
		} catch (error) {
			Logger.warn(
				{
					error,
					userId: userId.toString(),
					reason: context.reason,
					subscriptionId: context.subscriptionId,
				},
				'Failed to enqueue premium reconciliation from Stripe webhook',
			);
		}
	}
}
