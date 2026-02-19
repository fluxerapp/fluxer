/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type {AuthService} from '@fluxer/api/src/auth/AuthService';
import type {UserID} from '@fluxer/api/src/BrandedTypes';
import {Config} from '@fluxer/api/src/Config';
import {nextVersion} from '@fluxer/api/src/database/Cassandra';
import type {UserRow} from '@fluxer/api/src/database/types/UserTypes';
import type {IDonationRepository} from '@fluxer/api/src/donation/IDonationRepository';
import {Donor} from '@fluxer/api/src/donation/models/Donor';
import type {IGatewayService} from '@fluxer/api/src/infrastructure/IGatewayService';
import type {UserCacheService} from '@fluxer/api/src/infrastructure/UserCacheService';
import {Logger} from '@fluxer/api/src/Logger';
import type {GiftCode} from '@fluxer/api/src/models/GiftCode';
import type {User} from '@fluxer/api/src/models/User';
import type {ProductRegistry} from '@fluxer/api/src/stripe/ProductRegistry';
import {
	getPrimarySubscriptionItem,
	getSubscriptionCurrentPeriodEnd,
	getSubscriptionItemPeriodEnd,
} from '@fluxer/api/src/stripe/StripeSubscriptionPeriod';
import {extractId} from '@fluxer/api/src/stripe/StripeUtils';
import type {StripeGiftService} from '@fluxer/api/src/stripe/services/StripeGiftService';
import type {StripePremiumService} from '@fluxer/api/src/stripe/services/StripePremiumService';
import type {IUserRepository} from '@fluxer/api/src/user/IUserRepository';
import {mapUserToPrivateResponse} from '@fluxer/api/src/user/UserMappers';
import {DeletionReasons} from '@fluxer/constants/src/Core';
import {UserFlags, UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import type {IEmailService} from '@fluxer/email/src/IEmailService';
import {StripeError} from '@fluxer/errors/src/domains/payment/StripeError';
import {StripeWebhookNotAvailableError} from '@fluxer/errors/src/domains/payment/StripeWebhookNotAvailableError';
import {StripeWebhookSignatureInvalidError} from '@fluxer/errors/src/domains/payment/StripeWebhookSignatureInvalidError';
import {ms} from 'itty-time';
import type Stripe from 'stripe';

export interface HandleWebhookParams {
	body: string;
	signature: string;
}

interface DonationCustomerDetails {
	businessName: string | null;
	taxId: string | null;
	taxIdType: string | null;
}

interface DonationSubscriptionDetails {
	amountCents: number | null;
	currency: string | null;
	interval: string | null;
	currentPeriodEnd: Date | null;
	cancelAt: Date | null;
}

export class StripeWebhookService {
	constructor(
		private stripe: Stripe | null,
		private userRepository: IUserRepository,
		private userCacheService: UserCacheService,
		private authService: AuthService,
		private emailService: IEmailService,
		private gatewayService: IGatewayService,
		private productRegistry: ProductRegistry,
		private giftService: StripeGiftService,
		private premiumService: StripePremiumService,
		private donationRepository: IDonationRepository,
	) {}

	async handleWebhook({body, signature}: HandleWebhookParams): Promise<void> {
		if (!this.stripe || !Config.stripe.webhookSecret) {
			throw new StripeWebhookNotAvailableError();
		}

		let event: Stripe.Event;

		try {
			event = this.stripe.webhooks.constructEvent(body, signature, Config.stripe.webhookSecret);
		} catch (error: unknown) {
			Logger.error({error}, 'Invalid webhook signature');
			throw new StripeWebhookSignatureInvalidError();
		}

		Logger.debug({eventType: event.type, eventId: event.id}, 'Processing Stripe webhook');

		try {
			switch (event.type) {
				case 'checkout.session.completed':
					await this.handleCheckoutSessionCompleted(event.data.object as Stripe.Checkout.Session);
					break;

				case 'invoice.payment_succeeded':
					await this.handleInvoicePaymentSucceeded(event.data.object as Stripe.Invoice);
					break;

				case 'customer.subscription.updated':
					await this.handleSubscriptionUpdated(event.data.object as Stripe.Subscription);
					break;

				case 'customer.subscription.deleted':
					await this.handleSubscriptionDeleted(event.data.object as Stripe.Subscription);
					break;

				case 'charge.dispute.created':
					await this.handleChargebackCreated(event.data.object as Stripe.Dispute);
					break;

				case 'charge.dispute.closed':
					await this.handleChargebackClosed(event.data.object as Stripe.Dispute);
					break;

				case 'charge.refunded':
					await this.handleRefund(event.data.object as Stripe.Charge);
					break;

				default:
					Logger.debug({eventType: event.type}, 'Unhandled webhook event type');
			}
		} catch (error: unknown) {
			Logger.error({error, eventType: event.type, eventId: event.id}, 'Failed to process webhook event');
			throw error;
		}
	}

	private async handleCheckoutSessionCompleted(session: Stripe.Checkout.Session): Promise<void> {
		if (session.metadata?.is_donation === 'true') {
			await this.handleDonationCheckoutCompleted(session);
			return;
		}

		const payment = await this.userRepository.getPaymentByCheckoutSession(session.id);
		if (!payment) {
			Logger.warn(
				{
					sessionId: session.id,
					paymentLinkId: extractId(session.payment_link),
					mode: session.mode,
					submitType: session.submit_type,
					paymentStatus: session.payment_status,
				},
				'No payment record found for checkout session, skipping event',
			);
			return;
		}

		if (payment.status === 'completed' && payment.isGift && !payment.giftCode) {
			Logger.warn(
				{sessionId: session.id, userId: payment.userId},
				'Legacy completed gift payment with no gift code — skipping (no longer recoverable via webhook)',
			);
			return;
		}

		if (payment.status !== 'pending') {
			Logger.debug({sessionId: session.id, status: payment.status}, 'Payment already processed');
			return;
		}

		const productInfo = this.productRegistry.getProduct(payment.priceId!);
		if (!productInfo) {
			Logger.error({sessionId: session.id, priceId: payment.priceId}, 'Unknown price ID');
			throw new StripeError('Unknown price ID for checkout session');
		}

		const user = await this.userRepository.findUnique(payment.userId);
		if (!user) {
			Logger.error({userId: payment.userId, sessionId: session.id}, 'User not found');
			throw new StripeError('User not found for checkout session');
		}

		if (session.amount_total == null || !session.currency) {
			Logger.error(
				{sessionId: session.id, amountTotal: session.amount_total, currency: session.currency},
				'Checkout session missing amount or currency',
			);
			throw new StripeError('Checkout session missing amount or currency');
		}

		let giftCode: string | null = null;
		if (payment.isGift) {
			const paymentIntentId = extractId(session.payment_intent);
			giftCode = await this.giftService.prepareGiftCode(session.id, user, productInfo, paymentIntentId);
		}

		const updateResult = await this.userRepository.updatePayment({
			...payment.toRow(),
			stripe_customer_id: extractId(session.customer),
			payment_intent_id: extractId(session.payment_intent),
			subscription_id: extractId(session.subscription),
			invoice_id: typeof session.invoice === 'string' ? session.invoice : null,
			amount_cents: session.amount_total,
			currency: session.currency,
			status: 'completed',
			completed_at: payment.completedAt ?? new Date(),
			gift_code: giftCode,
		});

		if (!updateResult.applied) {
			Logger.debug({sessionId: session.id}, 'Payment update failed - already processed by concurrent webhook');
			return;
		}

		const customerId = extractId(session.customer);
		const subscriptionId = extractId(session.subscription);
		const isRecurring = this.productRegistry.isRecurringSubscription(productInfo);

		const userUpdates: Partial<UserRow> = {};

		if (customerId && !user.stripeCustomerId) {
			userUpdates.stripe_customer_id = customerId;
		}

		if (subscriptionId && isRecurring) {
			userUpdates.stripe_subscription_id = subscriptionId;
			userUpdates.premium_billing_cycle = productInfo.billingCycle || null;
		}

		if (payment.isGift) {
			userUpdates.has_ever_purchased = true;
		}

		if (Object.keys(userUpdates).length > 0) {
			await this.userRepository.patchUpsert(payment.userId, userUpdates, user.toRow());
		}

		if (payment.isGift) {
			await this.giftService.finaliseGiftCode(payment.userId);
		} else {
			if (productInfo.premiumType === UserPremiumTypes.LIFETIME && user.stripeSubscriptionId && this.stripe) {
				await this.cancelStripeSubscriptionImmediately(user);
			}
			await this.premiumService.grantPremium(
				payment.userId,
				productInfo.premiumType,
				productInfo.durationMonths,
				productInfo.billingCycle || null,
				true,
			);
		}

		Logger.debug(
			{
				userId: payment.userId,
				sessionId: session.id,
				productType: productInfo.type,
				isGift: payment.isGift,
			},
			'Checkout session completed and processed',
		);
	}

	private async handleDonationCheckoutCompleted(session: Stripe.Checkout.Session): Promise<void> {
		const email = session.metadata?.donation_email;
		if (!email) {
			Logger.error({sessionId: session.id}, 'Donation checkout missing email in metadata');
			throw new StripeError('Donation checkout missing email');
		}

		const customerId = extractId(session.customer);
		if (!customerId) {
			Logger.error({sessionId: session.id}, 'Donation checkout missing customer');
			throw new StripeError('Donation checkout missing customer id');
		}

		const isRecurring = session.mode === 'subscription';
		const subscriptionId = extractId(session.subscription);

		if (isRecurring && !subscriptionId) {
			Logger.error({sessionId: session.id}, 'Donation checkout missing subscription id');
			throw new StripeError('Donation checkout missing subscription id');
		}

		const customerDetails = await this.loadDonationCustomerDetails(customerId);
		const subscriptionDetails = isRecurring
			? await this.loadDonationSubscriptionDetails(subscriptionId)
			: {amountCents: null, currency: null, interval: null, currentPeriodEnd: null, cancelAt: null};

		const existingDonor = await this.donationRepository.findDonorByEmail(email);

		let updateResult: {applied: boolean; donor: Donor | null};
		if (existingDonor) {
			updateResult = await this.donationRepository.updateDonorSubscription(email, {
				stripeCustomerId: customerId,
				businessName: customerDetails.businessName,
				taxId: customerDetails.taxId,
				taxIdType: customerDetails.taxIdType,
				stripeSubscriptionId: subscriptionId,
				subscriptionAmountCents: subscriptionDetails.amountCents,
				subscriptionCurrency: subscriptionDetails.currency,
				subscriptionInterval: subscriptionDetails.interval,
				subscriptionCurrentPeriodEnd: subscriptionDetails.currentPeriodEnd,
				subscriptionCancelAt: subscriptionDetails.cancelAt,
			});
		} else {
			const newDonor = await this.donationRepository.createDonor({
				email,
				stripeCustomerId: customerId,
				businessName: customerDetails.businessName,
				taxId: customerDetails.taxId,
				taxIdType: customerDetails.taxIdType,
				stripeSubscriptionId: subscriptionId,
				subscriptionAmountCents: subscriptionDetails.amountCents,
				subscriptionCurrency: subscriptionDetails.currency,
				subscriptionInterval: subscriptionDetails.interval,
				subscriptionCurrentPeriodEnd: subscriptionDetails.currentPeriodEnd,
				subscriptionCancelAt: subscriptionDetails.cancelAt,
			});
			updateResult = {applied: true, donor: newDonor};
		}

		if (!updateResult.applied) {
			Logger.debug({sessionId: session.id, email}, 'Donation update failed - already processed by concurrent webhook');
			return;
		}

		const encodedEmail = encodeURIComponent(email);
		const manageUrl = `${Config.endpoints.marketing}/donate/manage?email=${encodedEmail}`;

		if (isRecurring) {
			const recurringAmountCents = subscriptionDetails.amountCents;
			const recurringCurrency = subscriptionDetails.currency;
			const recurringInterval = subscriptionDetails.interval;
			if (recurringAmountCents == null || !recurringCurrency || !recurringInterval) {
				Logger.error({sessionId: session.id, subscriptionId}, 'Donation subscription details incomplete');
				throw new StripeError('Donation subscription details incomplete');
			}
			await this.emailService.sendDonationConfirmation(
				email,
				recurringAmountCents,
				recurringCurrency,
				recurringInterval,
				manageUrl,
				null,
			);
		} else {
			const oneTimeAmountCents = session.amount_total;
			const oneTimeCurrency = session.currency;
			if (oneTimeAmountCents == null || !oneTimeCurrency) {
				Logger.error(
					{sessionId: session.id, amountTotal: session.amount_total, currency: session.currency},
					'Donation checkout missing amount or currency',
				);
				throw new StripeError('Donation checkout missing amount or currency');
			}
			await this.emailService.sendDonationConfirmation(
				email,
				oneTimeAmountCents,
				oneTimeCurrency,
				'once',
				manageUrl,
				null,
			);
		}

		Logger.info(
			{
				email,
				customerId,
				subscriptionId,
				businessName: customerDetails.businessName,
				taxId: customerDetails.taxId,
				taxIdType: customerDetails.taxIdType,
				isRecurring,
				amountCents: isRecurring ? subscriptionDetails.amountCents : session.amount_total,
				currency: isRecurring ? subscriptionDetails.currency : session.currency,
				interval: subscriptionDetails.interval,
			},
			'Donation checkout completed',
		);
	}

	private async handleInvoicePaymentSucceeded(invoice: Stripe.Invoice): Promise<void> {
		if (invoice.billing_reason === 'subscription_create') {
			Logger.debug({invoiceId: invoice.id}, 'Skipping first invoice - handled by checkout.session.completed');
			return;
		}

		const subscriptionId = this.getSubscriptionIdFromInvoice(invoice);
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

		const subscriptionInfo = await this.userRepository.getSubscriptionInfo(subscriptionId);
		if (!subscriptionInfo) {
			Logger.error({invoiceId: invoice.id, subscriptionId}, 'No subscription info found');
			throw new StripeError('No subscription info found for invoice');
		}

		const productInfo = this.productRegistry.getProduct(subscriptionInfo.price_id);
		if (!productInfo) {
			Logger.error({invoiceId: invoice.id, priceId: subscriptionInfo.price_id}, 'Unknown product for renewal');
			throw new StripeError('Unknown product for invoice renewal');
		}

		await this.premiumService.grantPremium(
			subscriptionInfo.user_id,
			productInfo.premiumType,
			productInfo.durationMonths,
			productInfo.billingCycle || null,
			true,
		);

		Logger.debug(
			{
				userId: subscriptionInfo.user_id,
				invoiceId: invoice.id,
				subscriptionId,
				durationMonths: productInfo.durationMonths,
			},
			'Subscription renewed from invoice payment',
		);
	}

	private async handleSubscriptionUpdated(subscription: Stripe.Subscription): Promise<void> {
		const donor = await this.donationRepository.findDonorByStripeSubscriptionId(subscription.id);
		if (donor) {
			await this.handleDonationSubscriptionUpdated(subscription, donor);
			return;
		}

		const subscriptionInfo = await this.userRepository.getSubscriptionInfo(subscription.id);
		if (!subscriptionInfo) {
			Logger.error({subscriptionId: subscription.id}, 'No subscription info found');
			throw new StripeError('No subscription info found for subscription update');
		}

		const computedBase = getSubscriptionCurrentPeriodEnd(subscription);
		const willCancel = subscription.cancel_at_period_end || subscription.cancel_at != null;
		const computedPremiumUntil = subscription.cancel_at ? new Date(subscription.cancel_at * 1000) : computedBase;

		if (!computedPremiumUntil) {
			Logger.error({subscriptionId: subscription.id}, 'Subscription update missing period end');
			throw new StripeError('Subscription update missing period end');
		}

		const result = await this.userRepository.updateSubscriptionStatus(subscriptionInfo.user_id, {
			premiumWillCancel: willCancel,
			computedPremiumUntil,
		});

		if (result.finalVersion === null) {
			Logger.error(
				{subscriptionId: subscription.id, userId: subscriptionInfo.user_id},
				'Failed to update subscription status after retries',
			);
			throw new StripeError('Failed to update subscription status');
		}

		const updatedUser = await this.userRepository.findUnique(subscriptionInfo.user_id);
		if (!updatedUser) {
			Logger.error({subscriptionId: subscription.id, userId: subscriptionInfo.user_id}, 'Updated user not found');
			throw new StripeError('Updated user not found for subscription update');
		}
		await this.dispatchUser(updatedUser);

		Logger.debug(
			{
				userId: subscriptionInfo.user_id,
				subscriptionId: subscription.id,
				willCancel,
				computedPremiumUntil,
				status: subscription.status,
			},
			'Subscription updated (preserved gifted extension)',
		);
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

		const updateResult = await this.donationRepository.updateDonorSubscription(donor.email, {
			stripeCustomerId: donor.stripeCustomerId,
			stripeSubscriptionId: subscription.id,
			subscriptionAmountCents: amountCents,
			subscriptionCurrency: currency,
			subscriptionInterval: interval,
			subscriptionCurrentPeriodEnd: currentPeriodEnd,
			subscriptionCancelAt: cancelAt,
		});

		if (!updateResult.applied) {
			Logger.debug(
				{subscriptionId: subscription.id, email: donor.email},
				'Donation subscription update failed - already processed by concurrent webhook',
			);
			return;
		}

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

	private async handleSubscriptionDeleted(subscription: Stripe.Subscription): Promise<void> {
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
		if (!info) {
			Logger.error({subscriptionId: subscription.id}, 'Subscription delete missing subscription info');
			throw new StripeError('Subscription delete missing subscription info');
		}

		const user = await this.userRepository.findUnique(info.user_id);
		if (!user) {
			Logger.error({subscriptionId: subscription.id, userId: info.user_id}, 'User not found for subscription delete');
			throw new StripeError('User not found for subscription delete');
		}

		const updates: Partial<UserRow> = {
			premium_will_cancel: false,
			stripe_subscription_id: null,
			premium_billing_cycle: null,
		};

		if (user.premiumType !== UserPremiumTypes.LIFETIME) {
			Object.assign(updates, {premium_type: UserPremiumTypes.NONE, premium_until: null});
		}

		const updatedUser = await this.userRepository.patchUpsert(info.user_id, updates, user.toRow());
		await this.dispatchUser(updatedUser);
	}

	private async handleChargebackCreated(dispute: Stripe.Dispute): Promise<void> {
		const paymentIntentId = extractId(dispute.payment_intent);
		if (!paymentIntentId) {
			Logger.error({dispute}, 'Chargeback missing payment intent');
			throw new StripeError('Chargeback missing payment intent');
		}

		const giftCode = await this.userRepository.findGiftCodeByPaymentIntent(paymentIntentId);
		if (giftCode) {
			await this.handleGiftChargeback(giftCode);
			return;
		}

		const payment = await this.userRepository.getPaymentByPaymentIntent(paymentIntentId);
		if (!payment) {
			Logger.error({paymentIntentId}, 'No payment found for chargeback');
			throw new StripeError('No payment found for chargeback');
		}

		await this.scheduleAccountDeletionForFraud(payment.userId);
	}

	private async handleGiftChargeback(giftCode: GiftCode): Promise<void> {
		if (giftCode.redeemedByUserId) {
			await this.premiumService.revokePremium(giftCode.redeemedByUserId);

			const redeemer = await this.userRepository.findUnique(giftCode.redeemedByUserId);
			if (redeemer?.email) {
				await this.emailService.sendGiftChargebackNotification(redeemer.email, redeemer.username, redeemer.locale);
			}

			Logger.debug(
				{giftCode: giftCode.code, redeemerId: giftCode.redeemedByUserId},
				'Premium revoked due to gift chargeback',
			);
		}

		await this.scheduleAccountDeletionForFraud(giftCode.createdByUserId);
	}

	private async handleChargebackClosed(dispute: Stripe.Dispute): Promise<void> {
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
			if (user.pendingDeletionAt) {
				await this.userRepository.removePendingDeletion(payment.userId, user.pendingDeletionAt);
			}

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
				'User unsuspended after chargeback withdrawal - 30 day purchase block applied',
			);
		}
	}

	private async handleRefund(charge: Stripe.Charge): Promise<void> {
		const paymentIntentId = extractId(charge.payment_intent);
		if (!paymentIntentId) {
			Logger.error({chargeId: charge.id}, 'Refund missing payment intent');
			throw new StripeError('Refund missing payment intent');
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
			// Subscription-mode checkout sessions do not set payment_intent on the session object,
			// so the PaymentsByPaymentIntent index is never populated. Fall back to customer ID lookup.
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

		const updates: Partial<UserRow> = {
			premium_type: UserPremiumTypes.NONE,
			premium_until: null,
		};

		if (!user.firstRefundAt) {
			updates.first_refund_at = new Date();
			const updatedUser = await this.userRepository.patchUpsert(user.id, updates, user.toRow());
			await this.dispatchUser(updatedUser);
			Logger.debug(
				{userId: user.id, chargeId: charge.id, paymentIntentId},
				'First refund recorded - 30 day purchase block applied',
			);
		} else {
			updates.flags = user.flags | UserFlags.PREMIUM_PURCHASE_DISABLED;
			const updatedUser = await this.userRepository.patchUpsert(user.id, updates, user.toRow());
			await this.dispatchUser(updatedUser);
			Logger.debug(
				{userId: user.id, chargeId: charge.id, paymentIntentId},
				'Second refund recorded - permanent purchase block applied',
			);
		}
	}

	private getSubscriptionIdFromInvoice(invoice: Stripe.Invoice): string | null {
		type InvoiceWithSubscription = Stripe.Invoice & {
			subscription?: string | Stripe.Subscription;
		};
		const invoiceWithSubscription = invoice as InvoiceWithSubscription;
		const directSubscription = invoiceWithSubscription.subscription;
		if (directSubscription) {
			return extractId(directSubscription);
		}
		type InvoiceWithParent = Stripe.Invoice & {
			parent?: {
				subscription_details?: {
					subscription?: string;
				};
			};
		};
		type InvoiceLineWithParent = Stripe.InvoiceLineItem & {
			parent?: {
				subscription_item_details?: {
					subscription?: string;
				};
			};
		};

		const invoiceWithParent = invoice as InvoiceWithParent;
		const parentSubscription = invoiceWithParent.parent?.subscription_details?.subscription;
		if (parentSubscription) {
			return extractId(parentSubscription);
		}

		if (invoice.lines?.data?.length) {
			for (const line of invoice.lines.data) {
				const lineWithParent = line as InvoiceLineWithParent;
				const subscriptionId = lineWithParent.parent?.subscription_item_details?.subscription;
				if (subscriptionId) {
					return extractId(subscriptionId);
				}
			}
		}

		return null;
	}

	private async scheduleAccountDeletionForFraud(userId: UserID): Promise<void> {
		const user = await this.userRepository.findUnique(userId);
		if (!user) {
			throw new StripeError('User not found for fraud deletion');
		}

		const pendingDeletionAt = new Date(Date.now() + ms('30 days'));

		const updatedUser = await this.userRepository.patchUpsert(
			userId,
			{
				flags: user.flags | UserFlags.DELETED,
				pending_deletion_at: pendingDeletionAt,
				deletion_reason_code: DeletionReasons.BILLING_DISPUTE_OR_ABUSE,
				deletion_public_reason: 'Payment dispute',
				deletion_audit_log_reason: 'Chargeback filed',
			},
			user.toRow(),
		);
		await this.userCacheService.setUserPartialResponseFromUser(updatedUser);

		await this.userRepository.addPendingDeletion(userId, pendingDeletionAt, DeletionReasons.BILLING_DISPUTE_OR_ABUSE);

		await this.authService.terminateAllUserSessions(userId);

		if (updatedUser.email) {
			await this.emailService.sendScheduledDeletionNotification(
				updatedUser.email,
				updatedUser.username,
				pendingDeletionAt,
				'Payment dispute - chargeback filed',
				updatedUser.locale,
			);
		}

		Logger.debug({userId, pendingDeletionAt}, 'Account scheduled for deletion due to chargeback');
	}

	private async cancelStripeSubscriptionImmediately(user: User): Promise<void> {
		if (!this.stripe) {
			throw new StripeError('Stripe client not available for immediate cancellation');
		}
		if (!user.stripeSubscriptionId) {
			throw new StripeError('User missing subscription id for immediate cancellation');
		}
		await this.stripe.subscriptions.cancel(user.stripeSubscriptionId, {invoice_now: false, prorate: false});
		const updatedUser = await this.userRepository.patchUpsert(user.id, {
			stripe_subscription_id: null,
			premium_billing_cycle: null,
			premium_will_cancel: false,
		});
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

	private async loadDonationCustomerDetails(customerId: string): Promise<DonationCustomerDetails> {
		if (!this.stripe) {
			throw new StripeError('Stripe client not available for donation customer lookup');
		}

		try {
			const customer = await this.stripe.customers.retrieve(customerId);
			if (customer && !customer.deleted) {
				const businessName = customer.name ?? null;
				const primaryTaxId = customer.tax_ids?.data?.[0] ?? null;
				return {
					businessName,
					taxId: primaryTaxId?.value ?? null,
					taxIdType: primaryTaxId?.type ?? null,
				};
			}
		} catch (error) {
			Logger.error({error, customerId}, 'Failed to retrieve customer details');
			throw error;
		}

		throw new StripeError('Donation customer not found');
	}

	private async loadDonationSubscriptionDetails(subscriptionId: string | null): Promise<DonationSubscriptionDetails> {
		if (!subscriptionId) {
			throw new StripeError('Donation subscription id missing for lookup');
		}
		if (!this.stripe) {
			throw new StripeError('Stripe client not available for donation subscription lookup');
		}

		try {
			const subscription = await this.stripe.subscriptions.retrieve(subscriptionId);
			const item = getPrimarySubscriptionItem(subscription);
			if (!item) {
				Logger.error({subscriptionId}, 'Subscription has no items for donation checkout');
				throw new StripeError('Donation subscription has no items');
			}

			if (!item.price?.recurring || item.price.unit_amount == null || !item.price.currency) {
				Logger.error({subscriptionId}, 'Donation subscription missing pricing details');
				throw new StripeError('Donation subscription missing pricing details');
			}

			const currentPeriodEnd = getSubscriptionItemPeriodEnd(item);
			if (!currentPeriodEnd) {
				Logger.error({subscriptionId}, 'Donation subscription missing period end');
				throw new StripeError('Donation subscription missing period end');
			}

			const cancelAt = subscription.cancel_at ? new Date(subscription.cancel_at * 1000) : null;

			return {
				amountCents: item.price.unit_amount,
				currency: item.price.currency,
				interval: item.price.recurring.interval,
				currentPeriodEnd,
				cancelAt,
			};
		} catch (error) {
			Logger.error({error, subscriptionId}, 'Failed to retrieve subscription details');
			throw error;
		}
	}
}
