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

import type Stripe from 'stripe';
import type {AuthService} from '~/auth/AuthService';
import type {UserID} from '~/BrandedTypes';
import {Config} from '~/Config';
import {DeletionReasons, UserFlags, UserPremiumTypes} from '~/Constants';
import type {UserRow} from '~/database/CassandraTypes';
import {StripeError, StripeWebhookSignatureInvalidError} from '~/Errors';
import type {IEmailService} from '~/infrastructure/IEmailService';
import type {IGatewayService} from '~/infrastructure/IGatewayService';
import {Logger} from '~/Logger';
import type {GiftCode, User} from '~/Models';
import type {IUserRepository} from '~/user/IUserRepository';
import {mapUserToPrivateResponse} from '~/user/UserModel';
import type {ProductRegistry} from '../ProductRegistry';
import {extractId} from '../StripeUtils';
import type {StripeGiftService} from './StripeGiftService';
import type {StripePremiumService} from './StripePremiumService';

export interface HandleWebhookParams {
	body: string;
	signature: string;
}

export class StripeWebhookService {
	constructor(
		private stripe: Stripe | null,
		private userRepository: IUserRepository,
		private authService: AuthService,
		private emailService: IEmailService,
		private gatewayService: IGatewayService,
		private productRegistry: ProductRegistry,
		private giftService: StripeGiftService,
		private premiumService: StripePremiumService,
	) {}

	async handleWebhook({body, signature}: HandleWebhookParams): Promise<void> {
		if (!this.stripe || !Config.stripe.webhookSecret) {
			throw new StripeError('Webhook processing is not available');
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
		const payment = await this.userRepository.getPaymentByCheckoutSession(session.id);
		if (!payment) {
			Logger.error({sessionId: session.id}, 'No payment record found for checkout session');
			return;
		}

		if (payment.status !== 'pending') {
			Logger.debug({sessionId: session.id, status: payment.status}, 'Payment already processed');
			return;
		}

		const productInfo = this.productRegistry.getProduct(payment.priceId!);
		if (!productInfo) {
			Logger.error({sessionId: session.id, priceId: payment.priceId}, 'Unknown price ID');
			return;
		}

		const user = await this.userRepository.findUnique(payment.userId);
		if (!user) {
			Logger.error({userId: payment.userId, sessionId: session.id}, 'User not found');
			return;
		}

		await this.userRepository.updatePayment({
			...payment.toRow(),
			stripe_customer_id: extractId(session.customer),
			payment_intent_id: extractId(session.payment_intent),
			subscription_id: extractId(session.subscription),
			invoice_id: typeof session.invoice === 'string' ? session.invoice : null,
			amount_cents: session.amount_total || 0,
			currency: session.currency || 'usd',
			status: 'completed',
			completed_at: new Date(),
		});

		const customerId = extractId(session.customer);
		if (customerId && !user.stripeCustomerId) {
			await this.userRepository.patchUpsert(payment.userId, {
				stripe_customer_id: customerId,
			});
		}

		const subscriptionId = extractId(session.subscription);
		if (subscriptionId && this.productRegistry.isRecurringSubscription(productInfo)) {
			await this.userRepository.patchUpsert(payment.userId, {
				stripe_subscription_id: subscriptionId,
				premium_billing_cycle: productInfo.billingCycle || null,
			});
		}

		if (payment.isGift) {
			const paymentIntentId = extractId(session.payment_intent);
			if (payment.giftCode) {
				Logger.debug({sessionId: session.id, giftCode: payment.giftCode}, 'Gift already created for payment');
			} else {
				await this.giftService.createGiftCode(session.id, user, productInfo, paymentIntentId);
			}
			await this.userRepository.patchUpsert(payment.userId, {
				has_ever_purchased: true,
			});
			await this.dispatchUser(user);
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

	private async handleInvoicePaymentSucceeded(invoice: Stripe.Invoice): Promise<void> {
		if (invoice.billing_reason === 'subscription_create') {
			Logger.debug({invoiceId: invoice.id}, 'Skipping first invoice - handled by checkout.session.completed');
			return;
		}

		const subscriptionId = this.getSubscriptionIdFromInvoice(invoice);
		if (!subscriptionId) {
			Logger.warn({invoiceId: invoice.id}, 'No subscription ID found in invoice');
			return;
		}

		const subscriptionInfo = await this.userRepository.getSubscriptionInfo(subscriptionId);
		if (!subscriptionInfo) {
			Logger.warn({invoiceId: invoice.id, subscriptionId}, 'No subscription info found');
			return;
		}

		const productInfo = this.productRegistry.getProduct(subscriptionInfo.price_id);
		if (!productInfo) {
			Logger.error({invoiceId: invoice.id, priceId: subscriptionInfo.price_id}, 'Unknown product for renewal');
			return;
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
		const subscriptionInfo = await this.userRepository.getSubscriptionInfo(subscription.id);
		if (!subscriptionInfo) {
			Logger.warn({subscriptionId: subscription.id}, 'No subscription info found');
			return;
		}

		const currentPeriodEnd = subscription.items.data[0]?.current_period_end;
		const computedBase = currentPeriodEnd ? new Date(currentPeriodEnd * 1000) : null;

		const user = await this.userRepository.findUnique(subscriptionInfo.user_id);
		if (!user) {
			Logger.warn({subscriptionId: subscription.id}, 'User not found for subscription update');
			return;
		}

		let nextPremiumUntil: Date | null = user.premiumUntil ?? null;

		if (subscription.cancel_at != null) {
			nextPremiumUntil = new Date(subscription.cancel_at * 1000);
		} else if (computedBase) {
			if (!nextPremiumUntil || computedBase > nextPremiumUntil) {
				nextPremiumUntil = computedBase;
			}
		}

		const updatedUser = await this.userRepository.patchUpsert(subscriptionInfo.user_id, {
			premium_will_cancel: subscription.cancel_at != null,
			premium_until: nextPremiumUntil,
		});

		if (updatedUser) {
			await this.dispatchUser(updatedUser);
		}

		Logger.debug(
			{
				userId: subscriptionInfo.user_id,
				subscriptionId: subscription.id,
				willCancel: subscription.cancel_at != null,
				premiumUntil: nextPremiumUntil,
				status: subscription.status,
			},
			'Subscription updated (preserved gifted extension)',
		);
	}

	private async handleSubscriptionDeleted(subscription: Stripe.Subscription): Promise<void> {
		const info = await this.userRepository.getSubscriptionInfo(subscription.id);
		if (!info) return;

		const user = await this.userRepository.findUnique(info.user_id);
		if (!user) return;

		const updates: Partial<UserRow> = {
			premium_will_cancel: false,
			stripe_subscription_id: null,
			premium_billing_cycle: null,
		};

		if (user.premiumType !== UserPremiumTypes.LIFETIME) {
			Object.assign(updates, {premium_type: UserPremiumTypes.NONE, premium_until: null});
		}

		const updatedUser = await this.userRepository.patchUpsert(info.user_id, updates);
		if (updatedUser) await this.dispatchUser(updatedUser);
	}

	private async handleChargebackCreated(dispute: Stripe.Dispute): Promise<void> {
		const paymentIntentId = extractId(dispute.payment_intent);
		if (!paymentIntentId) {
			Logger.warn({dispute}, 'Chargeback missing payment intent');
			return;
		}

		const giftCode = await this.userRepository.findGiftCodeByPaymentIntent(paymentIntentId);
		if (giftCode) {
			await this.handleGiftChargeback(giftCode);
			return;
		}

		const payment = await this.userRepository.getPaymentByPaymentIntent(paymentIntentId);
		if (!payment) {
			Logger.warn({paymentIntentId}, 'No payment found for chargeback');
			return;
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
			return;
		}

		const payment = await this.userRepository.getPaymentByPaymentIntent(paymentIntentId);
		if (!payment) {
			return;
		}

		const user = await this.userRepository.findUnique(payment.userId);
		if (!user) {
			return;
		}

		if (user.flags & UserFlags.DELETED && user.deletionReasonCode === DeletionReasons.FRIENDLY_FRAUD) {
			if (user.pendingDeletionAt) {
				await this.userRepository.removePendingDeletion(payment.userId, user.pendingDeletionAt);
			}

			const updatedUser = await this.userRepository.patchUpsert(payment.userId, {
				flags: user.flags & ~UserFlags.DELETED,
				pending_deletion_at: null,
				deletion_reason_code: null,
				deletion_public_reason: null,
				deletion_audit_log_reason: null,
				first_refund_at: user.firstRefundAt || new Date(),
			});

			if (updatedUser?.email) {
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
			Logger.warn({chargeId: charge.id}, 'Refund missing payment intent');
			return;
		}

		const payment = await this.userRepository.getPaymentByPaymentIntent(paymentIntentId);
		if (!payment) {
			Logger.warn({paymentIntentId, chargeId: charge.id}, 'No payment found for refund');
			return;
		}

		const user = await this.userRepository.findUnique(payment.userId);
		if (!user) {
			Logger.error({userId: payment.userId, chargeId: charge.id}, 'User not found for refund');
			return;
		}

		await this.userRepository.updatePayment({
			...payment.toRow(),
			status: 'refunded',
		});

		await this.premiumService.revokePremium(payment.userId);

		if (!user.firstRefundAt) {
			await this.userRepository.patchUpsert(payment.userId, {
				first_refund_at: new Date(),
			});
			Logger.debug(
				{userId: payment.userId, chargeId: charge.id, paymentIntentId},
				'First refund recorded - 30 day purchase block applied',
			);
		} else {
			const updatedUser = await this.userRepository.patchUpsert(payment.userId, {
				flags: user.flags | UserFlags.PREMIUM_PURCHASE_DISABLED,
			});

			if (updatedUser) {
				await this.dispatchUser(updatedUser);
			}

			Logger.debug(
				{userId: payment.userId, chargeId: charge.id, paymentIntentId},
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
			return;
		}

		const pendingDeletionAt = new Date(Date.now() + 30 * 24 * 60 * 60 * 1000);

		const updatedUser = await this.userRepository.patchUpsert(userId, {
			flags: user.flags | UserFlags.DELETED,
			pending_deletion_at: pendingDeletionAt,
			deletion_reason_code: DeletionReasons.FRIENDLY_FRAUD,
			deletion_public_reason: 'Payment dispute',
			deletion_audit_log_reason: 'Chargeback filed',
		});

		await this.userRepository.addPendingDeletion(userId, pendingDeletionAt, DeletionReasons.FRIENDLY_FRAUD);

		await this.authService.terminateAllUserSessions(userId);

		if (updatedUser?.email) {
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
		if (!this.stripe || !user.stripeSubscriptionId) return;
		await this.stripe.subscriptions.cancel(user.stripeSubscriptionId, {invoice_now: false, prorate: false});
		const updatedUser = await this.userRepository.patchUpsert(user.id, {
			stripe_subscription_id: null,
			premium_billing_cycle: null,
			premium_will_cancel: false,
		});
		if (updatedUser) await this.dispatchUser(updatedUser);
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
