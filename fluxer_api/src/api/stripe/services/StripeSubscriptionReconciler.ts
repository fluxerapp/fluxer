// SPDX-License-Identifier: AGPL-3.0-or-later

import {UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import {StripeError} from '@fluxer/errors/src/domains/payment/StripeError';
import type Stripe from 'stripe';
import type {UserID} from '../../BrandedTypes';
import type {UserRow} from '../../database/types/UserTypes';
import {Logger} from '../../Logger';
import type {User} from '../../models/User';
import type {IUserRepository} from '../../user/IUserRepository';
import type {ProductInfo, ProductRegistry} from '../ProductRegistry';
import {getPrimarySubscriptionItem, getSubscriptionStartDate} from '../StripeSubscriptionPeriod';
import {extractId} from '../StripeUtils';

interface InvoiceRenewalContext {
	userId: UserID;
	productInfo: ProductInfo;
	reconciledFromStripe: boolean;
}

export class StripeSubscriptionReconciler {
	constructor(
		private stripe: Stripe | null,
		private userRepository: IUserRepository,
		private productRegistry: ProductRegistry,
	) {}

	async resolveInvoiceRenewalContext(subscriptionId: string, invoice: Stripe.Invoice): Promise<InvoiceRenewalContext> {
		const invoicePriceId = this.getPriceIdFromInvoice(invoice);
		const subscriptionInfo = await this.userRepository.getSubscriptionInfo(subscriptionId);
		if (subscriptionInfo) {
			const productInfo = this.productRegistry.getProduct(subscriptionInfo.price_id);
			const mappedUser = await this.userRepository.findUnique(subscriptionInfo.user_id);
			const hasInvoicePriceMismatch = invoicePriceId != null && invoicePriceId !== subscriptionInfo.price_id;
			if (mappedUser && productInfo && !hasInvoicePriceMismatch) {
				return {
					userId: mappedUser.id,
					productInfo,
					reconciledFromStripe: false,
				};
			}
			Logger.warn(
				{
					invoiceId: invoice.id,
					subscriptionId,
					mappedUserId: subscriptionInfo.user_id,
					mappedUserFound: Boolean(mappedUser),
					mappedPriceId: subscriptionInfo.price_id,
					invoicePriceId,
					hasInvoicePriceMismatch,
				},
				'Invoice renewal mapping stale or incomplete; reconciling with Stripe subscription state',
			);
		}
		if (!this.stripe) {
			Logger.error({invoiceId: invoice.id, subscriptionId}, 'Stripe client unavailable for invoice reconciliation');
			throw new StripeError('No subscription info found for invoice');
		}
		let stripeSubscription: Stripe.Subscription;
		try {
			stripeSubscription = await this.stripe.subscriptions.retrieve(subscriptionId);
		} catch (error) {
			Logger.error(
				{error, invoiceId: invoice.id, subscriptionId},
				'Failed to load subscription for invoice reconciliation',
			);
			throw new StripeError('Failed to reconcile invoice subscription');
		}
		const targetUser = await this.resolveUserForSubscriptionReconciliation(stripeSubscription, {
			allowStripeFetch: false,
			reason: 'invoice_renewal_reconciliation',
			fallbackInvoice: invoice,
		});
		if (!targetUser) {
			Logger.error({invoiceId: invoice.id, subscriptionId}, 'No user found for invoice reconciliation');
			throw new StripeError('No subscription info found for invoice');
		}
		const item = getPrimarySubscriptionItem(stripeSubscription);
		const priceId = item?.price?.id ?? invoicePriceId;
		if (!priceId) {
			Logger.error({invoiceId: invoice.id, subscriptionId}, 'Stripe subscription missing price for invoice renewal');
			throw new StripeError('Stripe subscription missing price for invoice renewal');
		}
		const productInfo = this.productRegistry.getProduct(priceId);
		if (!productInfo) {
			Logger.error({invoiceId: invoice.id, subscriptionId, priceId}, 'Unknown product for reconciled renewal');
			throw new StripeError('Unknown product for invoice renewal');
		}
		const reconciledUser = await this.reconcileUserWithSubscriptionState(targetUser, stripeSubscription, {
			productInfo,
			fallbackInvoice: invoice,
		});
		return {
			userId: reconciledUser.id,
			productInfo,
			reconciledFromStripe: true,
		};
	}

	async resolveUserForInvoiceReconciliation(
		invoice: Stripe.Invoice,
		options: {
			subscriptionId?: string | null;
			reason: string;
		},
	): Promise<User | null> {
		const subscriptionId = options.subscriptionId ?? this.getSubscriptionIdFromInvoice(invoice);
		const customerId = this.getCustomerIdFromInvoice(invoice);
		if (subscriptionId) {
			const mappedUser = await this.userRepository.findByStripeSubscriptionId(subscriptionId);
			if (mappedUser) {
				return mappedUser;
			}
			const subscriptionInfo = await this.userRepository.getSubscriptionInfo(subscriptionId);
			if (subscriptionInfo) {
				const infoUser = await this.userRepository.findUnique(subscriptionInfo.user_id);
				if (infoUser) {
					return infoUser;
				}
			}
		}
		if (customerId) {
			const customerUser = await this.userRepository.findByStripeCustomerId(customerId);
			if (customerUser) {
				return customerUser;
			}
		}
		if (!subscriptionId || !this.stripe) {
			Logger.warn(
				{
					reason: options.reason,
					invoiceId: invoice.id,
					subscriptionId,
					customerId,
				},
				'Unable to resolve user for invoice reconciliation without Stripe subscription lookup',
			);
			return null;
		}
		try {
			const subscription = await this.stripe.subscriptions.retrieve(subscriptionId);
			return await this.resolveUserForSubscriptionReconciliation(subscription, {
				allowStripeFetch: false,
				reason: options.reason,
				fallbackInvoice: invoice,
			});
		} catch (error) {
			Logger.warn(
				{
					error,
					reason: options.reason,
					invoiceId: invoice.id,
					subscriptionId,
					customerId,
				},
				'Failed to retrieve Stripe subscription while resolving invoice reconciliation user',
			);
			return null;
		}
	}

	async resolveUserForSubscriptionReconciliation(
		subscription: Stripe.Subscription,
		options: {
			allowStripeFetch: boolean;
			reason: string;
			fallbackInvoice?: Stripe.Invoice;
		},
	): Promise<User | null> {
		let candidateSubscription = subscription;
		if (options.allowStripeFetch) {
			candidateSubscription = await this.getLatestSubscriptionSnapshot(subscription);
		}
		const customerId =
			extractId(candidateSubscription.customer) ?? this.getCustomerIdFromInvoice(options.fallbackInvoice);
		let user = await this.userRepository.findByStripeSubscriptionId(candidateSubscription.id);
		if (!user && customerId) {
			user = await this.userRepository.findByStripeCustomerId(customerId);
		}
		if (!user) {
			Logger.warn(
				{
					reason: options.reason,
					subscriptionId: candidateSubscription.id,
					customerId,
				},
				'Unable to resolve user for subscription reconciliation',
			);
			return null;
		}
		return await this.reconcileUserWithSubscriptionState(user, candidateSubscription, {
			fallbackInvoice: options.fallbackInvoice,
		});
	}

	async reconcileUserWithSubscriptionState(
		user: User,
		subscription: Stripe.Subscription,
		options?: {
			productInfo?: ProductInfo;
			fallbackInvoice?: Stripe.Invoice;
		},
	): Promise<User> {
		const customerId = extractId(subscription.customer) ?? this.getCustomerIdFromInvoice(options?.fallbackInvoice);
		const productInfo = options?.productInfo;
		const billingCycle = productInfo?.billingCycle || this.getBillingCycleFromSubscription(subscription);
		const subscriptionStartDate = getSubscriptionStartDate(subscription);
		const patch: Partial<UserRow> = {};
		if (user.stripeSubscriptionId !== subscription.id) {
			patch.stripe_subscription_id = subscription.id;
		}
		if (customerId && user.stripeCustomerId !== customerId) {
			patch.stripe_customer_id = customerId;
		}
		if (billingCycle && user.premiumBillingCycle !== billingCycle) {
			patch.premium_billing_cycle = billingCycle;
		}
		if (
			user.premiumType === UserPremiumTypes.SUBSCRIPTION &&
			(!user.premiumSince || user.premiumSince.getTime() > subscriptionStartDate.getTime())
		) {
			patch.premium_since = subscriptionStartDate;
		}
		if (Object.keys(patch).length === 0) {
			return user;
		}
		Logger.info(
			{
				userId: user.id,
				subscriptionId: subscription.id,
				customerId,
				billingCycle,
			},
			'Reconciled user Stripe subscription state from webhook',
		);
		return await this.userRepository.patchUpsert(user.id, patch, user.toRow());
	}

	hasSubscriptionIdentityMismatch(user: User, subscription: Stripe.Subscription): boolean {
		if (user.stripeSubscriptionId && user.stripeSubscriptionId !== subscription.id) {
			return true;
		}
		const subscriptionCustomerId = extractId(subscription.customer);
		if (subscriptionCustomerId && user.stripeCustomerId && user.stripeCustomerId !== subscriptionCustomerId) {
			return true;
		}
		return false;
	}

	getBillingCycleFromSubscription(subscription: Stripe.Subscription): 'monthly' | 'yearly' | null {
		const item = getPrimarySubscriptionItem(subscription);
		const interval = item?.price?.recurring?.interval;
		if (interval === 'month') {
			return 'monthly';
		}
		if (interval === 'year') {
			return 'yearly';
		}
		return null;
	}

	getPriceIdFromInvoice(invoice: Stripe.Invoice): string | null {
		if (!invoice.lines?.data?.length) {
			return null;
		}
		for (const line of invoice.lines.data) {
			const priceId = extractId(line.pricing?.price_details?.price ?? null);
			if (priceId) {
				return priceId;
			}
		}
		return null;
	}

	async getLatestSubscriptionSnapshot(subscription: Stripe.Subscription): Promise<Stripe.Subscription> {
		if (!this.stripe) {
			return subscription;
		}
		try {
			return await this.stripe.subscriptions.retrieve(subscription.id);
		} catch (error) {
			Logger.warn({error, subscriptionId: subscription.id}, 'Failed to fetch latest subscription snapshot');
			return subscription;
		}
	}

	async getSubscriptionSnapshotById(subscriptionId: string): Promise<Stripe.Subscription | null> {
		if (!this.stripe) {
			return null;
		}
		try {
			return await this.stripe.subscriptions.retrieve(subscriptionId);
		} catch (error) {
			Logger.warn({error, subscriptionId}, 'Failed to fetch subscription snapshot by id');
			return null;
		}
	}

	getCustomerIdFromInvoice(invoice?: Stripe.Invoice): string | null {
		if (!invoice) {
			return null;
		}
		return extractId(invoice.customer);
	}

	getSubscriptionIdFromInvoice(invoice: Stripe.Invoice): string | null {
		const parentSubscription = extractId(invoice.parent?.subscription_details?.subscription ?? null);
		if (parentSubscription) {
			return parentSubscription;
		}
		if (invoice.lines?.data?.length) {
			for (const line of invoice.lines.data) {
				const subscriptionId = line.parent?.subscription_item_details?.subscription ?? null;
				if (subscriptionId) {
					return subscriptionId;
				}
			}
		}
		return null;
	}
}
