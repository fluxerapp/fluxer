// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import type {DonationRepository} from '@app/api/donation/DonationRepository';
import {Logger} from '@app/api/Logger';
import {getPrimarySubscriptionItem, getSubscriptionItemPeriodEnd} from '@app/api/stripe/StripeSubscriptionPeriod';
import {extractId} from '@app/api/stripe/StripeUtils';
import {StripeError} from '@fluxer/errors/src/domains/payment/StripeError';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import type {IEmailService} from '@pkgs/email/src/IEmailService';
import {seconds} from 'itty-time';
import type Stripe from 'stripe';

const DONATION_CONFIRMATION_SENT_TTL_SECONDS = seconds('365 days');

export interface DonationCheckoutDonor {
	email: string;
	locale: string | null;
	isBusiness: boolean;
}

interface DonationCustomerDetails {
	businessName: string | undefined;
	taxId: string | undefined;
	taxIdType: string | undefined;
}

interface OneTimeDonationReceipt {
	paidAt: Date;
	reference: string | null;
	receiptUrl: string | null;
}

interface DonationSubscriptionDetails {
	amountCents: number | null;
	currency: string | null;
	interval: string | null;
	currentPeriodEnd: Date | null;
	cancelAt: Date | null;
	status: string | null;
}

export class StripeDonationCheckoutFulfilment {
	constructor(
		private stripe: Stripe | null,
		private donationRepository: DonationRepository,
		private emailService: IEmailService,
		private cacheService: ICacheService,
	) {}

	async registerDonation(session: Stripe.Checkout.Session, donation: DonationCheckoutDonor): Promise<void> {
		const {email, locale: donationLocale, isBusiness} = donation;
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
		const customerDetails = await this.loadDonationCustomerDetails(customerId, isBusiness);
		const subscriptionDetails = isRecurring
			? await this.loadDonationSubscriptionDetails(subscriptionId)
			: {amountCents: null, currency: null, interval: null, currentPeriodEnd: null, cancelAt: null, status: null};
		const existingDonor = await this.donationRepository.findDonorByEmail(email);
		const subscriptionUpdate = {
			stripeSubscriptionId: subscriptionId,
			subscriptionAmountCents: subscriptionDetails.amountCents,
			subscriptionCurrency: subscriptionDetails.currency,
			subscriptionInterval: subscriptionDetails.interval,
			subscriptionCurrentPeriodEnd: subscriptionDetails.currentPeriodEnd,
			subscriptionCancelAt: subscriptionDetails.cancelAt,
			subscriptionStatus: subscriptionDetails.status,
		};
		if (existingDonor && isRecurring) {
			await this.donationRepository.updateDonorSubscription(email, {
				stripeCustomerId: customerId,
				businessName: customerDetails.businessName,
				taxId: customerDetails.taxId,
				taxIdType: customerDetails.taxIdType,
				...subscriptionUpdate,
			});
		} else if (existingDonor) {
			const subscriptionCustomerId = existingDonor.stripeSubscriptionId ? existingDonor.stripeCustomerId : null;
			await this.donationRepository.updateDonorCustomerDetails(email, {
				stripeCustomerId: subscriptionCustomerId ?? customerId,
				businessName: customerDetails.businessName,
				taxId: customerDetails.taxId,
				taxIdType: customerDetails.taxIdType,
			});
			await this.donationRepository.linkDonorStripeCustomer(email, customerId);
		} else {
			await this.donationRepository.createDonor({
				email,
				stripeCustomerId: customerId,
				businessName: customerDetails.businessName,
				taxId: customerDetails.taxId,
				taxIdType: customerDetails.taxIdType,
				...subscriptionUpdate,
			});
		}
		if (session.payment_status === 'unpaid') {
			Logger.info(
				{sessionId: session.id, email, isRecurring, paymentStatus: session.payment_status},
				'Donation checkout completed with unpaid status; deferring confirmation until settlement',
			);
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
			await this.sendDonationConfirmationOnce({
				sessionId: session.id,
				email,
				amountCents: recurringAmountCents,
				currency: recurringCurrency,
				interval: recurringInterval,
				manageUrl,
				locale: donationLocale,
			});
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
			await this.sendDonationConfirmationOnce({
				sessionId: session.id,
				email,
				amountCents: oneTimeAmountCents,
				currency: oneTimeCurrency,
				interval: 'once',
				manageUrl,
				locale: donationLocale,
				...(await this.loadOneTimeReceipt(session)),
			});
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

	private async loadOneTimeReceipt(session: Stripe.Checkout.Session): Promise<OneTimeDonationReceipt> {
		const paymentIntentId = extractId(session.payment_intent);
		if (!this.stripe || !paymentIntentId) {
			return {paidAt: new Date(session.created * 1000), reference: null, receiptUrl: null};
		}
		const intent = await this.stripe.paymentIntents.retrieve(paymentIntentId, {expand: ['latest_charge']});
		const charge = typeof intent.latest_charge === 'object' ? intent.latest_charge : null;
		const invoiceId = extractId(session.invoice);
		const invoice = invoiceId ? await this.stripe.invoices.retrieve(invoiceId) : null;
		return {
			paidAt: charge ? new Date(charge.created * 1000) : new Date(session.created * 1000),
			reference: invoice?.number ?? charge?.receipt_number ?? null,
			receiptUrl: invoice?.hosted_invoice_url ?? charge?.receipt_url ?? null,
		};
	}

	private async sendDonationConfirmationOnce({
		sessionId,
		email,
		amountCents,
		currency,
		interval,
		manageUrl,
		locale,
		...receipt
	}: {
		sessionId: string;
		email: string;
		amountCents: number;
		currency: string;
		interval: string;
		manageUrl: string;
		locale: string | null;
	} & Partial<OneTimeDonationReceipt>): Promise<void> {
		const confirmationSentKey = this.getDonationConfirmationSentKey(sessionId);
		if (await this.cacheService.get<boolean>(confirmationSentKey)) {
			Logger.debug({sessionId}, 'Donation confirmation already sent for checkout session');
			return;
		}
		const sent = await this.emailService.send(
			'donation_confirmation',
			email,
			{amountCents, currency, interval, manageUrl, ...receipt},
			locale,
		);
		if (!sent) {
			Logger.error({sessionId, email, interval}, 'Failed to send donation confirmation email');
			throw new StripeError('Failed to send donation confirmation email');
		}
		await this.cacheService.set(confirmationSentKey, true, DONATION_CONFIRMATION_SENT_TTL_SECONDS);
	}

	private getDonationConfirmationSentKey(checkoutSessionId: string): string {
		return `stripe:checkout:donation:confirmation:sent:${checkoutSessionId}`;
	}

	private async loadDonationCustomerDetails(customerId: string, isBusiness: boolean): Promise<DonationCustomerDetails> {
		if (!this.stripe) {
			throw new StripeError('Stripe client not available for donation customer lookup');
		}
		try {
			const customer = await this.stripe.customers.retrieve(customerId, {expand: ['tax_ids']});
			if (customer && !customer.deleted) {
				const primaryTaxId = customer.tax_ids?.data?.[0] ?? null;
				const isBusinessDonor = isBusiness || primaryTaxId !== null;
				return {
					businessName: isBusinessDonor ? (customer.name ?? undefined) : undefined,
					taxId: primaryTaxId?.value ?? undefined,
					taxIdType: primaryTaxId?.type ?? undefined,
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
				status: subscription.status,
			};
		} catch (error) {
			Logger.error({error, subscriptionId}, 'Failed to retrieve subscription details');
			throw error;
		}
	}
}
