// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {getContentMessage} from '@app/api/content_i18n/ContentI18n';
import type {IDonationRepository} from '@app/api/donation/IDonationRepository';
import type {IEmailDnsValidationService} from '@app/api/infrastructure/IEmailDnsValidationService';
import {Logger} from '@app/api/Logger';
import {getBillingRepository} from '@app/api/middleware/ServiceRegistry';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {DonationAmountInvalidError} from '@fluxer/errors/src/domains/donation/DonationAmountInvalidError';
import {StripeError} from '@fluxer/errors/src/domains/payment/StripeError';
import {StripePaymentNotAvailableError} from '@fluxer/errors/src/domains/payment/StripePaymentNotAvailableError';
import {isDonationAmountWithinConstraints} from '@fluxer/schema/src/domains/donation/DonationAmountUtils';
import type {DonationCurrency} from '@fluxer/schema/src/domains/donation/DonationSchemas';
import type Stripe from 'stripe';

type CheckoutSessionCreateParams = Stripe.Checkout.SessionCreateParams;
type CheckoutSessionMode = CheckoutSessionCreateParams['mode'];
type CheckoutSessionLineItem = NonNullable<CheckoutSessionCreateParams['line_items']>[number];

const PRODUCT_NAME = 'Fluxer';

function getDonationProductData(interval: 'month' | 'year' | null, locale: string | null) {
	if (interval === 'month') {
		return {
			name: getContentMessage('billing.donation_name_recurring', locale, {product_name: PRODUCT_NAME}),
			description: getContentMessage('billing.donation_description_monthly', locale, {product_name: PRODUCT_NAME}),
		};
	}
	if (interval === 'year') {
		return {
			name: getContentMessage('billing.donation_name_recurring', locale, {product_name: PRODUCT_NAME}),
			description: getContentMessage('billing.donation_description_yearly', locale, {product_name: PRODUCT_NAME}),
		};
	}
	return {
		name: getContentMessage('billing.donation_name_one_time', locale, {product_name: PRODUCT_NAME}),
		description: getContentMessage('billing.donation_description_one_time', locale, {product_name: PRODUCT_NAME}),
	};
}

export class DonationCheckoutService {
	constructor(
		private stripe: Stripe | null,
		private donationRepository: IDonationRepository,
		private emailDnsValidationService: IEmailDnsValidationService,
	) {}

	async createCheckout(params: {
		email: string;
		amountCents: number;
		currency: DonationCurrency;
		interval: 'month' | 'year' | null;
		isBusiness?: boolean;
		locale?: string | null;
	}): Promise<string> {
		if (!this.stripe) {
			throw new StripePaymentNotAvailableError();
		}
		if (!isDonationAmountWithinConstraints(params.amountCents, params.currency)) {
			throw new DonationAmountInvalidError();
		}
		const hasValidDns = await this.emailDnsValidationService.hasValidDnsRecords(params.email);
		if (!hasValidDns) {
			throw InputValidationError.fromCode('email', ValidationErrorCodes.EMAIL_DOMAIN_CANNOT_RECEIVE_MAIL);
		}
		const isRecurring = params.interval !== null;
		const existingDonor = await this.donationRepository.findDonorByEmail(params.email);
		if (isRecurring && existingDonor?.hasActiveSubscription()) {
			const encodedEmail = encodeURIComponent(params.email);
			return `${Config.endpoints.marketing}/donate/manage?email=${encodedEmail}&alert=active_subscription`;
		}
		const locale = params.locale ?? null;
		const donationProductData = getDonationProductData(params.interval, locale);
		try {
			const mode: CheckoutSessionMode = isRecurring ? 'subscription' : 'payment';
			const lineItem: CheckoutSessionLineItem = isRecurring
				? {
						price_data: {
							currency: params.currency,
							product_data: donationProductData,
							unit_amount: params.amountCents,
							recurring: {
								interval: params.interval as 'month' | 'year',
							},
						},
						quantity: 1,
					}
				: {
						price_data: {
							currency: params.currency,
							product_data: donationProductData,
							unit_amount: params.amountCents,
						},
						quantity: 1,
					};
			const isBusiness = params.isBusiness === true;
			const sessionParams: CheckoutSessionCreateParams = {
				line_items: [lineItem],
				mode,
				metadata: {
					is_donation: 'true',
					donation_email: params.email,
					donation_type: isRecurring ? 'recurring' : 'one_time',
					is_business: isBusiness ? 'true' : 'false',
					...(locale ? {donation_locale: locale} : {}),
				},
				success_url: `${Config.endpoints.marketing}/donate/success`,
				cancel_url: `${Config.endpoints.marketing}/donate`,
				automatic_tax: {
					enabled: false,
				},
				tax_id_collection: {
					enabled: true,
				},
				...(isBusiness ? {billing_address_collection: 'required' as const} : {}),
				...(mode === 'payment'
					? {
							invoice_creation: {
								enabled: true,
							},
						}
					: {}),
			};
			if (existingDonor?.stripeCustomerId) {
				sessionParams.customer = existingDonor.stripeCustomerId;
				sessionParams.customer_update = {
					address: 'auto',
					name: 'auto',
				};
			} else {
				sessionParams.customer_email = params.email;
			}
			const session = await this.stripe.checkout.sessions.create(sessionParams);
			try {
				await getBillingRepository().checkoutSessions.upsertFromStripe(session);
			} catch (mirrorErr) {
				Logger.error(
					{mirrorErr, sessionId: session.id},
					'Mirror upsert failed after Stripe write; reconciler will heal',
				);
			}
			if (!session.url) {
				throw new StripeError('Failed to create checkout session');
			}
			Logger.debug(
				{
					email: params.email,
					amountCents: params.amountCents,
					interval: params.interval,
					mode,
					sessionId: session.id,
				},
				'Donation checkout session created',
			);
			return session.url;
		} catch (error: unknown) {
			if (error instanceof StripeError || error instanceof DonationAmountInvalidError) {
				throw error;
			}
			Logger.error({error, email: params.email}, 'Failed to create donation checkout session');
			const message = error instanceof Error ? error.message : 'Failed to create checkout session';
			throw new StripeError(message);
		}
	}

	async createPortalSession(stripeCustomerId: string): Promise<string> {
		if (!this.stripe) {
			throw new StripePaymentNotAvailableError();
		}
		try {
			const session = await this.stripe.billingPortal.sessions.create({
				customer: stripeCustomerId,
				return_url: `${Config.endpoints.marketing}/donate`,
			});
			Logger.debug({stripeCustomerId}, 'Donation portal session created');
			return session.url;
		} catch (error: unknown) {
			Logger.error({error, stripeCustomerId}, 'Failed to create donor portal session');
			const message = error instanceof Error ? error.message : 'Failed to create portal session';
			throw new StripeError(message);
		}
	}
}
