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

import {Config} from '@fluxer/api/src/Config';
import type {IDonationRepository} from '@fluxer/api/src/donation/IDonationRepository';
import type {IEmailDnsValidationService} from '@fluxer/api/src/infrastructure/IEmailDnsValidationService';
import {Logger} from '@fluxer/api/src/Logger';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {DonationAmountInvalidError} from '@fluxer/errors/src/domains/donation/DonationAmountInvalidError';
import {StripeError} from '@fluxer/errors/src/domains/payment/StripeError';
import {StripePaymentNotAvailableError} from '@fluxer/errors/src/domains/payment/StripePaymentNotAvailableError';
import type Stripe from 'stripe';

const MIN_DONATION_CENTS = 500;
const MAX_DONATION_CENTS = 100000;

export class DonationCheckoutService {
	constructor(
		private stripe: Stripe | null,
		private donationRepository: IDonationRepository,
		private emailDnsValidationService: IEmailDnsValidationService,
	) {}

	async createCheckout(params: {
		email: string;
		amountCents: number;
		currency: 'usd' | 'eur';
		interval: 'month' | 'year' | null;
	}): Promise<string> {
		if (!this.stripe) {
			throw new StripePaymentNotAvailableError();
		}

		if (params.amountCents < MIN_DONATION_CENTS || params.amountCents > MAX_DONATION_CENTS) {
			throw new DonationAmountInvalidError();
		}

		const hasValidDns = await this.emailDnsValidationService.hasValidDnsRecords(params.email);
		if (!hasValidDns) {
			throw InputValidationError.fromCode('email', ValidationErrorCodes.INVALID_EMAIL_ADDRESS);
		}

		const isRecurring = params.interval !== null;
		const existingDonor = await this.donationRepository.findDonorByEmail(params.email);

		if (isRecurring && existingDonor?.hasActiveSubscription()) {
			const encodedEmail = encodeURIComponent(params.email);
			return `${Config.endpoints.marketing}/donate/manage?email=${encodedEmail}&alert=active_subscription`;
		}

		try {
			const mode: Stripe.Checkout.SessionCreateParams.Mode = isRecurring ? 'subscription' : 'payment';

			const lineItem: Stripe.Checkout.SessionCreateParams.LineItem = isRecurring
				? {
						price_data: {
							currency: params.currency,
							product_data: {
								name: 'Fluxer Recurring Donation',
								description: `${params.interval === 'month' ? 'Monthly' : 'Yearly'} donation to support Fluxer`,
							},
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
							product_data: {
								name: 'Fluxer Donation',
								description: 'One-time donation to support Fluxer',
							},
							unit_amount: params.amountCents,
						},
						quantity: 1,
					};

			const sessionParams: Stripe.Checkout.SessionCreateParams = {
				line_items: [lineItem],
				mode,
				metadata: {
					is_donation: 'true',
					donation_email: params.email,
					donation_type: isRecurring ? 'recurring' : 'one_time',
				},
				success_url: `${Config.endpoints.marketing}/donate/success`,
				cancel_url: `${Config.endpoints.marketing}/donate`,
				allow_promotion_codes: true,
				automatic_tax: {
					enabled: false,
				},
				tax_id_collection: {
					enabled: true,
				},
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
