// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID, type UserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import {Logger} from '@app/api/Logger';
import {getBillingRepository} from '@app/api/middleware/ServiceRegistry';
import {ensureStripeCustomer} from '@app/api/stripe/StripeCustomer';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {mapUserToPrivateResponse} from '@app/api/user/UserMappers';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {AgeVerificationAlreadyVerifiedError} from '@fluxer/errors/src/domains/payment/AgeVerificationAlreadyVerifiedError';
import {StripeError} from '@fluxer/errors/src/domains/payment/StripeError';
import {StripePaymentNotAvailableError} from '@fluxer/errors/src/domains/payment/StripePaymentNotAvailableError';
import {UnknownUserError} from '@fluxer/errors/src/domains/user/UnknownUserError';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import type Stripe from 'stripe';

export class AgeVerificationService {
	constructor(
		private stripe: Stripe | null,
		private userRepository: UserRepository,
		private gatewayService: IGatewayService,
		private cacheService: ICacheService,
	) {}

	async createVerificationSession(userId: UserID): Promise<string> {
		if (!this.stripe) {
			throw new StripePaymentNotAvailableError();
		}
		const user = await this.userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}
		if (user.flags & UserFlags.AGE_VERIFIED_ADULT) {
			throw new AgeVerificationAlreadyVerifiedError();
		}
		const customerUser = await ensureStripeCustomer({
			stripe: this.stripe,
			user,
			userRepository: this.userRepository,
			cacheService: this.cacheService,
		});
		const customerId = customerUser.stripeCustomerId;
		if (!customerId) {
			throw new StripeError('Stripe customer id missing after customer setup');
		}
		try {
			const session = await this.stripe.checkout.sessions.create({
				customer: customerId,
				client_reference_id: userId.toString(),
				metadata: {
					user_id: userId.toString(),
					verification_type: 'uk_age_verification',
				},
				mode: 'setup',
				payment_method_types: ['card'],
				payment_method_options: {
					card: {
						request_three_d_secure: 'any',
					},
				},
				success_url: `${Config.endpoints.webApp}/age-verification-callback?status=success`,
				cancel_url: `${Config.endpoints.webApp}/age-verification-callback?status=cancel`,
			});
			try {
				await getBillingRepository().checkoutSessions.upsertFromStripe(session, {knownUserId: userId});
			} catch (mirrorErr) {
				Logger.error(
					{mirrorErr, sessionId: session.id},
					'Mirror upsert failed after Stripe write; reconciler will heal',
				);
			}
			if (!session.url) {
				Logger.error({userId, sessionId: session.id}, 'Stripe age verification session missing url');
				throw new StripeError('Stripe age verification session missing url');
			}
			Logger.debug({userId, sessionId: session.id}, 'Age verification checkout session created');
			return session.url;
		} catch (error: unknown) {
			if (error instanceof StripeError || error instanceof AgeVerificationAlreadyVerifiedError) {
				throw error;
			}
			Logger.error({error, userId}, 'Failed to create Stripe age verification session');
			const message = error instanceof Error ? error.message : 'Failed to create age verification session';
			throw new StripeError(message);
		}
	}

	async completeVerification(session: Stripe.Checkout.Session): Promise<void> {
		if (!this.stripe) {
			throw new StripePaymentNotAvailableError();
		}
		const userId = session.metadata?.user_id;
		if (!userId) {
			Logger.error({sessionId: session.id}, 'Age verification session missing user_id metadata');
			return;
		}
		const setupIntentId = typeof session.setup_intent === 'string' ? session.setup_intent : session.setup_intent?.id;
		if (!setupIntentId) {
			Logger.error({sessionId: session.id, userId}, 'Age verification session missing setup_intent');
			return;
		}
		const setupIntent = await this.stripe.setupIntents.retrieve(setupIntentId, {
			expand: ['payment_method'],
		});
		const paymentMethod = setupIntent.payment_method;
		if (!paymentMethod || typeof paymentMethod === 'string') {
			Logger.error({sessionId: session.id, userId}, 'Could not resolve payment method for age verification');
			return;
		}
		const funding = paymentMethod.card?.funding;
		if (funding !== 'credit') {
			Logger.info({sessionId: session.id, userId, funding}, 'Age verification rejected: card is not a credit card');
			return;
		}
		const parsedUserId = createUserID(BigInt(userId));
		const user = await this.userRepository.findUnique(parsedUserId);
		if (!user) {
			Logger.error({userId}, 'User not found during age verification completion');
			return;
		}
		if (user.flags & UserFlags.AGE_VERIFIED_ADULT) {
			Logger.debug({userId}, 'User already age-verified, skipping flag update');
			return;
		}
		const updatedUser = await this.userRepository.updateFlags(user.id, (flags) => flags | UserFlags.AGE_VERIFIED_ADULT);
		if (!updatedUser) {
			Logger.error({userId}, 'User not found during age verification completion');
			return;
		}
		await this.gatewayService.dispatchPresence({
			userId: updatedUser.id,
			event: 'USER_UPDATE',
			data: mapUserToPrivateResponse(updatedUser),
		});
		Logger.info({userId}, 'Age verification completed successfully');
	}
}
