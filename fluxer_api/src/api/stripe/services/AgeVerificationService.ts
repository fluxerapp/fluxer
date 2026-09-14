// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID, type UserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import {Logger} from '@app/api/Logger';
import {getBillingRepository} from '@app/api/middleware/ServiceRegistry';
import type {User} from '@app/api/models/User';
import type {IUserRepository} from '@app/api/user/IUserRepository';
import {mapUserToPrivateResponse} from '@app/api/user/UserMappers';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {AgeVerificationAlreadyVerifiedError} from '@fluxer/errors/src/domains/payment/AgeVerificationAlreadyVerifiedError';
import {StripeError} from '@fluxer/errors/src/domains/payment/StripeError';
import {StripePaymentNotAvailableError} from '@fluxer/errors/src/domains/payment/StripePaymentNotAvailableError';
import {UnknownUserError} from '@fluxer/errors/src/domains/user/UnknownUserError';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import {seconds} from 'itty-time';
import type Stripe from 'stripe';

const CUSTOMER_LOCK_TTL_SECONDS = seconds('30 seconds');

export class AgeVerificationService {
	constructor(
		private stripe: Stripe | null,
		private userRepository: IUserRepository,
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
		const customerUser = await this.ensureStripeCustomer(user);
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
		const updatedUser = await this.userRepository.patchUpsert(
			user.id,
			{flags: user.flags | UserFlags.AGE_VERIFIED_ADULT},
			user.toRow(),
		);
		await this.gatewayService.dispatchPresence({
			userId: updatedUser.id,
			event: 'USER_UPDATE',
			data: mapUserToPrivateResponse(updatedUser),
		});
		Logger.info({userId}, 'Age verification completed successfully');
	}

	private async ensureStripeCustomer(user: User): Promise<User> {
		if (user.stripeCustomerId) {
			return user;
		}
		if (!this.stripe) {
			throw new StripePaymentNotAvailableError();
		}
		const lockKey = `stripe_customer_create_lock:${user.id}`;
		const lockToken = await this.cacheService.acquireLock(lockKey, CUSTOMER_LOCK_TTL_SECONDS);
		if (!lockToken) {
			const freshUser = await this.userRepository.findUnique(user.id);
			if (freshUser?.stripeCustomerId) {
				return freshUser;
			}
			throw new StripeError('Failed to acquire customer creation lock');
		}
		try {
			const freshUser = await this.userRepository.findUnique(user.id);
			if (freshUser?.stripeCustomerId) {
				return freshUser;
			}
			const customer = await this.stripe.customers.create({
				email: user.email ?? undefined,
				metadata: {
					userId: user.id.toString(),
				},
			});
			try {
				await getBillingRepository().customers.upsertFromStripe(customer, {knownUserId: user.id});
			} catch (mirrorErr) {
				Logger.error(
					{mirrorErr, customerId: customer.id},
					'Mirror upsert failed after Stripe write; reconciler will heal',
				);
			}
			const updatedUser = await this.userRepository.patchUpsert(
				user.id,
				{stripe_customer_id: customer.id},
				user.toRow(),
			);
			Logger.debug({userId: user.id, customerId: customer.id}, 'Stripe customer created for age verification');
			return updatedUser;
		} finally {
			try {
				const released = await this.cacheService.releaseLock(lockKey, lockToken);
				if (!released) {
					Logger.warn({userId: user.id, lockKey}, 'Customer creation lock token no longer matched on release');
				}
			} catch (error) {
				Logger.error({error, userId: user.id, lockKey}, 'Failed to release customer creation lock');
			}
		}
	}
}
