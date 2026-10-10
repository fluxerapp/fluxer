// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/api/Logger';
import {getBillingRepository} from '@app/api/middleware/ServiceRegistry';
import type {User} from '@app/api/models/User';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import type {WorkerTaskName} from '@app/api/worker/WorkerLaneConfig';
import {StripeError} from '@fluxer/errors/src/domains/payment/StripeError';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import type {IWorkerService} from '@pkgs/worker/src/contracts/IWorkerService';
import {seconds} from 'itty-time';
import type Stripe from 'stripe';

const CUSTOMER_LOCK_TTL_SECONDS = seconds('30 seconds');

interface EnsureStripeCustomerParams {
	stripe: Stripe;
	user: User;
	userRepository: UserRepository;
	cacheService: ICacheService;
}

export function isStripeResourceMissingError(error: unknown): boolean {
	return typeof error === 'object' && error !== null && 'code' in error && error.code === 'resource_missing';
}

export async function ensureStripeCustomer({
	stripe,
	user,
	userRepository,
	cacheService,
}: EnsureStripeCustomerParams): Promise<User> {
	if (user.stripeCustomerId) {
		try {
			await syncStripeCustomerEmail(stripe, user);
		} catch (error) {
			Logger.warn(
				{error, userId: user.id, customerId: user.stripeCustomerId},
				'Failed to sync Stripe customer email, continuing with the stored customer',
			);
		}
		return user;
	}
	const lockKey = `stripe_customer_create_lock:${user.id}`;
	const lockToken = await cacheService.acquireLock(lockKey, CUSTOMER_LOCK_TTL_SECONDS);
	if (!lockToken) {
		const freshUser = await userRepository.findUnique(user.id);
		if (freshUser?.stripeCustomerId) {
			return freshUser;
		}
		throw new StripeError('Failed to acquire customer creation lock');
	}
	try {
		const freshUser = await userRepository.findUnique(user.id);
		if (freshUser?.stripeCustomerId) {
			return freshUser;
		}
		const customer = await stripe.customers.create({
			email: user.email ?? undefined,
			metadata: {
				userId: user.id.toString(),
			},
		});
		await mirrorStripeCustomer(customer, user);
		const updatedUser = await userRepository.patchUpsert(user.id, {stripe_customer_id: customer.id}, user.toRow());
		Logger.debug({userId: user.id, customerId: customer.id}, 'Stripe customer created');
		return updatedUser;
	} finally {
		try {
			const released = await cacheService.releaseLock(lockKey, lockToken);
			if (!released) {
				Logger.warn({userId: user.id, lockKey}, 'Customer creation lock token no longer matched on release');
			}
		} catch (error) {
			Logger.error({error, userId: user.id, lockKey}, 'Failed to release customer creation lock');
		}
	}
}

export async function syncStripeCustomerEmail(stripe: Stripe, user: User): Promise<void> {
	const customerId = user.stripeCustomerId;
	const email = user.email;
	if (!customerId || !email) {
		return;
	}
	const mirrored = await getBillingRepository().customers.findById(customerId);
	if (mirrored?.deleted || mirrored?.email === email) {
		return;
	}
	const customer = await stripe.customers.update(customerId, {email});
	await mirrorStripeCustomer(customer, user);
	Logger.info({userId: user.id, customerId}, 'Synced Stripe customer email with account email');
}

export async function enqueueStripeCustomerEmailSync(
	workerService: IWorkerService<WorkerTaskName>,
	oldUser: User,
	newUser: User,
): Promise<void> {
	if (!newUser.stripeCustomerId || !newUser.email || oldUser.email === newUser.email) {
		return;
	}
	try {
		await workerService.addJob('syncStripeCustomerEmail', {userId: newUser.id.toString()});
	} catch (error) {
		Logger.warn(
			{error, userId: newUser.id, customerId: newUser.stripeCustomerId},
			'Failed to enqueue Stripe customer email sync, checkout will sync it instead',
		);
	}
}

async function mirrorStripeCustomer(customer: Stripe.Customer, user: User): Promise<void> {
	try {
		await getBillingRepository().customers.upsertFromStripe(customer, {knownUserId: user.id});
	} catch (mirrorErr) {
		Logger.error({mirrorErr, customerId: customer.id}, 'Mirror upsert failed after Stripe write; reconciler will heal');
	}
}
