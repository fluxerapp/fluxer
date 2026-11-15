// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID} from '@app/api/BrandedTypes';
import {isStripeResourceMissingError, syncStripeCustomerEmail} from '@app/api/stripe/StripeCustomer';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';

const syncStripeCustomerEmailTask: WorkerTaskHandler = async (payload, helpers) => {
	const {userRepository, stripe} = getWorkerDependencies();
	if (!stripe) {
		helpers.logger.debug('Stripe is disabled, skipping customer email sync');
		return;
	}
	const userIdStr = payload.userId as string;
	if (!userIdStr) {
		helpers.logger.warn({payload}, 'Stripe customer email sync task missing userId');
		return;
	}
	const user = await userRepository.findUnique(createUserID(BigInt(userIdStr)));
	if (!user) {
		return;
	}
	try {
		await syncStripeCustomerEmail(stripe, user);
	} catch (error) {
		if (isStripeResourceMissingError(error)) {
			helpers.logger.warn(
				{userId: userIdStr, customerId: user.stripeCustomerId},
				'Stripe customer no longer exists, skipping email sync',
			);
			return;
		}
		throw error;
	}
};

export default syncStripeCustomerEmailTask;
