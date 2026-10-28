// SPDX-License-Identifier: AGPL-3.0-or-later

import {getEffectiveBillingConfig} from '@app/api/stripe/BillingConfigCache';
import {getProductRegistry} from '@app/api/stripe/ProductRegistry';
import {
	BillingEmailJobSchema,
	BillingEmailService,
	billingEmailJobKey,
} from '@app/api/stripe/services/BillingEmailService';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import {JobFailedError, type WorkerTaskHandler, type WorkerTaskHelpers} from '@pkgs/worker/src/contracts/WorkerTask';
import {ms} from 'itty-time';
import Stripe from 'stripe';
import {z} from 'zod';

const RETRY_DELAYS_MS = [
	ms('1 minute'),
	ms('5 minutes'),
	ms('15 minutes'),
	ms('1 hour'),
	ms('3 hours'),
	ms('6 hours'),
	ms('12 hours'),
	ms('1 day'),
	ms('1 day'),
	ms('1 day'),
];

const RetrySchema = z.object({retry: z.number().int().nonnegative().optional()});

export function createBillingEmailService(stripe: Stripe, addJob: WorkerTaskHelpers['addJob']): BillingEmailService {
	const deps = getWorkerDependencies();
	return new BillingEmailService(
		stripe,
		deps.userRepository,
		deps.emailService,
		deps.cacheService,
		getProductRegistry(),
		deps.donationRepository,
		deps.operatorPassRepository,
		addJob,
	);
}

const sendBillingEmail: WorkerTaskHandler = async (payload, helpers) => {
	const job = BillingEmailJobSchema.parse(payload);
	const attempt = RetrySchema.parse(payload).retry ?? 0;
	if (!getEffectiveBillingConfig().billingEmails) {
		helpers.logger.info({job}, 'Billing emails are turned off; not sending a queued billing email');
		return;
	}
	const {stripe} = getWorkerDependencies();
	if (!stripe) {
		helpers.logger.warn({job}, 'Stripe is not configured; billing email not sent');
		return;
	}
	try {
		await createBillingEmailService(stripe, helpers.addJob).deliver(job, attempt);
	} catch (error) {
		if (error instanceof Stripe.errors.StripeInvalidRequestError) {
			helpers.logger.error({job, error}, 'Stripe rejected the lookup for a billing email; not retrying');
			return;
		}
		const delay = RETRY_DELAYS_MS[attempt];
		if (delay === undefined) {
			throw new JobFailedError(`Billing email ${billingEmailJobKey(job)} failed after ${attempt} retries`);
		}
		helpers.logger.warn({job, retry: attempt, error}, 'Billing email failed; trying again later');
		await helpers.addJob(
			'sendBillingEmail',
			{...job, retry: attempt + 1},
			{runAt: new Date(Date.now() + delay), jobKey: `${billingEmailJobKey(job)}:retry:${attempt + 1}`},
		);
	}
};

export default sendBillingEmail;
