// SPDX-License-Identifier: AGPL-3.0-or-later

import {getProductRegistry} from '@app/api/stripe/ProductRegistry';
import {
	type BillingEmailJob,
	BillingEmailService,
	billingEmailJobKey,
} from '@app/api/stripe/services/BillingEmailService';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import {JobFailedError, type WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
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

const PayloadSchema = z.object({
	kind: z.enum(['invoice_paid', 'invoice_payment_problem', 'refund']),
	id: z.string().min(1),
	paymentAttempt: z.number().int().nonnegative().optional(),
	actionRequired: z.boolean().optional(),
	retry: z.number().int().nonnegative().optional(),
});

function toJob(payload: z.infer<typeof PayloadSchema>): BillingEmailJob {
	if (payload.kind === 'invoice_payment_problem') {
		return {
			kind: payload.kind,
			id: payload.id,
			paymentAttempt: payload.paymentAttempt ?? 0,
			actionRequired: payload.actionRequired ?? false,
		};
	}
	return {kind: payload.kind, id: payload.id};
}

const sendBillingEmail: WorkerTaskHandler = async (payload, helpers) => {
	const parsed = PayloadSchema.parse(payload);
	const job = toJob(parsed);
	const deps = getWorkerDependencies();
	if (!deps.stripe) {
		helpers.logger.warn({job}, 'Stripe is not configured; billing email not sent');
		return;
	}
	const service = new BillingEmailService(
		deps.stripe,
		deps.userRepository,
		deps.emailService,
		deps.cacheService,
		getProductRegistry(),
		deps.donationRepository,
		deps.operatorPassRepository,
	);
	try {
		await service.deliver(job);
	} catch (error) {
		if (error instanceof Stripe.errors.StripeInvalidRequestError) {
			helpers.logger.error({job, error}, 'Stripe rejected the lookup for a billing email; not retrying');
			return;
		}
		const retry = parsed.retry ?? 0;
		const delay = RETRY_DELAYS_MS[retry];
		if (delay === undefined) {
			throw new JobFailedError(`Billing email ${billingEmailJobKey(job)} failed after ${retry} retries`);
		}
		helpers.logger.warn({job, retry, error}, 'Billing email failed; trying again later');
		await helpers.addJob(
			'sendBillingEmail',
			{...job, retry: retry + 1},
			{runAt: new Date(Date.now() + delay), jobKey: `${billingEmailJobKey(job)}:retry:${retry + 1}`},
		);
	}
};

export default sendBillingEmail;
