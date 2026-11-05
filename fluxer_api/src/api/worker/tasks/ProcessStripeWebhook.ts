// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {createWorkerStripeWebhookService} from '@app/api/worker/tasks/WorkerStripeWebhookService';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import {z} from 'zod';

const PayloadSchema = z.object({
	body: z.string(),
	signature: z.string(),
});

const processStripeWebhook: WorkerTaskHandler = async (payload, helpers) => {
	const {body, signature} = PayloadSchema.parse(payload);
	const deps = getWorkerDependencies();
	const stripe = deps.stripe;
	if (!stripe) {
		if (Config.instance.selfHosted) {
			throw new Error('Stripe is not configured on this worker yet; retrying webhook event');
		}
		helpers.logger.warn('Stripe is not configured; discarding webhook event');
		return;
	}
	const webhookService = createWorkerStripeWebhookService(deps, stripe);
	await webhookService.handleWebhook({body, signature});
};

export default processStripeWebhook;
