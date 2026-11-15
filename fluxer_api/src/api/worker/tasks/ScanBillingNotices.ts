// SPDX-License-Identifier: AGPL-3.0-or-later

import {getWorkerService} from '@app/api/middleware/ServiceRegistry';
import {getEffectiveBillingConfig} from '@app/api/stripe/BillingConfigCache';
import {createBillingEmailService} from '@app/api/worker/tasks/SendBillingEmail';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';

const scanBillingNotices: WorkerTaskHandler = async (_payload, helpers) => {
	const {stripe} = getWorkerDependencies();
	if (!stripe || !getEffectiveBillingConfig().billingEmails) {
		return;
	}
	await createBillingEmailService(stripe, helpers.addJob).scanSubscriptionNotices(
		getWorkerService(),
		Math.floor(Date.now() / 1000),
	);
};

export default scanBillingNotices;
