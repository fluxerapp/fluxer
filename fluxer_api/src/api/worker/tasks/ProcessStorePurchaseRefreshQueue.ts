// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {isAppStoreConfigured, isGooglePlayConfigured} from '@app/api/store_billing/StoreBillingConfig';
import {redactStoreKey} from '@app/api/store_billing/StoreBillingTypes';
import {STORE_PURCHASE_REFRESH_QUEUE_KEY} from '@app/api/store_billing/StorePurchaseRefresh';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import {ms} from 'itty-time';

const MAX_KEYS_PER_RUN = 200;
const REFRESH_LEASE_MS = ms('10 minutes');

const processStorePurchaseRefreshQueue: WorkerTaskHandler = async (_payload, helpers) => {
	if (Config.instance.selfHosted || (!isAppStoreConfigured() && !isGooglePlayConfigured())) {
		return;
	}
	const {kvClient, workerService} = getWorkerDependencies();
	const now = Date.now();
	const dueKeys = await kvClient.zrangebyscore(
		STORE_PURCHASE_REFRESH_QUEUE_KEY,
		'-inf',
		now,
		'LIMIT',
		0,
		MAX_KEYS_PER_RUN,
	);
	if (dueKeys.length === 0) {
		return;
	}
	let enqueued = 0;
	for (const storeKey of dueKeys) {
		await kvClient.zadd(STORE_PURCHASE_REFRESH_QUEUE_KEY, now + REFRESH_LEASE_MS, storeKey);
		try {
			await workerService.addJob('refreshStorePurchase', {storeKey}, {jobKey: `refreshStorePurchase:${storeKey}`});
			enqueued += 1;
		} catch (error) {
			helpers.logger.warn({error, storeKey: redactStoreKey(storeKey)}, 'Failed to enqueue a store purchase refresh');
		}
	}
	helpers.logger.info({due: dueKeys.length, enqueued}, 'Enqueued store purchase refreshes');
};

export default processStorePurchaseRefreshQueue;
