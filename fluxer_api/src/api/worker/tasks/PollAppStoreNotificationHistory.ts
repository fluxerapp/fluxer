// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {getAppStoreServerApiClient} from '@app/api/middleware/ServiceSingletons';
import {isAppStoreConfigured} from '@app/api/store_billing/StoreBillingConfig';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import {ms} from 'itty-time';

const HISTORY_WINDOW_MS = ms('2 days');
const MAX_PAGES_PER_QUERY = 50;
const HISTORY_QUERIES: ReadonlyArray<{notificationType?: string; onlyFailures?: boolean}> = [
	{onlyFailures: true},
	{notificationType: 'REFUND'},
	{notificationType: 'REFUND_REVERSED'},
	{notificationType: 'REVOKE'},
];

const pollAppStoreNotificationHistory: WorkerTaskHandler = async (_payload, helpers) => {
	if (Config.instance.selfHosted || !isAppStoreConfigured()) {
		return;
	}
	const {workerService} = getWorkerDependencies();
	const client = getAppStoreServerApiClient();
	const endDate = new Date();
	const startDate = new Date(endDate.getTime() - HISTORY_WINDOW_MS);
	const seen = new Set<string>();
	for (const app of Config.appStore.apps) {
		for (const query of HISTORY_QUERIES) {
			let paginationToken: string | null = null;
			for (let page = 0; page < MAX_PAGES_PER_QUERY; page++) {
				const result = await client.getNotificationHistory({
					environment: 'production',
					bundleId: app.bundleId,
					startDate,
					endDate,
					paginationToken,
					...query,
				});
				for (const signedPayload of result.signedPayloads) {
					if (seen.has(signedPayload)) {
						continue;
					}
					seen.add(signedPayload);
					await workerService.addJob('processAppStoreNotification', {signedPayload});
				}
				if (!result.paginationToken) {
					break;
				}
				paginationToken = result.paginationToken;
			}
		}
	}
	helpers.logger.info({enqueued: seen.size}, 'Polled App Store notification history');
};

export default pollAppStoreNotificationHistory;
