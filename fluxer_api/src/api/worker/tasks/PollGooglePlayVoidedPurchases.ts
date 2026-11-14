// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {getGooglePlayDeveloperApiClient} from '@app/api/middleware/ServiceSingletons';
import {isGooglePlayConfigured} from '@app/api/store_billing/StoreBillingConfig';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import {ms} from 'itty-time';

const VOIDED_WINDOW_MS = ms('2 days');
const MAX_PAGES_PER_PACKAGE = 50;

const pollGooglePlayVoidedPurchases: WorkerTaskHandler = async (_payload, helpers) => {
	if (Config.instance.selfHosted || !isGooglePlayConfigured()) {
		return;
	}
	const {storeEntitlementService} = getWorkerDependencies();
	const client = getGooglePlayDeveloperApiClient();
	const endTime = new Date();
	const startTime = new Date(endTime.getTime() - VOIDED_WINDOW_MS);
	let refreshed = 0;
	for (const packageName of Config.googlePlay.packages) {
		let pageToken: string | undefined;
		for (let page = 0; page < MAX_PAGES_PER_PACKAGE; page++) {
			const result = await client.listVoidedPurchases(packageName, {startTime, endTime, pageToken});
			for (const voided of result.voidedPurchases) {
				try {
					const voidedAt = voided.voidedTimeMillis ? new Date(Number(voided.voidedTimeMillis)) : null;
					const applied = await storeEntitlementService.applyGooglePlayVoided({
						packageName,
						purchaseToken: voided.purchaseToken,
						orderId: voided.orderId ?? null,
						productType: null,
						voidedAt: voidedAt && !Number.isNaN(voidedAt.getTime()) ? voidedAt : null,
					});
					if (applied) {
						refreshed += 1;
					}
				} catch (error) {
					helpers.logger.warn({error, orderId: voided.orderId}, 'Failed to refresh a voided Google Play purchase');
				}
			}
			if (!result.nextPageToken) {
				break;
			}
			pageToken = result.nextPageToken;
		}
	}
	helpers.logger.info({refreshed}, 'Polled Google Play voided purchases');
};

export default pollGooglePlayVoidedPurchases;
