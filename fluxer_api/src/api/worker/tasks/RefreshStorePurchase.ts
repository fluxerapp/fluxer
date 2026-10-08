// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {redactStoreKey} from '@app/api/store_billing/StoreBillingTypes';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import {StorePurchaseInvalidError} from '@fluxer/errors/src/domains/payment/StorePurchaseInvalidError';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import {z} from 'zod';

const PayloadSchema = z.object({
	storeKey: z.string().min(1),
});

const refreshStorePurchase: WorkerTaskHandler = async (payload, helpers) => {
	const {storeKey} = PayloadSchema.parse(payload);
	if (Config.instance.selfHosted) {
		return;
	}
	try {
		await getWorkerDependencies().storeEntitlementService.refreshStorePurchase(storeKey);
	} catch (error) {
		if (error instanceof StorePurchaseInvalidError) {
			helpers.logger.warn({storeKey: redactStoreKey(storeKey)}, 'Store purchase could not be verified during refresh');
			return;
		}
		throw error;
	}
};

export default refreshStorePurchase;
