// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {
	AppStoreJwsVerificationError,
	type AppStoreVerifiedNotification,
	toStoreEnvironment,
	verifyNotification,
} from '@app/api/store_billing/app_store/AppStoreJwsVerifier';
import {getAppStoreProductSlot, isAppStoreConfigured} from '@app/api/store_billing/StoreBillingConfig';
import type {StoreEntitlementService} from '@app/api/store_billing/StoreEntitlementService';
import {
	appStoreNotificationClaimKey,
	rescheduleStoreNotification,
	StoreNotificationClaims,
} from '@app/api/store_billing/StoreNotificationClaims';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import {StorePurchaseInvalidError} from '@fluxer/errors/src/domains/payment/StorePurchaseInvalidError';
import type {LoggerInterface} from '@fluxer/logger/src/LoggerInterface';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import {z} from 'zod';

const PayloadSchema = z.object({
	signedPayload: z.string().min(1),
	retry: z.number().int().min(0).optional(),
});

async function handleNotification(
	notification: AppStoreVerifiedNotification,
	storeEntitlementService: StoreEntitlementService,
	logger: LoggerInterface,
): Promise<void> {
	const context = {
		notificationType: notification.notificationType,
		subtype: notification.subtype,
		notificationUUID: notification.notificationUUID,
		environment: notification.environment,
	};
	if (notification.notificationType === 'TEST') {
		logger.info(context, 'Received App Store test notification');
		return;
	}
	const transaction = notification.transaction;
	if (!transaction) {
		logger.info(context, 'Acknowledged App Store notification without a transaction');
		return;
	}
	if (!getAppStoreProductSlot(transaction.productId)) {
		logger.info({...context, productId: transaction.productId}, 'Ignored App Store notification for another product');
		return;
	}
	try {
		await storeEntitlementService.syncAppStorePurchase({
			environment: toStoreEnvironment(notification.environment),
			bundleId: transaction.bundleId,
			productId: transaction.productId,
			transactionId: transaction.transactionId,
			originalTransactionId: transaction.originalTransactionId,
			hintAccountToken: notification.renewalInfo?.appAccountToken ?? transaction.appAccountToken ?? null,
		});
	} catch (error) {
		if (error instanceof StorePurchaseInvalidError) {
			logger.warn(context, 'App Store notification refers to a purchase that could not be verified');
			return;
		}
		throw error;
	}
}

const processAppStoreNotification: WorkerTaskHandler = async (payload, helpers) => {
	const {signedPayload, retry = 0} = PayloadSchema.parse(payload);
	if (Config.instance.selfHosted || !isAppStoreConfigured()) {
		helpers.logger.warn('App Store billing is not configured; discarding notification');
		return;
	}
	let notification: AppStoreVerifiedNotification;
	try {
		notification = await verifyNotification(signedPayload);
	} catch (error) {
		if (error instanceof AppStoreJwsVerificationError) {
			helpers.logger.warn({reason: error.reason}, 'Discarded App Store notification that failed verification');
			return;
		}
		throw error;
	}
	const {kvClient, storeEntitlementService} = getWorkerDependencies();
	const claims = new StoreNotificationClaims(kvClient);
	const claimKey = appStoreNotificationClaimKey(notification.notificationUUID);
	const claim = await claims.tryClaim(claimKey);
	if (claim === 'already_processed') {
		return;
	}
	try {
		if (claim === 'in_flight') {
			throw new Error('App Store notification is in flight, retrying');
		}
		try {
			await handleNotification(notification, storeEntitlementService, helpers.logger);
			await claims.markProcessed(claimKey);
		} catch (error) {
			await claims.releaseClaim(claimKey);
			throw error;
		}
	} catch (error) {
		if (await rescheduleStoreNotification(helpers, 'processAppStoreNotification', {signedPayload}, retry, error)) {
			return;
		}
		throw error;
	}
};

export default processAppStoreNotification;
