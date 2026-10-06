// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {
	decodeGooglePlayDeveloperNotification,
	type GooglePlayDeveloperNotification,
} from '@app/api/store_billing/google_play/GooglePlayPushVerifier';
import {isGooglePlayConfigured, isGooglePlayPackageConfigured} from '@app/api/store_billing/StoreBillingConfig';
import type {StoreEntitlementService} from '@app/api/store_billing/StoreEntitlementService';
import {
	googlePlayMessageClaimKey,
	rescheduleStoreNotification,
	StoreNotificationClaims,
} from '@app/api/store_billing/StoreNotificationClaims';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import {StorePurchaseInvalidError} from '@fluxer/errors/src/domains/payment/StorePurchaseInvalidError';
import type {LoggerInterface} from '@fluxer/logger/src/LoggerInterface';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import {z} from 'zod';

const PayloadSchema = z.object({
	messageId: z.string().min(1),
	data: z.string().min(1),
	retry: z.number().int().min(0).optional(),
});

async function handleNotification(
	notification: GooglePlayDeveloperNotification,
	storeEntitlementService: StoreEntitlementService,
	logger: LoggerInterface,
): Promise<void> {
	const {packageName, event, eventTime} = notification;
	switch (event.type) {
		case 'test':
			logger.info({packageName}, 'Received Google Play test notification');
			return;
		case 'subscription':
			await storeEntitlementService.syncGooglePlaySubscription({packageName, purchaseToken: event.purchaseToken});
			return;
		case 'one_time_product':
			await storeEntitlementService.syncGooglePlayProduct({
				packageName,
				purchaseToken: event.purchaseToken,
				productId: event.productId,
			});
			return;
		case 'voided_purchase':
			await storeEntitlementService.applyGooglePlayVoided({
				packageName,
				purchaseToken: event.purchaseToken,
				orderId: event.orderId,
				productType: event.productType,
				voidedAt: eventTime,
			});
			return;
		case 'pending_refund_review':
			logger.warn({packageName, orderId: event.orderId}, 'Google Play opened a refund review for an order');
			return;
		default:
			logger.info({packageName}, 'Ignored Google Play notification of an unknown type');
	}
}

const processGooglePlayNotification: WorkerTaskHandler = async (payload, helpers) => {
	const {messageId, data, retry = 0} = PayloadSchema.parse(payload);
	if (Config.instance.selfHosted || !isGooglePlayConfigured()) {
		helpers.logger.warn('Google Play billing is not configured; discarding notification');
		return;
	}
	const notification = decodeGooglePlayDeveloperNotification(data);
	if (!notification) {
		helpers.logger.warn({messageId}, 'Discarded a Google Play notification that could not be decoded');
		return;
	}
	if (!isGooglePlayPackageConfigured(notification.packageName)) {
		helpers.logger.warn(
			{messageId, packageName: notification.packageName},
			'Discarded a Google Play notification for another package',
		);
		return;
	}
	const {kvClient, storeEntitlementService} = getWorkerDependencies();
	const claims = new StoreNotificationClaims(kvClient);
	const claimKey = googlePlayMessageClaimKey(messageId);
	const claim = await claims.tryClaim(claimKey);
	if (claim === 'already_processed') {
		return;
	}
	try {
		if (claim === 'in_flight') {
			throw new Error('Google Play notification is in flight, retrying');
		}
		try {
			try {
				await handleNotification(notification, storeEntitlementService, helpers.logger);
			} catch (error) {
				if (!(error instanceof StorePurchaseInvalidError)) {
					throw error;
				}
				helpers.logger.warn({messageId}, 'Google Play notification refers to a purchase that could not be verified');
			}
			await claims.markProcessed(claimKey);
		} catch (error) {
			await claims.releaseClaim(claimKey);
			throw error;
		}
	} catch (error) {
		if (await rescheduleStoreNotification(helpers, 'processGooglePlayNotification', {messageId, data}, retry, error)) {
			return;
		}
		throw error;
	}
};

export default processGooglePlayNotification;
