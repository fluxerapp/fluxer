// SPDX-License-Identifier: AGPL-3.0-or-later

import type {WorkerTaskHelpers} from '@pkgs/worker/src/contracts/WorkerTask';
import {ms} from 'itty-time';

const STORE_NOTIFICATION_MAX_RETRIES = 12;
const STORE_NOTIFICATION_RETRY_BASE_MS = ms('5 minutes');
const STORE_NOTIFICATION_RETRY_MAX_MS = ms('12 hours');

function storeNotificationRetryDelayMs(retry: number): number {
	return Math.min(STORE_NOTIFICATION_RETRY_BASE_MS * 2 ** retry, STORE_NOTIFICATION_RETRY_MAX_MS);
}

export async function rescheduleStoreNotification(
	helpers: WorkerTaskHelpers,
	taskType: 'processAppStoreNotification' | 'processGooglePlayNotification',
	payload: Record<string, string>,
	retry: number,
	error: unknown,
): Promise<boolean> {
	if (!helpers.attempt?.isLastAttempt || retry >= STORE_NOTIFICATION_MAX_RETRIES) {
		return false;
	}
	await helpers.addJob(
		taskType,
		{...payload, retry: retry + 1},
		{runAt: new Date(Date.now() + storeNotificationRetryDelayMs(retry))},
	);
	helpers.logger.warn({error, retry: retry + 1}, 'Rescheduled a store notification after repeated failures');
	return true;
}

export function appStoreNotificationClaimKey(notificationUUID: string): string {
	return `app-store-notification:${notificationUUID}`;
}

export function googlePlayMessageClaimKey(messageId: string): string {
	return `google-play-message:${messageId}`;
}
