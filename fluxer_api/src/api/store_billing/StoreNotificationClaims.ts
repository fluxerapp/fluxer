// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import type {WorkerTaskHelpers} from '@pkgs/worker/src/contracts/WorkerTask';
import {ms, seconds} from 'itty-time';

const NOTIFICATION_IN_FLIGHT_TTL_SECONDS = seconds('90 seconds');
const NOTIFICATION_PROCESSED_TTL_SECONDS = seconds('7 days');
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

type StoreNotificationClaimResult = 'claimed' | 'already_processed' | 'in_flight';

export function appStoreNotificationClaimKey(notificationUUID: string): string {
	return `app-store-notification:${notificationUUID}`;
}

export function googlePlayMessageClaimKey(messageId: string): string {
	return `google-play-message:${messageId}`;
}

export class StoreNotificationClaims {
	constructor(private readonly kv: IKVProvider) {}

	async tryClaim(key: string): Promise<StoreNotificationClaimResult> {
		const claimed = await this.kv.setnx(key, 'in_flight', NOTIFICATION_IN_FLIGHT_TTL_SECONDS);
		if (claimed) {
			return 'claimed';
		}
		const current = await this.kv.get(key);
		return current === 'processed' ? 'already_processed' : 'in_flight';
	}

	async markProcessed(key: string): Promise<void> {
		await this.kv.setex(key, NOTIFICATION_PROCESSED_TTL_SECONDS, 'processed');
	}

	async releaseClaim(key: string): Promise<void> {
		await this.kv.del(key);
	}
}
