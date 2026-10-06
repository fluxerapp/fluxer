// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import type {User} from '@app/api/models/User';
import {getValidTimestamp, parseStoredTimestamp} from '@app/api/utils/TimestampUtils';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import {ms, seconds} from 'itty-time';

const INACTIVITY_WARNING_TTL_DAYS = 30;
const INACTIVITY_WARNING_PREFIX = 'inactivity_warning_sent';

export class UserDeletionEligibilityService {
	constructor(private readonly kvClient: IKVProvider) {}

	async isEligibleForInactivityDeletion(user: User): Promise<boolean> {
		if (user.isBot) {
			return false;
		}
		if (user.isSystem) {
			return false;
		}
		if (this.isAppStoreReviewer(user)) {
			return false;
		}
		if (user.pendingDeletionAt !== null) {
			return false;
		}
		if (user.lastActiveAt === null) {
			return false;
		}
		const inactivityThresholdMs = this.getInactivityThresholdMs();
		const lastActiveAt = getValidTimestamp(user.lastActiveAt, `Last activity timestamp for user ${user.id}`);
		return Date.now() - lastActiveAt >= inactivityThresholdMs;
	}

	async isEligibleForWarningEmail(user: User): Promise<boolean> {
		const isEligibleForDeletion = await this.isEligibleForInactivityDeletion(user);
		if (!isEligibleForDeletion) {
			return false;
		}
		return !(await this.hasWarningSent(user.id));
	}

	async markWarningSent(userId: UserID): Promise<void> {
		const key = this.getWarningKey(userId);
		const ttlSeconds = seconds(`${INACTIVITY_WARNING_TTL_DAYS + 5} days`);
		const timestamp = Date.now().toString();
		await this.kvClient.setex(key, ttlSeconds, timestamp);
	}

	async hasWarningSent(userId: UserID): Promise<boolean> {
		const key = this.getWarningKey(userId);
		const exists = await this.kvClient.exists(key);
		return exists === 1;
	}

	async getWarningSentTimestamp(userId: UserID): Promise<number | null> {
		const key = this.getWarningKey(userId);
		return parseStoredTimestamp(await this.kvClient.get(key), `Inactivity warning timestamp for user ${userId}`);
	}

	async hasWarningGracePeriodExpired(userId: UserID): Promise<boolean> {
		const timestamp = await this.getWarningSentTimestamp(userId);
		if (timestamp === null) {
			return false;
		}
		const timeSinceWarningMs = Date.now() - timestamp;
		const gracePeriodMs = INACTIVITY_WARNING_TTL_DAYS * ms('1 day');
		return timeSinceWarningMs >= gracePeriodMs;
	}

	private getInactivityThresholdMs(): number {
		const thresholdDays = Config.inactivityDeletionThresholdDays ?? 365 * 2;
		return thresholdDays * ms('1 day');
	}

	private getWarningKey(userId: UserID): string {
		return `${INACTIVITY_WARNING_PREFIX}:${userId}`;
	}

	private isAppStoreReviewer(user: User): boolean {
		return (user.flags & UserFlags.APP_STORE_REVIEWER) !== 0n;
	}
}
