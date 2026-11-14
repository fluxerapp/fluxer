// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {getCachedAccountsConfig} from '@app/api/instance/AccountsConfigCache';
import type {User} from '@app/api/models/User';
import type {OperatorPassRepository} from '@app/api/operator_pass/OperatorPassRepository';
import {isTemporarilyBanned} from '@app/api/user/UserHelpers';
import {getValidTimestamp, parseStoredTimestamp} from '@app/api/utils/TimestampUtils';
import {UserFlags, UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import {ms, seconds} from 'itty-time';

const INACTIVITY_WARNING_KEY_EXTRA_DAYS = 5;
const INACTIVITY_WARNING_PREFIX = 'inactivity_warning_sent';

export type OperatorPassHolderLookup = Pick<OperatorPassRepository, 'findOperatorPass'>;

export class UserDeletionEligibilityService {
	constructor(
		private readonly kvClient: IKVProvider,
		private readonly operatorPassLookup: OperatorPassHolderLookup | null = null,
	) {}

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
		if (isTemporarilyBanned(user)) {
			return false;
		}
		if (user.lastActiveAt === null) {
			return false;
		}
		const inactivityThresholdMs = this.getInactivityThresholdMs();
		const lastActiveAt = getValidTimestamp(user.lastActiveAt, `Last activity timestamp for user ${user.id}`);
		if (Date.now() - lastActiveAt < inactivityThresholdMs) {
			return false;
		}
		return !(await this.isProtectedByOperatorPass(user));
	}

	async isProtectedFromSystemDeletion(user: User): Promise<boolean> {
		return isTemporarilyBanned(user) || (await this.isProtectedByOperatorPass(user));
	}

	private async isProtectedByOperatorPass(user: User): Promise<boolean> {
		if (Config.instance.selfHosted) {
			return false;
		}
		if (user.premiumType === UserPremiumTypes.LIFETIME) {
			return true;
		}
		if (!this.operatorPassLookup) {
			return false;
		}
		return (await this.operatorPassLookup.findOperatorPass(user.id)) !== null;
	}

	async markWarningSent(userId: UserID): Promise<void> {
		const key = this.getWarningKey(userId);
		const ttlSeconds = seconds(`${this.getWarningLeadDays() + INACTIVITY_WARNING_KEY_EXTRA_DAYS} days`);
		const timestamp = Date.now().toString();
		await this.kvClient.setex(key, ttlSeconds, timestamp);
	}

	async hasWarningSent(userId: UserID): Promise<boolean> {
		const key = this.getWarningKey(userId);
		const exists = await this.kvClient.exists(key);
		return exists === 1;
	}

	private async getWarningSentTimestamp(userId: UserID): Promise<number | null> {
		const key = this.getWarningKey(userId);
		return parseStoredTimestamp(await this.kvClient.get(key), `Inactivity warning timestamp for user ${userId}`);
	}

	async hasWarningGracePeriodExpired(userId: UserID): Promise<boolean> {
		const timestamp = await this.getWarningSentTimestamp(userId);
		if (timestamp === null) {
			return false;
		}
		const timeSinceWarningMs = Date.now() - timestamp;
		const gracePeriodMs = this.getWarningLeadDays() * ms('1 day');
		return timeSinceWarningMs >= gracePeriodMs;
	}

	private getInactivityThresholdMs(): number {
		return getCachedAccountsConfig().inactivity_deletion.inactive_days * ms('1 day');
	}

	private getWarningLeadDays(): number {
		return getCachedAccountsConfig().inactivity_deletion.warning_lead_days;
	}

	private getWarningKey(userId: UserID): string {
		return `${INACTIVITY_WARNING_PREFIX}:${userId}`;
	}

	private isAppStoreReviewer(user: User): boolean {
		return (user.flags & UserFlags.APP_STORE_REVIEWER) !== 0n;
	}
}
