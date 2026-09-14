// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildID, UserID} from '@app/api/BrandedTypes';
import type {UserRow} from '@app/api/database/types/UserTypes';
import type {User} from '@app/api/models/User';

export interface UserDeletionScheduleUpdate {
	pending_deletion_at: Date | null;
	flags?: bigint;
	deletion_reason_code?: number | null;
	deletion_public_reason?: string | null;
	deletion_audit_log_reason?: string | null;
	temp_banned_until?: Date | null;
	first_refund_at?: Date | null;
}

export interface IUserAccountRepository {
	create(data: UserRow): Promise<User>;
	upsert(data: UserRow, oldData?: UserRow | null): Promise<User>;
	patchUpsert(userId: UserID, patchData: Partial<UserRow>, oldData?: UserRow | null): Promise<User>;
	updateDeletionSchedule(user: User, patch: UserDeletionScheduleUpdate): Promise<User>;
	startDeletion(userId: UserID, pendingDeletionAt: Date): Promise<User | null>;
	anonymizeForDeletion(user: User, patch: Partial<UserRow>): Promise<User>;
	completeDeletion(user: User): Promise<void>;
	findUnique(userId: UserID): Promise<User | null>;
	findUniqueAssert(userId: UserID): Promise<User>;
	findByUsernameDiscriminator(username: string, discriminator: number): Promise<User | null>;
	findDiscriminatorsByUsername(username: string): Promise<Set<number>>;
	findByEmail(email: string): Promise<User | null>;
	findByStripeSubscriptionId(stripeSubscriptionId: string): Promise<User | null>;
	findByStripeCustomerId(stripeCustomerId: string): Promise<User | null>;
	listUserIdsByLastActiveIp(
		lastActiveIp: string,
		limit: number,
		offset: number,
	): Promise<{
		userIds: Array<UserID>;
		total: number;
	}>;
	listUsers(userIds: Array<UserID>): Promise<Array<User>>;
	listAllUsersPaginated(limit: number, lastUserId?: UserID): Promise<Array<User>>;
	scanAllUsersPage(
		limit: number,
		pageState?: string | null,
	): Promise<{
		users: Array<User>;
		pageState: string | null;
	}>;
	getUserGuildIds(userId: UserID): Promise<Array<GuildID>>;
	addPendingDeletion(userId: UserID, pendingDeletionAt: Date, deletionReasonCode: number): Promise<void>;
	removePendingDeletion(userId: UserID, pendingDeletionAt: Date): Promise<void>;
	findUsersPendingDeletion(now: Date): Promise<Array<User>>;
	findUsersPendingDeletionByDate(deletionDate: string): Promise<
		Array<{
			user_id: bigint;
			deletion_reason_code: number;
		}>
	>;
	isUserPendingDeletion(userId: UserID, deletionDate: string): Promise<boolean>;
	scheduleDeletion(userId: UserID, pendingDeletionAt: Date, deletionReasonCode: number): Promise<void>;
	deleteUserSecondaryIndices(userId: UserID): Promise<void>;
	removeFromAllGuilds(userId: UserID): Promise<void>;
	updateLastActiveAt(params: {userId: UserID; lastActiveAt: Date; lastActiveIp?: string}): Promise<void>;
	updateSubscriptionStatus(
		userId: UserID,
		updates: {
			premiumWillCancel: boolean;
			computedPremiumUntil: Date | null;
		},
	): Promise<{
		finalVersion: number | null;
	}>;
	deleteAllPasswordResetTokens(userId: UserID): Promise<void>;
}
