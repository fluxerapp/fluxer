// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildID, UserID} from '@app/api/BrandedTypes';
import type {UserRow} from '@app/api/database/types/UserTypes';
import type {User} from '@app/api/models/User';
import {UserEmailOwnershipRepository} from '@app/api/user/repositories/account/crud/UserEmailOwnershipRepository';
import {UserAccountRepository as UserAccountCrudRepository} from '@app/api/user/repositories/account/UserAccountRepository';
import {UserDeletionRepository} from '@app/api/user/repositories/account/UserDeletionRepository';
import {UserGuildRepository} from '@app/api/user/repositories/account/UserGuildRepository';
import {UserLookupRepository} from '@app/api/user/repositories/account/UserLookupRepository';
import {TokenRepository} from '@app/api/user/repositories/auth/TokenRepository';
import type {
	IUserAccountRepository,
	UserDeletionScheduleUpdate,
} from '@app/api/user/repositories/IUserAccountRepository';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';

export class UserAccountRepository implements IUserAccountRepository {
	private accountRepo: UserAccountCrudRepository;
	private lookupRepo: UserLookupRepository;
	private deletionRepo: UserDeletionRepository;
	private guildRepo: UserGuildRepository;
	private tokenRepo: TokenRepository;

	constructor(kv: IKVProvider) {
		this.accountRepo = new UserAccountCrudRepository(kv);
		this.lookupRepo = new UserLookupRepository(
			this.accountRepo.findUnique.bind(this.accountRepo),
			new UserEmailOwnershipRepository(this.accountRepo.findUnique.bind(this.accountRepo), kv),
		);
		this.deletionRepo = new UserDeletionRepository(this.accountRepo.findUnique.bind(this.accountRepo));
		this.guildRepo = new UserGuildRepository();
		this.tokenRepo = new TokenRepository();
	}

	async create(data: UserRow): Promise<User> {
		return this.accountRepo.create(data);
	}

	async findUnique(userId: UserID): Promise<User | null> {
		return this.accountRepo.findUnique(userId);
	}

	async findUniqueAssert(userId: UserID): Promise<User> {
		return this.accountRepo.findUniqueAssert(userId);
	}

	async listAllUsersPaginated(limit: number, lastUserId?: UserID): Promise<Array<User>> {
		return this.accountRepo.listAllUsersPaginated(limit, lastUserId);
	}

	async scanAllUsersPage(
		limit: number,
		pageState?: string | null,
	): Promise<{
		users: Array<User>;
		pageState: string | null;
	}> {
		return this.accountRepo.scanAllUsersPage(limit, pageState);
	}

	async listUsers(userIds: Array<UserID>): Promise<Array<User>> {
		return this.accountRepo.listUsers(userIds);
	}

	async upsert(data: UserRow, oldData?: UserRow | null): Promise<User> {
		return this.accountRepo.upsert(data, oldData);
	}

	async patchUpsert(userId: UserID, patchData: Partial<UserRow>, oldData?: UserRow | null): Promise<User> {
		return this.accountRepo.patchUpsert(userId, patchData, oldData);
	}

	async updateDeletionSchedule(user: User, patch: UserDeletionScheduleUpdate): Promise<User> {
		return this.accountRepo.updateDeletionSchedule(user, patch);
	}

	async startDeletion(userId: UserID, pendingDeletionAt: Date): Promise<User | null> {
		return this.accountRepo.startDeletion(userId, pendingDeletionAt);
	}

	async anonymizeForDeletion(user: User, patch: Partial<UserRow>): Promise<User> {
		return this.accountRepo.anonymizeForDeletion(user, patch);
	}

	async completeDeletion(user: User): Promise<void> {
		return this.accountRepo.completeDeletion(user);
	}

	async deleteUserSecondaryIndices(userId: UserID): Promise<void> {
		return this.accountRepo.deleteUserSecondaryIndices(userId);
	}

	async findByEmail(email: string): Promise<User | null> {
		return this.lookupRepo.findByEmail(email);
	}

	async findByStripeCustomerId(stripeCustomerId: string): Promise<User | null> {
		return this.lookupRepo.findByStripeCustomerId(stripeCustomerId);
	}

	async findByStripeSubscriptionId(stripeSubscriptionId: string): Promise<User | null> {
		return this.lookupRepo.findByStripeSubscriptionId(stripeSubscriptionId);
	}

	async listUserIdsByLastActiveIp(
		lastActiveIp: string,
		limit: number,
		offset: number,
	): Promise<{
		userIds: Array<UserID>;
		total: number;
	}> {
		return this.lookupRepo.listUserIdsByLastActiveIp(lastActiveIp, limit, offset);
	}

	async findByUsernameDiscriminator(username: string, discriminator: number): Promise<User | null> {
		return this.lookupRepo.findByUsernameDiscriminator(username, discriminator);
	}

	async findDiscriminatorsByUsername(username: string): Promise<Set<number>> {
		return this.lookupRepo.findDiscriminatorsByUsername(username);
	}

	async getActivityTracking(userId: UserID): Promise<{
		last_active_at: Date | null;
		last_active_ip: string | null;
	}> {
		const result = await this.accountRepo.getActivityTracking(userId);
		return result ?? {last_active_at: null, last_active_ip: null};
	}

	async addPendingDeletion(userId: UserID, pendingDeletionAt: Date, deletionReasonCode: number): Promise<void> {
		return this.deletionRepo.addPendingDeletion(userId, pendingDeletionAt, deletionReasonCode);
	}

	async findUsersPendingDeletion(now: Date): Promise<Array<User>> {
		return this.deletionRepo.findUsersPendingDeletion(now);
	}

	async findUsersPendingDeletionByDate(deletionDate: string): Promise<
		Array<{
			user_id: bigint;
			deletion_reason_code: number;
		}>
	> {
		return this.deletionRepo.findUsersPendingDeletionByDate(deletionDate);
	}

	async isUserPendingDeletion(userId: UserID, deletionDate: string): Promise<boolean> {
		return this.deletionRepo.isUserPendingDeletion(userId, deletionDate);
	}

	async removePendingDeletion(userId: UserID, pendingDeletionAt: Date): Promise<void> {
		return this.deletionRepo.removePendingDeletion(userId, pendingDeletionAt);
	}

	async scheduleDeletion(userId: UserID, pendingDeletionAt: Date, deletionReasonCode: number): Promise<void> {
		return this.deletionRepo.scheduleDeletion(userId, pendingDeletionAt, deletionReasonCode);
	}

	async getUserGuildIds(userId: UserID): Promise<Array<GuildID>> {
		return this.guildRepo.getUserGuildIds(userId);
	}

	async removeFromAllGuilds(userId: UserID): Promise<void> {
		return this.guildRepo.removeFromAllGuilds(userId);
	}

	async updateLastActiveAt(params: {userId: UserID; lastActiveAt: Date; lastActiveIp?: string}): Promise<void> {
		return this.accountRepo.updateLastActiveAt(params);
	}

	async updateSubscriptionStatus(
		userId: UserID,
		updates: {
			premiumWillCancel: boolean;
			computedPremiumUntil: Date | null;
		},
	): Promise<{
		finalVersion: number | null;
	}> {
		return this.accountRepo.updateSubscriptionStatus(userId, updates);
	}

	async deleteAllPasswordResetTokens(userId: UserID): Promise<void> {
		return this.tokenRepo.deleteAllPasswordResetTokens(userId);
	}
}
