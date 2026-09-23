// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import {Db, type DbOp} from '@app/api/database/CassandraTypes';
import type {UserRow} from '@app/api/database/types/UserTypes';
import {Logger} from '@app/api/Logger';
import {User} from '@app/api/models/User';
import {
	UserDataRepository,
	type UserDeletionTransition,
} from '@app/api/user/repositories/account/crud/UserDataRepository';
import {
	type EmailClaimReservation,
	UserEmailOwnershipRepository,
} from '@app/api/user/repositories/account/crud/UserEmailOwnershipRepository';
import {UserIndexRepository} from '@app/api/user/repositories/account/crud/UserIndexRepository';
import {UserSearchRepository} from '@app/api/user/repositories/account/crud/UserSearchRepository';
import {UserLookupRepository} from '@app/api/user/repositories/account/UserLookupRepository';
import type {UserDeletionScheduleUpdate} from '@app/api/user/repositories/IUserAccountRepository';
import {
	extractPremiumFlagsFromLegacyUserFlags,
	LEGACY_DEAD_USER_FLAGS_MASK,
	LEGACY_PREMIUM_FLAGS_MASK,
} from '@fluxer/constants/src/UserConstants';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';

export class UserAccountRepository {
	private dataRepo: UserDataRepository;
	private emailOwnershipRepo: UserEmailOwnershipRepository;
	private indexRepo: UserIndexRepository;
	private searchRepo: UserSearchRepository;
	private lookupRepo: UserLookupRepository;

	constructor(kv: IKVProvider) {
		this.dataRepo = new UserDataRepository();
		this.emailOwnershipRepo = new UserEmailOwnershipRepository((userId: UserID) => this.findUnique(userId), kv);
		this.indexRepo = new UserIndexRepository();
		this.searchRepo = new UserSearchRepository();
		this.lookupRepo = new UserLookupRepository((userId: UserID) => this.findUnique(userId), this.emailOwnershipRepo);
	}

	async create(data: UserRow): Promise<User> {
		return this.upsert(data);
	}

	async findUnique(userId: UserID): Promise<User | null> {
		return this.dataRepo.findUnique(userId);
	}

	async findUniqueAssert(userId: UserID): Promise<User> {
		return this.dataRepo.findUniqueAssert(userId);
	}

	async listAllUsersPaginated(limit: number, lastUserId?: UserID): Promise<Array<User>> {
		return this.dataRepo.listAllUsersPaginated(limit, lastUserId);
	}

	async scanAllUsersPage(
		limit: number,
		pageState?: string | null,
	): Promise<{
		users: Array<User>;
		pageState: string | null;
	}> {
		return this.dataRepo.scanAllUsersPage(limit, pageState);
	}

	async listUsers(userIds: Array<UserID>): Promise<Array<User>> {
		return this.dataRepo.listUsers(userIds);
	}

	async upsert(data: UserRow, oldData?: UserRow | null): Promise<User> {
		const userId = data.user_id;
		if (oldData === undefined) {
			oldData = (await this.findUnique(userId))?.toRow() ?? null;
		}
		this.applyPremiumFlagsMigration(data, oldData);
		const claimedNewEmail = this.didEmailChange(oldData, data);
		let emailClaim: EmailClaimReservation | null = null;
		if (claimedNewEmail && data.email) {
			emailClaim = await this.emailOwnershipRepo.claimEmail(data.email, userId);
		}
		let dataCommitted = false;
		try {
			const result = await this.dataRepo.upsertUserRow(data, oldData);
			if (result.finalVersion === null) {
				throw new Error(`Failed to update user ${userId} after max retries due to concurrent updates`);
			}
			dataCommitted = true;
			const updatedData = result.updatedData;
			const previousData = result.previousData;
			const updatedUser = new User(updatedData);
			await this.emailOwnershipRepo.finalizeEmailClaim(emailClaim);
			await this.releasePreviousEmailIfChanged(previousData, updatedData);
			await this.indexRepo.syncIndices(updatedData, previousData);
			await this.searchRepo.indexUser(updatedUser);
			return updatedUser;
		} catch (error) {
			if (!dataCommitted && emailClaim) {
				await this.emailOwnershipRepo.abortEmailClaim(emailClaim).catch((abortError: unknown) => {
					Logger.warn(
						{userId: userId.toString(), abortError},
						'Failed to abort the email claim of a user write that did not commit',
					);
				});
			}
			throw error;
		}
	}

	async patchUpsert(userId: UserID, patchData: Partial<UserRow>, oldData?: UserRow | null): Promise<User> {
		return this.patchAccount(userId, patchData, oldData);
	}

	async updateDeletionSchedule(user: User, patch: UserDeletionScheduleUpdate): Promise<User> {
		return this.patchAccount(user.id, patch, user.toRow(), 'schedule');
	}

	async startDeletion(userId: UserID, pendingDeletionAt: Date): Promise<User | null> {
		return this.dataRepo.startDeletion(userId, pendingDeletionAt);
	}

	async anonymizeForDeletion(user: User, patch: Partial<UserRow>): Promise<User> {
		return this.patchAccount(user.id, patch, user.toRow(), 'anonymise');
	}

	async completeDeletion(user: User): Promise<void> {
		await this.dataRepo.patchDeletion(user, {pending_deletion_at: Db.clear()}, 'complete');
	}

	private async patchAccount(
		userId: UserID,
		patchData: Partial<UserRow>,
		oldData?: UserRow | null,
		deletionTransition?: UserDeletionTransition,
	): Promise<User> {
		if (!oldData) {
			const existingUser = await this.findUniqueAssert(userId);
			oldData = existingUser.toRow();
		}
		patchData = this.migratePremiumFlagsInPatch(patchData, oldData);
		const userPatch: Record<string, DbOp<unknown>> = {};
		for (const [key, value] of Object.entries(patchData)) {
			if (key === 'user_id' || value === undefined) continue;
			if (value !== null) {
				userPatch[key] = Db.set(value);
				continue;
			}
			if (oldData[key as keyof UserRow] != null) {
				userPatch[key] = Db.clear();
			}
		}
		const nextEmail = typeof patchData.email === 'string' ? patchData.email : null;
		const claimedNewEmail = nextEmail !== null && this.didEmailChange(oldData, {email: nextEmail});
		let emailClaim: EmailClaimReservation | null = null;
		if (claimedNewEmail && nextEmail) {
			emailClaim = await this.emailOwnershipRepo.claimEmail(nextEmail, userId);
		}
		let dataCommitted = false;
		try {
			const result = deletionTransition
				? await this.dataRepo.patchDeletion(new User(oldData), userPatch, deletionTransition)
				: await this.dataRepo.patchUser(userId, userPatch, oldData);
			if (result.finalVersion === null) {
				throw new Error(`Failed to update user ${userId} due to concurrent modification`);
			}
			dataCommitted = true;
			const updatedData = result.updatedData;
			const previousData = result.previousData;
			const updatedUser = new User(updatedData);
			await this.emailOwnershipRepo.finalizeEmailClaim(emailClaim);
			await this.releasePreviousEmailIfChanged(previousData, updatedData);
			if (deletionTransition !== 'anonymise') {
				await this.indexRepo.syncIndices(updatedData, previousData);
			}
			await this.searchRepo.updateUser(updatedUser);
			return updatedUser;
		} catch (error) {
			if (!dataCommitted && emailClaim) {
				await this.emailOwnershipRepo.abortEmailClaim(emailClaim);
			}
			throw error;
		}
	}

	async deleteUserSecondaryIndices(userId: UserID): Promise<void> {
		const user = await this.findUnique(userId);
		if (!user) return;
		await this.indexRepo.deleteIndices(
			userId,
			user.username,
			user.discriminator,
			user.email,
			user.stripeCustomerId,
			user.stripeSubscriptionId,
			user.lastActiveIp,
		);
	}

	async updateLastActiveAt(params: {userId: UserID; lastActiveAt: Date; lastActiveIp?: string}): Promise<void> {
		const activityUpdate = await this.dataRepo.updateLastActiveAt(params);
		if (params.lastActiveIp !== undefined) {
			await this.indexRepo.updateLastActiveIpIndex(
				params.userId,
				activityUpdate.updatedData.last_active_ip ?? params.lastActiveIp,
				activityUpdate.updatedData.last_active_at ?? params.lastActiveAt,
				activityUpdate.previousData.last_active_ip,
			);
		}
	}

	async getActivityTracking(userId: UserID): Promise<{
		last_active_at: Date | null;
		last_active_ip: string | null;
	} | null> {
		return this.dataRepo.getActivityTracking(userId);
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
		return this.dataRepo.updateSubscriptionStatus(userId, updates);
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

	private applyPremiumFlagsMigration(data: UserRow, oldData: UserRow | null | undefined): void {
		const rawFlags = data.flags ?? 0n;
		const legacyPremiumBits = extractPremiumFlagsFromLegacyUserFlags(rawFlags);
		const sanitizedFlags = rawFlags & ~LEGACY_PREMIUM_FLAGS_MASK & ~LEGACY_DEAD_USER_FLAGS_MASK;
		if (sanitizedFlags !== rawFlags) {
			data.flags = sanitizedFlags;
		}
		if (legacyPremiumBits !== 0) {
			const basePremiumFlags = data.premium_flags ?? oldData?.premium_flags ?? 0;
			data.premium_flags = basePremiumFlags | legacyPremiumBits;
		}
	}

	private migratePremiumFlagsInPatch(patchData: Partial<UserRow>, oldData: UserRow): Partial<UserRow> {
		const oldRawFlags = oldData.flags ?? 0n;
		const oldLegacyPremiumBits = extractPremiumFlagsFromLegacyUserFlags(oldRawFlags);
		const oldHasDeadBits = (oldRawFlags & LEGACY_DEAD_USER_FLAGS_MASK) !== 0n;
		const flagsInPatch = patchData.flags;
		let migratedPatch = patchData;
		if (flagsInPatch !== undefined && flagsInPatch !== null) {
			const inboundLegacyPremium = extractPremiumFlagsFromLegacyUserFlags(flagsInPatch);
			const sanitizedFlags = flagsInPatch & ~LEGACY_PREMIUM_FLAGS_MASK & ~LEGACY_DEAD_USER_FLAGS_MASK;
			migratedPatch = {...patchData, flags: sanitizedFlags};
			if (inboundLegacyPremium !== 0) {
				const basePremiumFlags = patchData.premium_flags ?? oldData.premium_flags ?? 0;
				migratedPatch.premium_flags = basePremiumFlags | inboundLegacyPremium;
			}
		} else if (oldLegacyPremiumBits !== 0 || oldHasDeadBits) {
			migratedPatch = {...patchData, flags: oldRawFlags & ~LEGACY_PREMIUM_FLAGS_MASK & ~LEGACY_DEAD_USER_FLAGS_MASK};
			if (oldLegacyPremiumBits !== 0) {
				const basePremiumFlags = patchData.premium_flags ?? oldData.premium_flags ?? 0;
				migratedPatch.premium_flags = basePremiumFlags | oldLegacyPremiumBits;
			}
		}
		return migratedPatch;
	}

	private didEmailChange(oldData: Pick<UserRow, 'email'> | null | undefined, newData: Pick<UserRow, 'email'>): boolean {
		return oldData?.email?.toLowerCase() !== newData.email?.toLowerCase();
	}

	private async releasePreviousEmailIfChanged(oldData: UserRow | null, newData: UserRow): Promise<void> {
		if (!oldData?.email) {
			return;
		}
		if (oldData.email.toLowerCase() === newData.email?.toLowerCase()) {
			return;
		}
		await this.emailOwnershipRepo.releaseEmail(oldData.email, oldData.user_id);
	}
}
