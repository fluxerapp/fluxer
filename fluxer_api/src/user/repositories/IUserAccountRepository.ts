/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type {GuildID, UserID} from '~/BrandedTypes';
import type {UserRow} from '~/database/CassandraTypes';
import type {User} from '~/Models';

export interface IUserAccountRepository {
	create(data: UserRow): Promise<User>;
	upsert(data: UserRow, oldData?: UserRow | null): Promise<User>;
	patchUpsert(userId: UserID, patchData: Partial<UserRow>, oldData?: UserRow | null): Promise<User | null>;
	findUnique(userId: UserID): Promise<User | null>;
	findUniqueAssert(userId: UserID): Promise<User>;
	findByUsernameDiscriminator(username: string, discriminator: number): Promise<User | null>;
	findDiscriminatorsByUsername(username: string): Promise<Set<number>>;
	findByEmail(email: string): Promise<User | null>;
	findByPhone(phone: string): Promise<User | null>;
	findByStripeSubscriptionId(stripeSubscriptionId: string): Promise<User | null>;
	findByStripeCustomerId(stripeCustomerId: string): Promise<User | null>;
	listUsers(userIds: Array<UserID>): Promise<Array<User>>;
	listAllUsersPaginated(limit: number, lastUserId?: UserID): Promise<Array<User>>;

	getUserGuildIds(userId: UserID): Promise<Array<GuildID>>;

	addPendingDeletion(userId: UserID, pendingDeletionAt: Date, deletionReasonCode: number): Promise<void>;
	removePendingDeletion(userId: UserID, pendingDeletionAt: Date): Promise<void>;
	findUsersPendingDeletion(now: Date): Promise<Array<User>>;
	findUsersPendingDeletionByDate(deletionDate: string): Promise<Array<{user_id: bigint; deletion_reason_code: number}>>;
	isUserPendingDeletion(userId: UserID, deletionDate: string): Promise<boolean>;
	scheduleDeletion(userId: UserID, pendingDeletionAt: Date, deletionReasonCode: number): Promise<void>;

	deleteUserSecondaryIndices(userId: UserID): Promise<void>;
	removeFromAllGuilds(userId: UserID): Promise<void>;

	updateLastActiveAt(params: {userId: UserID; lastActiveAt: Date; lastActiveIp?: string}): Promise<void>;
}
