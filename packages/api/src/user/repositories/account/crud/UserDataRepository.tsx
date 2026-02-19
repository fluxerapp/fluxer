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

import {createUserID, type UserID} from '@fluxer/api/src/BrandedTypes';
import {
	buildPatchFromData,
	Db,
	type DbOp,
	executeVersionedUpdate,
	fetchMany,
	fetchOne,
	upsertOne,
} from '@fluxer/api/src/database/Cassandra';
import type {UserRow} from '@fluxer/api/src/database/types/UserTypes';
import {EMPTY_USER_ROW, USER_COLUMNS} from '@fluxer/api/src/database/types/UserTypes';
import {User} from '@fluxer/api/src/models/User';
import {Users} from '@fluxer/api/src/Tables';

const FLUXER_BOT_USER_ID = 0n;
const DELETED_USER_ID = 1n;

const FETCH_USERS_BY_IDS_CQL = Users.selectCql({
	where: Users.where.in('user_id', 'user_ids'),
});

const FETCH_USER_BY_ID_CQL = Users.selectCql({
	where: Users.where.eq('user_id'),
	limit: 1,
});

const UPDATE_LAST_ACTIVE_CQL = `UPDATE users SET last_active_at = :last_active_at, last_active_ip = :last_active_ip WHERE user_id = :user_id`;

const FETCH_ACTIVITY_TRACKING_CQL = Users.selectCql({
	columns: ['last_active_at', 'last_active_ip'],
	where: Users.where.eq('user_id'),
	limit: 1,
});

function createFetchAllUsersFirstPageCql(limit: number) {
	return Users.selectCql({limit});
}

const createFetchAllUsersPaginatedCql = (limit: number) =>
	Users.selectCql({
		where: Users.where.tokenGt('user_id', 'last_user_id'),
		limit,
	});

type UserPatch = Partial<{
	[K in Exclude<keyof UserRow, 'user_id'> & string]: DbOp<UserRow[K]>;
}>;

export class UserDataRepository {
	async findUnique(userId: UserID): Promise<User | null> {
		if (userId === FLUXER_BOT_USER_ID) {
			return new User({
				...EMPTY_USER_ROW,
				user_id: createUserID(FLUXER_BOT_USER_ID),
				username: 'Fluxer',
				discriminator: 0,
				bot: true,
				system: true,
			});
		}

		if (userId === DELETED_USER_ID) {
			return new User({
				...EMPTY_USER_ROW,
				user_id: createUserID(DELETED_USER_ID),
				username: 'DeletedUser',
				discriminator: 0,
				bot: false,
				system: true,
			});
		}

		const userRow = await fetchOne<UserRow>(FETCH_USER_BY_ID_CQL, {user_id: userId});
		if (!userRow) {
			return null;
		}

		return new User(userRow);
	}

	async findUniqueAssert(userId: UserID): Promise<User> {
		return (await this.findUnique(userId))!;
	}

	async listAllUsersPaginated(limit: number, lastUserId?: UserID): Promise<Array<User>> {
		let users: Array<UserRow>;

		if (lastUserId) {
			const cql = createFetchAllUsersPaginatedCql(limit);
			users = await fetchMany<UserRow>(cql, {last_user_id: lastUserId});
		} else {
			const cql = createFetchAllUsersFirstPageCql(limit);
			users = await fetchMany<UserRow>(cql, {});
		}

		return users.map((user) => new User(user));
	}

	async listUsers(userIds: Array<UserID>): Promise<Array<User>> {
		if (userIds.length === 0) return [];
		const users = await fetchMany<UserRow>(FETCH_USERS_BY_IDS_CQL, {user_ids: userIds});
		return users.map((user) => new User(user));
	}

	async upsertUserRow(data: UserRow, oldData?: UserRow | null): Promise<{finalVersion: number | null}> {
		const userId = data.user_id;

		const result = await executeVersionedUpdate<UserRow, 'user_id'>(
			async () => {
				const user = await this.findUnique(userId);
				return user?.toRow() ?? null;
			},
			(current) => ({
				pk: {user_id: userId},
				patch: buildPatchFromData(data, current, USER_COLUMNS, ['user_id']),
			}),
			Users,
			{initialData: oldData},
		);

		return {finalVersion: result.finalVersion};
	}

	async patchUser(userId: UserID, patch: UserPatch, oldData?: UserRow | null): Promise<{finalVersion: number | null}> {
		const result = await executeVersionedUpdate<UserRow, 'user_id'>(
			async () => {
				const user = await this.findUnique(userId);
				return user?.toRow() ?? null;
			},
			(_current) => ({
				pk: {user_id: userId},
				patch,
			}),
			Users,
			{initialData: oldData},
		);

		return {finalVersion: result.finalVersion};
	}

	async updateLastActiveAt(params: {userId: UserID; lastActiveAt: Date; lastActiveIp?: string}): Promise<void> {
		const {userId, lastActiveAt, lastActiveIp} = params;
		const updateParams: {user_id: UserID; last_active_at: Date; last_active_ip?: string} = {
			user_id: userId,
			last_active_at: lastActiveAt,
		};
		if (lastActiveIp !== undefined) {
			updateParams.last_active_ip = lastActiveIp;
		}

		await upsertOne(UPDATE_LAST_ACTIVE_CQL, updateParams);
	}

	async getActivityTracking(
		userId: UserID,
	): Promise<{last_active_at: Date | null; last_active_ip: string | null} | null> {
		const result = await fetchOne<{last_active_at: Date | null; last_active_ip: string | null}>(
			FETCH_ACTIVITY_TRACKING_CQL,
			{user_id: userId},
		);
		return result;
	}

	async updateSubscriptionStatus(
		userId: UserID,
		updates: {
			premiumWillCancel: boolean;
			computedPremiumUntil: Date | null;
		},
	): Promise<{finalVersion: number | null}> {
		const result = await executeVersionedUpdate<UserRow, 'user_id'>(
			async () => {
				const user = await this.findUnique(userId);
				return user?.toRow() ?? null;
			},
			(current) => {
				const currentPremiumUntil = current?.premium_until ?? null;
				const computedPremiumUntil = updates.computedPremiumUntil;

				let nextPremiumUntil: Date | null = currentPremiumUntil;

				if (computedPremiumUntil) {
					if (!nextPremiumUntil || computedPremiumUntil > nextPremiumUntil) {
						nextPremiumUntil = computedPremiumUntil;
					}
				}

				const patch: UserPatch = {
					premium_will_cancel: Db.set(updates.premiumWillCancel),
					premium_until: nextPremiumUntil ? Db.set(nextPremiumUntil) : Db.clear(),
				};

				return {
					pk: {user_id: userId},
					patch,
				};
			},
			Users,
		);

		return {finalVersion: result.finalVersion};
	}
}
