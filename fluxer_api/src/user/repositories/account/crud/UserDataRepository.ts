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

import {createUserID, type UserID} from '~/BrandedTypes';
import {buildPatchFromData, Db, executeVersionedUpdate, fetchMany, fetchOne} from '~/database/Cassandra';
import {EMPTY_USER_ROW, USER_COLUMNS, type UserRow} from '~/database/CassandraTypes';
import {User} from '~/Models';
import {Users} from '~/Tables';

const FETCH_USERS_BY_IDS_CQL = Users.selectCql({
	where: Users.where.in('user_id', 'user_ids'),
});

const FETCH_USER_BY_ID_CQL = Users.selectCql({
	where: Users.where.eq('user_id'),
	limit: 1,
});

const createFetchAllUsersFirstPageCql = (limit: number) => Users.selectCql({limit});

const createFetchAllUsersPaginatedCql = (limit: number) =>
	Users.selectCql({
		where: Users.where.tokenGt('user_id', 'last_user_id'),
		limit,
	});

type UserPatch = Partial<{
	[K in Exclude<keyof UserRow, 'user_id'> & string]: import('~/database/Cassandra').DbOp<UserRow[K]>;
}>;

export class UserDataRepository {
	async findUnique(userId: UserID): Promise<User | null> {
		if (userId === 0n) {
			return new User({
				...EMPTY_USER_ROW,
				user_id: createUserID(0n),
				username: 'Fluxer',
				discriminator: 0,
				bot: true,
				system: true,
			});
		}

		if (userId === 1n) {
			return new User({
				...EMPTY_USER_ROW,
				user_id: createUserID(1n),
				username: 'Deleted User',
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
		let isFirstAttempt = true;

		const result = await executeVersionedUpdate<UserRow, 'user_id'>(
			async () => {
				if (isFirstAttempt && oldData !== undefined) {
					isFirstAttempt = false;
					return oldData;
				}
				isFirstAttempt = false;
				const user = await this.findUnique(userId);
				return user?.toRow() ?? null;
			},
			(current) => ({
				pk: {user_id: userId},
				patch: buildPatchFromData(data, current, USER_COLUMNS, ['user_id']),
			}),
			Users,
			{onFailure: 'log'},
		);

		return {finalVersion: result.finalVersion};
	}

	async patchUser(userId: UserID, patch: UserPatch): Promise<{finalVersion: number | null}> {
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
			{onFailure: 'log'},
		);

		return {finalVersion: result.finalVersion};
	}

	async updateLastActiveAt(params: {userId: UserID; lastActiveAt: Date; lastActiveIp?: string}): Promise<void> {
		const {userId, lastActiveAt, lastActiveIp} = params;

		const patch: UserPatch = {
			last_active_at: Db.set(lastActiveAt),
			...(lastActiveIp !== undefined ? {last_active_ip: Db.set(lastActiveIp)} : {}),
		};

		await this.patchUser(userId, patch);
	}
}
