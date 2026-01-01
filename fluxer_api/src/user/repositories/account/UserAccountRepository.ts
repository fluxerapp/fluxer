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

import type {UserID} from '~/BrandedTypes';
import {Db} from '~/database/Cassandra';
import type {UserRow} from '~/database/CassandraTypes';
import {User} from '~/Models';
import {UserDataRepository} from './crud/UserDataRepository';
import {UserIndexRepository} from './crud/UserIndexRepository';
import {UserSearchRepository} from './crud/UserSearchRepository';

export class UserAccountRepository {
	private dataRepo: UserDataRepository;
	private indexRepo: UserIndexRepository;
	private searchRepo: UserSearchRepository;

	constructor() {
		this.dataRepo = new UserDataRepository();
		this.indexRepo = new UserIndexRepository();
		this.searchRepo = new UserSearchRepository();
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

	async listUsers(userIds: Array<UserID>): Promise<Array<User>> {
		return this.dataRepo.listUsers(userIds);
	}

	async upsert(data: UserRow, oldData?: UserRow | null): Promise<User> {
		const result = await this.dataRepo.upsertUserRow(data, oldData);

		if (result.finalVersion === null) {
			throw new Error(`Failed to update user ${data.user_id} after max retries due to concurrent updates`);
		}

		await this.indexRepo.syncIndices(data, oldData);
		const user = new User(data);
		await this.searchRepo.indexUser(user);
		return user;
	}

	async patchUpsert(userId: UserID, patchData: Partial<UserRow>, oldData?: UserRow | null): Promise<User | null> {
		if (!oldData) {
			const existingUser = await this.findUnique(userId);
			if (!existingUser) return null;
			oldData = existingUser.toRow();
		}

		const newData: UserRow = {...oldData, ...patchData, user_id: userId};

		const userPatch: Record<string, ReturnType<typeof Db.set> | ReturnType<typeof Db.clear>> = {};
		for (const [key, value] of Object.entries(patchData)) {
			if (key === 'user_id') continue;
			if (value === undefined) continue;

			const userRowKey = key as keyof UserRow;

			if (value === null) {
				const oldVal = oldData?.[userRowKey];
				if (oldVal !== null && oldVal !== undefined) {
					userPatch[key] = Db.clear();
				}
			} else {
				userPatch[key] = Db.set(value);
			}
		}

		const result = await this.dataRepo.patchUser(userId, userPatch);

		if (result.finalVersion === null) {
			throw new Error(`Failed to patch user ${userId} after max retries due to concurrent updates`);
		}

		await this.indexRepo.syncIndices(newData, oldData);

		const user = new User(newData);
		await this.searchRepo.indexUser(user);

		return user;
	}

	async deleteUserSecondaryIndices(userId: UserID): Promise<void> {
		const user = await this.findUnique(userId);
		if (!user) return;

		await this.indexRepo.deleteIndices(
			userId,
			user.username,
			user.discriminator,
			user.email,
			user.phone,
			user.stripeSubscriptionId,
		);
	}

	async updateLastActiveAt(params: {userId: UserID; lastActiveAt: Date; lastActiveIp?: string}): Promise<void> {
		await this.dataRepo.updateLastActiveAt(params);
	}
}
