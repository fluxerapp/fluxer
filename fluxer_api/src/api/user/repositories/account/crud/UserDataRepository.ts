// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID, type UserID} from '@app/api/BrandedTypes';
import {isSyntheticUserId} from '@app/api/constants/Core';
import {executeConditional, fetchMany, fetchOne, fetchPage, upsertOne} from '@app/api/database/CassandraQueryExecution';
import {Db, type DbOp, nextVersion} from '@app/api/database/CassandraTypes';
import {applyPatchToRow, buildPatchFromData, executeVersionedUpdate} from '@app/api/database/CassandraVersionedUpdate';
import type {UserRow} from '@app/api/database/types/UserTypes';
import {EMPTY_USER_ROW, USER_COLUMNS} from '@app/api/database/types/UserTypes';
import {User} from '@app/api/models/User';
import {Users} from '@app/api/Tables';
import {isPendingDeletionBlocked} from '@app/api/user/services/PendingDeletionCoordinator';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {DELETED_USER_ID, UserFlags} from '@fluxer/constants/src/UserConstants';
import {ConflictError} from '@fluxer/errors/src/domains/core/ConflictError';
import {UnknownUserError} from '@fluxer/errors/src/domains/user/UnknownUserError';
import {BACKGROUND_READ_TIMEOUT_MS} from '@pkgs/cassandra/src/Client';

const FLUXER_BOT_USER_ID = 0n;
const FETCH_USERS_BY_IDS_CQL = Users.selectCql({
	where: Users.where.in('user_id', 'user_ids'),
});
const FETCH_USER_BY_ID_CQL = Users.selectCql({
	where: Users.where.eq('user_id'),
	limit: 1,
});
const FETCH_ALL_USERS_SCAN_CQL = Users.selectCql();
const FETCH_ACTIVITY_TRACKING_CQL = Users.selectCql({
	columns: ['last_active_at', 'last_active_ip'],
	where: Users.where.eq('user_id'),
	limit: 1,
});

function createFetchAllUsersFirstPageQuery(limit: number) {
	return Users.select({limit});
}

const createFetchAllUsersPaginatedQuery = (limit: number) =>
	Users.select({
		where: Users.where.tokenGt('user_id', 'last_user_id'),
		limit,
	});

type UserPatch = Partial<{
	[K in Exclude<keyof UserRow, 'user_id'> & string]: DbOp<UserRow[K]>;
}>;

export type UserDeletionTransition = 'schedule' | 'anonymise' | 'complete';

function assertOrdinaryUserPatch(patch: UserPatch): void {
	if (patch.pending_deletion_at || patch.deletion_started_at) {
		throw new Error('User deletion state must be changed through the deletion lifecycle');
	}
}

function assertWritableUserId(userId: UserID): void {
	if (isSyntheticUserId(userId)) {
		throw new Error(`Refusing to write a users row for synthetic user ${userId}`);
	}
}

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
				flags: UserFlags.STAFF,
			});
		}
		if (userId === DELETED_USER_ID) {
			return new User({
				...EMPTY_USER_ROW,
				user_id: createUserID(DELETED_USER_ID),
				username: 'DeletedUser',
				discriminator: 0,
				bot: false,
				system: false,
			});
		}
		const userRow = await fetchOne<UserRow>(FETCH_USER_BY_ID_CQL, {user_id: userId});
		return userRow ? new User(userRow) : null;
	}

	async findUniqueAssert(userId: UserID): Promise<User> {
		const user = await this.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}
		return user;
	}

	async listAllUsersPaginated(limit: number, lastUserId?: UserID): Promise<Array<User>> {
		let users: Array<UserRow>;
		if (lastUserId) {
			const query = createFetchAllUsersPaginatedQuery(limit);
			users = await fetchMany<UserRow>(query.bind({last_user_id: lastUserId}));
		} else {
			const query = createFetchAllUsersFirstPageQuery(limit);
			users = await fetchMany<UserRow>(query.bind({}));
		}
		return users.map((user) => new User(user));
	}

	async scanAllUsersPage(
		limit: number,
		pageState?: string | null,
	): Promise<{
		users: Array<User>;
		pageState: string | null;
	}> {
		const result = await fetchPage<UserRow>(
			FETCH_ALL_USERS_SCAN_CQL,
			{},
			{pageSize: limit, pageState, readTimeout: BACKGROUND_READ_TIMEOUT_MS},
		);
		return {
			users: result.rows.map((user) => new User(user)),
			pageState: result.pageState,
		};
	}

	async listUsers(userIds: Array<UserID>): Promise<Array<User>> {
		if (userIds.length === 0) return [];
		const users = await fetchMany<UserRow>(FETCH_USERS_BY_IDS_CQL, {user_ids: userIds});
		return users.map((user) => new User(user));
	}

	async upsertUserRow(
		data: UserRow,
		oldData?: UserRow | null,
	): Promise<{
		finalVersion: number | null;
		previousData: UserRow | null;
		updatedData: UserRow;
	}> {
		const userId = data.user_id;
		assertWritableUserId(userId);
		const result = await executeVersionedUpdate<UserRow, 'user_id'>(
			async () => {
				return fetchOne<UserRow>(FETCH_USER_BY_ID_CQL, {user_id: userId});
			},
			(current) => {
				const patch = buildPatchFromData(data, current, USER_COLUMNS, ['user_id']);
				assertOrdinaryUserPatch(patch);
				return {pk: {user_id: userId}, patch};
			},
			Users,
			{initialData: oldData},
		);
		return {
			finalVersion: result.finalVersion,
			previousData: result.previousData,
			updatedData: {...data, version: result.finalVersion ?? data.version},
		};
	}

	async patchUser(
		userId: UserID,
		patch: UserPatch,
		oldData?: UserRow | null,
	): Promise<{
		finalVersion: number | null;
		previousData: UserRow | null;
		updatedData: UserRow;
	}> {
		assertWritableUserId(userId);
		assertOrdinaryUserPatch(patch);
		const result = await executeVersionedUpdate<UserRow, 'user_id'>(
			async () => {
				return fetchOne<UserRow>(FETCH_USER_BY_ID_CQL, {user_id: userId});
			},
			(_current) => ({
				pk: {user_id: userId},
				patch,
			}),
			Users,
			{initialData: oldData},
		);
		const previousData = result.previousData;
		const updatedData = {
			...(applyPatchToRow<UserRow>(previousData, patch) as UserRow),
			user_id: userId,
			version: result.finalVersion ?? nextVersion(previousData?.version),
		};
		return {finalVersion: result.finalVersion, previousData, updatedData};
	}

	async patchDeletion(
		user: User,
		patch: UserPatch,
		transition: UserDeletionTransition,
	): Promise<{
		finalVersion: number;
		previousData: UserRow;
		updatedData: UserRow;
	}> {
		assertWritableUserId(user.id);
		if (patch.deletion_started_at) throw new Error('The deletion start marker is immutable');
		if (transition === 'schedule' && user.deletionStartedAt) {
			throw new ConflictError({code: APIErrorCodes.CONFLICT, message: 'Account deletion has already started'});
		}
		if (transition !== 'schedule' && (!user.deletionStartedAt || !user.pendingDeletionAt)) {
			throw new Error('Account deletion must be started before completion');
		}
		if (transition === 'anonymise' && patch.pending_deletion_at) {
			throw new Error('Account deletion must retain its schedule until cleanup completes');
		}
		const previousData = user.toRow();
		const finalVersion = nextVersion(user.version);
		const applied = await executeConditional(
			Users.conditionalPatchByPk(
				{user_id: user.id},
				{...patch, version: Db.set(finalVersion)},
				{
					pending_deletion_at: user.pendingDeletionAt,
					deletion_started_at: transition === 'schedule' ? null : user.deletionStartedAt,
					flags: previousData.flags,
					version: user.version,
				},
			),
		);
		if (!applied) {
			throw new ConflictError({code: APIErrorCodes.CONFLICT, message: 'Account deletion state has changed'});
		}
		const updatedData = {...previousData, ...applyPatchToRow(previousData, patch), version: finalVersion};
		return {finalVersion, previousData, updatedData};
	}

	async startDeletion(userId: UserID, pendingDeletionAt: Date): Promise<User | null> {
		assertWritableUserId(userId);
		const scheduledAt = pendingDeletionAt.getTime();
		if (!Number.isFinite(scheduledAt)) throw new Error('Account deletion requires a valid captured schedule');
		const user = await this.findUnique(userId);
		if (!user || user.pendingDeletionAt?.getTime() !== scheduledAt) return null;
		if (user.deletionStartedAt) return user;
		const startedAt = new Date();
		if (scheduledAt > startedAt.getTime() || isPendingDeletionBlocked(user)) return null;
		const version = nextVersion(user.version);
		const applied = await executeConditional(
			Users.conditionalPatchByPk(
				{user_id: userId},
				{deletion_started_at: Db.set(startedAt), version: Db.set(version)},
				{pending_deletion_at: pendingDeletionAt, deletion_started_at: null, flags: user.flags, version: user.version},
			),
		);
		if (!applied) {
			throw new ConflictError({code: APIErrorCodes.CONFLICT, message: 'Account deletion state has changed'});
		}
		return new User({...user.toRow(), deletion_started_at: startedAt, version});
	}

	async updateLastActiveAt(params: {userId: UserID; lastActiveAt: Date; lastActiveIp?: string}): Promise<{
		previousData: {
			last_active_at: Date | null;
			last_active_ip: string | null;
		};
		updatedData: {
			last_active_at: Date | null;
			last_active_ip: string | null;
		};
	}> {
		const {userId, lastActiveAt, lastActiveIp} = params;
		assertWritableUserId(userId);
		const previousData = (await this.getActivityTracking(userId)) ?? {last_active_at: null, last_active_ip: null};
		await upsertOne(
			Users.patchByPk(
				{user_id: userId},
				{
					last_active_at: Db.set(lastActiveAt),
					last_active_ip: lastActiveIp !== undefined ? Db.set(lastActiveIp) : Db.clear(),
				},
			),
		);
		return {
			previousData,
			updatedData: {last_active_at: lastActiveAt, last_active_ip: lastActiveIp ?? null},
		};
	}

	async getActivityTracking(userId: UserID): Promise<{
		last_active_at: Date | null;
		last_active_ip: string | null;
	} | null> {
		const result = await fetchOne<{
			last_active_at: Date | null;
			last_active_ip: string | null;
		}>(FETCH_ACTIVITY_TRACKING_CQL, {user_id: userId});
		return result;
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
		assertWritableUserId(userId);
		const result = await executeVersionedUpdate<UserRow, 'user_id'>(
			async () => {
				return fetchOne<UserRow>(FETCH_USER_BY_ID_CQL, {user_id: userId});
			},
			(_current) => {
				const computedPremiumUntil = updates.computedPremiumUntil;
				const patch: UserPatch = {
					premium_will_cancel: Db.set(updates.premiumWillCancel),
					premium_until: computedPremiumUntil ? Db.set(computedPremiumUntil) : Db.clear(),
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
