// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildID, UserID} from '@app/api/BrandedTypes';
import {deleteOneOrMany, fetchMany, fetchOne} from '@app/api/database/CassandraQueryExecution';
import {buildPatchFromData, executeVersionedUpdate} from '@app/api/database/CassandraVersionedUpdate';
import type {ExactRow} from '@app/api/database/types/DatabaseRowTypes';
import type {UserGuildSettingsRow, UserSettingsRow} from '@app/api/database/types/UserTypes';
import {USER_GUILD_SETTINGS_COLUMNS, USER_SETTINGS_COLUMNS} from '@app/api/database/types/UserTypes';
import {Logger} from '@app/api/Logger';
import {UserGuildSettings} from '@app/api/models/UserGuildSettings';
import {UserSettings} from '@app/api/models/UserSettings';
import {UserGuildSettings as UserGuildSettingsTable, UserSettings as UserSettingsTable} from '@app/api/Tables';
import type {IUserSettingsRepository} from '@app/api/user/repositories/IUserSettingsRepository';

const FETCH_USER_SETTINGS_CQL = UserSettingsTable.selectCql({
	where: UserSettingsTable.where.eq('user_id'),
	limit: 1,
});
const FETCH_USER_GUILD_SETTINGS_CQL = UserGuildSettingsTable.selectCql({
	where: [UserGuildSettingsTable.where.eq('user_id'), UserGuildSettingsTable.where.eq('guild_id')],
	limit: 1,
});
const FETCH_ALL_USER_GUILD_SETTINGS_CQL = UserGuildSettingsTable.selectCql({
	where: UserGuildSettingsTable.where.eq('user_id'),
});

export class UserSettingsRepository implements IUserSettingsRepository {
	async deleteAllUserGuildSettings(userId: UserID): Promise<void> {
		await deleteOneOrMany(
			UserGuildSettingsTable.deleteCql({
				where: UserGuildSettingsTable.where.eq('user_id'),
			}),
			{user_id: userId},
		);
	}

	async deleteGuildSettings(userId: UserID, guildId: GuildID): Promise<void> {
		await deleteOneOrMany(
			UserGuildSettingsTable.deleteByPk({
				user_id: userId,
				guild_id: guildId,
			}),
		);
	}

	async deleteUserSettings(userId: UserID): Promise<void> {
		await deleteOneOrMany(UserSettingsTable.deleteByPk({user_id: userId}));
	}

	async findGuildSettings(userId: UserID, guildId: GuildID | null): Promise<UserGuildSettings | null> {
		const settings = await fetchOne<UserGuildSettingsRow>(FETCH_USER_GUILD_SETTINGS_CQL, {
			user_id: userId,
			guild_id: guildId ? guildId : 0n,
		});
		return settings ? new UserGuildSettings(settings) : null;
	}

	async findAllGuildSettings(userId: UserID): Promise<Array<UserGuildSettings>> {
		const rows = await fetchMany<UserGuildSettingsRow>(FETCH_ALL_USER_GUILD_SETTINGS_CQL, {
			user_id: userId,
		});
		return rows.map((row) => new UserGuildSettings(row));
	}

	async findSettings(userId: UserID): Promise<UserSettings | null> {
		const settings = await fetchOne<UserSettingsRow>(FETCH_USER_SETTINGS_CQL, {user_id: userId});
		return settings ? new UserSettings(settings) : null;
	}

	async upsertGuildSettings(settings: ExactRow<UserGuildSettingsRow>): Promise<UserGuildSettings> {
		const userId = settings.user_id;
		const guildId = settings.guild_id;
		const result = await executeVersionedUpdate<UserGuildSettingsRow, 'user_id' | 'guild_id'>(
			() =>
				this.findGuildSettings(userId, guildId)
					.then((s) => s?.toRow() ?? null)
					.catch((error) => {
						Logger.error(
							{userId: userId.toString(), guildId: guildId.toString(), error},
							'Failed to fetch guild settings',
						);
						throw error;
					}),
			(current) => ({
				pk: {user_id: userId, guild_id: guildId},
				patch: buildPatchFromData(settings, current, USER_GUILD_SETTINGS_COLUMNS, ['user_id', 'guild_id']),
			}),
			UserGuildSettingsTable,
		);
		return new UserGuildSettings({...settings, version: result.finalVersion ?? 1});
	}

	async upsertSettings(settings: ExactRow<UserSettingsRow>): Promise<UserSettings> {
		const userId = settings.user_id;
		const result = await executeVersionedUpdate<UserSettingsRow, 'user_id'>(
			() =>
				this.findSettings(userId)
					.then((s) => s?.toRow() ?? null)
					.catch((error) => {
						Logger.error({userId: userId.toString(), error}, 'Failed to fetch settings');
						throw error;
					}),
			(current) => ({
				pk: {user_id: userId},
				patch: buildPatchFromData(settings, current, USER_SETTINGS_COLUMNS, ['user_id']),
			}),
			UserSettingsTable,
		);
		return new UserSettings({...settings, version: result.finalVersion ?? 1});
	}
}
