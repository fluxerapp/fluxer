// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildID, UserID} from '@app/api/BrandedTypes';
import type {ExactRow} from '@app/api/database/types/DatabaseRowTypes';
import type {UserGuildSettingsRow, UserSettingsRow} from '@app/api/database/types/UserTypes';
import type {UserGuildSettings} from '@app/api/models/UserGuildSettings';
import type {UserSettings} from '@app/api/models/UserSettings';

export interface IUserSettingsRepository {
	findSettings(userId: UserID): Promise<UserSettings | null>;
	upsertSettings(settings: ExactRow<UserSettingsRow>): Promise<UserSettings>;
	deleteUserSettings(userId: UserID): Promise<void>;
	findGuildSettings(userId: UserID, guildId: GuildID | null): Promise<UserGuildSettings | null>;
	findAllGuildSettings(userId: UserID): Promise<Array<UserGuildSettings>>;
	upsertGuildSettings(settings: ExactRow<UserGuildSettingsRow>): Promise<UserGuildSettings>;
	deleteGuildSettings(userId: UserID, guildId: GuildID): Promise<void>;
	deleteAllUserGuildSettings(userId: UserID): Promise<void>;
}
