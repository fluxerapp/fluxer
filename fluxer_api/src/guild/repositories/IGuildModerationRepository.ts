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
import type {AuditLogActionType} from '~/constants/AuditLogActionType';
import type {GuildAuditLogRow, GuildBanRow} from '~/database/CassandraTypes';
import type {GuildAuditLog, GuildBan} from '~/Models';

export abstract class IGuildModerationRepository {
	abstract getBan(guildId: GuildID, userId: UserID): Promise<GuildBan | null>;
	abstract listBans(guildId: GuildID): Promise<Array<GuildBan>>;
	abstract upsertBan(data: GuildBanRow): Promise<GuildBan>;
	abstract deleteBan(guildId: GuildID, userId: UserID): Promise<void>;
	abstract createAuditLog(data: GuildAuditLogRow): Promise<GuildAuditLog>;
	abstract getAuditLog(guildId: GuildID, logId: bigint): Promise<GuildAuditLog | null>;
	abstract listAuditLogs(params: {
		guildId: GuildID;
		limit: number;
		afterLogId?: bigint;
		beforeLogId?: bigint;
		userId?: UserID;
		actionType?: AuditLogActionType;
	}): Promise<Array<GuildAuditLog>>;
	abstract listAuditLogsByIds(guildId: GuildID, logIds: Array<bigint>): Promise<Array<GuildAuditLog>>;
	abstract deleteAuditLogs(guildId: GuildID, logs: Array<GuildAuditLog>): Promise<void>;
	abstract updateAuditLogsIndexedAt(guildId: GuildID, indexedAt: Date | null): Promise<void>;
}
