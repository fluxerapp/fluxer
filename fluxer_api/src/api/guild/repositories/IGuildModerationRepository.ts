// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildID, UserID} from '@app/api/BrandedTypes';
import type {GuildAuditLogRow, GuildBanRow} from '@app/api/database/types/GuildTypes';
import type {GuildAuditLog} from '@app/api/models/GuildAuditLog';
import type {GuildBan} from '@app/api/models/GuildBan';
import type {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';

export abstract class IGuildModerationRepository {
	abstract getBan(guildId: GuildID, userId: UserID): Promise<GuildBan | null>;

	abstract listBans(guildId: GuildID): Promise<Array<GuildBan>>;

	abstract upsertBan(data: GuildBanRow): Promise<GuildBan>;

	abstract deleteBan(guildId: GuildID, userId: UserID): Promise<void>;

	abstract deleteAllBansForUser(userId: UserID): Promise<void>;

	abstract getBanByEmail(guildId: GuildID, email: string): Promise<GuildBan | null>;

	abstract createAuditLog(data: GuildAuditLogRow): Promise<GuildAuditLog>;

	abstract batchDeleteAndCreateAuditLogs(
		guildId: GuildID,
		logsToDelete: Array<GuildAuditLog>,
		logToCreate: GuildAuditLogRow,
	): Promise<GuildAuditLog>;

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
