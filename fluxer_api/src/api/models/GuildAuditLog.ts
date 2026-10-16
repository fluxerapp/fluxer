// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildID, UserID} from '@app/api/BrandedTypes';
import type {GuildAuditLogRow} from '@app/api/database/types/GuildTypes';
import type {GuildAuditLogChange} from '@app/api/guild/GuildAuditLogTypes';
import {isJsonRecord, parseJsonWithGuard} from '@app/api/utils/JsonBoundaryUtils';
import type {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import {snowflakeToDate} from '@fluxer/snowflake/src/Snowflake';

export class GuildAuditLog {
	readonly guildId: GuildID;
	readonly logId: bigint;
	readonly userId: UserID;
	readonly targetId: string | null;
	readonly actionType: AuditLogActionType;
	readonly reason: string | null;
	readonly options: Map<string, string>;
	readonly changes: GuildAuditLogChange | null;
	readonly createdAt: Date;

	constructor(row: GuildAuditLogRow) {
		this.guildId = row.guild_id;
		this.logId = row.log_id;
		this.userId = row.user_id;
		this.targetId = row.target_id ?? null;
		this.actionType = row.action_type;
		this.reason = row.reason ?? null;
		this.options = row.options ?? new Map();
		this.changes = row.changes ? this.safeParseChanges(row.changes) : null;
		this.createdAt = snowflakeToDate(this.logId);
	}

	toRow(): GuildAuditLogRow {
		return {
			guild_id: this.guildId,
			log_id: this.logId,
			user_id: this.userId,
			target_id: this.targetId,
			action_type: this.actionType,
			reason: this.reason,
			options: this.options.size > 0 ? this.options : null,
			changes: this.changes ? JSON.stringify(this.changes) : null,
		};
	}

	private safeParseChanges(raw: string): GuildAuditLogChange | null {
		return parseJsonWithGuard(raw, isGuildAuditLogChange);
	}
}

function isGuildAuditLogChange(value: unknown): value is GuildAuditLogChange {
	return Array.isArray(value) && value.every((change) => isJsonRecord(change) && typeof change.key === 'string');
}
