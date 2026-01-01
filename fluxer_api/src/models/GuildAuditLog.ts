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

import type {GuildAuditLogRow} from '~/database/CassandraTypes';
import type {GuildAuditLogChange} from '~/guild/GuildAuditLogTypes';
import type {GuildID, UserID} from '../BrandedTypes';
import {extractTimestamp} from '../utils/SnowflakeUtils';

export class GuildAuditLog {
	readonly guildId: GuildID;
	readonly logId: bigint;
	readonly userId: UserID;
	readonly targetId: string | null;
	readonly actionType: number;
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
		this.createdAt = new Date(extractTimestamp(this.logId));
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
		try {
			return JSON.parse(raw) as GuildAuditLogChange;
		} catch {
			return null;
		}
	}
}
