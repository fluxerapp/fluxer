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

import type {ExpressionPackRow} from '~/database/CassandraTypes';
import type {GuildID, UserID} from '../BrandedTypes';

export type ExpressionPackType = 'emoji' | 'sticker';

export class ExpressionPack {
	readonly id: GuildID;
	readonly type: ExpressionPackType;
	readonly creatorId: UserID;
	readonly name: string;
	readonly description: string | null;
	readonly createdAt: Date;
	readonly updatedAt: Date;
	readonly version: number;

	constructor(row: ExpressionPackRow) {
		this.id = row.pack_id;
		this.type = row.pack_type as ExpressionPackType;
		this.creatorId = row.creator_id;
		this.name = row.name;
		this.description = row.description ?? null;
		this.createdAt = row.created_at;
		this.updatedAt = row.updated_at;
		this.version = row.version;
	}

	toRow(): ExpressionPackRow {
		return {
			pack_id: this.id,
			pack_type: this.type,
			creator_id: this.creatorId,
			name: this.name,
			description: this.description,
			created_at: this.createdAt,
			updated_at: this.updatedAt,
			version: this.version,
		};
	}
}
