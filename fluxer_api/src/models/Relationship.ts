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

import type {RelationshipRow} from '~/database/CassandraTypes';
import type {UserID} from '../BrandedTypes';

export class Relationship {
	readonly sourceUserId: UserID;
	readonly targetUserId: UserID;
	readonly type: number;
	readonly nickname: string | null;
	readonly since: Date | null;
	readonly version: number;

	constructor(row: RelationshipRow) {
		this.sourceUserId = row.source_user_id;
		this.targetUserId = row.target_user_id;
		this.type = row.type;
		this.nickname = row.nickname ?? null;
		this.since = row.since ?? null;
		this.version = row.version;
	}

	toRow(): RelationshipRow {
		return {
			source_user_id: this.sourceUserId,
			target_user_id: this.targetUserId,
			type: this.type,
			nickname: this.nickname,
			since: this.since,
			version: this.version,
		};
	}
}
