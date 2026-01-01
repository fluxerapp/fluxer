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

import type {BetaCodeRow} from '~/database/CassandraTypes';
import type {UserID} from '../BrandedTypes';
import {createBetaCode} from '../BrandedTypes';

export class BetaCode {
	readonly code: string;
	readonly creatorId: UserID;
	readonly createdAt: Date;
	readonly redeemerId: UserID | null;
	readonly redeemedAt: Date | null;
	readonly version: number;

	constructor(row: BetaCodeRow) {
		this.code = row.code;
		this.creatorId = row.creator_id;
		this.createdAt = row.created_at;
		this.redeemerId = row.redeemer_id ?? null;
		this.redeemedAt = row.redeemed_at ?? null;
		this.version = row.version;
	}

	toRow(): BetaCodeRow {
		return {
			code: createBetaCode(this.code),
			creator_id: this.creatorId,
			created_at: this.createdAt,
			redeemer_id: this.redeemerId,
			redeemed_at: this.redeemedAt,
			version: this.version,
		};
	}
}
