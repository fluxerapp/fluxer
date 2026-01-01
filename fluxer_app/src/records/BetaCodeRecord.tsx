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

import {type UserPartial, UserRecord} from '~/records/UserRecord';
import UserStore from '~/stores/UserStore';

export type BetaCode = Readonly<{
	code: string;
	created_at: string;
	redeemed_at: string | null;
	redeemer: UserPartial | null;
}>;

export class BetaCodeRecord {
	readonly code: string;
	readonly createdAt: Date;
	readonly redeemedAt: Date | null;
	readonly redeemer: UserRecord | null;

	constructor(data: BetaCode) {
		this.code = data.code;
		this.createdAt = new Date(data.created_at);
		this.redeemedAt = data.redeemed_at ? new Date(data.redeemed_at) : null;
		this.redeemer = data.redeemer ? new UserRecord(data.redeemer) : null;

		if (this.redeemer != null) {
			UserStore.cacheUsers([this.redeemer.toJSON()]);
		}
	}

	toJSON(): BetaCode {
		return {
			code: this.code,
			created_at: this.createdAt.toISOString(),
			redeemed_at: this.redeemedAt?.toISOString() ?? null,
			redeemer: this.redeemer ? this.redeemer.toJSON() : null,
		};
	}

	equals(other: BetaCodeRecord): boolean {
		return JSON.stringify(this) === JSON.stringify(other);
	}
}
