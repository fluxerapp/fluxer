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

import type {MfaBackupCodeRow} from '~/database/CassandraTypes';
import type {UserID} from '../BrandedTypes';
import {createMfaBackupCode} from '../BrandedTypes';

export class MfaBackupCode {
	readonly userId: UserID;
	readonly code: string;
	readonly consumed: boolean;

	constructor(row: MfaBackupCodeRow) {
		this.userId = row.user_id;
		this.code = row.code;
		this.consumed = row.consumed ?? false;
	}

	toRow(): MfaBackupCodeRow {
		return {
			user_id: this.userId,
			code: createMfaBackupCode(this.code),
			consumed: this.consumed,
		};
	}
}
