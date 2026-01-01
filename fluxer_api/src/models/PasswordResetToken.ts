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

import type {PasswordResetTokenRow} from '~/database/CassandraTypes';
import type {UserID} from '../BrandedTypes';
import {createPasswordResetToken} from '../BrandedTypes';

export class PasswordResetToken {
	readonly token: string;
	readonly userId: UserID;
	readonly email: string;

	constructor(row: PasswordResetTokenRow) {
		this.token = row.token_;
		this.userId = row.user_id;
		this.email = row.email;
	}

	toRow(): PasswordResetTokenRow {
		return {
			token_: createPasswordResetToken(this.token),
			user_id: this.userId,
			email: this.email,
		};
	}
}
