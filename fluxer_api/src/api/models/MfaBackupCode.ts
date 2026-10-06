// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import {createMfaBackupCode} from '@app/api/BrandedTypes';
import type {MfaBackupCodeRow} from '@app/api/database/types/AuthTypes';

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
