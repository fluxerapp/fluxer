// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ConflictError} from '@fluxer/errors/src/domains/core/ConflictError';

export class SyncedPreferencesConflictError extends ConflictError {
	constructor() {
		super({
			code: APIErrorCodes.SYNCED_PREFERENCES_CONFLICT,
		});
	}
}
