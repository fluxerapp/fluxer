// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ConflictError} from '@fluxer/errors/src/domains/core/ConflictError';

export class EnforcementDecisionEndedError extends ConflictError {
	constructor() {
		super({code: APIErrorCodes.ENFORCEMENT_DECISION_ENDED});
	}
}
