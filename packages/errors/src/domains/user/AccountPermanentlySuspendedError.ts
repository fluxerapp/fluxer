// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ForbiddenError} from '@fluxer/errors/src/domains/core/ForbiddenError';
import type {EnforcementNotice} from '@fluxer/schema/src/domains/auth/EnforcementNoticeSchemas';

export class AccountPermanentlySuspendedError extends ForbiddenError {
	constructor(notice: EnforcementNotice | null) {
		super({code: APIErrorCodes.ACCOUNT_SUSPENDED_PERMANENTLY, data: {notice}});
	}
}
