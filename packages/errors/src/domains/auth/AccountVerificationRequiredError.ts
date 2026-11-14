// SPDX-License-Identifier: AGPL-3.0-or-later

import type {
	AccountVerificationNeed,
	RestrictedAccountTier,
	TierGatedAction,
} from '@fluxer/constants/src/AccountTierConstants';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ForbiddenError} from '@fluxer/errors/src/domains/core/ForbiddenError';

export class AccountVerificationRequiredError extends ForbiddenError {
	constructor(action: TierGatedAction, tier: RestrictedAccountTier, need: AccountVerificationNeed) {
		super({
			code: APIErrorCodes.ACCOUNT_VERIFICATION_REQUIRED,
			data: {action, tier, need},
			messageVariables: {action, need},
		});
	}
}
