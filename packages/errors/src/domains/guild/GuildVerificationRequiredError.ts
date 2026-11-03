// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import type {ValueOf} from '@fluxer/constants/src/ValueOf';
import {ForbiddenError} from '@fluxer/errors/src/domains/core/ForbiddenError';

export const GuildVerificationRequiredReasons = {
	ACCOUNT_SETUP: 'account_setup',
	MEMBERSHIP_AGE: 'membership_age',
} as const;

export type GuildVerificationRequiredReason = ValueOf<typeof GuildVerificationRequiredReasons>;

export class GuildVerificationRequiredError extends ForbiddenError {
	constructor(reason: GuildVerificationRequiredReason = GuildVerificationRequiredReasons.ACCOUNT_SETUP) {
		super({code: APIErrorCodes.GUILD_VERIFICATION_REQUIRED, messageVariables: {reason}});
	}
}
