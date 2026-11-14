// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import type {ValueOf} from '@fluxer/constants/src/ValueOf';
import {ForbiddenError} from '@fluxer/errors/src/domains/core/ForbiddenError';

export const SsoRequiredReasons = {
	SIGN_IN: 'sign_in',
	MANAGED_CREDENTIALS: 'managed_credentials',
	NO_ACCOUNT: 'no_account',
	MANAGED_USERNAME: 'managed_username',
} as const;

export type SsoRequiredReason = ValueOf<typeof SsoRequiredReasons>;

export class SsoRequiredError extends ForbiddenError {
	constructor(reason: SsoRequiredReason) {
		super({code: APIErrorCodes.SSO_REQUIRED, messageVariables: {reason}});
		this.name = 'SsoRequiredError';
	}
}
