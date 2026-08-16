// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ForbiddenError} from '@fluxer/errors/src/domains/core/ForbiddenError';

export interface SudoModeMethods {
	totp: boolean;
	webauthn: boolean;
	sso?: boolean;
}

const EMPTY_METHODS: SudoModeMethods = {totp: false, webauthn: false};

export class SudoModeRequiredError extends ForbiddenError {
	constructor(hasMfa: boolean, methods: SudoModeMethods = EMPTY_METHODS) {
		super({
			code: APIErrorCodes.SUDO_MODE_REQUIRED,
			data: {
				has_mfa: hasMfa,
				methods,
			},
		});
	}
}
