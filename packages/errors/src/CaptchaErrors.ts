// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {BadRequestError} from '@fluxer/errors/src/domains/core/BadRequestError';
import type {FluxerErrorData} from '@fluxer/errors/src/FluxerError';

export class CaptchaRequiredError extends BadRequestError {
	constructor(data?: FluxerErrorData) {
		super({code: APIErrorCodes.CAPTCHA_REQUIRED, data});
		this.name = 'CaptchaRequiredError';
	}
}

export class InvalidCaptchaError extends BadRequestError {
	constructor(data?: FluxerErrorData) {
		super({code: APIErrorCodes.INVALID_CAPTCHA, data});
		this.name = 'InvalidCaptchaError';
	}
}
