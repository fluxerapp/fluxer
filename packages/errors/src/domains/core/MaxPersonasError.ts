// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {BadRequestError} from '@fluxer/errors/src/domains/core/BadRequestError';

export class MaxPersonasError extends BadRequestError {
	constructor(params: {maxPersonas: number}) {
		const {maxPersonas} = params;
		super({
			code: APIErrorCodes.MAX_PERSONAS,
			messageVariables: {count: maxPersonas},
			data: {max_personas: maxPersonas},
		});
	}
}
