// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ConflictError} from '@fluxer/errors/src/domains/core/ConflictError';

export interface OperatorPassPriceChangedData {
	phase: string;
	currency: string;
	price_minor: number;
	quote: string;
}

export class OperatorPassPriceChangedError extends ConflictError {
	constructor(data: OperatorPassPriceChangedData) {
		super({code: APIErrorCodes.OPERATOR_PASS_PRICE_CHANGED, data: {...data}});
	}
}
