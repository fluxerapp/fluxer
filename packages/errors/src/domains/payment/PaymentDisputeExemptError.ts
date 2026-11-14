// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ConflictError} from '@fluxer/errors/src/domains/core/ConflictError';

export class PaymentDisputeExemptError extends ConflictError {
	constructor(reasons: ReadonlyArray<string>) {
		super({code: APIErrorCodes.PAYMENT_DISPUTE_EXEMPT, data: {reasons: [...reasons]}});
	}
}
