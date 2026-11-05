// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {FluxerError} from '@fluxer/errors/src/FluxerError';

export class StripeBillingCycleChangeNeedsResubscribeError extends FluxerError {
	constructor() {
		super({
			code: APIErrorCodes.STRIPE_BILLING_CYCLE_CHANGE_NEEDS_RESUBSCRIBE,
			status: 400,
		});
	}
}
