// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {BadRequestError} from '@fluxer/errors/src/domains/core/BadRequestError';

export class InvalidReportFlowAnswersError extends BadRequestError {
	constructor(stepIndex: number) {
		super({
			code: APIErrorCodes.INVALID_REPORT_FLOW_ANSWERS,
			data: {
				step_index: stepIndex,
			},
		});
	}
}
