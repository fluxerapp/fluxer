// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {FluxerError} from '@fluxer/errors/src/FluxerError';

export class SearchIndexNotReadyError extends FluxerError {
	constructor(retryAfterSeconds: number) {
		super({
			code: APIErrorCodes.SEARCH_INDEX_NOT_READY,
			status: 202,
			data: {
				documents_indexed: 0,
				retry_after: retryAfterSeconds,
			},
		});
	}
}
