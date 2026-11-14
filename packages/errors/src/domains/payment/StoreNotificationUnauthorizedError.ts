// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {FluxerError} from '@fluxer/errors/src/FluxerError';

export class StoreNotificationUnauthorizedError extends FluxerError {
	constructor() {
		super({
			code: APIErrorCodes.STORE_NOTIFICATION_UNAUTHORIZED,
			status: 401,
		});
	}
}
