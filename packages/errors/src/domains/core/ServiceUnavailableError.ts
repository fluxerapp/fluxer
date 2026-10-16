// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {defaultCodeStatusError} from '@fluxer/errors/src/FluxerError';

export class ServiceUnavailableError extends defaultCodeStatusError<string>(503, APIErrorCodes.SERVICE_UNAVAILABLE) {}
