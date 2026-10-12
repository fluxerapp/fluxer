// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {defaultCodeStatusError} from '@fluxer/errors/src/FluxerError';

export class UnauthorizedError extends defaultCodeStatusError(401, APIErrorCodes.UNAUTHORIZED) {}
