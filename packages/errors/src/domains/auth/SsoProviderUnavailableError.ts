// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {defaultCodeStatusError} from '@fluxer/errors/src/FluxerError';

export class SsoProviderUnavailableError extends defaultCodeStatusError(503, APIErrorCodes.SSO_PROVIDER_UNAVAILABLE) {}
