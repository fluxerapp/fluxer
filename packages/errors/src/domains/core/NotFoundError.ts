// SPDX-License-Identifier: AGPL-3.0-or-later

import {statusError} from '@fluxer/errors/src/FluxerError';

export class NotFoundError extends statusError(404) {}
