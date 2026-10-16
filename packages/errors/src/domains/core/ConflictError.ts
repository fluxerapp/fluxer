// SPDX-License-Identifier: AGPL-3.0-or-later

import {statusError} from '@fluxer/errors/src/FluxerError';

export class ConflictError extends statusError(409) {}
