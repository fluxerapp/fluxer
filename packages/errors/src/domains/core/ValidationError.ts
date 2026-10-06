// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ValidationErrorCode} from '@fluxer/constants/src/ValidationErrorCodes';
import type {ValidationErrorItem} from '@fluxer/schema/src/domains/common/ErrorSchemas';

export interface ValidationError extends ValidationErrorItem {
	code?: ValidationErrorCode;
}
