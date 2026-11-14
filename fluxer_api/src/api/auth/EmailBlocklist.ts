// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminRepository} from '@app/api/admin/AdminRepository';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';

const adminRepository = new AdminRepository();

export async function isEmailBlocklisted(email: string): Promise<boolean> {
	return adminRepository.isEmailBanned(email.trim());
}

export async function assertEmailNotBlocklisted(email: string, field: string): Promise<void> {
	if (await isEmailBlocklisted(email)) {
		throw InputValidationError.fromCode(field, ValidationErrorCodes.INVALID_EMAIL_ADDRESS);
	}
}
