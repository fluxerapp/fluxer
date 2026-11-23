// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminRepository} from '@app/api/admin/AdminRepository';
import type {EmailDnsValidationService} from '@app/api/infrastructure/EmailDnsValidationService';
import {ipBanCache} from '@app/api/middleware/IpBanMiddleware';
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

export async function assertMailHostsNotBlocklisted(
	email: string,
	field: string,
	emailDnsValidation: Pick<EmailDnsValidationService, 'mailHosts' | 'mailHostAddresses'>,
): Promise<void> {
	for (const host of await emailDnsValidation.mailHosts(email)) {
		if (await adminRepository.isMailHostBanned(host)) {
			throw InputValidationError.fromCode(field, ValidationErrorCodes.INVALID_EMAIL_ADDRESS);
		}
	}
	for (const address of await emailDnsValidation.mailHostAddresses(email)) {
		if (ipBanCache.isBanned(address)) {
			throw InputValidationError.fromCode(field, ValidationErrorCodes.INVALID_EMAIL_ADDRESS);
		}
	}
}
