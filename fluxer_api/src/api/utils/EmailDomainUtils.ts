// SPDX-License-Identifier: AGPL-3.0-or-later

import {domainToASCII} from 'node:url';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';

export function extractEmailDomain(email: string | null | undefined): string | null {
	const normalized = email?.trim().toLowerCase();
	if (!normalized) return null;
	const atIndex = normalized.lastIndexOf('@');
	if (atIndex <= 0 || atIndex === normalized.length - 1) return null;
	return normalized.slice(atIndex + 1);
}

export function assertEmailDomainAllowed(email: string, allowedDomains: ReadonlyArray<string>, field: string): void {
	if (allowedDomains.length === 0) return;
	const domain = extractEmailDomain(email);
	if (domain && allowedDomains.includes(domainToASCII(domain))) return;
	throw InputValidationError.fromCode(field, ValidationErrorCodes.EMAIL_DOMAIN_NOT_ALLOWED);
}
