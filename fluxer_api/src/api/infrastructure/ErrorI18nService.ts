// SPDX-License-Identifier: AGPL-3.0-or-later

import {resolveContactEmails} from '@app/api/instance/ContactEmails';
import {ErrorCodeToI18nKey} from '@fluxer/errors/src/i18n/ErrorCodeMappings';
import {getErrorMessageUnsafe} from '@fluxer/errors/src/i18n/ErrorI18n';

export class ErrorI18nService {
	getMessage(
		code: string,
		locale: string | null | undefined,
		variables?: Record<string, unknown>,
		fallbackMessage?: string,
	): string {
		const i18nKey = ErrorCodeToI18nKey[code as keyof typeof ErrorCodeToI18nKey] ?? code;
		const instanceVariables = {supportEmail: resolveContactEmails().supportEmail, ...variables};
		return getErrorMessageUnsafe(i18nKey, locale, instanceVariables, fallbackMessage);
	}
}
