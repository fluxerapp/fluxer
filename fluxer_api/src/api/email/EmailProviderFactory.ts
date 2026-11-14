// SPDX-License-Identifier: AGPL-3.0-or-later

import type {APIConfig} from '@app/api/config/APIConfig';
import {DomainRoutedEmailProvider} from '@pkgs/email/src/DomainRoutedEmailProvider';
import type {IEmailProvider} from '@pkgs/email/src/EmailProviderTypes';
import {SmtpEmailProvider} from '@pkgs/email/src/SmtpEmailProvider';

export function createEmailProvider(emailConfig: APIConfig['email']): IEmailProvider | null {
	if (!emailConfig.enabled) {
		return null;
	}
	switch (emailConfig.provider) {
		case 'smtp': {
			if (!emailConfig.smtp) return null;
			const primary = new SmtpEmailProvider(emailConfig.smtp);
			const {fallback} = emailConfig;
			return fallback
				? new DomainRoutedEmailProvider(primary, new SmtpEmailProvider(fallback.smtp), fallback.domains)
				: primary;
		}
		default:
			return null;
	}
}
