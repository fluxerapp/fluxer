// SPDX-License-Identifier: AGPL-3.0-or-later

import type {APIConfig} from '@app/api/config/APIConfig';
import type {IEmailProvider} from '@pkgs/email/src/EmailProviderTypes';
import {SmtpEmailProvider} from '@pkgs/email/src/SmtpEmailProvider';

export function createEmailProvider(emailConfig: APIConfig['email']): IEmailProvider | null {
	if (!emailConfig.enabled) {
		return null;
	}
	switch (emailConfig.provider) {
		case 'smtp':
			return emailConfig.smtp
				? new SmtpEmailProvider({
						host: emailConfig.smtp.host,
						port: emailConfig.smtp.port,
						username: emailConfig.smtp.username,
						password: emailConfig.smtp.password,
						secure: emailConfig.smtp.secure,
					})
				: null;
		default:
			return null;
	}
}
