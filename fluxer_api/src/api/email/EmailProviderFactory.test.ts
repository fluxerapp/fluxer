// SPDX-License-Identifier: AGPL-3.0-or-later

import type {APIConfig} from '@app/api/config/APIConfig';
import {createEmailProvider} from '@app/api/email/EmailProviderFactory';
import {DomainRoutedEmailProvider} from '@pkgs/email/src/DomainRoutedEmailProvider';
import {SmtpEmailProvider} from '@pkgs/email/src/SmtpEmailProvider';
import {describe, expect, it} from 'vitest';

const smtp = {host: 'smtp.example.com', port: 587, username: 'user', password: 'pass', tlsMode: null};

function emailConfig(overrides: Partial<APIConfig['email']> = {}): APIConfig['email'] {
	return {
		enabled: true,
		provider: 'smtp',
		fromEmail: 'noreply@example.com',
		fromName: 'Example',
		replyToEmail: '',
		appBaseUrl: 'https://example.com',
		smtp,
		...overrides,
	};
}

describe('createEmailProvider', () => {
	it('returns the plain SMTP provider without a fallback', () => {
		expect(createEmailProvider(emailConfig())).toBeInstanceOf(SmtpEmailProvider);
	});

	it('routes by domain when a fallback is set', () => {
		const provider = createEmailProvider(
			emailConfig({fallback: {domains: ['gmail.com'], smtp: {...smtp, host: 'smtp.sendgrid.net'}}}),
		);
		expect(provider).toBeInstanceOf(DomainRoutedEmailProvider);
	});

	it.each([{enabled: false}, {provider: 'none' as const}, {smtp: undefined}])(
		'returns no provider for %j even with a fallback',
		(overrides) => {
			const fallback = {domains: ['gmail.com'], smtp};
			expect(createEmailProvider(emailConfig({fallback, ...overrides}))).toBeNull();
		},
	);
});
