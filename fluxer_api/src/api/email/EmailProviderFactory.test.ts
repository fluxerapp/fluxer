// SPDX-License-Identifier: AGPL-3.0-or-later

import type {APIConfig} from '@app/api/config/APIConfig';
import {createEmailProvider} from '@app/api/email/EmailProviderFactory';
import {DomainRoutedEmailProvider} from '@pkgs/email/src/DomainRoutedEmailProvider';
import {SmtpEmailProvider} from '@pkgs/email/src/SmtpEmailProvider';
import nodemailer, {type Transporter} from 'nodemailer';
import {describe, expect, it, vi} from 'vitest';

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

	it('adds the fallback headers only to mail through the fallback', async () => {
		const sendMail = vi.fn();
		vi.spyOn(nodemailer, 'createTransport').mockReturnValue({sendMail} as unknown as Transporter);
		const headers = {'X-Tracking': '{"enable":0}'};
		const provider = createEmailProvider(emailConfig({fallback: {domains: ['gmail.com'], smtp: {...smtp, headers}}}));
		const from = {email: 'noreply@example.com', name: 'Example'};
		await provider?.sendEmail({to: 'user@gmail.com', from, subject: 'Hi', text: 'Body'});
		await provider?.sendEmail({to: 'user@example.com', from, subject: 'Hi', text: 'Body'});
		expect(sendMail.mock.calls[0][0].headers).toEqual({'X-Tracking': {prepared: true, value: '{"enable":0}'}});
		expect(sendMail.mock.calls[1][0]).not.toHaveProperty('headers');
	});

	it.each([{enabled: false}, {provider: 'none' as const}, {smtp: undefined}])(
		'returns no provider for %j even with a fallback',
		(overrides) => {
			const fallback = {domains: ['gmail.com'], smtp};
			expect(createEmailProvider(emailConfig({fallback, ...overrides}))).toBeNull();
		},
	);
});
