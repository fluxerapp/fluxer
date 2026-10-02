// SPDX-License-Identifier: AGPL-3.0-or-later

import {EmailI18nService} from '@pkgs/email/src/EmailI18nService';
import type {EmailConfig, EmailMessage, IEmailProvider} from '@pkgs/email/src/EmailProviderTypes';
import {EmailService} from '@pkgs/email/src/EmailService';
import {describe, expect, it} from 'vitest';

const CONFIG: EmailConfig = {
	enabled: true,
	fromEmail: 'noreply@example.com',
	fromName: 'Fluxer',
	appBaseUrl: 'https://example.com',
	marketingBaseUrl: 'https://example.com',
};

async function sendWith(config: EmailConfig): Promise<EmailMessage> {
	const sent: Array<EmailMessage> = [];
	const provider: IEmailProvider = {
		sendEmail: async (message) => {
			sent.push(message);
			return true;
		},
	};
	const service = new EmailService(config, new EmailI18nService(), provider);
	await expect(service.sendRegistrationApprovedEmail('user@example.com', 'testuser', 'en-US')).resolves.toBe(true);
	expect(sent).toHaveLength(1);
	return sent[0];
}

describe('EmailService reply-to', () => {
	it('sets the configured reply-to address on every message', async () => {
		const message = await sendWith({...CONFIG, replyTo: 'support@example.com'});
		expect(message.replyTo).toBe('support@example.com');
		expect(message.from).toEqual({email: 'noreply@example.com', name: 'Fluxer'});
	});

	it.each([undefined, null, ''])('omits the reply-to address when it is %j', async (replyTo) => {
		const message = await sendWith({...CONFIG, replyTo});
		expect(message).not.toHaveProperty('replyTo');
	});
});
