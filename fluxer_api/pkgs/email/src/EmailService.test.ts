// SPDX-License-Identifier: AGPL-3.0-or-later

import {EmailI18nService} from '@pkgs/email/src/EmailI18nService';
import type {EmailConfig, EmailMessage, IEmailProvider} from '@pkgs/email/src/EmailProviderTypes';
import {EmailService} from '@pkgs/email/src/EmailService';
import {TestEmailService} from '@pkgs/email/src/TestEmailService';
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

function createCapturingService(): {service: EmailService; sent: Array<EmailMessage>} {
	const sent: Array<EmailMessage> = [];
	const provider: IEmailProvider = {
		sendEmail: async (message) => {
			sent.push(message);
			return true;
		},
	};
	return {service: new EmailService(CONFIG, new EmailI18nService(), provider), sent};
}

describe('EmailService report notices', () => {
	it('sends a receipt with the report id and target kind', async () => {
		const {service, sent} = createCapturingService();
		await expect(service.sendReportReceivedEmail('notifier@example.com', '1234567890', 'guild', 'en-US')).resolves.toBe(
			true,
		);
		expect(sent).toHaveLength(1);
		expect(sent[0].to).toBe('notifier@example.com');
		expect(sent[0].subject).toBe('We received your Fluxer report');
		expect(sent[0].text).toContain('report about a community on Fluxer.');
		expect(sent[0].text).toContain('Report ID: 1234567890');
	});

	it('sends the receipt in the requested locale', async () => {
		const {service, sent} = createCapturingService();
		await service.sendReportReceivedEmail('notifier@example.com', '1234567890', 'message', 'de');
		expect(sent[0].subject).toBe('Wir haben deine Fluxer-Meldung erhalten');
		expect(sent[0].text).toContain('Meldungs-ID: 1234567890');
	});

	it('sends a DSA decision notice with the public comment', async () => {
		const {service, sent} = createCapturingService();
		await expect(
			service.sendDsaReportResolvedEmail('notifier@example.com', '1234567890', 'We removed the content.', 'en-US'),
		).resolves.toBe(true);
		expect(sent).toHaveLength(1);
		expect(sent[0].to).toBe('notifier@example.com');
		expect(sent[0].subject).toBe('We made a decision on your Fluxer report');
		expect(sent[0].text).toContain('(ID: 1234567890)');
		expect(sent[0].text).toContain('Response from the Safety Team:\nWe removed the content.');
		expect(sent[0].text).toContain('Email appeals@fluxer.app from this email address');
	});

	it('sends a generic DSA decision notice without a public comment', async () => {
		const {service, sent} = createCapturingService();
		await service.sendDsaReportResolvedEmail('notifier@example.com', '1234567890', '', 'en-US');
		expect(sent[0].text).not.toContain('Response from the Safety Team');
		expect(sent[0].text).not.toContain('\n\n\n');
		expect(sent[0].text).toContain('Email appeals@fluxer.app from this email address');
	});
});

describe('TestEmailService report notices', () => {
	it('records the receipt and the DSA decision notice', async () => {
		const service = new TestEmailService();
		await service.sendReportReceivedEmail('notifier@example.com', '1234567890', 'user', 'en-US');
		await service.sendDsaReportResolvedEmail('notifier@example.com', '1234567890', '', 'en-US');
		expect(service.listSentEmails().map(({to, type, metadata}) => ({to, type, metadata}))).toEqual([
			{to: 'notifier@example.com', type: 'report_received', metadata: {report_id: '1234567890', target_kind: 'user'}},
			{
				to: 'notifier@example.com',
				type: 'dsa_report_resolved',
				metadata: {report_id: '1234567890', public_comment: ''},
			},
		]);
	});
});
