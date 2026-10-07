// SPDX-License-Identifier: AGPL-3.0-or-later

import {EmailI18nService, type IEmailI18nService} from '@pkgs/email/src/EmailI18nService';
import type {EmailConfig, EmailMessage, IEmailProvider} from '@pkgs/email/src/EmailProviderTypes';
import {EmailService} from '@pkgs/email/src/EmailService';
import {TestEmailService} from '@pkgs/email/src/TestEmailService';
import {describe, expect, it} from 'vitest';

const CONFIG: EmailConfig = {
	enabled: true,
	fromEmail: 'noreply@example.com',
	fromName: 'Fluxer',
	appBaseUrl: 'https://example.com',
	termsUrl: 'https://example.com/terms',
	guidelinesUrl: 'https://example.com/guidelines',
	appealsEmail: 'appeals@fluxer.com',
	safetyEmail: 'safety@fluxer.com',
	supportEmail: 'support@fluxer.com',
	productName: 'Fluxer',
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

function createCapturingServiceFor(config: EmailConfig): {service: EmailService; sent: Array<EmailMessage>} {
	const sent: Array<EmailMessage> = [];
	const provider: IEmailProvider = {
		sendEmail: async (message) => {
			sent.push(message);
			return true;
		},
	};
	return {service: new EmailService(config, new EmailI18nService(), provider), sent};
}

function createCapturingService(): {service: EmailService; sent: Array<EmailMessage>} {
	return createCapturingServiceFor(CONFIG);
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
		expect(sent[0].text).toContain('Email appeals@fluxer.com from this email address');
	});

	it('sends a generic DSA decision notice without a public comment', async () => {
		const {service, sent} = createCapturingService();
		await service.sendDsaReportResolvedEmail('notifier@example.com', '1234567890', '', 'en-US');
		expect(sent[0].text).not.toContain('Response from the Safety Team');
		expect(sent[0].text).not.toContain('\n\n\n');
		expect(sent[0].text).toContain('Email appeals@fluxer.com from this email address');
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

describe('EmailService enforcement notices', () => {
	function capture(config: EmailConfig): {service: EmailService; sent: Array<EmailMessage>} {
		const sent: Array<EmailMessage> = [];
		const provider: IEmailProvider = {
			sendEmail: async (message) => {
				sent.push(message);
				return true;
			},
		};
		return {service: new EmailService(config, new EmailI18nService(), provider), sent};
	}

	async function sendBoth(config: EmailConfig, reason: string | null = 'Spam'): Promise<Array<string>> {
		const {service, sent} = capture(config);
		const until = new Date('2026-10-01T23:30:00Z');
		await service.sendAccountTempBannedEmail('user@example.com', 'testuser', reason, 24, until, 'en-US');
		await service.sendAccountScheduledForDeletionEmail('user@example.com', 'testuser', reason, until, 'en-US');
		expect(sent).toHaveLength(2);
		return sent.map((message) => message.text);
	}

	it('links the configured terms and guidelines', async () => {
		for (const text of await sendBoth(CONFIG)) {
			expect(text).toContain(
				'Please review:\n- Terms of Service: https://example.com/terms\n- Community Guidelines: https://example.com/guidelines\n\n',
			);
		}
	});

	it('leaves out the review list when no policy page is configured', async () => {
		for (const text of await sendBoth({...CONFIG, termsUrl: null, guidelinesUrl: null})) {
			expect(text).not.toContain('Please review:');
			expect(text).not.toContain('https://');
			expect(text).not.toContain('\n\n\n');
			expect(text).toContain('appeals@fluxer.com');
		}
	});

	it('lists only the configured page', async () => {
		for (const text of await sendBoth({...CONFIG, termsUrl: null, guidelinesUrl: 'https://rules.example.org'})) {
			expect(text).toContain('Please review:\n- Community Guidelines: https://rules.example.org\n\n');
			expect(text).not.toContain('Terms of Service:');
		}
		for (const text of await sendBoth({...CONFIG, termsUrl: 'https://tos.example.org', guidelinesUrl: null})) {
			expect(text).toContain('Please review:\n- Terms of Service: https://tos.example.org\n\n');
			expect(text).not.toContain('Community Guidelines:');
		}
	});

	it('states the reason as its own paragraph', async () => {
		const [suspended, deletion] = await sendBoth(CONFIG);
		expect(suspended).toContain(' UTC\n\nReason: Spam\n\nDuring this time,');
		expect(deletion).toContain(' UTC\n\nReason: Spam\n\nThis is a serious enforcement action.');
		for (const text of [suspended, deletion]) {
			expect(text).not.toContain('\n\n\n');
		}
	});

	it.each([null, '', '  '])('leaves no gap when the reason is %j', async (reason) => {
		const withReason = await sendBoth(CONFIG);
		const [suspended, deletion] = await sendBoth(CONFIG, reason);
		expect(suspended).toContain(' UTC\n\nDuring this time,');
		expect(deletion).toContain(' UTC\n\nThis is a serious enforcement action.');
		for (const [index, text] of [suspended, deletion].entries()) {
			expect(text).not.toContain('Reason:');
			expect(text).not.toContain('\n\n\n');
			expect(text.split('\n\n')).toHaveLength(withReason[index].split('\n\n').length - 1);
		}
	});
});

describe('EmailService contact addresses', () => {
	const SELF_HOSTED: EmailConfig = {...CONFIG, appealsEmail: null, safetyEmail: null, supportEmail: null};
	const DATE = new Date('2026-10-01T23:30:00Z');

	async function sendAll(config: EmailConfig, locale: string): Promise<Record<string, string>> {
		const sent: Array<EmailMessage> = [];
		const provider: IEmailProvider = {
			sendEmail: async (message) => {
				sent.push(message);
				return true;
			},
		};
		const service = new EmailService(config, new EmailI18nService(), provider);
		const to = 'user@example.com';
		await service.sendAccountTempBannedEmail(to, 'testuser', 'Spam', 24, DATE, locale);
		await service.sendAccountScheduledForDeletionEmail(to, 'testuser', 'Spam', DATE, locale);
		await service.sendScheduledDeletionNotification(to, 'testuser', DATE, 'Spam', locale);
		await service.sendDsaReportResolvedEmail(to, '1234567890', 'We removed the content.', locale);
		await service.sendReportResolvedEmail(to, 'testuser', '1234567890', 'We removed the content.', locale);
		await service.sendAccountDeletionCancelledEmail(to, 'testuser', locale);
		await service.sendAccountDeletionRequestedEmail(to, 'testuser', null, DATE, locale);
		await service.sendAccountDeletionInactivityEmail(to, 'testuser', null, DATE, locale);
		expect(sent).toHaveLength(8);
		const [tempBan, deletion, deletionNotice, dsaResolved, resolved, cancelled, requested, inactivity] = sent.map(
			(message) => message.text,
		);
		return {tempBan, deletion, deletionNotice, dsaResolved, resolved, cancelled, requested, inactivity};
	}

	it('names the hosted mailboxes in en-US', async () => {
		const hosted = await sendAll(CONFIG, 'en-US');
		expect(hosted.tempBan).toContain(
			'Email appeals@fluxer.com from this email address and clearly explain why you believe the decision was incorrect.',
		);
		expect(hosted.deletion).toContain(
			'you have 60 days to submit an appeal. Email appeals@fluxer.com from this email address.\n',
		);
		expect(hosted.deletionNotice).toContain(
			'you can submit an appeal. Email appeals@fluxer.com from this email address.\n',
		);
		expect(hosted.dsaResolved).toContain('Email appeals@fluxer.com from this email address, include your report ID,');
		expect(hosted.resolved).toContain('please contact safety@fluxer.com.\n');
		expect(hosted.cancelled).toContain('If you have any questions, contact safety@fluxer.com.\n');
		expect(hosted.requested).toContain('contact safety@fluxer.com from this email address before that date.\n');
		expect(hosted.inactivity).toContain('contact safety@fluxer.com from this email address before that date.\n');
	});

	it('points at the instance administrators in en-US when no mailbox is configured', async () => {
		const neutral = await sendAll(SELF_HOSTED, 'en-US');
		expect(neutral.tempBan).toContain(
			'you can submit an appeal. Contact the administrators of this instance and clearly explain why you believe the decision was incorrect.',
		);
		expect(neutral.deletion).toContain(
			'you have 60 days to submit an appeal. Contact the administrators of this instance.\n',
		);
		expect(neutral.deletionNotice).toContain(
			'you can submit an appeal. Contact the administrators of this instance.\n',
		);
		expect(neutral.dsaResolved).toContain(
			'Contact the administrators of this instance, include your report ID, and explain why you think the decision is wrong.',
		);
		expect(neutral.resolved).toContain(
			'If you have any questions or concerns about this outcome, please contact the administrators of this instance.\n',
		);
		expect(neutral.cancelled).toContain('If you have any questions, contact the administrators of this instance.\n');
		expect(neutral.requested).toContain(
			"Your account is locked until then. If you didn't request this, or you want to keep your account, contact the administrators of this instance before that date.\n",
		);
		expect(neutral.inactivity).toContain(
			'If you want to keep your account, contact the administrators of this instance before that date.\n',
		);
	});

	it.each([
		['de', 'die Administratoren dieser Instanz', 'Sende eine E-Mail von dieser E-Mail-Adresse an appeals@fluxer.com'],
		['ja', 'このインスタンスの管理者', 'このメールアドレスからappeals@fluxer.comまでメールを送信'],
	])('renders both variants in %s', async (locale, administrators, hostedAppeal) => {
		const hosted = await sendAll(CONFIG, locale);
		const neutral = await sendAll(SELF_HOSTED, locale);
		expect(hosted.tempBan).toContain(hostedAppeal);
		for (const [name, text] of Object.entries(hosted)) {
			expect(text, name).toMatch(/(appeals|safety)@fluxer\.com/);
			expect(text, name).not.toContain(administrators);
		}
		for (const [name, text] of Object.entries(neutral)) {
			expect(text, name).toContain(administrators);
			expect(text, name).not.toContain('@');
			expect(text, name).not.toMatch(/\bnull\b/);
			expect(text, name).not.toContain('{');
			expect(text, name).not.toContain('\n\n\n');
			expect(text.split('\n'), name).toHaveLength(hosted[name].split('\n').length);
		}
	});
});

describe('EmailService support contact', () => {
	const SELF_HOSTED: EmailConfig = {...CONFIG, appealsEmail: null, safetyEmail: null, supportEmail: null};
	const DATE = new Date('2026-10-01T23:30:00Z');

	async function sendSupportEmails(config: EmailConfig, locale: string): Promise<Array<string>> {
		const {service, sent} = createCapturingServiceFor(config);
		const to = 'user@example.com';
		await service.sendGiftChargebackNotification(to, 'testuser', locale);
		await service.sendHarvestCompletedEmail(to, 'testuser', 'https://example.com/d', 10, 1024, DATE, locale);
		await service.sendInactivityWarningEmail(to, 'testuser', DATE, DATE, locale);
		expect(sent).toHaveLength(3);
		return sent.map((message) => message.text);
	}

	it('keeps the support team sentences on the hosted instance', async () => {
		const [gift, harvest, inactivity] = await sendSupportEmails(CONFIG, 'en-US');
		expect(gift).toContain(
			'\n\nIf you think this is a mistake, please contact our support team and include any details you have about the gift code and when you redeemed it.\n\n',
		);
		expect(harvest).toContain(
			"\n\nIf you didn't request this export, please change your password immediately and contact our support team.\n\n",
		);
		expect(inactivity).toContain("\n\nIf you've used Fluxer recently, please contact our support team right away.\n\n");
	});

	it('points at the instance administrators when the instance has no support mailbox', async () => {
		const [gift, harvest, inactivity] = await sendSupportEmails({...SELF_HOSTED, productName: 'Example Chat'}, 'en-US');
		expect(gift).toContain(
			'\n\nIf you think this is a mistake, please contact the administrators of this instance and include any details you have about the gift code and when you redeemed it.\n\n',
		);
		expect(harvest).toContain(
			"\n\nIf you didn't request this export, please change your password immediately and contact the administrators of this instance.\n\n",
		);
		expect(inactivity).toContain(
			"\n\nIf you've used Example Chat recently, please contact the administrators of this instance right away.\n\n",
		);
		for (const text of [gift, harvest, inactivity]) {
			expect(text).not.toMatch(/support/i);
		}
	});

	it.each([
		['de', 'unser Support-Team', 'die Administratoren dieser Instanz'],
		['ja', 'サポートチーム', 'このインスタンスの管理者'],
	])('renders both variants in %s', async (locale, supportTeam, administrators) => {
		const hosted = await sendSupportEmails(CONFIG, locale);
		const neutral = await sendSupportEmails(SELF_HOSTED, locale);
		for (const [index, text] of hosted.entries()) {
			expect(text).toContain(supportTeam);
			expect(text).not.toContain(administrators);
			expect(neutral[index]).toContain(administrators);
			expect(neutral[index]).not.toContain(supportTeam);
			expect(neutral[index].split('\n')).toHaveLength(text.split('\n').length);
		}
	});
});

describe('EmailService product name', () => {
	async function sendPasswordReset(config: EmailConfig, locale: string): Promise<EmailMessage> {
		const {service, sent} = createCapturingServiceFor(config);
		await expect(service.sendPasswordResetEmail('user@example.com', 'testuser', 'token', locale)).resolves.toBe(true);
		expect(sent).toHaveLength(1);
		return sent[0];
	}

	it('names Fluxer on the hosted instance', async () => {
		const message = await sendPasswordReset(CONFIG, 'en-US');
		expect(message.subject).toBe('Reset your Fluxer password');
		expect(message.text).toContain('Fluxer');
	});

	it.each(['en-US', 'de', 'ja'])('names the configured instance in the %s subject and body', async (locale) => {
		const message = await sendPasswordReset({...CONFIG, productName: 'Example Chat'}, locale);
		expect(message.subject).toContain('Example Chat');
		expect(message.text).toContain('Example Chat');
		expect(`${message.subject}\n${message.text}`).not.toContain('Fluxer');
	});

	it('uses the name of the config it was built with, so a renamed instance sends the new name', async () => {
		const before = await sendPasswordReset({...CONFIG, productName: 'Example Chat'}, 'en-US');
		const after = await sendPasswordReset({...CONFIG, productName: 'Renamed Chat'}, 'en-US');
		expect(before.subject).toBe('Reset your Example Chat password');
		expect(after.subject).toBe('Reset your Renamed Chat password');
		expect(after.text).not.toContain('Example Chat');
	});

	it('passes the name to every template the service sends', async () => {
		const names: Array<string> = [];
		const i18n = new EmailI18nService();
		const recording: IEmailI18nService = {
			getTemplate: (key, locale, variables, productName) => {
				names.push(productName);
				return i18n.getTemplate(key, locale, variables, productName);
			},
		};
		const service = new EmailService({...CONFIG, productName: 'Example Chat'}, recording, {
			sendEmail: async () => true,
		});
		const date = new Date('2026-10-01T23:30:00Z');
		const to = 'user@example.com';
		await service.sendEmailVerification(to, 'testuser', 'token', 'en-US');
		await service.sendAccountTempBannedEmail(to, 'testuser', 'Spam', 24, date, 'en-US');
		await service.sendReportResolvedEmail(to, 'testuser', '1', 'Thanks', 'en-US');
		await service.sendSelfDeletionScheduledEmail(to, 'testuser', date, 'en-US');
		expect(names).toEqual(['Example Chat', 'Example Chat', 'Example Chat', 'Example Chat']);
	});
});
