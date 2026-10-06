// SPDX-License-Identifier: AGPL-3.0-or-later

import {SmtpEmailProvider} from '@pkgs/email/src/SmtpEmailProvider';
import {beforeEach, describe, expect, it, vi} from 'vitest';

const {sendMail} = vi.hoisted(() => ({sendMail: vi.fn()}));

vi.mock('nodemailer', () => ({
	default: {createTransport: () => ({sendMail, verify: vi.fn()})},
}));

const MESSAGE = {
	to: 'user@example.com',
	from: {email: 'noreply@example.com', name: 'Fluxer'},
	subject: 'Subject',
	text: 'Body',
};

function createProvider(): SmtpEmailProvider {
	return new SmtpEmailProvider({host: 'smtp.example.com', port: 587, username: 'user', password: 'pass'});
}

describe('SmtpEmailProvider', () => {
	beforeEach(() => {
		sendMail.mockReset();
		sendMail.mockResolvedValue({});
	});

	it('passes the reply-to address to nodemailer', async () => {
		await expect(createProvider().sendEmail({...MESSAGE, replyTo: 'support@example.com'})).resolves.toBe(true);
		expect(sendMail).toHaveBeenCalledWith({
			to: 'user@example.com',
			from: 'Fluxer <noreply@example.com>',
			replyTo: 'support@example.com',
			subject: 'Subject',
			text: 'Body',
		});
	});

	it('omits the reply-to address when the message has none', async () => {
		await expect(createProvider().sendEmail(MESSAGE)).resolves.toBe(true);
		expect(sendMail.mock.calls[0][0]).not.toHaveProperty('replyTo');
	});
});
