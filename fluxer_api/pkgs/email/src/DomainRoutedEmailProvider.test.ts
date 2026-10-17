// SPDX-License-Identifier: AGPL-3.0-or-later

import {DomainRoutedEmailProvider} from '@pkgs/email/src/DomainRoutedEmailProvider';
import type {EmailMessage} from '@pkgs/email/src/EmailProviderTypes';
import {EmailService} from '@pkgs/email/src/EmailService';
import {describe, expect, it, vi} from 'vitest';

function message(to: string): EmailMessage {
	return {to, from: {email: 'noreply@example.com', name: 'Example'}, subject: 'Hello', text: 'Body'};
}

describe('DomainRoutedEmailProvider', () => {
	it.each([
		{to: 'user@gmail.com', route: 'fallback'},
		{to: 'user@GOOGLEMAIL.COM', route: 'fallback'},
		{to: 'User.Name+tag@Gmail.Com', route: 'fallback'},
		{to: 'user@mail.gmail.com', route: 'primary'},
		{to: 'user@gmail.com.example.org', route: 'primary'},
		{to: 'user@example.com', route: 'primary'},
		{to: '"odd@gmail.com"@example.com', route: 'primary'},
	])('sends $to through the $route provider', async ({to, route}) => {
		const primary = {sendEmail: vi.fn().mockResolvedValue(true)};
		const fallback = {sendEmail: vi.fn().mockResolvedValue(false)};
		const provider = new DomainRoutedEmailProvider(primary, fallback, ['gmail.com', 'googlemail.com']);
		const sent = message(to);
		const result = await provider.sendEmail(sent);
		const [chosen, other] = route === 'fallback' ? [fallback, primary] : [primary, fallback];
		expect(chosen.sendEmail).toHaveBeenCalledWith(sent);
		expect(other.sendEmail).not.toHaveBeenCalled();
		expect(result).toBe(route === 'primary');
	});

	it('sends every address through the fallback provider when the domains are *', async () => {
		const primary = {sendEmail: vi.fn().mockResolvedValue(true)};
		const fallback = {sendEmail: vi.fn().mockResolvedValue(true)};
		const provider = new DomainRoutedEmailProvider(primary, fallback, ['*']);
		await provider.sendEmail(message('user@example.com'));
		await provider.sendEmail(message('user@gmail.com'));
		expect(fallback.sendEmail).toHaveBeenCalledTimes(2);
		expect(primary.sendEmail).not.toHaveBeenCalled();
	});

	it('never reaches either provider for a bounced address', async () => {
		const primary = {sendEmail: vi.fn().mockResolvedValue(true)};
		const fallback = {sendEmail: vi.fn().mockResolvedValue(true)};
		const service = new EmailService(
			{
				enabled: true,
				fromEmail: 'noreply@example.com',
				fromName: 'Example',
				appBaseUrl: 'https://example.com',
				appealsEmail: null,
				safetyEmail: null,
				supportEmail: null,
				productName: 'Example',
			},
			new DomainRoutedEmailProvider(primary, fallback, ['gmail.com']),
			{isEmailBounced: async () => true},
		);
		expect(await service.sendPasswordResetEmail('user@gmail.com', 'user', 'token')).toBe(false);
		expect(primary.sendEmail).not.toHaveBeenCalled();
		expect(fallback.sendEmail).not.toHaveBeenCalled();
	});
});
