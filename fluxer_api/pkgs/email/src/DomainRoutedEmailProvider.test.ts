// SPDX-License-Identifier: AGPL-3.0-or-later

import {DomainRoutedEmailProvider} from '@pkgs/email/src/DomainRoutedEmailProvider';
import type {EmailMessage} from '@pkgs/email/src/EmailProviderTypes';
import {EmailService} from '@pkgs/email/src/EmailService';
import {describe, expect, it} from 'vitest';

function message(to: string): EmailMessage {
	return {to, from: {email: 'noreply@example.com', name: 'Example'}, subject: 'Hello', text: 'Body'};
}

function recordingProviders(results: {primary: boolean; fallback: boolean}) {
	const routes: Array<{route: 'primary' | 'fallback'; message: EmailMessage}> = [];
	const primary = {
		sendEmail: async (sent: EmailMessage) => {
			routes.push({route: 'primary', message: sent});
			return results.primary;
		},
	};
	const fallback = {
		sendEmail: async (sent: EmailMessage) => {
			routes.push({route: 'fallback', message: sent});
			return results.fallback;
		},
	};
	return {routes, primary, fallback};
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
		const {routes, primary, fallback} = recordingProviders({primary: true, fallback: false});
		const provider = new DomainRoutedEmailProvider(primary, fallback, ['gmail.com', 'googlemail.com']);
		const sent = message(to);
		const result = await provider.sendEmail(sent);
		expect(routes).toEqual([{route, message: sent}]);
		expect(result).toBe(route === 'primary');
	});

	it('sends every address through the fallback provider when the domains are *', async () => {
		const {routes, primary, fallback} = recordingProviders({primary: true, fallback: true});
		const provider = new DomainRoutedEmailProvider(primary, fallback, ['*']);
		await provider.sendEmail(message('user@example.com'));
		await provider.sendEmail(message('user@gmail.com'));
		expect(routes.map((entry) => entry.route)).toEqual(['fallback', 'fallback']);
	});

	it('never reaches either provider for a bounced address', async () => {
		const {routes, primary, fallback} = recordingProviders({primary: true, fallback: true});
		const service = new EmailService(
			{
				enabled: true,
				fromEmail: 'noreply@example.com',
				fromName: 'Example',
				appBaseUrl: 'https://example.com',
				apiBaseUrl: 'https://api.example.com',
				appealsEmail: null,
				privacyEmail: null,
				safetyEmail: null,
				supportEmail: null,
				productName: 'Example',
				marketingBaseUrl: 'https://example.com',
				staticCdnBaseUrl: 'https://static.example.com',
				legalAddress: null,
				brandLogo: false,
			},
			new DomainRoutedEmailProvider(primary, fallback, ['gmail.com']),
			{isEmailBounced: async () => true},
		);
		expect(await service.send('password_reset', 'user@gmail.com', {resetToken: 'token'})).toBe(false);
		expect(routes).toEqual([]);
	});
});
