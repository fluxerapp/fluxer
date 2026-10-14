// SPDX-License-Identifier: AGPL-3.0-or-later

import {buildMessagingPausedHelpEmail} from '@app/features/user/utils/MessagingPausedHelpEmail';
import type {I18n} from '@lingui/core';
import {describe, expect, it, vi} from 'vitest';

vi.mock('@lingui/core/macro', () => ({
	msg: (descriptor: unknown) => descriptor,
}));

const i18n = {
	_: (descriptor: {message: string}, values: Record<string, string> = {}) =>
		descriptor.message.replace(/\{(\w+)\}/g, (_match, key: string) => values[key] ?? ''),
} as unknown as I18n;

describe('buildMessagingPausedHelpEmail', () => {
	it('fills the support email with the account facts', () => {
		const {body, mailto} = buildMessagingPausedHelpEmail(
			i18n,
			{id: '1425000000000000000', tag: 'marty#0001', createdAt: new Date('2026-10-07T12:30:00.000Z')},
			'  Philippines ',
		);
		expect(body).toBe(
			[
				"My text didn't go through",
				'',
				'Account ID: 1425000000000000000',
				'Username: marty#0001',
				'Account created: 2026-10-07T12:30:00.000Z',
				'Country: Philippines',
			].join('\n'),
		);
		const url = new URL(mailto);
		expect(url.protocol).toBe('mailto:');
		expect(url.pathname).toBe('support@fluxer.com');
		expect(url.searchParams.get('subject')).toBe("My text didn't go through");
		expect(decodeURIComponent(mailto.split('&body=')[1])).toBe(body);
	});
});
