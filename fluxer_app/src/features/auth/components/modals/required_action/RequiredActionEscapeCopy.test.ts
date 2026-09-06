// SPDX-License-Identifier: AGPL-3.0-or-later

import {setupI18n} from '@lingui/core';
import {describe, expect, it, vi} from 'vitest';

vi.mock('@lingui/core/macro', () => {
	const descriptor = (value: unknown): unknown => (typeof value === 'string' ? {message: value} : value);
	return {msg: descriptor, t: descriptor, plural: () => '', select: () => '', selectOrdinal: () => ''};
});

vi.mock('@app/features/app/config/Config', () => ({
	default: {
		PUBLIC_BUILD_VERSION: 'test',
		PUBLIC_RELEASE_CHANNEL: 'canary',
		PUBLIC_BOOTSTRAP_API_ENDPOINT: 'https://example.invalid',
		PUBLIC_BOOTSTRAP_API_PUBLIC_ENDPOINT: 'https://example.invalid',
	},
}));

const {buildPhoneGateEscapeConfirmCopy, buildPhoneGateEscapeHint} = await import('./RequiredActionEscapeCopy');

const i18n = setupI18n({locale: 'en', messages: {en: {}}});

function confirmCopy(overrides: Partial<Parameters<typeof buildPhoneGateEscapeConfirmCopy>[1]> = {}) {
	return buildPhoneGateEscapeConfirmCopy(i18n, {
		guildNames: [],
		ownedGuildNames: [],
		emailStepRemains: false,
		...overrides,
	});
}

describe('buildPhoneGateEscapeHint', () => {
	it('promises no removal when the user is in no community the check applies to', () => {
		const hint = buildPhoneGateEscapeHint(i18n, 0);
		expect(hint).toContain('nothing is removed');
		expect(hint).not.toContain('leave');
	});

	it('counts the communities that will be left', () => {
		expect(buildPhoneGateEscapeHint(i18n, 1)).toContain('1 community');
		expect(buildPhoneGateEscapeHint(i18n, 4)).toContain('4 communities');
	});
});

describe('buildPhoneGateEscapeConfirmCopy', () => {
	it('never mentions leaving when nothing will be left', () => {
		const copy = confirmCopy();
		expect(copy.title).toBe('Set this check aside?');
		expect(copy.primaryText).toBe('Set this check aside');
		expect(copy.primaryVariant).toBe('primary');
		expect(copy.bodyLines).toHaveLength(2);
		for (const line of copy.bodyLines) {
			expect(line).not.toContain('leave');
			expect(line).not.toContain('join');
		}
	});

	it('uses the singular form and the destructive style for a single community', () => {
		const copy = confirmCopy({guildNames: ['Cat Fans']});
		expect(copy.title).toBe('Leave 1 community and set this check aside?');
		expect(copy.primaryText).toBe('Leave and set aside');
		expect(copy.primaryVariant).toBe('danger');
		expect(copy.bodyLines[1]).toContain('You will leave Cat Fans.');
	});

	it('names every community that will be left', () => {
		const copy = confirmCopy({guildNames: ['Cat Fans', 'Dog Fans', 'Bird Fans']});
		expect(copy.title).toBe('Leave 3 communities and set this check aside?');
		expect(copy.bodyLines[1]).toContain('You will leave Cat Fans, Dog Fans, Bird Fans.');
	});

	it('explains kept communities without claiming anything is left', () => {
		const copy = confirmCopy({ownedGuildNames: ['My Server']});
		expect(copy.title).toBe('Set this check aside?');
		expect(copy.primaryVariant).toBe('primary');
		expect(copy.bodyLines[1]).toBe('You stay in My Server. Owners cannot leave their own community.');
	});

	it('states the leave line before the ownership line', () => {
		const copy = confirmCopy({guildNames: ['Cat Fans'], ownedGuildNames: ['My Server', 'My Other Server']});
		expect(copy.bodyLines[1]).toContain('You will leave Cat Fans.');
		expect(copy.bodyLines[2]).toBe('You stay in My Server, My Other Server. Owners cannot leave their own community.');
	});

	it('warns about the remaining email step before the support line, which is always last', () => {
		const copy = confirmCopy({emailStepRemains: true});
		expect(copy.bodyLines.at(-2)).toBe('You will still need to finish the email step.');
		expect(copy.bodyLines.at(-1)).toBe('You can still email support@fluxer.app for a human review at any time.');
		expect(confirmCopy().bodyLines.at(-1)).toBe(
			'You can still email support@fluxer.app for a human review at any time.',
		);
	});
});
