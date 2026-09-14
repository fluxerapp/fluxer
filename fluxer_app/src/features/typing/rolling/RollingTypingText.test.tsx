// SPDX-License-Identifier: AGPL-3.0-or-later

import {readFileSync} from 'node:fs';
import {join} from 'node:path';
import {fileURLToPath} from 'node:url';
import type {Channel} from '@app/features/channel/models/Channel';
import type {User} from '@app/features/user/models/User';
import type {I18n} from '@lingui/core';
import {createElement, Fragment, type ReactNode} from 'react';
import {renderToStaticMarkup} from 'react-dom/server';
import {afterEach, describe, expect, it, vi} from 'vitest';

const doubles = vi.hoisted(() => ({
	getNickname: vi.fn((user: {id: string}, guildId: string | null) => `${user.id} in ${guildId ?? 'no guild'}`),
}));

vi.mock('@lingui/core/macro', () => ({msg: (descriptor: {message: string}) => descriptor}));
vi.mock('@lingui/react/macro', async () => {
	const {createElement: create, Fragment: ReactFragment} = await import('react');
	return {Trans: ({children}: {children?: ReactNode}) => create(ReactFragment, null, children)};
});
vi.mock('@app/features/user/utils/NicknameUtils', () => ({getNickname: doubles.getNickname}));
vi.mock('@app/features/member/state/GuildMembers', () => ({
	default: {getMember: () => ({getColorString: () => 'rgb(1, 2, 3)'})},
}));

const {getRollingTypingAnnouncement, getRollingTypingText} = await import(
	'@app/features/typing/rolling/RollingTypingText'
);

const APP_SRC = fileURLToPath(new URL('../../../', import.meta.url));
const GUILD_CHANNEL = {id: 'channel', guildId: 'guild'} as unknown as Channel;
const DM_CHANNEL = {id: 'dm'} as unknown as Channel;

function i18nFor(locale: string): I18n {
	return {
		locale,
		_: (descriptor: {message: string}, values: Record<string, string> = {}) =>
			descriptor.message.replace(/\{(\w+)\}/g, (_placeholder, name: string) => values[name] ?? ''),
	} as unknown as I18n;
}

const EN_US = i18nFor('en-US');
const DE = i18nFor('de');

function typists(count: number): Array<User> {
	return Array.from({length: count}, (_, index) => ({id: `typist-${index + 1}`}) as unknown as User);
}

function markupOf(node: ReactNode): string {
	return renderToStaticMarkup(createElement(Fragment, null, node));
}

function textOf(node: ReactNode): string {
	return markupOf(node).replace(/<[^>]+>/g, '');
}

function transMessages(source: string): Array<string> {
	return [...source.matchAll(/<Trans>([\s\S]*?)<\/Trans>/g)].map((match) => match[1]!.replace(/\s+/g, ' ').trim());
}

function catalogueIds(locale: string): Set<string> {
	const catalogue = readFileSync(join(APP_SRC, `features/i18n/locales/${locale}/messages.po`), 'utf8');
	return new Set([...catalogue.matchAll(/^msgid "(.*)"$/gm)].map((match) => match[1]!));
}

afterEach(() => {
	doubles.getNickname.mockClear();
});

describe('getRollingTypingText', () => {
	it('names a single typist', () => {
		const node = getRollingTypingText(EN_US, typists(1), GUILD_CHANNEL, false);

		expect(textOf(node)).toBe('typist-1 in guild is typing...');
		expect(markupOf(node)).toContain('data-flx="channel.typing-users.get-typing-text.username"');
		expect(markupOf(node)).toContain('color:rgb(1, 2, 3)');
	});

	it('names two typists', () => {
		expect(textOf(getRollingTypingText(EN_US, typists(2), GUILD_CHANNEL, false))).toBe(
			'typist-1 in guild and typist-2 in guild are typing...',
		);
	});

	it('names three typists without an Oxford comma', () => {
		expect(textOf(getRollingTypingText(EN_US, typists(3), GUILD_CHANNEL, false))).toBe(
			'typist-1 in guild, typist-2 in guild and typist-3 in guild are typing...',
		);
	});

	it('collapses four typists to several people', () => {
		expect(getRollingTypingText(EN_US, typists(4), GUILD_CHANNEL, false)).toBe('Several people are typing...');
		expect(getRollingTypingText(DE, typists(4), GUILD_CHANNEL, false)).toBe('Several people are typing...');
		expect(doubles.getNickname).not.toHaveBeenCalled();
	});

	it('collapses twenty typists to several people', () => {
		expect(getRollingTypingText(DE, typists(20), GUILD_CHANNEL, false)).toBe('Several people are typing...');
		expect(getRollingTypingText(EN_US, typists(20), GUILD_CHANNEL, false)).toBe("Whoa, it's a typing apocalypse");
	});

	it('shows multiple people for one to three typists when overflowing', () => {
		for (const count of [1, 2, 3]) {
			expect(getRollingTypingText(EN_US, typists(count), GUILD_CHANNEL, true)).toBe('Multiple people are typing...');
		}
	});

	it('never shows the overflow string for four or more typists', () => {
		expect(getRollingTypingText(EN_US, typists(4), GUILD_CHANNEL, true)).toBe('Several people are typing...');
		expect(getRollingTypingText(DE, typists(12), GUILD_CHANNEL, true)).toBe('Several people are typing...');
		expect(getRollingTypingText(EN_US, typists(12), GUILD_CHANNEL, true)).toBe(
			'A symphony of clacking keys is underway...',
		);
	});
});

describe('getRollingTypingAnnouncement', () => {
	it('announces the full count-based string for three typists', () => {
		expect(getRollingTypingAnnouncement(EN_US, typists(1), GUILD_CHANNEL)).toBe('typist-1 in guild is typing...');
		expect(getRollingTypingAnnouncement(EN_US, typists(2), GUILD_CHANNEL)).toBe(
			'typist-1 in guild and typist-2 in guild are typing...',
		);
		expect(getRollingTypingAnnouncement(EN_US, typists(3), GUILD_CHANNEL)).toBe(
			'typist-1 in guild, typist-2 in guild and typist-3 in guild are typing...',
		);
	});

	it('announces several people for four or more typists', () => {
		expect(getRollingTypingAnnouncement(EN_US, typists(4), GUILD_CHANNEL)).toBe('Several people are typing...');
		expect(getRollingTypingAnnouncement(DE, typists(4), GUILD_CHANNEL)).toBe('Several people are typing...');
		expect(getRollingTypingAnnouncement(DE, typists(16), GUILD_CHANNEL)).toBe('Several people are typing...');
		expect(getRollingTypingAnnouncement(EN_US, typists(16), GUILD_CHANNEL)).toBe(
			"It's a full-blown typing fiesta in here",
		);
	});

	it('announces nothing when nobody is typing', () => {
		expect(getRollingTypingAnnouncement(EN_US, [], GUILD_CHANNEL)).toBe('');
		expect(getRollingTypingText(EN_US, [], GUILD_CHANNEL, false)).toBeNull();
	});
});

describe('rolling typing catalogue', () => {
	it('reuses the head catalogue message for each named form', () => {
		const rollingSource = readFileSync(join(APP_SRC, 'features/typing/rolling/RollingTypingText.tsx'), 'utf8');
		const legacySource = readFileSync(join(APP_SRC, 'features/channel/components/LegacyTypingUsers.tsx'), 'utf8');
		const namedForms = ['{a} is typing...', '{a} and {b} are typing...', '{a}, {b} and {c} are typing...'];
		const declaredMessages = [...rollingSource.matchAll(/message: '([^']*)'/g)].map((match) => match[1]);

		expect(transMessages(rollingSource)).toEqual(namedForms);
		expect(transMessages(legacySource)).toEqual(namedForms);
		expect(declaredMessages).toEqual([...namedForms, 'Multiple people are typing...']);
		const sourceCatalogue = catalogueIds('en-US');
		for (const message of [...namedForms, 'Several people are typing...']) {
			expect(sourceCatalogue.has(message), message).toBe(true);
		}
	});

	it('resolves guild nicknames for names', () => {
		const [typist] = typists(1);

		expect(textOf(getRollingTypingText(EN_US, [typist!], GUILD_CHANNEL, false))).toBe('typist-1 in guild is typing...');
		expect(getRollingTypingAnnouncement(EN_US, [typist!], DM_CHANNEL)).toBe('typist-1 in no guild is typing...');
		expect(doubles.getNickname.mock.calls).toEqual([
			[typist, 'guild'],
			[typist, null],
		]);
	});
});
