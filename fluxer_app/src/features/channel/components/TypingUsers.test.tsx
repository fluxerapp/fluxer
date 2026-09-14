// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import type {User} from '@app/features/user/models/User';
import type {I18n} from '@lingui/core';
import {act, createElement, Fragment, type ReactNode} from 'react';
import {createRoot, type Root} from 'react-dom/client';
import {renderToStaticMarkup} from 'react-dom/server';
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';

const doubles = vi.hoisted(() => ({locale: 'en-US'}));

vi.mock('@lingui/core/macro', () => ({msg: (descriptor: {message: string}) => descriptor}));
vi.mock('@lingui/react/macro', async () => {
	const {createElement: create, Fragment: ReactFragment} = await import('react');
	return {
		Trans: ({children}: {children?: ReactNode}) => create(ReactFragment, null, children),
		useLingui: () => ({
			i18n: {
				locale: doubles.locale,
				_: (descriptor: {message: string}, values: Record<string, string> = {}) =>
					descriptor.message.replace(/\{(\w+)\}/g, (_placeholder, name: string) => values[name] ?? ''),
			},
		}),
	};
});
vi.mock('@app/features/platform/utils/AppLogger', () => ({
	Logger: class {
		debug = vi.fn();
		info = vi.fn();
		warn = vi.fn();
		error = vi.fn();
	},
}));
vi.mock('@app/features/platform/transport/RestTransport', () => ({
	http: {get: vi.fn(), post: vi.fn(() => Promise.resolve({ok: true}))},
}));
vi.mock('@app/features/auth/state/Authentication', () => ({default: {currentUserId: 'me'}}));
vi.mock('@app/features/devtools/state/DeveloperOptions', () => ({default: {showMyselfTyping: false}}));
vi.mock('@app/features/relationship/state/Relationships', () => ({default: {isBlocked: () => false}}));
vi.mock('@app/features/user/state/Users', () => ({
	default: {getUser: (userId: string) => ({id: userId, username: userId})},
}));
vi.mock('@app/features/user/utils/NicknameUtils', () => ({
	getNickname: (user: {username: string}) => user.username,
}));
vi.mock('@app/features/member/state/GuildMembers', () => ({
	default: {getMember: (_guildId: string, userId: string) => ({getColorString: () => `var(--member-${userId})`})},
}));
vi.mock('@app/features/channel/components/ChannelTyping', async () => {
	const {createElement: create} = await import('react');
	return {Typing: ({className}: {className?: string}) => create('svg', {className})};
});
vi.mock('@app/features/ui/avatars/AvatarStack', async () => {
	const {createElement: create} = await import('react');
	return {
		AvatarStack: ({className, users}: {className?: string; users: ReadonlyArray<{id: string}>}) =>
			create('div', {className, 'data-avatars': users.map((user) => user.id).join(' ')}),
	};
});

const {TypingAnnouncer, TypingUsers, getTypingText} = await import('@app/features/channel/components/TypingUsers');
const {TypingUsers: LegacyTypingUsers} = await import('@app/features/channel/components/LegacyTypingUsers');
const {default: TypingPolicy} = await import('@app/features/typing/state/TypingPolicy');
const {default: LegacyTypingIndicator} = await import('@app/features/typing/legacy/LegacyTypingIndicator');
const {default: RollingTypingStore} = await import('@app/features/typing/rolling/RollingTypingStore');
const {getTypingTierText} = await import('@app/features/typing/utils/TypingTierText');

const CHANNEL = {id: 'channel', guildId: 'guild'} as unknown as Channel;
const TYPIST_IDS = ['alice', 'bob', 'carol', 'dave', 'erin', 'frank', 'grace'];

let hosts: Array<{host: HTMLDivElement; root: Root}> = [];

function mount(node: ReactNode): HTMLDivElement {
	const host = document.createElement('div');
	document.body.append(host);
	const root = createRoot(host);
	hosts.push({host, root});
	act(() => {
		root.render(node);
	});
	return host;
}

function i18nFor(locale: string): I18n {
	return {locale, _: (descriptor: {message: string}) => `${locale}: ${descriptor.message}`} as unknown as I18n;
}

function usersFor(count: number): Array<User> {
	return TYPIST_IDS.slice(0, count).map((id) => ({id, username: id}) as unknown as User);
}

function manyUsers(count: number): Array<User> {
	return Array.from(
		{length: count},
		(_, index) => ({id: `typist-${index}`, username: `typist-${index}`}) as unknown as User,
	);
}

function markupOf(node: ReactNode): string {
	return renderToStaticMarkup(createElement(Fragment, null, node));
}

beforeEach(() => {
	(globalThis as {IS_REACT_ACT_ENVIRONMENT?: boolean}).IS_REACT_ACT_ENVIRONMENT = true;
	doubles.locale = 'en-US';
	vi.useFakeTimers();
});

afterEach(() => {
	act(() => {
		for (const {root} of hosts) {
			root.unmount();
		}
	});
	hosts = [];
	TypingPolicy.applyPolicy('legacy');
	LegacyTypingIndicator.reset();
	RollingTypingStore.reset();
	document.body.replaceChildren();
	vi.useRealTimers();
});

describe('TypingUsers facade', () => {
	it('renders the same markup as the legacy component under control', () => {
		for (const count of [1, 2, 3, 4, 7]) {
			LegacyTypingIndicator.reset();
			for (const userId of TYPIST_IDS.slice(0, count)) {
				LegacyTypingIndicator.startRemoteTyping(CHANNEL.id, userId);
			}

			const facadeHost = mount(createElement(TypingUsers, {channel: CHANNEL, withText: true, showAvatars: true}));
			const legacyHost = mount(createElement(LegacyTypingUsers, {channel: CHANNEL, withText: true, showAvatars: true}));

			expect(facadeHost.innerHTML).not.toBe('');
			expect(facadeHost.innerHTML).toBe(legacyHost.innerHTML);
		}
	});

	it('renders no announcer under control', () => {
		LegacyTypingIndicator.startRemoteTyping(CHANNEL.id, 'alice');

		const host = mount(createElement(TypingAnnouncer, {channel: CHANNEL}));

		expect(host.innerHTML).toBe('');
		expect(host.querySelector('[aria-live]')).toBeNull();
	});

	it('renders the rolling row under treatment', () => {
		TypingPolicy.applyPolicy('rolling');
		RollingTypingStore.start(CHANNEL.id, 'alice', 'gateway');

		const host = mount(
			createElement(
				Fragment,
				null,
				createElement(TypingUsers, {channel: CHANNEL, withText: true, showAvatars: true}),
				createElement(TypingAnnouncer, {channel: CHANNEL}),
			),
		);

		const visible = host.querySelector('[data-flx="channel.typing-users.span"]');
		expect(visible?.textContent).toBe('alice is typing...');
		expect(visible?.getAttribute('aria-hidden')).toBe('true');
		expect(host.querySelector('[data-flx="channel.rolling-typing-users.measure"]')).not.toBeNull();
		expect(host.querySelector('[aria-live="polite"]')?.textContent).toBe('alice is typing...');
	});

	it('switches rows when the policy flips', () => {
		LegacyTypingIndicator.startRemoteTyping(CHANNEL.id, 'alice');
		const host = mount(
			createElement(
				Fragment,
				null,
				createElement(TypingUsers, {channel: CHANNEL}),
				createElement(TypingAnnouncer, {channel: CHANNEL}),
			),
		);
		expect(host.querySelector('[data-flx="channel.typing-users.span"]')?.getAttribute('aria-live')).toBe('polite');
		expect(host.querySelector('[data-flx="channel.rolling-typing-users.announcer"]')).toBeNull();

		act(() => {
			TypingPolicy.applyPolicy('rolling');
		});
		expect(host.querySelector('[data-flx="channel.typing-users.div"]')).toBeNull();
		expect(host.querySelector('[data-flx="channel.rolling-typing-users.announcer"]')?.textContent).toBe('');

		act(() => {
			RollingTypingStore.start(CHANNEL.id, 'bob', 'gateway');
		});
		expect(host.querySelector('[data-flx="channel.typing-users.span"]')?.getAttribute('aria-hidden')).toBe('true');
		expect(host.querySelector('[data-flx="channel.typing-users.span"]')?.textContent).toBe('bob is typing...');

		act(() => {
			TypingPolicy.applyPolicy('legacy');
		});
		expect(host.querySelector('[data-flx="channel.typing-users.div"]')).toBeNull();
		expect(host.querySelector('[data-flx="channel.rolling-typing-users.announcer"]')).toBeNull();
	});

	it('keeps the one, two and three name forms identical in both arms', () => {
		for (const count of [1, 2, 3]) {
			const users = usersFor(count);
			const legacyMarkup = markupOf(getTypingText(i18nFor('en-US'), users, CHANNEL));
			TypingPolicy.applyPolicy('rolling');
			const rollingMarkup = markupOf(getTypingText(i18nFor('en-US'), users, CHANNEL));
			TypingPolicy.applyPolicy('legacy');

			expect(rollingMarkup).toBe(legacyMarkup);
		}
		expect(markupOf(getTypingText(i18nFor('en-US'), usersFor(3), CHANNEL)).replace(/<[^>]+>/g, '')).toBe(
			'alice, bob and carol are typing...',
		);
	});

	it('resolves the four or more ladder per locale identically in both arms', () => {
		for (const locale of ['en-US', 'en-GB', 'de', 'fr', 'ja', 'pt-BR']) {
			const i18n = i18nFor(locale);
			for (const count of [4, 5, 9, 10, 14, 15, 19, 20, 31]) {
				const users = manyUsers(count);
				const expected = getTypingTierText(i18n, count);
				const legacyText = getTypingText(i18n, users, CHANNEL);
				TypingPolicy.applyPolicy('rolling');
				const rollingText = getTypingText(i18n, users, CHANNEL);
				TypingPolicy.applyPolicy('legacy');

				expect(legacyText, `${locale} ${count}`).toBe(expected);
				expect(rollingText, `${locale} ${count}`).toBe(expected);
			}
		}
		expect(getTypingText(i18nFor('en-GB'), manyUsers(12), CHANNEL)).toBe('A symphony of clacking keys is underway...');
		expect(getTypingText(i18nFor('de'), manyUsers(12), CHANNEL)).toBe('de: Several people are typing...');
	});
});
