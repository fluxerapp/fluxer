// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import messageStyles from '@app/features/theme/styles/Message.module.css';
import {runInAction} from 'mobx';
import {act, createElement, Fragment, Profiler, type ReactNode} from 'react';
import {createRoot, type Root} from 'react-dom/client';
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';

const doubles = await vi.hoisted(async () => {
	const {observable} = await import('mobx');
	return {
		auth: observable({currentUserId: 'me' as string | null}),
		developerOptions: observable({showMyselfTyping: false}),
		blockedUserIds: observable.set<string>(),
		users: observable.map<string, {id: string; username: string}>(),
	};
});

vi.mock('@lingui/core/macro', () => ({msg: (descriptor: {message: string}) => descriptor}));
vi.mock('@lingui/react/macro', async () => {
	const {createElement: create, Fragment: ReactFragment} = await import('react');
	return {
		Trans: ({children}: {children?: ReactNode}) => create(ReactFragment, null, children),
		useLingui: () => ({
			i18n: {
				locale: 'en-US',
				_: (descriptor: {message: string}, values: Record<string, string> = {}) =>
					descriptor.message.replace(/\{(\w+)\}/g, (_placeholder, name: string) => values[name] ?? ''),
			},
		}),
	};
});
vi.mock('@app/features/auth/state/Authentication', () => ({default: doubles.auth}));
vi.mock('@app/features/devtools/state/DeveloperOptions', () => ({default: doubles.developerOptions}));
vi.mock('@app/features/relationship/state/Relationships', () => ({
	default: {isBlocked: (userId: string) => doubles.blockedUserIds.has(userId)},
}));
vi.mock('@app/features/user/state/Users', () => ({
	default: {getUser: (userId: string) => doubles.users.get(userId)},
}));
vi.mock('@app/features/user/utils/NicknameUtils', () => ({
	getNickname: (user: {username: string}) => user.username,
}));
vi.mock('@app/features/member/state/GuildMembers', () => ({default: {getMember: () => undefined}}));
vi.mock('@app/features/channel/components/ChannelTyping', async () => {
	const {createElement: create} = await import('react');
	return {Typing: () => create('svg')};
});
vi.mock('@app/features/ui/avatars/AvatarStack', async () => {
	const {createElement: create} = await import('react');
	return {
		AvatarStack: ({className, users}: {className?: string; users: ReadonlyArray<unknown>}) =>
			create('div', {className, 'data-avatar-count': users.length}),
	};
});

const {RollingTypingAnnouncer, RollingTypingUsers} = await import(
	'@app/features/channel/components/RollingTypingUsers'
);
const {default: RollingTypingStore} = await import('@app/features/typing/rolling/RollingTypingStore');

class StubResizeObserver {
	static instances: Array<StubResizeObserver> = [];
	readonly targets: Array<Element> = [];
	disconnected = false;

	constructor(private readonly callback: () => void) {
		StubResizeObserver.instances.push(this);
	}

	observe(target: Element): void {
		this.targets.push(target);
	}

	disconnect(): void {
		this.disconnected = true;
	}

	trigger(): void {
		this.callback();
	}
}

const CHANNEL = {id: 'channel', guildId: 'guild'} as unknown as Channel;
const TYPIST_IDS = ['alice', 'bob', 'carol', 'dave', 'erin'];

let host: HTMLDivElement;
let root: Root;
let overflowContainer: HTMLElement;
let overflowContainerRef: {current: HTMLElement | null};

function setWidth(element: Element, property: 'clientWidth' | 'scrollWidth', width: number): void {
	Object.defineProperty(element, property, {configurable: true, value: width});
}

function render(node: ReactNode): void {
	act(() => {
		root.render(node);
	});
}

function renderRow(props: {showAvatars?: boolean} = {}): void {
	render(
		createElement(
			Fragment,
			null,
			createElement(RollingTypingUsers, {channel: CHANNEL, overflowContainerRef, showAvatars: props.showAvatars}),
			createElement(RollingTypingAnnouncer, {channel: CHANNEL}),
		),
	);
}

function startTyping(...userIds: Array<string>): void {
	act(() => {
		for (const userId of userIds) {
			RollingTypingStore.start(CHANNEL.id, userId, userId === 'me' ? 'local' : 'gateway');
		}
	});
}

function query(flx: string): HTMLElement | null {
	return host.querySelector<HTMLElement>(`[data-flx="${flx}"]`);
}

function visibleText(): string | null {
	return query('channel.typing-users.span')?.textContent ?? null;
}

function announcer(): HTMLElement {
	return query('channel.rolling-typing-users.announcer')!;
}

function measure(): HTMLElement | null {
	return query('channel.rolling-typing-users.measure');
}

function activeResizeObservers(): Array<StubResizeObserver> {
	return StubResizeObserver.instances.filter((instance) => !instance.disconnected);
}

function remeasure(): void {
	act(() => {
		for (const instance of activeResizeObservers()) {
			instance.trigger();
		}
	});
}

beforeEach(() => {
	(globalThis as {IS_REACT_ACT_ENVIRONMENT?: boolean}).IS_REACT_ACT_ENVIRONMENT = true;
	StubResizeObserver.instances = [];
	vi.stubGlobal('ResizeObserver', StubResizeObserver);
	host = document.createElement('div');
	overflowContainer = document.createElement('div');
	document.body.append(overflowContainer, host);
	setWidth(overflowContainer, 'clientWidth', 1000);
	overflowContainerRef = {current: overflowContainer};
	root = createRoot(host);
	runInAction(() => {
		doubles.auth.currentUserId = 'me';
		doubles.developerOptions.showMyselfTyping = false;
		doubles.blockedUserIds.clear();
		doubles.users.clear();
		for (const id of ['me', ...TYPIST_IDS]) {
			doubles.users.set(id, {id, username: id});
		}
	});
});

afterEach(() => {
	act(() => {
		root.unmount();
	});
	RollingTypingStore.reset();
	document.body.replaceChildren();
	vi.unstubAllGlobals();
});

describe('RollingTypingUsers', () => {
	it('hides the current user from the typing row', () => {
		renderRow();
		startTyping('me', 'alice');

		expect(visibleText()).toBe('alice is typing...');
		expect(host.querySelector('[data-avatar-count]')?.getAttribute('data-avatar-count')).toBe('1');
	});

	it('shows the current user when the developer option is on', () => {
		runInAction(() => {
			doubles.developerOptions.showMyselfTyping = true;
		});
		renderRow();
		startTyping('me', 'alice');

		expect(visibleText()).toBe('me and alice are typing...');
	});

	it('hides blocked users from the typing row', () => {
		runInAction(() => {
			doubles.blockedUserIds.add('bob');
		});
		renderRow();
		startTyping('bob', 'alice');

		expect(visibleText()).toBe('alice is typing...');
	});

	it('skips typists missing from the user cache', () => {
		renderRow();
		startTyping('stranger', 'alice');

		expect(RollingTypingStore.countTypists(CHANNEL.id)).toBe(2);
		expect(visibleText()).toBe('alice is typing...');
	});

	it('renders nothing visible when only self is typing', () => {
		renderRow();
		startTyping('me');

		expect(query('channel.typing-users.div')).toBeNull();
		expect(announcer().textContent).toBe('');
	});

	it('marks the visible text as hidden from assistive technology', () => {
		renderRow();
		startTyping('alice');

		const visible = query('channel.typing-users.span')!;
		expect(visible.getAttribute('aria-hidden')).toBe('true');
		expect(visible.hasAttribute('aria-live')).toBe(false);
		expect(measure()?.getAttribute('aria-hidden')).toBe('true');
	});

	it('keeps the announcer mounted when nobody is typing', () => {
		renderRow();
		const mounted = announcer();
		expect(mounted.textContent).toBe('');

		startTyping('alice');
		expect(announcer()).toBe(mounted);
		expect(mounted.textContent).toBe('alice is typing...');

		act(() => {
			RollingTypingStore.remove(CHANNEL.id, 'alice');
		});
		expect(announcer()).toBe(mounted);
		expect(mounted.textContent).toBe('');
	});

	it('announces through a polite atomic live region', () => {
		renderRow();
		startTyping('alice', 'bob', 'carol', 'dave');

		expect(announcer().getAttribute('aria-live')).toBe('polite');
		expect(announcer().getAttribute('aria-atomic')).toBe('true');
		expect(announcer().textContent).toBe('Several people are typing...');
	});

	it('swaps to multiple people when the measured text plus 48 px exceeds the available width', () => {
		setWidth(overflowContainer, 'clientWidth', 300);
		renderRow({showAvatars: false});
		startTyping('alice', 'bob');
		expect(visibleText()).toBe('alice and bob are typing...');

		setWidth(measure()!, 'scrollWidth', 253);
		remeasure();

		expect(visibleText()).toBe('Multiple people are typing...');
		expect(measure()?.textContent).toBe('alice and bob are typing...');
	});

	it('keeps the names when they fit with 48 px to spare', () => {
		setWidth(overflowContainer, 'clientWidth', 300);
		renderRow({showAvatars: false});
		startTyping('alice', 'bob');

		setWidth(measure()!, 'scrollWidth', 252);
		remeasure();

		expect(visibleText()).toBe('alice and bob are typing...');
	});

	it('keeps the full string in the announcer while the visible text overflows', () => {
		setWidth(overflowContainer, 'clientWidth', 300);
		renderRow({showAvatars: false});
		startTyping('alice', 'bob', 'carol');

		setWidth(measure()!, 'scrollWidth', 400);
		remeasure();

		expect(visibleText()).toBe('Multiple people are typing...');
		expect(announcer().textContent).toBe('alice, bob and carol are typing...');
	});

	it('restores the names when the container grows again', () => {
		setWidth(overflowContainer, 'clientWidth', 300);
		renderRow({showAvatars: false});
		startTyping('alice');
		setWidth(measure()!, 'scrollWidth', 280);
		remeasure();
		expect(visibleText()).toBe('Multiple people are typing...');

		setWidth(overflowContainer, 'clientWidth', 400);
		remeasure();

		expect(visibleText()).toBe('alice is typing...');
	});

	it('does not measure overflow with four or more typists', () => {
		setWidth(overflowContainer, 'clientWidth', 10);
		renderRow();
		startTyping('alice', 'bob', 'carol');
		expect(visibleText()).toBe('Multiple people are typing...');
		expect(activeResizeObservers()).toHaveLength(1);

		startTyping('dave');

		expect(visibleText()).toBe('Several people are typing...');
		expect(measure()).toBeNull();
		expect(activeResizeObservers()).toHaveLength(0);
	});

	it('subtracts the avatar stack width from the available width', () => {
		setWidth(overflowContainer, 'clientWidth', 300);
		renderRow({showAvatars: true});
		startTyping('alice');
		setWidth(measure()!, 'scrollWidth', 200);
		remeasure();
		expect(visibleText()).toBe('alice is typing...');

		const avatarStack = host.querySelector(`.${messageStyles.typingAvatarContainer}`)!;
		avatarStack.getBoundingClientRect = () => ({width: 60}) as DOMRect;
		remeasure();

		expect(visibleText()).toBe('Multiple people are typing...');
	});

	it('disconnects the resize observer on unmount', () => {
		renderRow();
		startTyping('alice');
		const [resizeObserver] = activeResizeObservers();
		expect(resizeObserver?.targets).toEqual([overflowContainer, measure()]);

		act(() => {
			root.unmount();
		});
		root = createRoot(host);

		expect(resizeObserver?.disconnected).toBe(true);
		expect(activeResizeObservers()).toHaveLength(0);
	});

	it("does not re-render on the current user's own typing", () => {
		const onRender = vi.fn();
		render(
			createElement(
				Profiler,
				{id: 'typing-row', onRender},
				createElement(RollingTypingUsers, {channel: CHANNEL, overflowContainerRef}),
			),
		);
		startTyping('alice');
		const commits = onRender.mock.calls.length;

		startTyping('me');
		act(() => {
			RollingTypingStore.start(CHANNEL.id, 'me', 'gateway');
			RollingTypingStore.start(CHANNEL.id, 'alice', 'gateway');
		});
		act(() => {
			RollingTypingStore.remove(CHANNEL.id, 'me');
		});

		expect(onRender).toHaveBeenCalledTimes(commits);
		expect(visibleText()).toBe('alice is typing...');
	});
});
