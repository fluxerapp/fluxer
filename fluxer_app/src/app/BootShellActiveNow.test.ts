// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {readFileSync} from 'node:fs';
import {resolve} from 'node:path';
import {FriendsSkeleton} from '@app/features/app/components/skeleton/FriendsSkeleton';
import {SHELL_HINT_VERSION} from '@app/features/app/components/skeleton/ShellHint';
import {
	type RememberedSkeletonActiveNowCard,
	type RememberedSkeletonFriendsLayout,
	SKELETON_DEFAULT_FRIENDS_LAYOUT,
} from '@app/features/app/components/skeleton/SkeletonLayoutMemory';
import {act, createElement, type ReactNode} from 'react';
import {createRoot} from 'react-dom/client';
import {afterEach, expect, test, vi} from 'vitest';

vi.mock('@lingui/core/macro', () => {
	const descriptor = (value: unknown): unknown => (typeof value === 'string' ? {message: value} : value);
	return {msg: descriptor, t: descriptor, plural: () => '', select: () => '', selectOrdinal: () => ''};
});
vi.mock('@lingui/react/macro', () => ({
	Trans: ({children}: {children?: ReactNode}) => children ?? null,
	useLingui: () => ({i18n: {_: (descriptor: {message?: string}) => descriptor.message ?? ''}}),
}));

const memory = vi.hoisted(() => ({friends: null as RememberedSkeletonFriendsLayout | null}));

vi.mock('@app/features/app/components/skeleton/SkeletonLayoutMemory', async (importOriginal) => {
	const actual = await importOriginal<typeof import('@app/features/app/components/skeleton/SkeletonLayoutMemory')>();
	return {...actual, getRememberedSkeletonFriendsLayout: () => memory.friends};
});

const ACCOUNT_KEY = 'https://one.example/api::100';
const html = readFileSync(resolve(process.cwd(), 'index.html'), 'utf8');

function readBootScript(): string {
	const body = html.slice(html.indexOf('<div id="root"></div>'));
	return /<script[^>]*>([\s\S]*?)<\/script>/u.exec(body)?.[1] ?? '';
}

function friendsHint(extra: Record<string, unknown>): Record<string, unknown> {
	return {
		v: SHELL_HINT_VERSION,
		t: Date.now(),
		p: '/channels/@me',
		a: ACCOUNT_KEY,
		mo: 0,
		ua: 0,
		sv: 'dm',
		ck: 'other',
		cb: 1,
		...extra,
	};
}

function bootSidebar(hint: Record<string, unknown> | null): Element | null {
	document.body.innerHTML = '<div id="root"></div>';
	window.history.pushState({}, '', '/channels/@me');
	localStorage.clear();
	localStorage.setItem('fluxer:gateway:preboot:session', '1');
	localStorage.setItem('fluxer:auth:active-account-key', ACCOUNT_KEY);
	if (hint != null) {
		localStorage.setItem('fluxer:ui:shell-hint', JSON.stringify(hint));
	}
	new Function(readBootScript())();
	const shell = document.querySelector('#fluxer-boot-shell');
	expect(shell?.querySelector('.fluxer-boot-friends')).not.toBeNull();
	return shell?.querySelector('.fluxer-boot-fr-side') ?? null;
}

async function reactSidebar(friends: RememberedSkeletonFriendsLayout | null): Promise<Element | null> {
	memory.friends = friends;
	const container = document.createElement('div');
	document.body.appendChild(container);
	const root = createRoot(container);
	await act(async () => root.render(createElement(FriendsSkeleton)));
	const sidebar = container.querySelector('flx-app-friends-skeleton-sidebar');
	const copy = sidebar?.cloneNode(true) as Element | undefined;
	await act(async () => root.unmount());
	return copy ?? null;
}

function bootGeometry(sidebar: Element | null): Array<string> {
	return Array.from(sidebar?.querySelectorAll('.fluxer-boot-block') ?? []).map((block) => {
		const style = block.getAttribute('style') ?? '';
		const width = /width:([^;]+);/.exec(style)?.[1];
		const height = /height:([^;]+);/.exec(style)?.[1];
		return `${width}x${height}`;
	});
}

function reactGeometry(sidebar: Element | null): Array<string> {
	return Array.from(sidebar?.querySelectorAll('flx-skeleton-line, flx-skeleton-block') ?? []).map((block) => {
		const style = (block as HTMLElement).style;
		return `${style.getPropertyValue('--skeleton-width')}x${style.getPropertyValue('--skeleton-height')}`;
	});
}

function layoutWith(activeNowVisible: boolean, cards: ReadonlyArray<RememberedSkeletonActiveNowCard>) {
	return {...SKELETON_DEFAULT_FRIENDS_LAYOUT, activeNowVisible, activeNowCards: cards};
}

afterEach(() => {
	memory.friends = null;
	localStorage.clear();
	document.body.innerHTML = '';
});

test('with nobody active the boot shell paints the same empty state React paints', async () => {
	const boot = bootSidebar(friendsHint({fa: 1, fn: []}));
	const react = await reactSidebar(layoutWith(true, []));

	expect(boot?.querySelector('.fluxer-boot-fr-empty')).not.toBeNull();
	expect(boot?.querySelector('.fluxer-boot-fr-card')).toBeNull();
	expect(bootGeometry(boot)).toEqual(reactGeometry(react));
});

test('remembered cards paint with the same count and geometry as the React skeleton', async () => {
	const cards = [
		{participantCount: 0, streaming: false},
		{participantCount: 3, streaming: true},
		{participantCount: 7, streaming: false},
	];
	const boot = bootSidebar(
		friendsHint({
			fa: 1,
			fn: [
				[0, 0],
				[3, 1],
				[7, 0],
			],
		}),
	);
	const react = await reactSidebar(layoutWith(true, cards));

	expect(boot?.querySelectorAll('.fluxer-boot-fr-card')).toHaveLength(3);
	expect(bootGeometry(boot)).toEqual(reactGeometry(react));
});

test('a hidden active now column paints no sidebar in either skeleton', async () => {
	expect(bootSidebar(friendsHint({fa: 0, fn: []}))).toBeNull();
	expect(await reactSidebar(layoutWith(false, []))).toBeNull();
});

test('without any remembered layout neither skeleton invents activity cards', async () => {
	const boot = bootSidebar(null);
	const react = await reactSidebar(null);

	expect(boot?.querySelector('.fluxer-boot-fr-card')).toBeNull();
	expect(react?.querySelector('flx-app-friends-skeleton-active-card')).toBeNull();
	expect(bootGeometry(boot)).toEqual(reactGeometry(react));
});
