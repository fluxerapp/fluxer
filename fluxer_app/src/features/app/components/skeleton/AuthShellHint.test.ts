// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	AuthShellHintKind,
	isPasskeyLikelyForDocumentHost,
	readAuthShellHintEntries,
	recordAuthShellHint,
	resolveAuthShellCategory,
	resolveAuthShellPlaceholder,
	resolveAuthShellRowCount,
	useAuthShellHintCapture,
} from '@app/features/app/components/skeleton/AuthShellHint';
import {AUTH_SHELL_HINT_STORAGE_KEY} from '@app/features/platform/state/PrebootMirror';
import {act, createElement} from 'react';
import {createRoot, type Root} from 'react-dom/client';
import {afterEach, expect, test, vi} from 'vitest';

const DESKTOP = {mobile: false, native: false, passkeyLikely: false};

afterEach(() => {
	localStorage.clear();
	document.body.innerHTML = '';
	vi.useRealTimers();
});

test('every route the signed-out app redirects to the sign-in page shares the login category', () => {
	for (const pathname of ['/', '/app', '/login', '/channels/@me', '/channels/1/2', '/bookmarks', '/you']) {
		expect(resolveAuthShellCategory(pathname)).toBe('login');
	}
	expect(resolveAuthShellCategory('/register')).toBe('register');
	expect(resolveAuthShellCategory('/invite/abc')).toBe('invite');
	expect(resolveAuthShellCategory('/gift/abc/login')).toBe('gift-login');
	expect(resolveAuthShellCategory('/forgot')).toBe('forgot');
	expect(resolveAuthShellCategory('/oauth2/authorize')).toBeNull();
});

test('a recorded card is read back for its category and replaces the previous entry', () => {
	recordAuthShellHint(
		{pathname: '/login', mobile: false, kind: AuthShellHintKind.CARD, heightPx: 300.4, widthPx: 512},
		1,
	);
	recordAuthShellHint(
		{pathname: '/channels/@me', mobile: false, kind: AuthShellHintKind.CARD, heightPx: 260, widthPx: 512},
		2,
	);

	const entries = readAuthShellHintEntries();

	expect(entries.size).toBe(1);
	expect(entries.get('login')).toEqual({t: 2, mo: 0, k: 'card', h: 260, w: 512});
	expect(resolveAuthShellPlaceholder('/login', DESKTOP)).toMatchObject({heightPx: 260, source: 'hint'});
});

test('the hint keeps only the most recent categories', () => {
	for (let index = 0; index < 14; index++) {
		recordAuthShellHint(
			{pathname: `/invite/code-${index}`, mobile: false, kind: AuthShellHintKind.CARD, heightPx: 100, widthPx: 0},
			index,
		);
		recordAuthShellHint(
			{pathname: '/register', mobile: false, kind: AuthShellHintKind.CARD, heightPx: 674, widthPx: 0},
			100 + index,
		);
	}

	expect(readAuthShellHintEntries().size).toBeLessThanOrEqual(12);
	expect(readAuthShellHintEntries().get('register')?.h).toBe(674);
});

test('a corrupt or foreign hint falls back to the defaults', () => {
	localStorage.setItem(AUTH_SHELL_HINT_STORAGE_KEY, '{not json');
	expect(resolveAuthShellPlaceholder('/login', DESKTOP)).toMatchObject({heightPx: 214, source: 'default'});

	localStorage.setItem(
		AUTH_SHELL_HINT_STORAGE_KEY,
		JSON.stringify({v: 99, c: {login: {t: 1, mo: 0, k: 'card', h: 999, w: 0}}}),
	);
	expect(resolveAuthShellPlaceholder('/login', DESKTOP)).toMatchObject({heightPx: 214, source: 'default'});
});

test('the default sign-in card reserves a passkey row only where a passkey can be offered', () => {
	expect(isPasskeyLikelyForDocumentHost('web.fluxer.app', true)).toBe(true);
	expect(isPasskeyLikelyForDocumentHost('web.fluxer.app', false)).toBe(false);
	expect(isPasskeyLikelyForDocumentHost('localhost', true)).toBe(false);
	expect(isPasskeyLikelyForDocumentHost('127.0.0.1', true)).toBe(false);
	expect(resolveAuthShellPlaceholder('/login', {...DESKTOP, passkeyLikely: true})).toMatchObject({
		heightPx: 260,
		rows: 3,
	});
});

test('a remembered card height maps to the rows that fit inside its padding', () => {
	const placeholder = resolveAuthShellPlaceholder('/register', DESKTOP);
	expect(placeholder).not.toBeNull();
	if (placeholder == null) {
		return;
	}
	expect(resolveAuthShellRowCount(placeholder, false, 1400, 1)).toBe(11);
	expect(resolveAuthShellRowCount(placeholder, true, 390, 1)).toBe(12);
});

function CaptureProbe({target, pathname}: {readonly target: HTMLElement; readonly pathname: string}) {
	useAuthShellHintCapture({
		enabled: true,
		pathname,
		mobile: false,
		kind: AuthShellHintKind.CARD,
		resolveTarget: () => target,
	});
	return null;
}

function mountCapture(target: HTMLElement, pathname: string): Root {
	const container = document.createElement('div');
	document.body.appendChild(container);
	const root = createRoot(container);
	act(() => {
		root.render(createElement(CaptureProbe, {target, pathname}));
	});
	return root;
}

function sizedTarget(height: number): HTMLElement {
	const target = document.createElement('div');
	Object.defineProperty(target, 'offsetHeight', {value: height, configurable: true});
	Object.defineProperty(target, 'offsetWidth', {value: 512, configurable: true});
	document.body.appendChild(target);
	return target;
}

test('the first settled card is remembered and a loading placeholder never is', async () => {
	vi.useFakeTimers();
	const target = sizedTarget(244);
	const loading = document.createElement('div');
	loading.setAttribute('data-flx', 'auth.flow.auth-shell-loading-state.card');
	target.appendChild(loading);
	const root = mountCapture(target, '/register');

	await act(async () => {
		vi.advanceTimersByTime(1000);
	});
	expect(readAuthShellHintEntries().get('register')).toBeUndefined();

	await act(async () => {
		loading.remove();
		target.appendChild(document.createElement('form'));
		await Promise.resolve();
		vi.advanceTimersByTime(1000);
	});
	expect(readAuthShellHintEntries().get('register')).toMatchObject({h: 244, w: 512, k: 'card', mo: 0});
	act(() => root.unmount());
});
