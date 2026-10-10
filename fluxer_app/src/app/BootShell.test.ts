// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {readFileSync} from 'node:fs';
import {resolve} from 'node:path';
import {SHELL_HINT_VERSION} from '@app/features/app/components/skeleton/ShellHint';
import {createElement, type ReactNode} from 'react';
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

const html = readFileSync(resolve(process.cwd(), 'index.html'), 'utf8');

function readBootScript(): string {
	const body = html.slice(html.indexOf('<div id="root"></div>'));
	const match = /<script[^>]*>([\s\S]*?)<\/script>/u.exec(body);
	expect(match).not.toBeNull();
	return match?.[1] ?? '';
}

interface ShellHintOverrides {
	readonly [key: string]: unknown;
}

const ACCOUNT_KEY = 'https://one.example/api::100';

function desktopDMHint(overrides: ShellHintOverrides = {}): Record<string, unknown> {
	return {
		v: SHELL_HINT_VERSION,
		t: Date.now(),
		p: '/channels/@me',
		a: ACCOUNT_KEY,
		mo: 0,
		ua: 0,
		fs: 0,
		sw: 320,
		sv: 'dm',
		ck: 'other',
		cb: 1,
		rf: 1,
		rv: 1,
		ru: [],
		rti: 0,
		ro: 0,
		ri: [
			[0, 0],
			[0, 0],
			[0, 0],
			[0, 0],
		],
		rgi: -1,
		rbm: 15,
		rbi: -1,
		rs: 0,
		gb: 0,
		ga: 1778,
		gn: 0,
		gd: 0,
		gm: 0,
		sr: 6,
		sam: 7,
		ss: 1,
		cid: '',
		ch: 2,
		ct: 0,
		cnw: 0,
		ctw: 0,
		cda: -1,
		cma: -1,
		cfv: 1,
		cst: 0,
		cuv: 0,
		ml: 0,
		co: 4,
		com: 2,
		cdv: 0,
		pc: 0,
		pg: 16,
		pf: 16,
		ps: 16,
		pa: 0,
		pt: 56,
		pv: 800,
		nr: [],
		vh: 0,
		vc: 0,
		mr: 0,
		ms: 0,
		mn: 0,
		...overrides,
	};
}

function setViewportWidth(width: number): void {
	Object.defineProperty(window, 'innerWidth', {value: width, configurable: true});
	const matches = width >= 1024;
	Object.defineProperty(window, 'matchMedia', {
		value: (query: string) => ({matches: query.includes('1024') ? matches : false, media: query}),
		configurable: true,
	});
}

function failScript(): void {
	const script = document.createElement('script');
	document.body.appendChild(script);
	script.dispatchEvent(new Event('error', {bubbles: false}));
}

const bootRoots: Array<HTMLElement> = [];

interface BootScriptOptions {
	readonly signedIn?: boolean;
	readonly storage?: Readonly<Record<string, string>>;
}

function runBootScript(
	hint: Record<string, unknown> | null,
	pathname = '/channels/@me',
	{signedIn = true, storage = {}}: BootScriptOptions = {},
): HTMLElement | null {
	document.body.innerHTML = '<div id="root"></div>';
	window.history.pushState({}, '', pathname);
	localStorage.clear();
	if (signedIn) {
		localStorage.setItem('fluxer:gateway:preboot:session', '1');
	}
	localStorage.setItem('fluxer:auth:active-account-key', ACCOUNT_KEY);
	if (hint != null) {
		localStorage.setItem('fluxer:ui:shell-hint', JSON.stringify(hint));
	}
	for (const [key, value] of Object.entries(storage)) {
		localStorage.setItem(key, value);
	}
	const root = document.getElementById('root');
	if (root != null) {
		bootRoots.push(root);
	}
	new Function(readBootScript())();
	return document.querySelector<HTMLElement>('#fluxer-boot-shell');
}

afterEach(async () => {
	for (const root of bootRoots.splice(0)) {
		root.replaceChildren(document.createElement('span'));
		root.replaceChildren();
	}
	await Promise.resolve();
	localStorage.clear();
	document.body.innerHTML = '';
	vi.useRealTimers();
});

test('a document script that never loads drops the boot shell because nothing else can mount the app', () => {
	setViewportWidth(1400);

	expect(runBootScript(desktopDMHint())).not.toBeNull();

	failScript();

	expect(document.querySelector('#fluxer-boot-shell')).toBeNull();
});

test('the backstop removes a boot shell React never cleared', () => {
	vi.useFakeTimers();
	setViewportWidth(1400);

	expect(runBootScript(desktopDMHint())).not.toBeNull();

	vi.advanceTimersByTime(60_000);

	expect(document.querySelector('#fluxer-boot-shell')).toBeNull();
});

test('the error listener and the backstop stop existing once React has cleared the boot shell', async () => {
	vi.useFakeTimers();
	setViewportWidth(1400);

	expect(runBootScript(desktopDMHint())).not.toBeNull();

	document.getElementById('root')?.replaceChildren();
	await Promise.resolve();

	const marker = document.createElement('div');
	marker.id = 'fluxer-boot-shell';
	document.body.appendChild(marker);

	failScript();
	vi.advanceTimersByTime(60_000);

	expect(document.querySelector('#fluxer-boot-shell')).not.toBeNull();
});

test('React clearing its own container is what removes the boot shell', async () => {
	const container = document.createElement('div');
	container.id = 'root';
	const shell = document.createElement('div');
	shell.id = 'fluxer-boot-shell';
	container.appendChild(shell);
	document.body.appendChild(container);

	createRoot(container).render(createElement('span', null, 'mounted'));

	await vi.waitFor(() => {
		expect(container.textContent).toContain('mounted');
	});

	expect(container.querySelector('#fluxer-boot-shell')).toBeNull();
});
