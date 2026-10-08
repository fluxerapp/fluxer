// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {readFileSync} from 'node:fs';
import {resolve} from 'node:path';
import {NagbarToneKind} from '@app/features/app/components/layout/NagbarTones';
import {WELCOME_ROTATION} from '@app/features/app/components/setup/SetupWizardWelcomeRotation';
import {
	AuthShellHintKind,
	recordAuthShellHint,
	resolveAuthShellPlaceholder,
} from '@app/features/app/components/skeleton/AuthShellHint';
import {SHELL_HINT_NAGBAR_TONE_ORDER, SHELL_HINT_VERSION} from '@app/features/app/components/skeleton/ShellHint';
import {
	LEGACY_SHELL_HINT_NAGBAR_TONE_ORDER,
	LEGACY_SHELL_HINT_SEED_VERSION,
	legacyShellHintSeed,
} from '@fluxer/desktop_ipc/src/LegacyShellHintSeed';
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

test('a desktop hint paints the boot shell', () => {
	setViewportWidth(1400);

	const shell = runBootScript(desktopDMHint());

	expect(shell).not.toBeNull();
	expect(shell?.dataset.mobile).toBe('0');
	expect(shell?.dataset.userArea).toBe('1');
	expect(shell?.firstElementChild?.className).toBe('fluxer-boot-app');
});

test('the boot shell is parsed in an inert template so the render-blocking expectation outlives it', () => {
	setViewportWidth(1400);
	const descriptor = Object.getOwnPropertyDescriptor(Element.prototype, 'innerHTML');
	const rootWrites: Array<string> = [];
	const innerHTML = vi.spyOn(Element.prototype, 'innerHTML', 'set').mockImplementation(function (
		this: Element,
		value: string,
	) {
		if (this.id === 'root') {
			rootWrites.push(value);
		}
		descriptor?.set?.call(this, value);
	});
	const createElement = vi.spyOn(document, 'createElement');
	try {
		const shell = runBootScript(desktopDMHint());

		expect(shell?.querySelector('svg')).not.toBeNull();
		expect(shell?.parentElement?.id).toBe('root');
		expect(rootWrites).toEqual([]);
		expect(createElement.mock.calls.map(([tagName]) => tagName)).toContain('template');
	} finally {
		createElement.mockRestore();
		innerHTML.mockRestore();
	}
});

function railGuildSlots(shell: HTMLElement | null): number {
	return shell?.querySelectorAll('.fluxer-boot-rail-items > .fluxer-boot-slot').length ?? 0;
}

test('a hint written for another account is ignored and the default shell paints instead', () => {
	setViewportWidth(1400);

	const shell = runBootScript(desktopDMHint({a: 'https://two.example/api::200', ri: [[0, 0]]}));

	expect(shell?.dataset.hint).toBe('default');
	expect(railGuildSlots(shell)).toBe(6);
});

test('a signed-in boot without a hint paints the generic friends shell at the DM root', () => {
	setViewportWidth(1400);

	const shell = runBootScript(null);

	expect(shell?.dataset.hint).toBe('default');
	expect(shell?.dataset.userArea).toBe('1');
	expect(railGuildSlots(shell)).toBe(6);
	expect(shell?.querySelector('.fluxer-boot-dsb')).not.toBeNull();
	expect(shell?.querySelector('.fluxer-boot-friends')).not.toBeNull();
});

test('a signed-in boot without a hint paints a guild chat shell with the member list on a channel route', () => {
	setViewportWidth(1400);

	const shell = runBootScript(null, '/channels/100/200');

	expect(shell?.dataset.hint).toBe('default');
	expect(shell?.querySelector('.fluxer-boot-gsb')).not.toBeNull();
	expect(shell?.querySelector('.fluxer-boot-chat')).not.toBeNull();
	expect(shell?.querySelector('.fluxer-boot-member-list')).not.toBeNull();
	expect(shell?.querySelector('.fluxer-boot-composer')).not.toBeNull();
});

test('a signed-in boot without a hint paints the mobile channel list with a bottom nav on a phone', () => {
	setViewportWidth(390);

	const shell = runBootScript(null, '/channels/@me');

	expect(shell?.dataset.mobile).toBe('1');
	expect(shell?.dataset.bottomNav).toBe('1');
	expect(shell?.querySelector('.fluxer-boot-dsb')).not.toBeNull();
});

test('a signed-in boot never paints a shell on a route the app renders without guild chrome', () => {
	setViewportWidth(1400);

	expect(runBootScript(null, '/notifications')).toBeNull();
	expect(runBootScript(null, '/channels/@discover')).toBeNull();
});

test('a days-old hint for the same account and route still paints as remembered', () => {
	setViewportWidth(1400);

	const shell = runBootScript(desktopDMHint({t: Date.now() - 3 * 86_400_000}));

	expect(shell?.dataset.hint).toBe('hint');
	expect(railGuildSlots(shell)).toBe(4);
});

test('a hint for another route keeps its rail and global chrome but resolves the route it lands on', () => {
	setViewportWidth(1400);

	const shell = runBootScript(desktopDMHint({nr: [[0, 0, 1]]}), '/channels/100/200');

	expect(shell?.dataset.hint).toBe('derived');
	expect(railGuildSlots(shell)).toBe(4);
	expect(shell?.querySelectorAll('.fluxer-boot-nagbar')).toHaveLength(1);
	expect(shell?.querySelector('.fluxer-boot-gsb')).not.toBeNull();
	expect(shell?.querySelector('.fluxer-boot-dsb')).toBeNull();
});

test('a hint seeded from the legacy desktop client paints its rail, nagbars and sidebar on the first boot', () => {
	setViewportWidth(1400);
	const seed = legacyShellHintSeed(
		{
			SkeletonLayoutMemory: JSON.stringify({
				version: 9,
				chrome: {
					dmSidebar: {
						isMobile: false,
						friendsVisible: true,
						personalNotesVisible: true,
						premiumVisible: false,
						sectionVisible: true,
						channelRowCount: 2,
						channelSubtextFlags: [true, false],
					},
					guildRail: {
						inlineDmRowCount: 0,
						inlineDmUnreadFlags: [],
						selectedInlineDmRowIndex: -1,
						outageVisible: false,
						fluxerVisible: true,
						favoritesVisible: true,
						discoveryVisible: true,
						addGuildVisible: true,
						downloadVisible: false,
						helpVisible: true,
						selectedItemIndex: -1,
						organizedItems: [
							{kind: 'guild', indicator: 'none'},
							{kind: 'guild', indicator: 'unread'},
						],
						scrollTopPx: 0,
					},
				},
				nagbar: {
					rows: [
						{tone: NagbarToneKind.ALERT, hasActions: true, dismissible: false},
						{tone: NagbarToneKind.BRAND, hasActions: true, dismissible: true},
					],
				},
			}),
			'fluxer:ui:sidebar-width': '306',
		},
		ACCOUNT_KEY,
		Date.now(),
	);
	const hint = JSON.parse(seed ?? '');
	expect(hint.v).toBe(SHELL_HINT_VERSION);
	expect(LEGACY_SHELL_HINT_SEED_VERSION).toBe(SHELL_HINT_VERSION);

	const shell = runBootScript(hint);

	expect(shell?.dataset.hint).toBe('hint');
	expect(railGuildSlots(shell)).toBe(2);
	expect(shell?.querySelectorAll('.fluxer-boot-nagbar')).toHaveLength(2);
	expect(shell?.querySelector('.fluxer-boot-dsb')).not.toBeNull();
	expect(shell?.querySelector('.fluxer-boot-friends')).not.toBeNull();
	expect(shell?.firstElementChild?.getAttribute('style')).toContain('--layout-sidebar-width:19.125rem');

	const guild = runBootScript(hint, '/channels/100/200');

	expect(guild?.dataset.hint).toBe('derived');
	expect(railGuildSlots(guild)).toBe(2);
	expect(guild?.querySelectorAll('.fluxer-boot-nagbar')).toHaveLength(2);
});

test('the legacy seed encodes nagbar tones in the order the boot shell decodes them', () => {
	expect(LEGACY_SHELL_HINT_NAGBAR_TONE_ORDER).toEqual(SHELL_HINT_NAGBAR_TONE_ORDER);
});

test('the DM root paints the remembered route because the app restores it on boot', () => {
	setViewportWidth(1400);

	const shell = runBootScript(
		desktopDMHint({p: '/channels/100/200', sv: 'guild', ck: 'chat', cb: 0, cid: '200', ch: 0}),
		'/channels/@me',
	);

	expect(shell?.dataset.hint).toBe('hint');
	expect(shell?.querySelector('.fluxer-boot-gsb')).not.toBeNull();
	expect(shell?.querySelector('.fluxer-boot-chat')).not.toBeNull();
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

test('the zoom script syncs --custom-zoom with the persisted preboot mirror before paint', () => {
	const chunk = html.split(/<script[^>]*>/u).find((part) => part.includes('zoom-preboot'));
	expect(chunk).toBeDefined();
	const script = (chunk ?? '').split('</script>')[0];
	expect(script).toContain('zoom-preboot');
	const run = (mirror: string | null): string => {
		document.documentElement.style.removeProperty('--custom-zoom');
		if (mirror === null) {
			localStorage.removeItem('fluxer:accessibility:zoom-preboot');
		} else {
			localStorage.setItem('fluxer:accessibility:zoom-preboot', mirror);
		}
		new Function(script)();
		return document.documentElement.style.getPropertyValue('--custom-zoom');
	};
	expect(run('125')).toBe('125');
	expect(run('75')).toBe('75');
	expect(run(null)).toBe('');
	expect(run('9999')).toBe('');
	expect(run('not-a-number')).toBe('');
	localStorage.removeItem('fluxer:accessibility:zoom-preboot');
});

function runSignedOutBootScript(pathname: string, storage: Readonly<Record<string, string>> = {}): HTMLElement | null {
	return runBootScript(null, pathname, {signedIn: false, storage});
}

function setNativePlatform(native: boolean): void {
	document.documentElement.classList.toggle('platform-native', native);
}

test('a signed-out boot paints the sign-in card at its final size instead of a flat background', () => {
	setViewportWidth(1400);

	const shell = runSignedOutBootScript('/login');

	expect(shell?.dataset.auth).toBe('card');
	expect(shell?.dataset.hint).toBe('default');
	expect(shell?.dataset.height).toBe('214');
	expect(shell?.querySelector<HTMLElement>('.fluxer-boot-auth-card')?.style.height).toBe('13.375rem');
	expect(shell?.querySelectorAll('.fluxer-boot-auth-list > .fluxer-boot-block')).toHaveLength(2);
});

test('a signed-out boot on an app route paints the sign-in card the app redirects to', () => {
	setViewportWidth(1400);

	expect(runSignedOutBootScript('/channels/@me')?.dataset.category).toBe('login');
	expect(runSignedOutBootScript('/channels/100/200')?.dataset.category).toBe('login');
});

test('a signed-out boot sizes the register card and the phone layout from their own defaults', () => {
	setViewportWidth(1400);
	expect(runSignedOutBootScript('/register')?.dataset.height).toBe('674');

	setViewportWidth(390);
	const mobile = runSignedOutBootScript('/register');
	expect(mobile?.dataset.mobile).toBe('1');
	expect(mobile?.dataset.height).toBe('592');
	expect(mobile?.querySelector('.fluxer-boot-auth-mmain')).not.toBeNull();
});

test('a remembered auth card size wins over the default', () => {
	setViewportWidth(1400);
	recordAuthShellHint({pathname: '/login', mobile: false, kind: AuthShellHintKind.CARD, heightPx: 388, widthPx: 512});
	const storage = {'fluxer:ui:auth-shell-hint': localStorage.getItem('fluxer:ui:auth-shell-hint') ?? ''};

	const shell = runSignedOutBootScript('/login', storage);

	expect(shell?.dataset.hint).toBe('hint');
	expect(shell?.dataset.height).toBe('388');
	expect(shell?.querySelector<HTMLElement>('.fluxer-boot-auth-card')?.style.maxWidth).toBe('32rem');
});

test('a remembered phone card is not used on a desktop-sized window', () => {
	setViewportWidth(1400);
	recordAuthShellHint({pathname: '/login', mobile: true, kind: AuthShellHintKind.CARD, heightPx: 500, widthPx: 342});
	const storage = {'fluxer:ui:auth-shell-hint': localStorage.getItem('fluxer:ui:auth-shell-hint') ?? ''};

	expect(runSignedOutBootScript('/login', storage)?.dataset.height).toBe('214');
});

test('a fresh desktop install paints the fullscreen welcome skeleton', () => {
	setViewportWidth(1400);
	setNativePlatform(true);
	try {
		const shell = runSignedOutBootScript('/login');

		expect(shell?.dataset.auth).toBe('full');
		expect(shell?.querySelector('.fluxer-boot-auth-full')).not.toBeNull();
		expect(shell?.querySelector('.fluxer-boot-auth-wtext')?.textContent).toBe('Welcome');
	} finally {
		setNativePlatform(false);
	}
});

test('the fullscreen welcome greets in the stored locale before the browser language', () => {
	setViewportWidth(1400);
	setNativePlatform(true);
	try {
		const shell = runSignedOutBootScript('/login', {locale: 'de'});

		expect(shell?.querySelector('.fluxer-boot-auth-wtext')?.textContent).toBe('Willkommen');
	} finally {
		setNativePlatform(false);
	}
});

test('the boot welcome words match the welcome rotation', () => {
	const table = /var t=(\{[^}]*\});/u.exec(readBootScript());
	expect(table).not.toBeNull();
	const words = new Function(`return ${table?.[1] ?? '{}'}`)() as Record<string, string>;

	expect(words).toEqual(Object.fromEntries(WELCOME_ROTATION.map((entry) => [entry.code, entry.text])));
});

test('a shell that stamps a stored session paints the app shell even before the preboot mirror exists', () => {
	setViewportWidth(1400);
	document.documentElement.setAttribute('data-boot-session', '1');
	try {
		const shell = runSignedOutBootScript('/channels/@me');

		expect(shell?.dataset.hint).toBe('default');
		expect(shell?.dataset.auth).toBeUndefined();
	} finally {
		document.documentElement.removeAttribute('data-boot-session');
	}
});

test('a session an older client stored paints the app shell before the preboot mirror exists', () => {
	setViewportWidth(1400);

	const shell = runSignedOutBootScript('/channels/200/300', {token: 'legacy-token', userId: '100'});

	expect(shell?.dataset.hint).toBe('default');
	expect(shell?.dataset.auth).toBeUndefined();
});

test('a half or emptied older session still paints the sign-in card', () => {
	setViewportWidth(1400);

	expect(runSignedOutBootScript('/channels/@me', {token: 'legacy-token'})?.dataset.auth).toBe('card');
	expect(runSignedOutBootScript('/channels/@me', {token: 'null', userId: '100'})?.dataset.auth).toBe('card');
	expect(
		runSignedOutBootScript('/channels/@me', {
			token: 'legacy-token',
			userId: '100',
			'fluxer:gateway:preboot:session': '0',
		})?.dataset.auth,
	).toBe('card');
});

test('a signed-out boot leaves routes without an auth card alone', () => {
	setViewportWidth(1400);

	expect(runSignedOutBootScript('/oauth2/authorize')).toBeNull();
});

test.each([
	{pathname: '/login', width: 1400, native: false},
	{pathname: '/login', width: 390, native: false},
	{pathname: '/login', width: 1400, native: true},
	{pathname: '/register', width: 1400, native: false},
	{pathname: '/register', width: 390, native: false},
	{pathname: '/forgot', width: 1400, native: false},
	{pathname: '/invite/abc', width: 390, native: false},
])(
	'the inline auth skeleton picks what the app placeholder picks for $pathname at $width px',
	({pathname, width, native}) => {
		setViewportWidth(width);
		setNativePlatform(native);
		try {
			const shell = runSignedOutBootScript(pathname);
			const placeholder = resolveAuthShellPlaceholder(pathname, {
				mobile: width < 640,
				native,
				passkeyLikely: false,
			});

			expect(shell?.dataset.category).toBe(placeholder?.category);
			expect(shell?.dataset.auth).toBe(placeholder?.kind);
			expect(shell?.dataset.hint).toBe(placeholder?.source);
			expect(Number(shell?.dataset.height)).toBe(placeholder?.heightPx);
		} finally {
			setNativePlatform(false);
		}
	},
);
