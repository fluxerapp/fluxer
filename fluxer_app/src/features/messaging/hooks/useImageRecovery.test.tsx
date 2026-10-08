// SPDX-License-Identifier: AGPL-3.0-or-later
// @vitest-environment happy-dom

import {afterEach, beforeEach, describe, expect, test, vi} from 'vitest';

vi.mock('@lingui/core/macro', () => ({msg: (descriptor: unknown) => descriptor}));
vi.mock('@lingui/react/macro', () => ({
	useLingui: () => ({i18n: {_: (descriptor: {message?: string} | undefined) => descriptor?.message ?? ''}}),
}));
vi.mock('@app/features/accessibility/state/Accessibility', () => ({default: {useReducedMotion: false}}));
vi.mock('@app/features/ui/tooltip/Tooltip', () => ({Tooltip: ({children}: {children: unknown}) => children}));
vi.mock('@app/features/ui/focus_ring/FocusRing', () => ({default: ({children}: {children: unknown}) => children}));
vi.mock('@app/features/app/constants/AppConstants', () => ({getStatusTypeLabel: () => ''}));

const AVATAR_URL = 'https://chat.example.com/media/avatars/1/abc.webp?size=128';
const GUILD_ICON_URL = 'https://chat.example.com/media/icons/2/def.webp?size=96';
const RESPONSE_DELAY_MS = 5;

const network = {up: true, requests: [] as Array<string>};

class SelfHostImage {
	onload: (() => void) | null = null;
	onerror: (() => void) | null = null;
	decoding = 'auto';
	complete = false;
	naturalWidth = 0;
	naturalHeight = 0;
	currentSrc = '';
	private source = '';

	get src(): string {
		return this.source;
	}

	set src(next: string) {
		this.source = next;
		const answered = network.up;
		network.requests.push(next);
		setTimeout(() => {
			this.complete = true;
			if (!answered) {
				this.onerror?.();
				return;
			}
			this.naturalWidth = 64;
			this.naturalHeight = 64;
			this.currentSrc = next;
			this.onload?.();
		}, RESPONSE_DELAY_MS);
	}

	getAttribute(): string {
		return this.source;
	}

	get ownerDocument(): Document {
		return document;
	}
}

async function loadHarness() {
	const React = await import('react');
	const {act} = React;
	const {createRoot} = await import('react-dom/client');
	const {BaseAvatar} = await import('@app/features/ui/components/BaseAvatar');
	const {useRecoveringBackgroundImageURL} = await import('@app/features/messaging/hooks/useImageRecovery');
	const ImageCacheUtils = await import('@app/features/messaging/utils/ImageCacheUtils');
	ImageCacheUtils._clearForTests();
	const host = document.createElement('div');
	document.body.appendChild(host);
	const root = createRoot(host);
	function GuildRailIcon({url}: {url: string}) {
		const paintable = useRecoveringBackgroundImageURL(url);
		return React.createElement('div', {
			'data-testid': 'guild-icon',
			style: paintable == null ? undefined : {backgroundImage: `url(${paintable})`},
		});
	}
	const advance = async (ms: number, step = 1000) => {
		for (let elapsed = 0; elapsed < ms; elapsed += step) {
			await act(async () => {
				await vi.advanceTimersByTimeAsync(Math.min(step, ms - elapsed));
			});
		}
	};
	const avatarImage = () => host.querySelector<HTMLImageElement>('[data-flx="ui.base-avatar.image"]');
	const guildIcon = () => host.querySelector<HTMLElement>('[data-testid="guild-icon"]');
	const failAvatarElement = async () => {
		await act(async () => {
			avatarImage()?.dispatchEvent(new Event('error'));
		});
	};
	const render = async (children: Array<React.ReactElement>) => {
		await act(async () => {
			root.render(React.createElement(React.Fragment, null, ...children));
		});
	};
	const avatar = React.createElement(BaseAvatar, {key: 'avatar', size: 40, avatarUrl: AVATAR_URL});
	const rail = React.createElement(GuildRailIcon, {key: 'rail', url: GUILD_ICON_URL});
	return {act, root, advance, avatarImage, guildIcon, failAvatarElement, render, avatar, rail, ImageCacheUtils};
}

beforeEach(() => {
	vi.useFakeTimers({toFake: ['setTimeout', 'clearTimeout', 'Date']});
	vi.stubGlobal('Image', SelfHostImage);
	vi.stubGlobal('IS_REACT_ACT_ENVIRONMENT', true);
	network.up = true;
	network.requests.length = 0;
});

afterEach(() => {
	vi.useRealTimers();
	vi.unstubAllGlobals();
	document.body.innerHTML = '';
});

describe('self-hosted media that failed during an outage loads again on the same mount', () => {
	test('a long-lived avatar and guild rail icon come back after a self-host restart without a remount', async () => {
		const harness = await loadHarness();
		network.up = false;
		await harness.render([harness.avatar, harness.rail]);
		await harness.failAvatarElement();
		await harness.advance(30_000);

		expect(harness.avatarImage()).toBeNull();
		expect(harness.guildIcon()?.style.backgroundImage).toBe('');
		const requestsDuringOutage = network.requests.length;

		network.up = true;
		await harness.advance(5 * 60_000);

		expect(harness.avatarImage()?.getAttribute('src')).toBe(AVATAR_URL);
		expect(harness.guildIcon()?.style.backgroundImage).toContain(GUILD_ICON_URL);
		expect(harness.ImageCacheUtils.hasFailedImage(AVATAR_URL)).toBe(false);
		expect(network.requests.length).toBeGreaterThan(requestsDuringOutage);
		await harness.act(async () => harness.root.unmount());
	});

	test('retries back off during a long outage instead of hammering the self-host', async () => {
		const harness = await loadHarness();
		network.up = false;
		await harness.render([harness.avatar]);
		await harness.failAvatarElement();
		await harness.advance(10 * 60_000, 5000);

		const lateWindowStart = network.requests.length;
		await harness.advance(5 * 60_000, 5000);
		const lateWindowRequests = network.requests.length - lateWindowStart;

		expect(lateWindowRequests).toBeGreaterThan(0);
		expect(lateWindowRequests).toBeLessThanOrEqual(5 * 3 + 3);
		await harness.act(async () => harness.root.unmount());
	});

	test('an avatar mounted while its URL is cooling down starts blank and still recovers', async () => {
		const harness = await loadHarness();
		network.up = false;
		await harness.render([harness.avatar]);
		await harness.failAvatarElement();
		expect(harness.ImageCacheUtils.hasFailedImage(AVATAR_URL)).toBe(true);

		await harness.render([]);
		await harness.render([harness.avatar]);
		expect(harness.avatarImage()).toBeNull();

		network.up = true;
		await harness.advance(70_000);
		expect(harness.avatarImage()?.getAttribute('src')).toBe(AVATAR_URL);
		await harness.act(async () => harness.root.unmount());
	});

	test('coming back online retries at once instead of waiting out the backoff', async () => {
		const harness = await loadHarness();
		network.up = false;
		await harness.render([harness.avatar]);
		await harness.failAvatarElement();
		await harness.advance(10 * 60_000, 5000);
		expect(harness.avatarImage()).toBeNull();

		network.up = true;
		await harness.act(async () => {
			window.dispatchEvent(new Event('online'));
		});
		await harness.advance(100, 10);
		expect(harness.avatarImage()?.getAttribute('src')).toBe(AVATAR_URL);
		await harness.act(async () => harness.root.unmount());
	});

	test('refocusing the window during an outage does not retry in a burst', async () => {
		const harness = await loadHarness();
		network.up = false;
		await harness.render([harness.avatar]);
		await harness.failAvatarElement();
		await harness.advance(10 * 60_000, 5000);
		const before = network.requests.length;
		for (let i = 0; i < 20; i++) {
			await harness.act(async () => {
				window.dispatchEvent(new Event('focus'));
			});
			await harness.advance(500, 10);
		}
		expect(network.requests.length - before).toBeLessThanOrEqual(3);
		await harness.act(async () => harness.root.unmount());
	});

	test('an unmounted avatar stops retrying', async () => {
		const harness = await loadHarness();
		network.up = false;
		await harness.render([harness.avatar]);
		await harness.failAvatarElement();
		await harness.render([]);
		await harness.advance(60_000, 5000);
		const afterUnmount = network.requests.length;
		await harness.advance(10 * 60_000, 5000);
		expect(network.requests.length).toBe(afterUnmount);
		await harness.act(async () => harness.root.unmount());
	});
});
