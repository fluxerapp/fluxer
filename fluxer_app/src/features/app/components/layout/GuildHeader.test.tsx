// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {GuildHeader} from '@app/features/app/components/layout/GuildHeader';
import styles from '@app/features/app/components/layout/GuildHeader.module.css';
import {useGuildBannerPresentation} from '@app/features/app/hooks/useGuildBannerPresentation';
import {Guild} from '@app/features/guild/models/Guild';
import {WINDOW_FOCUSED_CLASS} from '@app/features/ui/utils/WindowFocusInteractionGuard';
import UserSettings from '@app/features/user/state/UserSettings';
import type {Guild as WireGuild} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {setupI18n} from '@lingui/core';
import {useMotionValue} from 'framer-motion';
import {runInAction} from 'mobx';
import {observer} from 'mobx-react-lite';
import {act, useLayoutEffect} from 'react';
import {createRoot, type Root} from 'react-dom/client';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it, vi} from 'vitest';

vi.mock('@app/features/platform/utils/AppLogger', () => ({
	Logger: class {
		debug = vi.fn();
		info = vi.fn();
		warn = vi.fn();
		error = vi.fn();
	},
}));
vi.mock('@app/features/platform/transport/RestTransport', () => ({
	http: {get: vi.fn(), post: vi.fn()},
}));
vi.mock('@app/features/app/state/RuntimeConfig', () => ({default: {localInstanceDomain: 'fluxer.test'}}));
vi.mock('@app/features/app/config/Config', () => ({
	default: {
		PUBLIC_BUILD_VERSION: 'test',
		PUBLIC_RELEASE_CHANNEL: 'canary',
		PUBLIC_BOOTSTRAP_API_ENDPOINT: 'https://example.invalid',
		PUBLIC_BOOTSTRAP_API_PUBLIC_ENDPOINT: 'https://example.invalid',
	},
}));
vi.mock('@lingui/core/macro', () => {
	const descriptor = (value: unknown): unknown =>
		typeof value === 'string'
			? {id: value, message: value}
			: {...(value as object), id: (value as {message: string}).message};
	return {msg: descriptor, t: descriptor, plural: () => '', select: () => '', selectOrdinal: () => ''};
});
vi.mock('@lingui/react/macro', () => ({
	useLingui: () => ({i18n}),
}));
vi.mock('@app/features/guild/components/popouts/GuildHeaderPopout', () => ({GuildHeaderPopout: () => null}));
vi.mock('@app/features/guild/components/bottomsheets/GuildHeaderBottomSheet', () => ({
	GuildHeaderBottomSheet: () => null,
}));
vi.mock('@app/features/ui/action_menu/GuildContextMenu', () => ({GuildContextMenu: () => null}));
vi.mock('@app/features/app/hooks/useSkeletonLayoutMemoryCapture', () => ({
	useSkeletonLayoutReport: () => {},
	measureSkeletonTextWidthPx: () => 0,
}));
vi.mock('@app/features/messaging/utils/ImageCacheUtils', async (importOriginal) => ({
	...(await importOriginal<typeof import('@app/features/messaging/utils/ImageCacheUtils')>()),
	hasImage: () => true,
}));
vi.mock('@app/features/user/utils/AvatarUtils', async (importOriginal) => ({
	...(await importOriginal<typeof import('@app/features/user/utils/AvatarUtils')>()),
	getGuildBannerURL: ({id, banner}: {id: string; banner: string | null}, animated: boolean) =>
		banner == null ? null : `https://media.test/banners/${id}/${banner}.${animated ? 'gif' : 'webp'}`,
}));

const i18n = setupI18n({locale: 'en-US', messages: {'en-US': {}}});
const DEFAULT_GIF_AUTOPLAY = UserSettings.gifAutoPlay;

(globalThis as {IS_REACT_ACT_ENVIRONMENT?: boolean}).IS_REACT_ACT_ENVIRONMENT = true;

const HEADER_ROW_HEIGHT = 56;
const SIXTEEN_BY_NINE_SIDEBAR_BANNER_HEIGHT = 135;

interface Frame {
	readonly headerHeight: number;
	readonly bannerHeight: number;
	readonly listTop: number;
}

const layout = {sidebarWidth: 240};
const frames: Array<Frame> = [];
const originalOffsetWidth = Object.getOwnPropertyDescriptor(HTMLElement.prototype, 'offsetWidth');
const originalOffsetHeight = Object.getOwnPropertyDescriptor(HTMLElement.prototype, 'offsetHeight');

let container: HTMLDivElement;
let root: Root;

function createBannerGuild(): Guild {
	return new Guild({
		id: '1400000000000000000',
		name: 'Banner Community',
		icon: null,
		banner: 'b4nn3r',
		banner_width: 1600,
		banner_height: 900,
		features: [],
		roles: [],
		owner_id: '1400000000000000001',
	} as unknown as WireGuild);
}

function requireElement(selector: string): HTMLElement {
	const element = container.querySelector<HTMLElement>(selector);
	if (element == null) {
		throw new Error(`missing ${selector}`);
	}
	return element;
}

const Harness = observer(({guild}: {guild: Guild}) => {
	const scrollY = useMotionValue(0);
	const banner = useGuildBannerPresentation({guild, scrollY});
	useLayoutEffect(() => {
		const header = requireElement(`.${styles.headerContainer}`);
		const clip = container.querySelector<HTMLElement>('[data-flx="app.guild-header.banner-clip"]');
		frames.push({
			headerHeight: header.offsetHeight,
			bannerHeight: clip?.offsetHeight ?? 0,
			listTop: header.offsetHeight + banner.collapseDistance,
		});
	});
	return <GuildHeader guild={guild} banner={banner} />;
});

function mount(guild: Guild): void {
	act(() => {
		root.render(<Harness guild={guild} />);
	});
}

function lastFrame(): Frame {
	const frame = frames.at(-1);
	if (frame === undefined) {
		throw new Error('nothing rendered');
	}
	return frame;
}

beforeAll(() => {
	Object.defineProperty(HTMLElement.prototype, 'offsetWidth', {
		configurable: true,
		get() {
			return layout.sidebarWidth;
		},
	});
	Object.defineProperty(HTMLElement.prototype, 'offsetHeight', {
		configurable: true,
		get(this: HTMLElement) {
			const inlineHeight = Number.parseFloat(this.style.height);
			return Number.isFinite(inlineHeight) ? inlineHeight : HEADER_ROW_HEIGHT;
		},
	});
});

afterAll(() => {
	if (originalOffsetWidth) Object.defineProperty(HTMLElement.prototype, 'offsetWidth', originalOffsetWidth);
	if (originalOffsetHeight) Object.defineProperty(HTMLElement.prototype, 'offsetHeight', originalOffsetHeight);
});

beforeEach(() => {
	layout.sidebarWidth = 240;
	frames.length = 0;
	container = document.createElement('div');
	document.body.append(container);
	root = createRoot(container);
});

afterEach(() => {
	act(() => {
		root.unmount();
	});
	container.remove();
	document.documentElement.classList.remove(WINDOW_FOCUSED_CLASS);
	vi.restoreAllMocks();
	runInAction(() => {
		UserSettings.gifAutoPlay = DEFAULT_GIF_AUTOPLAY;
	});
});

describe('GuildHeader banner geometry', () => {
	it('keeps the header at the row height with the list below the banner', () => {
		mount(createBannerGuild());
		expect(lastFrame()).toEqual({
			headerHeight: HEADER_ROW_HEIGHT,
			bannerHeight: SIXTEEN_BY_NINE_SIDEBAR_BANNER_HEIGHT,
			listTop: SIXTEEN_BY_NINE_SIDEBAR_BANNER_HEIGHT,
		});
	});

	it('shrinks the collapsing banner again when the sidebar narrows', () => {
		mount(createBannerGuild());
		layout.sidebarWidth = 160;
		act(() => {
			window.dispatchEvent(new Event('resize'));
		});
		expect(lastFrame()).toEqual({headerHeight: HEADER_ROW_HEIGHT, bannerHeight: 90, listTop: 90});
	});

	it('plays the animated banner on header hover when the banner is too short to collapse', async () => {
		layout.sidebarWidth = 80;
		runInAction(() => {
			UserSettings.gifAutoPlay = false;
		});
		document.documentElement.classList.add(WINDOW_FOCUSED_CLASS);
		mount(createBannerGuild());
		expect(lastFrame()).toEqual({
			headerHeight: HEADER_ROW_HEIGHT,
			bannerHeight: HEADER_ROW_HEIGHT,
			listTop: HEADER_ROW_HEIGHT,
		});
		const bannerImage = requireElement('[data-flx="app.guild-header.banner-image"]');
		expect(bannerImage.style.backgroundImage).toContain('b4nn3r.webp');
		const header = requireElement(`.${styles.headerContainer}`);
		const matches = header.matches.bind(header);
		vi.spyOn(header, 'matches').mockImplementation((selector: string) => selector === ':hover' || matches(selector));
		await act(async () => {
			header.dispatchEvent(new MouseEvent('mouseenter'));
			await new Promise((resolve) => setTimeout(resolve, 0));
		});
		expect(bannerImage.style.backgroundImage).toContain('b4nn3r.gif');
	});
});
