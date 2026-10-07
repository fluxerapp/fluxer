// SPDX-License-Identifier: AGPL-3.0-or-later

import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';

const runtime = vi.hoisted(() => ({desktop: false}));

vi.mock('@lingui/core/macro', () => ({msg: (descriptor: unknown) => descriptor}));

vi.mock('@app/features/platform/DesktopLocalAppRuntime', async (importOriginal) => ({
	...(await importOriginal<typeof import('@app/features/platform/DesktopLocalAppRuntime')>()),
	isDesktopLocalAppDocument: () => runtime.desktop,
}));
vi.mock('@app/features/app/state/RuntimeConfig', () => ({
	default: {
		mediaEndpoint: 'https://fluxerusercontent.com',
		staticCdnEndpoint: 'https://fluxerstatic.com',
		uploadRelayEndpoint: null,
		getSnapshot: () => ({apiEndpoint: 'https://api.fluxer.app'}),
	},
}));
vi.mock('@app/features/app/state/InstanceSnapshotStore', () => ({runtimeInstanceKey: () => 'instance-key'}));
vi.mock('@app/features/messaging/state/AttachmentUrlRefresher', () => ({
	default: {refresh: async (url: string) => url},
}));

const {fetchTextualPreviewText} = await import(
	'@app/features/channel/components/embeds/attachments/TextualAttachmentPreviewFetch'
);

const ATTACHMENT_URL = 'https://fluxerusercontent.com/attachments/1/2/message.txt?ex=1&is=2&hm=3';

describe('fetchTextualPreviewText', () => {
	const fetchMock = vi.fn(async (_input: RequestInfo | URL, _init?: RequestInit) => new Response('hello'));

	beforeEach(() => {
		vi.stubGlobal('fetch', fetchMock);
		vi.stubGlobal('window', {location: {href: 'fluxer-app://app/channels/@me'}});
	});

	afterEach(() => {
		vi.unstubAllGlobals();
		fetchMock.mockClear();
		runtime.desktop = false;
	});

	it('routes media fetches through the desktop resource proxy', async () => {
		runtime.desktop = true;
		await expect(fetchTextualPreviewText(ATTACHMENT_URL, new AbortController().signal)).resolves.toBe('hello');
		const requested = new URL(String(fetchMock.mock.calls[0]?.[0]));
		expect(requested.protocol).toBe('fluxer-app:');
		expect(requested.pathname).toBe('/proxy/instance-key');
		expect(requested.searchParams.get('url')).toBe(ATTACHMENT_URL);
	});

	it('fetches media directly on the web', async () => {
		await expect(fetchTextualPreviewText(ATTACHMENT_URL, new AbortController().signal)).resolves.toBe('hello');
		expect(fetchMock.mock.calls[0]?.[0]).toBe(ATTACHMENT_URL);
	});
});
