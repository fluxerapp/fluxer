// SPDX-License-Identifier: AGPL-3.0-or-later
// @vitest-environment happy-dom
// @vitest-environment-options {"url":"fluxer-app://app/channels/@me"}

import {beforeEach, describe, expect, test, vi} from 'vitest';

const runtime = vi.hoisted(() => ({
	mediaEndpoint: 'https://media.example.com',
	staticCdnEndpoint: null as string | null,
}));

vi.mock('@app/features/platform/DesktopLocalAppRuntime', async (importOriginal) => ({
	...(await importOriginal<object>()),
	isDesktopLocalAppDocument: () => true,
}));
vi.mock('@app/features/app/state/InstanceSnapshotStore', () => ({runtimeInstanceKey: () => 'instance-key'}));
vi.mock('@app/features/app/state/RuntimeConfig', () => ({
	default: {
		get mediaEndpoint() {
			return runtime.mediaEndpoint;
		},
		get staticCdnEndpoint() {
			return runtime.staticCdnEndpoint;
		},
		uploadRelayEndpoint: null,
		getSnapshot: () => ({}),
	},
}));

const {buildMediaProxyURL} = await import('@app/features/messaging/utils/MediaProxyUtils');
const {resolveDesktopCrossOriginMediaURL} = await import('@app/features/messaging/utils/DesktopResourceUrl');

describe('desktop media URLs', () => {
	beforeEach(() => {
		runtime.mediaEndpoint = 'https://media.example.com';
	});

	test('images on https media load directly so the HTTP cache serves revisits', () => {
		const url = buildMediaProxyURL('https://media.example.com/attachments/1/2/a.png', {format: 'webp', width: 320});
		expect(url).toBe('https://media.example.com/attachments/1/2/a.png?format=webp&width=320');
	});

	test('images on loopback http media load directly', () => {
		runtime.mediaEndpoint = 'http://localhost:48090/media';
		expect(buildMediaProxyURL('http://localhost:48090/media/avatars/1/a.webp')).toBe(
			'http://localhost:48090/media/avatars/1/a.webp',
		);
	});

	test('images on cleartext non-loopback media stay behind the local proxy', () => {
		runtime.mediaEndpoint = 'http://192.168.1.20/media';
		expect(buildMediaProxyURL('http://192.168.1.20/media/avatars/1/a.webp')).toMatch(/^fluxer-app:\/\/app\/proxy\//);
	});

	test('cross-origin media elements keep a same-origin source', () => {
		expect(resolveDesktopCrossOriginMediaURL('https://media.example.com/attachments/1/2/v.mp4')).toMatch(
			/^fluxer-app:\/\/app\/proxy\/instance-key\?/,
		);
	});
});
