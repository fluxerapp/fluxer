// SPDX-License-Identifier: AGPL-3.0-or-later

import {DownloadController} from '@app/api/download/DownloadController';
import {PKGS_BASE_URL} from '@app/api/download/DownloadRedirects';
import type {HonoEnv} from '@app/api/types/HonoEnv';
import {Hono} from 'hono';
import {describe, expect, it} from 'vitest';

const COUNTRY_HEADERS = {'cf-ipcountry': 'BR', 'x-forwarded-for': '203.0.113.7'};

function createApp() {
	const app = new Hono<HonoEnv>();
	app.onError((_error, ctx) => ctx.text('Bad Request', 400));
	DownloadController(app);
	return app;
}

async function request(path: string, init?: RequestInit) {
	const response = await createApp().request(path, init);
	return {
		status: response.status,
		location: response.headers.get('Location'),
		cacheControl: response.headers.get('Cache-Control'),
		body: await response.text(),
	};
}

describe('legacy desktop download routes', () => {
	it('redirects the latest metadata route at the document the publisher writes', async () => {
		const response = await request('/dl/desktop/stable/darwin/arm64/latest');
		expect(response.status).toBe(302);
		expect(response.location).toBe(`${PKGS_BASE_URL}/desktop/stable/darwin/arm64/latest.json`);
		expect(response.cacheControl).toBe('no-store');
		expect(response.body).toBe('');
	});

	it('redirects the latest artifact route', async () => {
		const response = await request('/dl/desktop/stable/linux/x64/latest/appimage');
		expect(response.status).toBe(302);
		expect(response.location).toBe(`${PKGS_BASE_URL}/desktop/stable/linux/x64/latest/appimage`);
		expect(response.cacheControl).toBe('no-store');
	});

	it('redirects the latest checksum route', async () => {
		const response = await request('/dl/desktop/stable/linux/x64/latest/appimage.sha256');
		expect(response.status).toBe(302);
		expect(response.location).toBe(`${PKGS_BASE_URL}/desktop/stable/linux/x64/latest/appimage.sha256`);
		expect(response.cacheControl).toBe('no-store');
	});

	it('stops mapping the retired version listing route, so it passes through to an origin path that serves nothing', async () => {
		const response = await request('/dl/desktop/canary/linux/arm64/versions');
		expect(response.status).toBe(302);
		expect(response.location).toBe(`${PKGS_BASE_URL}/desktop/canary/linux/arm64/versions`);
	});

	it('redirects the latest appimage zsync sidecar rather than rejecting it', async () => {
		const response = await request('/dl/desktop/canary/linux/x64/latest/appimage.zsync');
		expect(response.status).toBe(302);
		expect(response.location).toBe(`${PKGS_BASE_URL}/desktop/canary/linux/x64/latest/appimage.zsync`);
		expect(response.cacheControl).toBe('no-store');
	});

	it('redirects the versioned appimage zsync sidecar and lets the redirect be cached', async () => {
		const response = await request('/dl/desktop/canary/linux/x64/1.4.2/appimage.zsync');
		expect(response.status).toBe(302);
		expect(response.location).toBe(`${PKGS_BASE_URL}/desktop/canary/linux/x64/1.4.2/appimage.zsync`);
		expect(response.cacheControl).toBe('public, max-age=31536000');
	});

	it('answers HEAD on the zsync sidecar the way it answers GET', async () => {
		const response = await request('/dl/desktop/canary/linux/x64/latest/appimage.zsync', {method: 'HEAD'});
		expect(response.status).toBe(302);
		expect(response.location).toBe(`${PKGS_BASE_URL}/desktop/canary/linux/x64/latest/appimage.zsync`);
	});

	it('still rejects a zsync sidecar for a format that publishes none', async () => {
		const response = await request('/dl/desktop/canary/linux/x64/latest/deb.zsync');
		expect(response.status).toBe(400);
	});

	it('redirects the versioned artifact route and lets the redirect be cached', async () => {
		const response = await request('/dl/desktop/stable/win32/x64/1.4.2/setup');
		expect(response.status).toBe(302);
		expect(response.location).toBe(`${PKGS_BASE_URL}/desktop/stable/win32/x64/1.4.2/setup`);
		expect(response.cacheControl).toBe('public, max-age=31536000');
	});

	it('redirects the versioned checksum route', async () => {
		const response = await request('/dl/desktop/stable/win32/x64/1.4.2/setup.sha256');
		expect(response.status).toBe(302);
		expect(response.location).toBe(`${PKGS_BASE_URL}/desktop/stable/win32/x64/1.4.2/setup.sha256`);
		expect(response.cacheControl).toBe('public, max-age=31536000');
	});

	it('answers HEAD on the artifact routes the way it answers GET', async () => {
		const response = await request('/dl/desktop/stable/win32/x64/1.4.2/setup', {method: 'HEAD'});
		expect(response.status).toBe(302);
		expect(response.location).toBe(`${PKGS_BASE_URL}/desktop/stable/win32/x64/1.4.2/setup`);
	});

	it('rejects a format outside the closed registry before it reaches the redirector', async () => {
		const response = await request('/dl/desktop/stable/linux/x64/latest/msix');
		expect(response.status).toBe(400);
	});
});

describe('release feed routes', () => {
	it.each([
		'/dl/desktop/stable/darwin/arm64/RELEASES.json',
		'/dl/desktop/stable/win32/x64/RELEASES',
		'/dl/desktop/canary/win32/arm64/releases.canary.json',
		'/dl/desktop/stable/win32/x64/releases.win.json',
		'/dl/desktop/stable/linux/x64/manifest.json',
	])('redirects %s without caching the redirect', async (path) => {
		const response = await request(path);
		expect(response.status).toBe(302);
		expect(response.location).toBe(`${PKGS_BASE_URL}${path.slice('/dl'.length)}`);
		expect(response.cacheControl).toBe('no-store');
	});

	it('redirects a nupkg named by a RELEASES body', async () => {
		const response = await request('/dl/desktop/stable/win32/x64/fluxer_app-0.0.8-full.nupkg');
		expect(response.status).toBe(302);
		expect(response.location).toBe(`${PKGS_BASE_URL}/desktop/stable/win32/x64/fluxer_app-0.0.8-full.nupkg`);
		expect(response.cacheControl).toBe('public, max-age=31536000');
	});
});

describe('channel, platform and architecture targeting', () => {
	it.each([
		['stable', 'darwin', 'arm64'],
		['stable', 'darwin', 'x64'],
		['stable', 'win32', 'x64'],
		['stable', 'win32', 'arm64'],
		['stable', 'linux', 'x64'],
		['stable', 'linux', 'arm64'],
		['canary', 'darwin', 'arm64'],
		['canary', 'win32', 'x64'],
		['canary', 'linux', 'arm64'],
	])('keeps %s/%s/%s in the redirect target', async (channel, plat, arch) => {
		const response = await request(`/dl/desktop/${channel}/${plat}/${arch}/latest`);
		expect(response.status).toBe(302);
		expect(response.location).toBe(`${PKGS_BASE_URL}/desktop/${channel}/${plat}/${arch}/latest.json`);
	});
});

describe('the geoip and github release route is gone', () => {
	it('sends every country to the package origin with the same cache control', async () => {
		const path = '/dl/desktop/stable/darwin/arm64/1.4.2/dmg';
		const withCountry = await request(path, {headers: COUNTRY_HEADERS});
		const withoutCountry = await request(path);
		expect(withCountry).toEqual(withoutCountry);
		expect(withCountry.location).toBe(`${PKGS_BASE_URL}/desktop/stable/darwin/arm64/1.4.2/dmg`);
		expect(withCountry.cacheControl).not.toBe('private, no-store');
	});

	it('never points a download at github', async () => {
		const response = await request('/dl/desktop/stable/darwin/arm64/1.4.2/zip', {headers: COUNTRY_HEADERS});
		expect(response.location).not.toContain('github.com');
		expect(response.location?.startsWith(`${PKGS_BASE_URL}/`)).toBe(true);
	});
});

describe('paths the redirector refuses', () => {
	it('answers 404 for a key outside the desktop prefix', async () => {
		const response = await request('/dl/harvests/dump.zip');
		expect(response.status).toBe(404);
		expect(response.body).toBe('Not Found');
	});

	it('answers 404 for a traversal attempt', async () => {
		const response = await request('/dl/desktop/../harvests/dump.zip');
		expect(response.status).toBe(404);
	});

	it('answers 404 for the retired test build prefix', async () => {
		const response = await request('/dl/desktop-test/canary/linux/x64/latest/appimage');
		expect(response.status).toBe(404);
	});

	it('ignores a test query parameter rather than resolving another prefix', async () => {
		const response = await request('/dl/desktop/canary/linux/x64/latest/appimage?test=1');
		expect(response.status).toBe(302);
		expect(response.location).toBe(`${PKGS_BASE_URL}/desktop/canary/linux/x64/latest/appimage`);
	});
});
