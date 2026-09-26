// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	DESKTOP_COORDINATE_DOCUMENTS,
	downloadRedirectCacheControl,
	PKGS_BASE_URL,
	resolveDownloadObjectPath,
	resolveDownloadRedirect,
} from '@app/api/download/DownloadRedirects';
import {describe, expect, it} from 'vitest';

const MUTABLE = 'no-store';
const IMMUTABLE = 'public, max-age=31536000';

describe('the coordinate document contract the publisher writes', () => {
	it('names the exact files scripts/packages/stage-desktop.sh and retention.sh publish', () => {
		expect(Object.fromEntries(DESKTOP_COORDINATE_DOCUMENTS)).toEqual({
			latest: 'latest.json',
		});
	});

	it('resolves the bare coordinate names to those files on every coordinate', () => {
		for (const channel of ['stable', 'canary']) {
			for (const plat of ['win32', 'darwin', 'linux']) {
				for (const arch of ['x64', 'arm64']) {
					expect(resolveDownloadObjectPath(`/dl/desktop/${channel}/${plat}/${arch}/latest`)).toBe(
						`desktop/${channel}/${plat}/${arch}/latest.json`,
					);
					expect(resolveDownloadObjectPath(`/dl/desktop/${channel}/${plat}/${arch}/versions`)).toBe(
						`desktop/${channel}/${plat}/${arch}/versions`,
					);
				}
			}
		}
	});

	it('leaves the latest directory alone when a format or a sidecar follows it', () => {
		expect(resolveDownloadObjectPath('/dl/desktop/stable/linux/x64/latest/appimage')).toBe(
			'desktop/stable/linux/x64/latest/appimage',
		);
		expect(resolveDownloadObjectPath('/dl/desktop/stable/linux/x64/latest/appimage.zsync')).toBe(
			'desktop/stable/linux/x64/latest/appimage.zsync',
		);
	});

	it('rewrites the legacy platform-arch form to the same documents', () => {
		expect(resolveDownloadObjectPath('/dl/desktop/stable/linux-x64/latest')).toBe(
			'desktop/stable/linux/x64/latest.json',
		);
		expect(resolveDownloadObjectPath('/dl/desktop/stable/linux-x64/versions')).toBe(
			'desktop/stable/linux/x64/versions',
		);
	});

	it('rewrites nothing else that sits at the coordinate root', () => {
		expect(resolveDownloadObjectPath('/dl/desktop/stable/linux/x64/manifest.json')).toBe(
			'desktop/stable/linux/x64/manifest.json',
		);
		expect(resolveDownloadObjectPath('/dl/desktop/stable/win32/x64/RELEASES')).toBe(
			'desktop/stable/win32/x64/RELEASES',
		);
		expect(resolveDownloadObjectPath('/dl/desktop/stable/linux/x64/constructor')).toBe(
			'desktop/stable/linux/x64/constructor',
		);
	});
});

describe('download object paths', () => {
	it('strips the /dl prefix and keeps the rest of the path verbatim', () => {
		expect(resolveDownloadObjectPath('/dl/desktop/stable/darwin/arm64/RELEASES.json')).toBe(
			'desktop/stable/darwin/arm64/RELEASES.json',
		);
		expect(resolveDownloadObjectPath('/dl/desktop/stable/win32/x64/1.4.2/Fluxer-1.4.2-win-x64.exe')).toBe(
			'desktop/stable/win32/x64/1.4.2/Fluxer-1.4.2-win-x64.exe',
		);
	});

	it('normalises the legacy platform-arch segment to the published layout', () => {
		expect(resolveDownloadObjectPath('/dl/desktop/stable/linux-x64/manifest.json')).toBe(
			'desktop/stable/linux/x64/manifest.json',
		);
	});

	it('refuses a key outside the desktop prefix', () => {
		expect(resolveDownloadObjectPath('/dl/reports/secret.json')).toBeNull();
		expect(resolveDownloadObjectPath('/dl/desktop-test/canary/linux/x64/latest/appimage')).toBeNull();
		expect(resolveDownloadObjectPath('/dl/')).toBeNull();
		expect(resolveDownloadObjectPath('/other/desktop/stable/linux/x64/latest')).toBeNull();
	});

	it('refuses a traversal attempt rather than pointing at another prefix', () => {
		expect(resolveDownloadObjectPath('/dl/desktop/../harvests/dump.zip')).toBeNull();
		expect(resolveDownloadObjectPath('/dl/../desktop/stable/linux/x64/latest')).toBeNull();
		expect(resolveDownloadObjectPath('/dl/desktop/stable/linux/x64/lat\0est')).toBeNull();
	});
});

describe('download redirect cache control', () => {
	it('never caches a redirect to a mutable document', () => {
		expect(downloadRedirectCacheControl('desktop/stable/darwin/arm64/latest.json')).toBe(MUTABLE);
		expect(downloadRedirectCacheControl('desktop/stable/darwin/arm64/version.json')).toBe(MUTABLE);
		expect(downloadRedirectCacheControl('desktop/stable/linux/x64/latest/appimage')).toBe(MUTABLE);
		expect(downloadRedirectCacheControl('desktop/stable/linux/x64/latest/appimage.sha256')).toBe(MUTABLE);
		expect(downloadRedirectCacheControl('desktop/stable/linux/x64/latest/appimage.zsync')).toBe(MUTABLE);
		expect(downloadRedirectCacheControl('desktop/stable/darwin/arm64/RELEASES.json')).toBe(MUTABLE);
		expect(downloadRedirectCacheControl('desktop/stable/win32/x64/RELEASES')).toBe(MUTABLE);
		expect(downloadRedirectCacheControl('desktop/canary/win32/x64/releases.canary.json')).toBe(MUTABLE);
		expect(downloadRedirectCacheControl('desktop/stable/linux/x64/manifest.json')).toBe(MUTABLE);
		expect(downloadRedirectCacheControl('desktop/stable/linux/x64/latest-linux.yml')).toBe(MUTABLE);
	});

	it('caches a redirect to a version pinned artifact for a year', () => {
		expect(downloadRedirectCacheControl('desktop/stable/darwin/arm64/1.4.2/dmg')).toBe(IMMUTABLE);
		expect(downloadRedirectCacheControl('desktop/stable/darwin/arm64/1.4.2/dmg.sha256')).toBe(IMMUTABLE);
		expect(downloadRedirectCacheControl('desktop/stable/linux/x64/1.4.2/appimage.zsync')).toBe(IMMUTABLE);
		expect(downloadRedirectCacheControl('desktop/stable/win32/x64/fluxer_app-0.0.8-full.nupkg')).toBe(IMMUTABLE);
	});
});

describe('download redirects', () => {
	it('points every desktop path at the package origin', () => {
		expect(resolveDownloadRedirect('/dl/desktop/stable/darwin/arm64/1.4.2/dmg')).toEqual({
			location: `${PKGS_BASE_URL}/desktop/stable/darwin/arm64/1.4.2/dmg`,
			cacheControl: IMMUTABLE,
		});
		expect(resolveDownloadRedirect('/dl/desktop/canary/linux/arm64/latest')).toEqual({
			location: `${PKGS_BASE_URL}/desktop/canary/linux/arm64/latest.json`,
			cacheControl: MUTABLE,
		});
	});

	it('returns null for a path it refuses to map', () => {
		expect(resolveDownloadRedirect('/dl/harvests/dump.zip')).toBeNull();
	});
});
