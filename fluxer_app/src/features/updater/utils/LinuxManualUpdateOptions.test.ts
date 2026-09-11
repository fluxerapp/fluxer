// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UpdaterDownloadOption} from '@app/features/platform/types/Electron';
import {describe, expect, it, vi} from 'vitest';
import {buildLinuxManualUpdateOptions} from './LinuxManualUpdateOptions';

vi.mock('@app/features/app/config/Config', () => ({
	default: {
		PUBLIC_BUILD_VERSION: 'test',
		PUBLIC_RELEASE_CHANNEL: 'canary',
		PUBLIC_BOOTSTRAP_API_ENDPOINT: 'https://example.invalid',
		PUBLIC_BOOTSTRAP_API_PUBLIC_ENDPOINT: 'https://example.invalid',
	},
}));

const REPORTED_VERSION = '2026.908.173325';
const NEWER_VERSION = '2026.909.202036';
const CANARY_X64 = 'https://api.canary.fluxer.app/dl/desktop/canary/linux/x64';
const STABLE_X64 = 'https://api.fluxer.app/dl/desktop/stable/linux/x64';

function desktopMainOptions(params: {
	linkBase: string;
	productName: string;
	version: string;
	linkVersion: string;
}): Array<UpdaterDownloadOption> {
	const {linkBase, productName, version, linkVersion} = params;
	return [
		{
			format: 'appimage',
			label: 'AppImage',
			url: `${linkBase}/${linkVersion}/appimage`,
			suggestedName: `${productName}-${version}-linux-x86_64.AppImage`,
			sha256: `appimage-${version}`,
		},
		{
			format: 'deb',
			label: 'DEB package',
			url: `${linkBase}/${linkVersion}/deb`,
			suggestedName: `${productName}-${version}-linux-amd64.deb`,
			sha256: `deb-${version}`,
		},
		{
			format: 'rpm',
			label: 'RPM package',
			url: `${linkBase}/${linkVersion}/rpm`,
			suggestedName: `${productName}-${version}-linux-x86_64.rpm`,
			sha256: `rpm-${version}`,
		},
		{
			format: 'tar_gz',
			label: 'tar.gz archive',
			url: `${linkBase}/${linkVersion}/tar_gz`,
			suggestedName: `${productName}-${version}-linux-x64.tar.gz`,
			sha256: `tar_gz-${version}`,
		},
	];
}

describe('buildLinuxManualUpdateOptions', () => {
	it('pins the link, file name and checksum of every format to the same version', () => {
		const options = buildLinuxManualUpdateOptions({channel: 'stable', arch: 'x64', version: REPORTED_VERSION});
		expect(options).toEqual([
			{
				format: 'appimage',
				label: 'AppImage',
				url: `${STABLE_X64}/${REPORTED_VERSION}/appimage`,
				suggestedName: `Fluxer-${REPORTED_VERSION}-linux-x86_64.AppImage`,
				sha256: null,
			},
			{
				format: 'deb',
				label: 'DEB package',
				url: `${STABLE_X64}/${REPORTED_VERSION}/deb`,
				suggestedName: `Fluxer-${REPORTED_VERSION}-linux-amd64.deb`,
				sha256: null,
			},
			{
				format: 'rpm',
				label: 'RPM package',
				url: `${STABLE_X64}/${REPORTED_VERSION}/rpm`,
				suggestedName: `Fluxer-${REPORTED_VERSION}-linux-x86_64.rpm`,
				sha256: null,
			},
			{
				format: 'tar_gz',
				label: 'tar.gz archive',
				url: `${STABLE_X64}/${REPORTED_VERSION}/tar_gz`,
				suggestedName: `Fluxer-${REPORTED_VERSION}-linux-x64.tar.gz`,
				sha256: null,
			},
		]);
	});

	it('pins the latest links sent by current desktop builds to the announced version', () => {
		const options = buildLinuxManualUpdateOptions({
			downloadUrl: `${CANARY_X64}/latest/appimage`,
			channel: 'canary',
			arch: 'x64',
			version: REPORTED_VERSION,
			knownOptions: desktopMainOptions({
				linkBase: CANARY_X64,
				productName: 'Fluxer-Canary',
				version: REPORTED_VERSION,
				linkVersion: 'latest',
			}),
		});
		expect(options).toEqual(
			desktopMainOptions({
				linkBase: CANARY_X64,
				productName: 'Fluxer-Canary',
				version: REPORTED_VERSION,
				linkVersion: REPORTED_VERSION,
			}),
		);
	});

	it('keeps the pinned links sent by newer desktop builds', () => {
		const pinnedOptions = desktopMainOptions({
			linkBase: CANARY_X64,
			productName: 'Fluxer-Canary',
			version: REPORTED_VERSION,
			linkVersion: REPORTED_VERSION,
		});
		const options = buildLinuxManualUpdateOptions({
			downloadUrl: pinnedOptions[0].url,
			channel: 'canary',
			arch: 'x64',
			version: REPORTED_VERSION,
			knownOptions: pinnedOptions,
		});
		expect(options).toEqual(pinnedOptions);
	});

	it('only uses latest links and latest file names when no version is known', () => {
		const options = buildLinuxManualUpdateOptions({
			downloadUrl: `${STABLE_X64}/latest/appimage`,
			channel: 'stable',
			arch: 'x64',
			version: null,
			knownOptions: desktopMainOptions({
				linkBase: STABLE_X64,
				productName: 'Fluxer',
				version: REPORTED_VERSION,
				linkVersion: 'latest',
			}),
		});
		expect(options.map(({url, suggestedName, sha256}) => ({url, suggestedName, sha256}))).toEqual([
			{url: `${STABLE_X64}/latest/appimage`, suggestedName: 'Fluxer-latest-linux-x86_64.AppImage', sha256: null},
			{url: `${STABLE_X64}/latest/deb`, suggestedName: 'Fluxer-latest-linux-amd64.deb', sha256: null},
			{url: `${STABLE_X64}/latest/rpm`, suggestedName: 'Fluxer-latest-linux-x86_64.rpm', sha256: null},
			{url: `${STABLE_X64}/latest/tar_gz`, suggestedName: 'Fluxer-latest-linux-x64.tar.gz', sha256: null},
		]);
	});

	it('uses arm64 links and file name tokens on arm64 systems', () => {
		const expected = [
			{
				url: `https://api.fluxer.app/dl/desktop/stable/linux/arm64/${REPORTED_VERSION}/appimage`,
				suggestedName: `Fluxer-${REPORTED_VERSION}-linux-arm64.AppImage`,
			},
			{
				url: `https://api.fluxer.app/dl/desktop/stable/linux/arm64/${REPORTED_VERSION}/deb`,
				suggestedName: `Fluxer-${REPORTED_VERSION}-linux-arm64.deb`,
			},
			{
				url: `https://api.fluxer.app/dl/desktop/stable/linux/arm64/${REPORTED_VERSION}/rpm`,
				suggestedName: `Fluxer-${REPORTED_VERSION}-linux-aarch64.rpm`,
			},
			{
				url: `https://api.fluxer.app/dl/desktop/stable/linux/arm64/${REPORTED_VERSION}/tar_gz`,
				suggestedName: `Fluxer-${REPORTED_VERSION}-linux-arm64.tar.gz`,
			},
		];
		const fromDesktopInfo = buildLinuxManualUpdateOptions({
			channel: 'stable',
			arch: 'aarch64',
			version: REPORTED_VERSION,
		});
		const fromLatestLink = buildLinuxManualUpdateOptions({
			downloadUrl: 'https://api.fluxer.app/dl/desktop/stable/linux/arm64/latest/appimage',
			version: REPORTED_VERSION,
		});
		const fromPinnedLink = buildLinuxManualUpdateOptions({
			downloadUrl: `https://api.fluxer.app/dl/desktop/stable/linux/arm64/${REPORTED_VERSION}/deb`,
			version: REPORTED_VERSION,
		});
		for (const options of [fromDesktopInfo, fromLatestLink, fromPinnedLink]) {
			expect(options.map(({url, suggestedName}) => ({url, suggestedName}))).toEqual(expected);
		}
	});

	it('preserves the query string of latest and pinned links', () => {
		const fromLatestLink = buildLinuxManualUpdateOptions({
			downloadUrl: `${CANARY_X64}/latest/appimage?test=1`,
			version: REPORTED_VERSION,
		});
		const fromPinnedLink = buildLinuxManualUpdateOptions({
			downloadUrl: `${CANARY_X64}/${REPORTED_VERSION}/appimage?test=1`,
			version: REPORTED_VERSION,
		});
		const expectedUrls = [
			`${CANARY_X64}/${REPORTED_VERSION}/appimage?test=1`,
			`${CANARY_X64}/${REPORTED_VERSION}/deb?test=1`,
			`${CANARY_X64}/${REPORTED_VERSION}/rpm?test=1`,
			`${CANARY_X64}/${REPORTED_VERSION}/tar_gz?test=1`,
		];
		expect(fromLatestLink.map((option) => option.url)).toEqual(expectedUrls);
		expect(fromPinnedLink.map((option) => option.url)).toEqual(expectedUrls);
	});

	it('builds links on the stable and canary endpoints', () => {
		const stable = buildLinuxManualUpdateOptions({channel: 'stable', arch: 'x64', version: REPORTED_VERSION});
		const canary = buildLinuxManualUpdateOptions({channel: 'canary', arch: 'x64', version: REPORTED_VERSION});
		const configuredCanary = buildLinuxManualUpdateOptions({
			channel: 'canary',
			arch: 'x64',
			version: REPORTED_VERSION,
			apiEndpoint: 'https://web.canary.fluxer.app/api/',
		});
		const proxiedCanaryLink = buildLinuxManualUpdateOptions({
			downloadUrl: 'https://web.canary.fluxer.app/api/dl/desktop/canary/linux/x64/latest/deb',
			version: REPORTED_VERSION,
			apiEndpoint: 'https://api.fluxer.app',
		});
		expect(stable[1].url).toBe(`${STABLE_X64}/${REPORTED_VERSION}/deb`);
		expect(canary[1].url).toBe(`${CANARY_X64}/${REPORTED_VERSION}/deb`);
		expect(configuredCanary[1].url).toBe(
			`https://web.canary.fluxer.app/api/dl/desktop/canary/linux/x64/${REPORTED_VERSION}/deb`,
		);
		expect(proxiedCanaryLink[1].url).toBe(
			`https://web.canary.fluxer.app/api/dl/desktop/canary/linux/x64/${REPORTED_VERSION}/deb`,
		);
	});

	it('never points a prompt for 2026.908.173325 at a link that could serve another build', () => {
		const options = buildLinuxManualUpdateOptions({
			downloadUrl: `${CANARY_X64}/latest/appimage`,
			channel: 'canary',
			arch: 'x64',
			version: REPORTED_VERSION,
			apiEndpoint: 'https://api.canary.fluxer.app',
			knownOptions: desktopMainOptions({
				linkBase: CANARY_X64,
				productName: 'Fluxer-Canary',
				version: REPORTED_VERSION,
				linkVersion: 'latest',
			}),
		});
		expect(options.find((option) => option.format === 'deb')).toMatchObject({
			url: `${CANARY_X64}/${REPORTED_VERSION}/deb`,
			suggestedName: `Fluxer-Canary-${REPORTED_VERSION}-linux-amd64.deb`,
			sha256: `deb-${REPORTED_VERSION}`,
		});
		for (const option of options) {
			expect(option.url).toContain(`/${REPORTED_VERSION}/`);
			expect(option.url).not.toContain('/latest/');
			expect(option.url).not.toContain(NEWER_VERSION);
			expect(option.suggestedName).toContain(REPORTED_VERSION);
		}
	});

	it.each([
		'v2026.908.173325',
		'2026.908',
		'2026.908.173325-beta',
	])('treats the off-shape version %s as unknown', (version) => {
		const options = buildLinuxManualUpdateOptions({
			downloadUrl: `${STABLE_X64}/latest/appimage`,
			version,
			knownOptions: desktopMainOptions({
				linkBase: STABLE_X64,
				productName: 'Fluxer',
				version,
				linkVersion: 'latest',
			}),
		});
		expect(options[1]).toEqual({
			format: 'deb',
			label: 'DEB package',
			url: `${STABLE_X64}/latest/deb`,
			suggestedName: 'Fluxer-latest-linux-amd64.deb',
			sha256: null,
		});
	});

	it('ignores a download link that is not for Linux', () => {
		const options = buildLinuxManualUpdateOptions({
			downloadUrl: 'https://api.fluxer.app/dl/desktop/stable/darwin/arm64/latest/dmg?test=1',
			channel: 'canary',
			arch: 'x64',
			version: REPORTED_VERSION,
		});
		expect(options.map((option) => option.url)).toEqual([
			`${CANARY_X64}/${REPORTED_VERSION}/appimage`,
			`${CANARY_X64}/${REPORTED_VERSION}/deb`,
			`${CANARY_X64}/${REPORTED_VERSION}/rpm`,
			`${CANARY_X64}/${REPORTED_VERSION}/tar_gz`,
		]);
	});

	it('reads the architecture of a known package from its pinned link before its file name', () => {
		const options = buildLinuxManualUpdateOptions({
			channel: 'stable',
			arch: 'x64',
			version: REPORTED_VERSION,
			knownOptions: [
				{
					format: 'deb',
					label: 'DEB package',
					url: `https://api.fluxer.app/dl/desktop/stable/linux/arm64/${REPORTED_VERSION}/deb`,
					suggestedName: `Fluxer-${REPORTED_VERSION}-linux-amd64.deb`,
					sha256: 'arm64-deb',
				},
			],
		});
		expect(options[1]).toEqual({
			format: 'deb',
			label: 'DEB package',
			url: `${STABLE_X64}/${REPORTED_VERSION}/deb`,
			suggestedName: `Fluxer-${REPORTED_VERSION}-linux-amd64.deb`,
			sha256: null,
		});
	});
});
