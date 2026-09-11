// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import {Readable} from 'node:stream';
import {S3ServiceException} from '@aws-sdk/client-s3';
import {describe, expect, it} from 'vitest';
import {getConfig} from '../../Config';
import type {IStorageService} from '../../infrastructure/IStorageService';
import {DownloadService} from '../DownloadService';

const PREFIX = 'desktop/canary/linux/x64';
const TEST_PREFIX = 'desktop-test/canary/linux/x64';
const RELEASES_PREFIX = 'desktop/canary/github-releases';
const SOURCE_SHA = 'b'.repeat(40);
const V904 = '2026.904.135113';
const V908 = '2026.908.173325';
const V909 = '2026.909.202036';

const LATEST_PARAMS = {channel: 'canary', plat: 'linux', arch: 'x64'} as const;
const APPIMAGE_PARAMS = {...LATEST_PARAMS, format: 'appimage'} as const;

const RELEASE_ROUTES: ReadonlyArray<readonly [string, string, number]> = [
	['darwin', 'arm64', 4],
	['darwin', 'x64', 4],
	['linux', 'arm64', 4],
	['linux', 'x64', 4],
	['win32', 'arm64', 6],
	['win32', 'x64', 6],
];

type StoredObjects = Map<string, string>;

function sha256Hex(value: string): string {
	return createHash('sha256').update(value).digest('hex');
}

function appImageFilename(version: string): string {
	return `Fluxer-Canary-${version}-linux-x86_64.AppImage`;
}

function uploadBuild(objects: StoredObjects, version: string, options: {prefix?: string; checksum?: boolean} = {}) {
	const prefix = options.prefix ?? PREFIX;
	const filename = appImageFilename(version);
	objects.set(`${prefix}/${filename}`, filename);
	if (options.checksum !== false) {
		objects.set(`${prefix}/${filename}.sha256`, `${sha256Hex(filename)}  ${filename}`);
	}
	objects.set(
		`${prefix}/manifest.json`,
		JSON.stringify({
			channel: 'canary',
			platform: 'linux',
			arch: 'x64',
			version,
			pub_date: '2026-09-08T18:06:00Z',
			files: {appimage: {filename, sha256: sha256Hex(filename)}},
		}),
	);
}

function publishDescriptor(objects: StoredObjects, version: string, routes = RELEASE_ROUTES): string {
	const assets = routes.flatMap(([plat, arch, count]) =>
		Array.from({length: count}, (_, index) => {
			const filename =
				plat === 'linux' && arch === 'x64' && index === 0
					? appImageFilename(version)
					: `Fluxer-Canary-${version}-${plat}-${arch}-${index}.bin`;
			return {
				storage_key: `desktop/canary/${plat}/${arch}/${filename}`,
				release_asset: filename,
				sha256: sha256Hex(filename),
				size: 1,
			};
		}),
	);
	const descriptor = JSON.stringify({
		schema_version: 1,
		channel: 'canary',
		version,
		release_tag: `fluxer-desktop-canary@${version}`,
		source_sha: SOURCE_SHA,
		assets,
	});
	objects.set(`${RELEASES_PREFIX}/${version}.json`, descriptor);
	return descriptor;
}

function publishMarker(objects: StoredObjects, version: string, descriptor: string) {
	objects.set(
		`${RELEASES_PREFIX}/${version}.ready.json`,
		JSON.stringify({
			schema_version: 1,
			channel: 'canary',
			version,
			release_tag: `fluxer-desktop-canary@${version}`,
			source_sha: SOURCE_SHA,
			descriptor_sha256: sha256Hex(descriptor),
		}),
	);
}

function releaseBuild(objects: StoredObjects, version: string) {
	const descriptor = publishDescriptor(objects, version);
	uploadBuild(objects, version);
	publishMarker(objects, version, descriptor);
}

function incidentObjects(): StoredObjects {
	const objects: StoredObjects = new Map();
	releaseBuild(objects, V904);
	publishDescriptor(objects, V908);
	uploadBuild(objects, V908);
	return objects;
}

function createService(objects: StoredObjects, onRead?: (key: string) => void) {
	const reads: Array<string> = [];
	const listings: Array<string> = [];
	const storageService = {
		streamObject: async (params: {key: string}) => {
			reads.push(params.key);
			onRead?.(params.key);
			const body = objects.get(params.key);
			if (body == null) {
				return null;
			}
			const buffer = Buffer.from(body, 'utf8');
			return {body: Readable.from([buffer]), contentLength: buffer.byteLength};
		},
		listObjects: async (params: {prefix: string}) => {
			listings.push(params.prefix);
			return Array.from(objects.keys())
				.filter((key) => key.startsWith(params.prefix))
				.sort()
				.map((key) => ({key}));
		},
		getObjectMetadata: async (_bucket: string, key: string) =>
			objects.has(key) ? {contentLength: 1, contentType: 'application/octet-stream'} : null,
	} as unknown as IStorageService;
	return {service: new DownloadService(storageService), reads, listings};
}

async function resolveLatest(service: DownloadService, test?: boolean) {
	const metadata = await service.getLatestDesktopVersion({...LATEST_PARAMS, test});
	const key = await service.resolveLatestDesktopKey({...APPIMAGE_PARAMS, test});
	const checksum = await service.resolveLatestDesktopChecksumFile({...APPIMAGE_PARAMS, test});
	return {version: metadata?.version, key, checksum: checksum?.body};
}

function latestOf(version: string, prefix = PREFIX) {
	const filename = appImageFilename(version);
	return {version, key: `${prefix}/${filename}`, checksum: `${sha256Hex(filename)}  ${filename}\n`};
}

describe('desktop release readiness', () => {
	it('offers a published manifest version after reading only its release state', async () => {
		const objects: StoredObjects = new Map();
		releaseBuild(objects, V904);
		releaseBuild(objects, V909);
		const {service, reads, listings} = createService(objects);
		await expect(service.getLatestDesktopVersion({...LATEST_PARAMS})).resolves.toMatchObject({version: V909});
		expect(reads).toEqual([
			`${PREFIX}/manifest.json`,
			`${RELEASES_PREFIX}/${V909}.json`,
			`${RELEASES_PREFIX}/${V909}.ready.json`,
		]);
		expect(listings).toEqual([]);
		await expect(resolveLatest(service)).resolves.toEqual(latestOf(V909));
	});

	it('falls back to the newest published version while the manifest version awaits its release', async () => {
		const {service} = createService(incidentObjects());
		await expect(resolveLatest(service)).resolves.toEqual(latestOf(V904));
	});

	it('reads each release state once and one checksum while the manifest version awaits its release', async () => {
		const {service, reads, listings} = createService(incidentObjects());
		await expect(service.getLatestDesktopVersion({...LATEST_PARAMS})).resolves.toMatchObject({version: V904});
		expect(reads).toEqual([
			`${PREFIX}/manifest.json`,
			`${RELEASES_PREFIX}/${V908}.json`,
			`${RELEASES_PREFIX}/${V908}.ready.json`,
			`${RELEASES_PREFIX}/${V904}.json`,
			`${RELEASES_PREFIX}/${V904}.ready.json`,
			`${PREFIX}/${appImageFilename(V904)}.sha256`,
		]);
		expect(listings).toEqual([`${PREFIX}/`]);
	});

	it('offers a manifest version that has no release descriptor', async () => {
		const objects: StoredObjects = new Map();
		uploadBuild(objects, V904);
		uploadBuild(objects, V908);
		const {service} = createService(objects);
		await expect(resolveLatest(service)).resolves.toEqual(latestOf(V908));
	});

	it('treats a readiness marker that does not match the stored descriptor as unpublished', async () => {
		const objects: StoredObjects = new Map();
		releaseBuild(objects, V904);
		publishDescriptor(objects, V908);
		uploadBuild(objects, V908);
		publishMarker(objects, V908, 'another descriptor');
		const {service} = createService(objects);
		await expect(service.resolveGitHubDesktopRelease(`${PREFIX}/${appImageFilename(V908)}`)).resolves.toEqual({
			kind: 'awaiting_release',
		});
		await expect(resolveLatest(service)).resolves.toEqual(latestOf(V904));
	});

	it('offers a version whose descriptor the parser rejects when its readiness marker matches', async () => {
		const objects: StoredObjects = new Map();
		releaseBuild(objects, V904);
		const descriptor = publishDescriptor(
			objects,
			V908,
			RELEASE_ROUTES.map(([plat, arch, count]) => [plat, arch, plat === 'linux' ? count - 1 : count] as const),
		);
		uploadBuild(objects, V908);
		publishMarker(objects, V908, descriptor);
		const {service} = createService(objects);
		await expect(resolveLatest(service)).resolves.toEqual(latestOf(V908));
		await expect(service.resolveGitHubDesktopRelease(`${PREFIX}/${appImageFilename(V908)}`)).rejects.toThrow(
			'Invalid GitHub desktop release descriptor',
		);
	});

	it.each([
		['descriptor', `${RELEASES_PREFIX}/${V908}.json`],
		['readiness marker', `${RELEASES_PREFIX}/${V908}.ready.json`],
	])('offers the manifest version when reading its release %s fails with a storage error', async (_name, failingKey) => {
		const {service} = createService(incidentObjects(), (key) => {
			if (key === failingKey) {
				throw new S3ServiceException({
					name: 'SlowDown',
					$fault: 'server',
					$metadata: {httpStatusCode: 503},
					message: 'Please reduce your request rate.',
				});
			}
		});
		await expect(resolveLatest(service)).resolves.toEqual(latestOf(V908));
	});

	it('still resolves the unpublished version through versioned routes', async () => {
		const objects: StoredObjects = new Map();
		releaseBuild(objects, V904);
		publishDescriptor(objects, V908);
		uploadBuild(objects, V908, {checksum: false});
		const {service} = createService(objects);
		const params = {...APPIMAGE_PARAMS, version: V908};
		const filename = appImageFilename(V908);
		await expect(service.resolveVersionedDesktopKey(params)).resolves.toBe(`${PREFIX}/${filename}`);
		await expect(service.resolveVersionedDesktopChecksumFile(params)).resolves.toMatchObject({
			sha256: sha256Hex(filename),
		});
	});

	it('keeps offering the manifest version on self-hosted instances', async () => {
		const config = getConfig();
		const originalSelfHosted = config.instance.selfHosted;
		config.instance.selfHosted = true;
		try {
			const {service, reads} = createService(incidentObjects());
			await expect(resolveLatest(service)).resolves.toEqual(latestOf(V908));
			expect(reads.filter((key) => key.startsWith(RELEASES_PREFIX))).toEqual([]);
		} finally {
			config.instance.selfHosted = originalSelfHosted;
		}
	});

	it('keeps offering the newest test build', async () => {
		const objects: StoredObjects = new Map();
		publishDescriptor(objects, V908);
		uploadBuild(objects, V908, {prefix: TEST_PREFIX});
		const {service, reads} = createService(objects);
		await expect(resolveLatest(service, true)).resolves.toEqual(latestOf(V908, TEST_PREFIX));
		expect(reads.filter((key) => key.startsWith(RELEASES_PREFIX))).toEqual([]);
	});

	it('offers 904 while 908 awaits its release, then 909 once its marker lands', async () => {
		const objects = incidentObjects();
		const {service} = createService(objects);
		await expect(resolveLatest(service)).resolves.toEqual(latestOf(V904));
		await expect(service.resolveGitHubDesktopRelease(`${PREFIX}/${appImageFilename(V908)}`)).resolves.toEqual({
			kind: 'awaiting_release',
		});
		const descriptor = publishDescriptor(objects, V909);
		uploadBuild(objects, V909);
		await expect(resolveLatest(service)).resolves.toEqual(latestOf(V904));
		publishMarker(objects, V909, descriptor);
		await expect(resolveLatest(service)).resolves.toEqual(latestOf(V909));
		await expect(service.resolveGitHubDesktopRelease(`${PREFIX}/${appImageFilename(V909)}`)).resolves.toEqual({
			kind: 'ready',
			location: `https://github.com/fluxerapp/fluxer/releases/download/${encodeURIComponent(`fluxer-desktop-canary@${V909}`)}/${appImageFilename(V909)}`,
		});
	});

	it('offers the newest version when ten unpublished versions hide a published one', async () => {
		const objects: StoredObjects = new Map();
		releaseBuild(objects, V904);
		const newest = '2026.908.170009';
		for (let build = 0; build < 10; build++) {
			const version = `2026.908.${170000 + build}`;
			publishDescriptor(objects, version);
			uploadBuild(objects, version);
		}
		const {service, reads} = createService(objects);
		await expect(resolveLatest(service)).resolves.toEqual(latestOf(newest));
		expect(reads).not.toContain(`${RELEASES_PREFIX}/${V904}.ready.json`);
	});

	it('pairs the latest checksum with the filename of the same version when a marker lands mid-request', async () => {
		const objects = incidentObjects();
		const descriptor = publishDescriptor(objects, V909);
		uploadBuild(objects, V909);
		let manifestReads = 0;
		const {service} = createService(objects, (key) => {
			if (key !== `${PREFIX}/manifest.json`) {
				return;
			}
			manifestReads += 1;
			if (manifestReads === 2) {
				publishMarker(objects, V909, descriptor);
			}
		});
		const checksum = await service.resolveLatestDesktopChecksumFile({...APPIMAGE_PARAMS});
		expect(checksum?.body).toBe(latestOf(V904).checksum);
	});

	it('lists an unpublished version while latest skips it', async () => {
		const {service} = createService(incidentObjects());
		const listed = await service.listDesktopVersions({...LATEST_PARAMS, limit: 10});
		expect(listed.versions.map((entry) => entry.version)).toEqual([V908, V904]);
		await expect(resolveLatest(service)).resolves.toEqual(latestOf(V904));
	});
});
