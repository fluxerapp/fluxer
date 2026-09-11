// SPDX-License-Identifier: AGPL-3.0-or-later

import {Readable} from 'node:stream';
import {describe, expect, it} from 'vitest';
import type {IStorageService} from '../../infrastructure/IStorageService';
import {DownloadService} from '../DownloadService';

const PREFIX = 'desktop/canary/linux/x64';
const BASE_URL = 'https://api.example.test';
const V1 = '2026.901.100000';
const V2 = '2026.902.100000';
const V3 = '2026.903.100000';
const V4 = '2026.904.100000';
const V5 = '2026.905.100000';

const LIST_PARAMS = {channel: 'canary', plat: 'linux', arch: 'x64', baseUrl: BASE_URL} as const;

type StoredObject = {body?: string; lastModified?: Date};
type StoredObjects = Map<string, StoredObject>;

function appImageFilename(version: string): string {
	return `Fluxer-Canary-${version}-linux-x86_64.AppImage`;
}

function debFilename(version: string): string {
	return `Fluxer-Canary-${version}-linux-amd64.deb`;
}

function addArtifact(objects: StoredObjects, filename: string, options: {sha256?: string; lastModified?: Date} = {}) {
	objects.set(`${PREFIX}/${filename}`, {lastModified: options.lastModified});
	if (options.sha256 !== undefined) {
		objects.set(`${PREFIX}/${filename}.sha256`, {body: `${options.sha256}  ${filename}\n`});
	}
}

function createService(objects: StoredObjects) {
	const reads: Array<string> = [];
	const storageService = {
		streamObject: async (params: {key: string}) => {
			reads.push(params.key);
			const object = objects.get(params.key);
			if (object?.body == null) {
				return null;
			}
			const buffer = Buffer.from(object.body, 'utf8');
			return {body: Readable.from([buffer]), contentLength: buffer.byteLength};
		},
		listObjects: async (params: {prefix: string}) =>
			Array.from(objects.entries())
				.filter(([key]) => key.startsWith(params.prefix))
				.sort(([left], [right]) => (left < right ? -1 : 1))
				.map(([key, object]) => ({key, lastModified: object.lastModified})),
		getObjectMetadata: async () => null,
	} as unknown as IStorageService;
	return {service: new DownloadService(storageService), reads};
}

function versionNumbers(versions: Array<{version: string}>): Array<string> {
	return versions.map((entry) => entry.version);
}

describe('desktop version listing', () => {
	it('lists versions newest first with the files of each version', async () => {
		const objects: StoredObjects = new Map();
		addArtifact(objects, appImageFilename(V1), {lastModified: new Date('2026-09-01T10:00:00Z')});
		addArtifact(objects, appImageFilename(V3), {lastModified: new Date('2026-09-03T10:00:00Z')});
		addArtifact(objects, debFilename(V3), {lastModified: new Date('2026-09-03T12:00:00Z')});
		addArtifact(objects, appImageFilename(V5), {lastModified: new Date('2026-09-05T10:00:00Z')});
		const {service} = createService(objects);
		const listed = await service.listDesktopVersions({...LIST_PARAMS, limit: 10});
		expect(versionNumbers(listed.versions)).toEqual([V5, V3, V1]);
		expect(listed.hasMore).toBe(false);
		expect(Object.keys(listed.versions[1].files).sort()).toEqual(['appimage', 'deb']);
		expect(listed.versions[1].pub_date).toBe('2026-09-03T12:00:00.000Z');
		expect(listed.versions[0].files.appimage.url).toBe(`${BASE_URL}/dl/desktop/canary/linux/x64/${V5}/appimage`);
	});

	it('excludes names that are not artefacts for the requested coordinate', async () => {
		const objects: StoredObjects = new Map();
		addArtifact(objects, appImageFilename(V3), {sha256: 'a'.repeat(64)});
		objects.set(`${PREFIX}/nested/${appImageFilename(V5)}`, {});
		objects.set(`${PREFIX}/manifest.json`, {body: '{}'});
		objects.set(`${PREFIX}/RELEASES.json`, {body: '{}'});
		objects.set(`${PREFIX}/releases.json`, {body: '{}'});
		objects.set(`${PREFIX}/latest-linux.yml`, {body: 'version: 1'});
		objects.set(`${PREFIX}/${appImageFilename(V4)}.blockmap`, {});
		objects.set(`${PREFIX}/Fluxer-Canary-${V4}-linux-aarch64.AppImage`, {});
		objects.set(`${PREFIX}/Fluxer-Canary-${V4}-mac-universal.dmg`, {});
		const {service} = createService(objects);
		const listed = await service.listDesktopVersions({...LIST_PARAMS, limit: 10});
		expect(versionNumbers(listed.versions)).toEqual([V3]);
		expect(Object.keys(listed.versions[0].files)).toEqual(['appimage']);
	});

	it('pages with limit, before and after and reports whether more remain', async () => {
		const objects: StoredObjects = new Map();
		for (const version of [V1, V2, V3, V4, V5]) {
			addArtifact(objects, appImageFilename(version));
		}
		const {service} = createService(objects);
		const firstPage = await service.listDesktopVersions({...LIST_PARAMS, limit: 2});
		expect(versionNumbers(firstPage.versions)).toEqual([V5, V4]);
		expect(firstPage.hasMore).toBe(true);
		const olderPage = await service.listDesktopVersions({...LIST_PARAMS, limit: 2, before: V3});
		expect(versionNumbers(olderPage.versions)).toEqual([V2, V1]);
		expect(olderPage.hasMore).toBe(false);
		const newerPage = await service.listDesktopVersions({...LIST_PARAMS, limit: 2, after: V3});
		expect(versionNumbers(newerPage.versions)).toEqual([V5, V4]);
		expect(newerPage.hasMore).toBe(false);
		const between = await service.listDesktopVersions({...LIST_PARAMS, limit: 1, before: V5, after: V1});
		expect(versionNumbers(between.versions)).toEqual([V4]);
		expect(between.hasMore).toBe(true);
	});

	it('reports the sibling hash and treats a missing or malformed one as absent', async () => {
		const hash = 'b'.repeat(64);
		const objects: StoredObjects = new Map();
		addArtifact(objects, appImageFilename(V3), {sha256: hash});
		addArtifact(objects, appImageFilename(V2));
		addArtifact(objects, appImageFilename(V1), {sha256: 'C'.repeat(64)});
		const {service} = createService(objects);
		const listed = await service.listDesktopVersions({...LIST_PARAMS, limit: 10});
		expect(listed.versions[0].files.appimage).toEqual({
			url: `${BASE_URL}/dl/desktop/canary/linux/x64/${V3}/appimage`,
			sha256: hash,
			checksum_url: `${BASE_URL}/dl/desktop/canary/linux/x64/${V3}/appimage.sha256`,
		});
		expect(listed.versions[1].files.appimage.sha256).toBeNull();
		expect(listed.versions[1].files.appimage.checksum_url).toBeNull();
		expect(listed.versions[2].files.appimage.sha256).toBeNull();
		expect(listed.versions[2].files.appimage.checksum_url).toBeNull();
	});

	it('reads a checksum only for the versions it returns', async () => {
		const objects: StoredObjects = new Map();
		for (const version of [V1, V2, V3, V4, V5]) {
			addArtifact(objects, appImageFilename(version), {sha256: 'd'.repeat(64)});
		}
		const {service, reads} = createService(objects);
		await service.listDesktopVersions({...LIST_PARAMS, limit: 2});
		expect(reads).toEqual([`${PREFIX}/${appImageFilename(V5)}.sha256`, `${PREFIX}/${appImageFilename(V4)}.sha256`]);
	});
});
