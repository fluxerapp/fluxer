// SPDX-License-Identifier: AGPL-3.0-or-later

import {Readable} from 'node:stream';
import {describe, expect, it} from 'vitest';
import type {IStorageService} from '../../infrastructure/IStorageService';
import {DownloadService} from '../DownloadService';

const PREFIX = 'desktop/stable/darwin/x64';
const MANIFEST_KEY = `${PREFIX}/manifest.json`;
const LISTED_FILENAME = 'Fluxer-1.2.3-mac-universal.dmg';
const MANIFEST_FILENAME = 'Fluxer-1.3.0-mac-universal.dmg';

const LATEST_PARAMS = {
	channel: 'stable',
	plat: 'darwin',
	arch: 'x64',
	format: 'dmg',
} as const;

function createService(overrides: {manifestBody?: string | null; objectKeys?: Array<string>} = {}) {
	const objectKeys = overrides.objectKeys ?? [`${PREFIX}/${LISTED_FILENAME}`];
	const storageService = {
		streamObject: async (params: {key: string}) => {
			if (params.key !== MANIFEST_KEY) {
				return null;
			}
			const body = overrides.manifestBody;
			if (body == null) {
				return null;
			}
			const buffer = Buffer.from(body, 'utf8');
			return {body: Readable.from([buffer]), contentLength: buffer.byteLength};
		},
		listObjects: async () => objectKeys.map((key) => ({key})),
		getObjectMetadata: async (_bucket: string, key: string) =>
			objectKeys.includes(key) ? {contentLength: 1, contentType: 'application/x-apple-diskimage'} : null,
	} as unknown as IStorageService;
	return new DownloadService(storageService);
}

describe('desktop manifest parsing', () => {
	it('falls back to the object listing when the manifest is not valid JSON', async () => {
		const service = createService({manifestBody: '{not json'});
		await expect(service.resolveLatestDesktopKey({...LATEST_PARAMS})).resolves.toBe(`${PREFIX}/${LISTED_FILENAME}`);
	});

	it('returns null rather than throwing when the manifest is malformed and no artifact is listed', async () => {
		const service = createService({manifestBody: '{not json', objectKeys: []});
		await expect(service.resolveLatestDesktopKey({...LATEST_PARAMS})).resolves.toBeNull();
	});

	it('falls back to the object listing when the manifest parses to an array', async () => {
		const service = createService({manifestBody: '[]'});
		await expect(service.resolveLatestDesktopKey({...LATEST_PARAMS})).resolves.toBe(`${PREFIX}/${LISTED_FILENAME}`);
	});

	it('still resolves through a well-formed manifest', async () => {
		const service = createService({
			manifestBody: JSON.stringify({
				channel: 'stable',
				platform: 'darwin',
				arch: 'x64',
				version: '1.3.0',
				pub_date: '2026-08-17T00:00:00Z',
				files: {dmg: MANIFEST_FILENAME},
			}),
			objectKeys: [`${PREFIX}/${LISTED_FILENAME}`, `${PREFIX}/${MANIFEST_FILENAME}`],
		});
		await expect(service.resolveLatestDesktopKey({...LATEST_PARAMS})).resolves.toBe(`${PREFIX}/${MANIFEST_FILENAME}`);
	});
});
