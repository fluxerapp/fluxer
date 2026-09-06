// SPDX-License-Identifier: AGPL-3.0-or-later

import {Readable} from 'node:stream';
import {describe, expect, it} from 'vitest';
import {Config} from '../../Config';
import type {IStorageService} from '../../infrastructure/IStorageService';
import {DownloadService} from '../DownloadService';

const PREFIX = 'desktop/stable/darwin/x64';
const MANIFEST_KEY = `${PREFIX}/manifest.json`;
const FILENAME = 'Fluxer-1.3.0-mac-universal.dmg';
const SHA256 = 'a'.repeat(64);

const LATEST_PARAMS = {
	channel: 'stable',
	plat: 'darwin',
	arch: 'x64',
} as const;

const MANIFEST_BODY = JSON.stringify({
	channel: 'stable',
	platform: 'darwin',
	arch: 'x64',
	version: '1.3.0',
	pub_date: '2026-08-17T00:00:00Z',
	files: {dmg: {filename: FILENAME, sha256: SHA256}},
});

function createService() {
	const objectKeys = [`${PREFIX}/${FILENAME}`];
	const storageService = {
		streamObject: async (params: {key: string}) => {
			if (params.key !== MANIFEST_KEY) {
				return null;
			}
			const buffer = Buffer.from(MANIFEST_BODY, 'utf8');
			return {body: Readable.from([buffer]), contentLength: buffer.byteLength};
		},
		listObjects: async () => objectKeys.map((key) => ({key})),
		getObjectMetadata: async (_bucket: string, key: string) =>
			objectKeys.includes(key) ? {contentLength: 1, contentType: 'application/x-apple-diskimage'} : null,
	} as unknown as IStorageService;
	return new DownloadService(storageService);
}

describe('desktop download base url', () => {
	it('builds artifact urls from the configured client API endpoint', async () => {
		const version = await createService().getLatestDesktopVersion({...LATEST_PARAMS});
		const base = Config.endpoints.apiClient.replace(/\/+$/u, '');
		expect(base.length).toBeGreaterThan(0);
		expect(version?.files.dmg?.url).toBe(`${base}/dl/desktop/stable/darwin/x64/1.3.0/dmg`);
		expect(version?.files.dmg?.checksum_url).toBe(`${base}/dl/desktop/stable/darwin/x64/1.3.0/dmg.sha256`);
	});

	it('prefers an explicit base url and strips its trailing slashes', async () => {
		const version = await createService().getLatestDesktopVersion({
			...LATEST_PARAMS,
			baseUrl: 'https://chat.example.com/api//',
		});
		expect(version?.files.dmg?.url).toBe('https://chat.example.com/api/dl/desktop/stable/darwin/x64/1.3.0/dmg');
	});
});
