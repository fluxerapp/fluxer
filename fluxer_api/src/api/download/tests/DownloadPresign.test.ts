// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it} from 'vitest';
import type {IStorageService} from '../../infrastructure/IStorageService';
import {DownloadService} from '../DownloadService';

const OBJECT_METADATA = {
	contentLength: 285_567_850,
	contentType: 'application/x-apple-diskimage',
	etag: '"abc123"',
	lastModified: new Date('2026-08-17T00:00:00Z'),
};

interface PresignCall {
	bucket: string;
	key: string;
	expiresIn?: number;
	responseContentType?: string;
	responseContentDisposition?: string;
}

function createService(overrides: {metadata?: typeof OBJECT_METADATA | null} = {}) {
	const presignCalls: Array<PresignCall> = [];
	const storageService = {
		getObjectMetadata: async () => (overrides.metadata === undefined ? OBJECT_METADATA : overrides.metadata),
		getPresignedDownloadURL: async (params: PresignCall) => {
			presignCalls.push(params);
			return `https://storage.example.test/${params.key}?signed=1`;
		},
	} as unknown as IStorageService;
	return {service: new DownloadService(storageService), presignCalls};
}

describe('presigned download redirects', () => {
	it('signs the requested object and returns its URL', async () => {
		const {service, presignCalls} = createService();
		const url = await service.getPresignedDownloadRedirect({
			key: 'desktop/canary/darwin/universal/Fluxer.dmg',
			filename: 'Fluxer.dmg',
			expiresIn: 900,
		});
		expect(url).toBe('https://storage.example.test/desktop/canary/darwin/universal/Fluxer.dmg?signed=1');
		expect(presignCalls).toHaveLength(1);
		expect(presignCalls[0]?.key).toBe('desktop/canary/darwin/universal/Fluxer.dmg');
		expect(presignCalls[0]?.expiresIn).toBe(900);
	});

	it('preserves the download filename and content type through the redirect', async () => {
		const {service, presignCalls} = createService();
		await service.getPresignedDownloadRedirect({
			key: 'desktop/canary/darwin/universal/Fluxer.dmg',
			filename: 'Fluxer Canary.dmg',
			expiresIn: 900,
		});
		expect(presignCalls[0]?.responseContentType).toBe('application/x-apple-diskimage');
		expect(presignCalls[0]?.responseContentDisposition).toBe(
			`attachment; filename="${encodeURIComponent('Fluxer Canary.dmg')}"`,
		);
	});

	it('falls back to a binary content type when storage reports none', async () => {
		const {service, presignCalls} = createService({
			metadata: {...OBJECT_METADATA, contentType: null} as unknown as typeof OBJECT_METADATA,
		});
		await service.getPresignedDownloadRedirect({key: 'desktop/x.bin', filename: 'x.bin', expiresIn: 900});
		expect(presignCalls[0]?.responseContentType).toBe('application/octet-stream');
	});

	it('returns null for a missing object so the caller can answer 404 without signing', async () => {
		const {service, presignCalls} = createService({metadata: null});
		const url = await service.getPresignedDownloadRedirect({
			key: 'desktop/missing.dmg',
			filename: 'missing.dmg',
			expiresIn: 900,
		});
		expect(url).toBeNull();
		expect(presignCalls).toHaveLength(0);
	});
});
