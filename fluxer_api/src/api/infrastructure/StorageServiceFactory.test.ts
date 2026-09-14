// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {createDownloadsStorageService} from '@app/api/infrastructure/StorageServiceFactory';
import {describe, expect, it} from 'vitest';

describe('createDownloadsStorageService', () => {
	it('returns null when no downloads override is configured', () => {
		expect(Config.s3Downloads.isOverridden).toBe(false);
		expect(createDownloadsStorageService()).toBeNull();
	});

	it('resolves the downloads provider to the shared provider by default', () => {
		expect(Config.s3Downloads.settings.endpoint).toBe(Config.s3.endpoint);
		expect(Config.s3Downloads.settings.region).toBe(Config.s3.region);
		expect(Config.s3Downloads.settings.accessKeyId).toBe(Config.s3.accessKeyId);
	});
});
