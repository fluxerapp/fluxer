// SPDX-License-Identifier: AGPL-3.0-or-later

import type {MasterConfig} from '@fluxer/config/src/MasterConfig';
import {resolveDownloadsProvider} from '@fluxer/config/src/S3DownloadsProvider';
import {describe, expect, test} from 'vitest';

const base: Pick<MasterConfig, 's3' | 's3_downloads'>['s3'] = {
	endpoint: 'https://main.example.com',
	presigned_url_base: 'https://public.example.com',
	force_path_style: true,
	region: 'us-east-1',
	access_key_id: 'MAIN_KEY',
	secret_access_key: 'MAIN_SECRET',
	buckets: {cdn: 'cdn', uploads: 'uploads', downloads: 'downloads', reports: 'r', harvests: 'h', static: 's'},
};

describe('resolveDownloadsProvider', () => {
	test('falls back to the main provider when no override is set', () => {
		const resolved = resolveDownloadsProvider({s3: base});
		expect(resolved.isOverridden).toBe(false);
		expect(resolved.settings.endpoint).toBe('https://main.example.com');
		expect(resolved.settings.accessKeyId).toBe('MAIN_KEY');
		expect(resolved.settings.secretAccessKey).toBe('MAIN_SECRET');
		expect(resolved.settings.region).toBe('us-east-1');
		expect(resolved.settings.presignedUrlBase).toBe('https://public.example.com');
	});

	test('ignores a partial override that does not set an endpoint', () => {
		const resolved = resolveDownloadsProvider({s3: base, s3_downloads: {endpoint: '', access_key_id: 'OTHER'}});
		expect(resolved.isOverridden).toBe(false);
		expect(resolved.settings.accessKeyId).toBe('MAIN_KEY');
	});

	test('uses the override provider when an endpoint is set', () => {
		const resolved = resolveDownloadsProvider({
			s3: base,
			s3_downloads: {
				endpoint: 'https://downloads.example.net',
				region: 'eu-central-1',
				access_key_id: 'DL_KEY',
				secret_access_key: 'DL_SECRET',
			},
		});
		expect(resolved.isOverridden).toBe(true);
		expect(resolved.settings.endpoint).toBe('https://downloads.example.net');
		expect(resolved.settings.region).toBe('eu-central-1');
		expect(resolved.settings.accessKeyId).toBe('DL_KEY');
	});

	test('inherits unspecified fields from the main provider', () => {
		const resolved = resolveDownloadsProvider({s3: base, s3_downloads: {endpoint: 'https://downloads.example.net'}});
		expect(resolved.isOverridden).toBe(true);
		expect(resolved.settings.endpoint).toBe('https://downloads.example.net');
		expect(resolved.settings.region).toBe('us-east-1');
		expect(resolved.settings.accessKeyId).toBe('MAIN_KEY');
		expect(resolved.settings.forcePathStyle).toBe(true);
	});

	test('does not inherit the main presigned base for an override provider', () => {
		const resolved = resolveDownloadsProvider({s3: base, s3_downloads: {endpoint: 'https://downloads.example.net'}});
		expect(resolved.settings.presignedUrlBase).toBeUndefined();
	});
});
