// SPDX-License-Identifier: AGPL-3.0-or-later

import {loadConfig, resetConfig} from '@fluxer/config/src/ConfigLoader';
import {afterEach, beforeEach, describe, expect, test, vi} from 'vitest';

const BASE_ENV: Record<string, string> = {
	FLUXER_ENV: 'test',
	FLUXER_BASE_DOMAIN: 'localhost',
	FLUXER_MEDIA_PROXY_SECRET_KEY: 'test-media-secret',
	FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SECRET_BASE64: 'AAECAwQFBgcICQoLDA0ODxAREhMUFRYXGBkaGxwdHh8=',
	FLUXER_ADMIN_SECRET_KEY_BASE: 'test-admin-secret',
	FLUXER_ADMIN_OAUTH_CLIENT_SECRET: 'test-admin-oauth-secret',
	FLUXER_GATEWAY_RPC_AUTH_TOKEN: 'test-gateway-token',
	FLUXER_SUDO_MODE_SECRET: 'test-sudo-secret',
	FLUXER_CONNECTION_INITIATION_SECRET: 'test-connection-secret',
	FLUXER_PROFILE_PSEUDONYM_SECRET: 'test-profile-pseudonym-secret',
	FLUXER_VAPID_PUBLIC_KEY: 'BB76bTFIuoqmxJtTfZX0yGTn1f_qu9H03B_nkj8OyExJFkN7Y-HBZZzShnHZoEhXKc5ZRy3jFu7OkBbnaQG-4aw',
	FLUXER_VAPID_PRIVATE_KEY: 'Xgi-3P8J-I3Q6U1HlCcXMuc_tKLGAM9nIfznX3Hz68o',
};

function stubEnv(overrides: Record<string, string>): void {
	for (const key of Object.keys(process.env)) {
		if (key.startsWith('FLUXER_')) {
			vi.stubEnv(key, undefined);
		}
	}
	for (const [key, value] of Object.entries({...BASE_ENV, ...overrides})) {
		vi.stubEnv(key, value);
	}
}

describe('storage and key-value modes', () => {
	beforeEach(() => resetConfig());
	afterEach(() => {
		resetConfig();
		vi.unstubAllEnvs();
	});

	test('postgres key-value mode needs the postgres database backend', async () => {
		stubEnv({
			FLUXER_S3_ACCESS_KEY_ID: 'test-access-key',
			FLUXER_S3_SECRET_ACCESS_KEY: 'test-secret-key',
			FLUXER_KV_MODE: 'postgres',
			FLUXER_KV_POSTGRES_MAX_CONNECTIONS: '6',
		});
		expect((await loadConfig()).internal.kv_postgres).toEqual({max_connections: 6});
		resetConfig();
		stubEnv({
			FLUXER_S3_ACCESS_KEY_ID: 'test-access-key',
			FLUXER_S3_SECRET_ACCESS_KEY: 'test-secret-key',
			FLUXER_KV_MODE: 'postgres',
			FLUXER_DATABASE_BACKEND: 'cassandra',
		});
		await expect(loadConfig()).rejects.toThrow('FLUXER_DATABASE_BACKEND must be postgres');
	});
});
