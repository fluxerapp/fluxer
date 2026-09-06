// SPDX-License-Identifier: AGPL-3.0-or-later

import {generateKeyPairSync} from 'node:crypto';
import {describe, expect, it} from 'vitest';
import type {BlueskyOAuthConfig} from '../config/APIConfig';
import {MockKVProvider} from '../test/mocks/MockKVProvider';
import {BlueskyOAuthService} from './BlueskyOAuthService';

const API_PUBLIC_ENDPOINT = 'https://chat.example.com';

function generateSigningKey(): string {
	const {privateKey} = generateKeyPairSync('ec', {
		namedCurve: 'P-256',
		privateKeyEncoding: {type: 'pkcs8', format: 'pem'},
		publicKeyEncoding: {type: 'spki', format: 'pem'},
	});
	return privateKey;
}

function buildConfig(overrides: Partial<BlueskyOAuthConfig> = {}): BlueskyOAuthConfig {
	return {
		enabled: true,
		client_name: 'Fluxer',
		client_uri: '',
		logo_uri: '',
		tos_uri: '',
		policy_uri: '',
		keys: [{kid: 'test-key', private_key: generateSigningKey()}],
		...overrides,
	};
}

async function serveClientMetadata(config: BlueskyOAuthConfig): Promise<Record<string, unknown>> {
	const service = await BlueskyOAuthService.create(config, new MockKVProvider(), API_PUBLIC_ENDPOINT);
	return JSON.parse(JSON.stringify(service.clientMetadata)) as Record<string, unknown>;
}

describe('BlueskyOAuthService', () => {
	it('omits the legal URLs from the served client document when none are configured', async () => {
		const metadata = await serveClientMetadata(buildConfig());

		expect(metadata).not.toHaveProperty('tos_uri');
		expect(metadata).not.toHaveProperty('policy_uri');
		expect(metadata).not.toHaveProperty('logo_uri');
		expect(metadata.client_id).toBe(`${API_PUBLIC_ENDPOINT}/connections/bluesky/client-metadata.json`);
	});

	it('echoes configured legal URLs verbatim', async () => {
		const metadata = await serveClientMetadata(
			buildConfig({
				tos_uri: 'https://chat.example.com/terms',
				policy_uri: 'https://chat.example.com/privacy',
			}),
		);

		expect(metadata.tos_uri).toBe('https://chat.example.com/terms');
		expect(metadata.policy_uri).toBe('https://chat.example.com/privacy');
	});
});
