// SPDX-License-Identifier: AGPL-3.0-or-later

import {mkdtempSync, rmSync, writeFileSync} from 'node:fs';
import {tmpdir} from 'node:os';
import {join} from 'node:path';
import {loadFederationConfig} from '@app/api/federation/FederationConfig';
import {
	exportFederationPrivateKey,
	exportFederationPublicKey,
	generateFederationKeyPair,
} from '@app/api/federation/FederationCrypto';
import {describe, expect, it} from 'vitest';

const INSTANCE_ID = 'chat-a.example.org';
const KEY_ID = 'key-1';
const PEER_KEY_B = generateFederationKeyPair();
const PEER_KEY_C = generateFederationKeyPair();
const PEERS = JSON.stringify([
	{
		instance_id: 'chat-b.example.org',
		base_url: 'https://chat-b.example.org/',
		public_key: exportFederationPublicKey(PEER_KEY_B.publicKey).base64Url,
	},
	{
		instance_id: 'chat-c.example.org',
		base_url: 'http://chat-c.example.org:8088',
		public_key: exportFederationPublicKey(PEER_KEY_C.publicKey).base64Url,
	},
]);

function validEnv(): NodeJS.ProcessEnv {
	const keyPair = generateFederationKeyPair();
	return {
		FLUXER_FEDERATION_ENABLED: 'true',
		FLUXER_FEDERATION_INSTANCE_ID: INSTANCE_ID,
		FLUXER_FEDERATION_KEY_ID: KEY_ID,
		FLUXER_FEDERATION_PRIVATE_KEY: exportFederationPrivateKey(keyPair.privateKey).base64Url,
		FLUXER_FEDERATION_PEERS: PEERS,
		FLUXER_FEDERATION_GUILD_IDS: '100, 200',
	};
}

describe('loadFederationConfig', () => {
	it('returns a disabled config when the feature flag is absent', () => {
		const config = loadFederationConfig({});

		expect(config.enabled).toBe(false);
		expect(config.peers).toEqual([]);
		expect(config.guildIds.size).toBe(0);
		expect(config.privateKey).toBeNull();
	});

	it('ignores incomplete settings while disabled', () => {
		const config = loadFederationConfig({
			FLUXER_FEDERATION_ENABLED: 'false',
			FLUXER_FEDERATION_PEERS: 'not json',
		});

		expect(config.enabled).toBe(false);
	});

	it('parses peers, guild ids, limits and an Ed25519 key', () => {
		const config = loadFederationConfig(validEnv());

		expect(config.enabled).toBe(true);
		expect(config.instanceId).toBe(INSTANCE_ID);
		expect(config.keyId).toBe(KEY_ID);
		expect(config.privateKey?.type).toBe('private');
		expect(config.peers.map(({instanceId, baseUrl}) => ({instanceId, baseUrl}))).toEqual([
			{instanceId: 'chat-b.example.org', baseUrl: 'https://chat-b.example.org'},
			{instanceId: 'chat-c.example.org', baseUrl: 'http://chat-c.example.org:8088'},
		]);
		expect(config.peers.every((peer) => peer.publicKey.type === 'public')).toBe(true);
		expect([...config.guildIds]).toEqual(['100', '200']);
		expect(config.timeoutMs).toBe(10_000);
		expect(config.maxAttempts).toBe(10);
		expect(config.retryBaseMs).toBe(1_000);
		expect(config.retryMaxMs).toBe(3_600_000);
	});

	it('reads the private key from the file form', () => {
		const directory = mkdtempSync(join(tmpdir(), 'fluxer-federation-config-'));
		const keyPath = join(directory, 'key');
		const keyPair = generateFederationKeyPair();
		writeFileSync(keyPath, exportFederationPrivateKey(keyPair.privateKey).base64Url);
		const env = validEnv();
		delete env.FLUXER_FEDERATION_PRIVATE_KEY;
		env.FLUXER_FEDERATION_PRIVATE_KEY_FILE = keyPath;
		try {
			const config = loadFederationConfig(env);

			expect(config.privateKey?.type).toBe('private');
		} finally {
			rmSync(directory, {recursive: true, force: true});
		}
	});

	it('rejects a missing private key', () => {
		const env = validEnv();
		delete env.FLUXER_FEDERATION_PRIVATE_KEY;

		expect(() => loadFederationConfig(env)).toThrow('FLUXER_FEDERATION_PRIVATE_KEY');
	});

	it('rejects invalid peers', () => {
		const env = validEnv();
		env.FLUXER_FEDERATION_PEERS = JSON.stringify([
			{
				instance_id: 'b.example.org',
				base_url: 'ftp://b.example.org',
				public_key: exportFederationPublicKey(PEER_KEY_B.publicKey).base64Url,
			},
		]);

		expect(() => loadFederationConfig(env)).toThrow('base_url');
	});

	it('rejects peers without a public key', () => {
		const env = validEnv();
		env.FLUXER_FEDERATION_PEERS = JSON.stringify([{instance_id: 'b.example.org', base_url: 'https://b.example.org'}]);

		expect(() => loadFederationConfig(env)).toThrow('public_key');
	});

	it('rejects duplicate peers', () => {
		const env = validEnv();
		const publicKey = exportFederationPublicKey(PEER_KEY_B.publicKey).base64Url;
		env.FLUXER_FEDERATION_PEERS = JSON.stringify([
			{instance_id: 'b.example.org', base_url: 'https://b.example.org', public_key: publicKey},
			{instance_id: 'b.example.org', base_url: 'https://b2.example.org', public_key: publicKey},
		]);

		expect(() => loadFederationConfig(env)).toThrow('duplicate');
	});

	it('rejects an invalid guild id', () => {
		const env = validEnv();
		env.FLUXER_FEDERATION_GUILD_IDS = '100,abc';

		expect(() => loadFederationConfig(env)).toThrow('invalid guild id');
	});

	it('rejects a retry ceiling below the base delay', () => {
		const env = validEnv();
		env.FLUXER_FEDERATION_RETRY_BASE_MS = '5000';
		env.FLUXER_FEDERATION_RETRY_MAX_MS = '1000';

		expect(() => loadFederationConfig(env)).toThrow('FLUXER_FEDERATION_RETRY_MAX_MS');
	});
});
