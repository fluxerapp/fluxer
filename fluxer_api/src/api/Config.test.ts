// SPDX-License-Identifier: AGPL-3.0-or-later

import {buildAPIConfigFromMaster, buildAPIServerOptions} from '@app/api/Config';
import {loadConfig, resetConfig} from '@fluxer/config/src/ConfigLoader';
import type {MasterConfig} from '@fluxer/config/src/MasterConfig';
import {createServer} from '@fluxer/hono/src/Server';
import {Hono} from 'hono';
import {afterAll, afterEach, beforeAll, describe, expect, it, test, vi} from 'vitest';

interface ListeningServer {
	close: (callback: () => void) => void;
	headersTimeout: number;
	requestTimeout: number;
}

const servers: Array<ListeningServer> = [];

async function listenWithEnv(env: Record<string, string> = {}): Promise<ListeningServer> {
	for (const [key, value] of Object.entries({FLUXER_API_PORT: '0', ...env})) {
		vi.stubEnv(key, value);
	}
	resetConfig();
	const config = buildAPIConfigFromMaster(await loadConfig());
	const server = createServer(new Hono(), buildAPIServerOptions(config)) as unknown as ListeningServer;
	servers.push(server);
	return server;
}

afterEach(async () => {
	await Promise.all(servers.splice(0).map((server) => new Promise<void>((resolve) => server.close(() => resolve()))));
	vi.unstubAllEnvs();
	resetConfig();
});

afterAll(async () => {
	await loadConfig();
});

describe('buildAPIServerOptions', () => {
	test('starts the api on the shipped header and request timeouts', async () => {
		const server = await listenWithEnv();
		expect(server.headersTimeout).toBe(30_000);
		expect(server.requestTimeout).toBe(120_000);
	});

	test('passes the operator header timeout from the environment into the server', async () => {
		const server = await listenWithEnv({FLUXER_API_HEADERS_TIMEOUT_MS: '45000'});
		expect(server.headersTimeout).toBe(45_000);
		expect(server.requestTimeout).toBe(120_000);
	});

	test('passes the operator request timeout from the environment into the server', async () => {
		const server = await listenWithEnv({FLUXER_API_REQUEST_TIMEOUT_MS: '600000'});
		expect(server.headersTimeout).toBe(30_000);
		expect(server.requestTimeout).toBe(600_000);
	});

	test('clamps a header timeout set above the request timeout', async () => {
		const server = await listenWithEnv({
			FLUXER_API_HEADERS_TIMEOUT_MS: '90000',
			FLUXER_API_REQUEST_TIMEOUT_MS: '45000',
		});
		expect(server.requestTimeout).toBe(45_000);
		expect(server.headersTimeout).toBe(45_000);
	});
});

function withUploadRelaySecret(master: MasterConfig, secretBase64: string): MasterConfig {
	return {
		...master,
		services: {
			...master.services,
			media_proxy: {
				...master.services.media_proxy,
				upload_relay: {
					...master.services.media_proxy.upload_relay,
					secret_base64: secretBase64,
				},
			},
		},
	};
}

function withStripeLegacyPrices(
	master: MasterConfig,
	legacyPrices: Record<string, Array<string> | undefined> | undefined,
): MasterConfig {
	return {
		...master,
		integrations: {
			...master.integrations,
			stripe: {
				...master.integrations.stripe,
				legacy_prices: legacyPrices,
			},
		},
	};
}

describe('buildAPIConfigFromMaster upload relay secret', () => {
	let master: MasterConfig;
	beforeAll(async () => {
		master = await loadConfig();
	});

	it('refuses to build without FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SECRET_BASE64', () => {
		expect(() => buildAPIConfigFromMaster(withUploadRelaySecret(master, ''))).toThrow(
			/FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SECRET_BASE64/,
		);
	});

	it('refuses a secret that decodes to fewer than 32 bytes', () => {
		const secret = Buffer.alloc(16, 7).toString('base64');
		expect(() => buildAPIConfigFromMaster(withUploadRelaySecret(master, secret))).toThrow(/at least 32 bytes/);
	});

	it('accepts a secret that decodes to 32 bytes', () => {
		const secret = Buffer.alloc(32, 7).toString('base64');
		expect(
			buildAPIConfigFromMaster(withUploadRelaySecret(master, secret)).mediaProxy.uploadRelay.relaySecretBase64,
		).toBe(secret);
	});

	it('reads the relay secret from the loaded config rather than the environment', () => {
		expect(buildAPIConfigFromMaster(master).mediaProxy.uploadRelay.relaySecretBase64).toBe(
			master.services.media_proxy.upload_relay.secret_base64,
		);
	});
});

describe('buildAPIConfigFromMaster stripe legacy prices', () => {
	let master: MasterConfig;
	beforeAll(async () => {
		master = await loadConfig();
	});

	it('copies the retired stripe price map from master config onto the api config', () => {
		const legacyPrices = {
			monthly_brl: ['price_retired_monthly_brl'],
			yearly_brl: ['price_retired_yearly_brl_a', 'price_retired_yearly_brl_b'],
			monthly_try: ['price_1TMYpdFPC94Os7FdZVRx98Up'],
		};
		expect(buildAPIConfigFromMaster(withStripeLegacyPrices(master, legacyPrices)).stripe.legacyPrices).toEqual(
			legacyPrices,
		);
	});

	it('copies the retired price map even when no live prices are configured', () => {
		const withoutPrices: MasterConfig = {
			...master,
			integrations: {
				...master.integrations,
				stripe: {
					...master.integrations.stripe,
					prices: undefined,
					legacy_prices: {monthly_try: ['price_1TMYpdFPC94Os7FdZVRx98Up']},
				},
			},
		};
		const config = buildAPIConfigFromMaster(withoutPrices);
		expect(config.stripe.prices).toBeUndefined();
		expect(config.stripe.legacyPrices).toEqual({monthly_try: ['price_1TMYpdFPC94Os7FdZVRx98Up']});
	});

	it('leaves the retired price map undefined when master config does not set one', () => {
		expect(buildAPIConfigFromMaster(withStripeLegacyPrices(master, undefined)).stripe.legacyPrices).toBeUndefined();
	});
});

function withOptionalOutboundLookups(
	master: MasterConfig,
	selfHosted: boolean,
	overrides: {breachedPasswordCheck?: boolean} = {},
): MasterConfig {
	return {
		...master,
		integrations: {
			...master.integrations,
			breached_password_check: {enabled: overrides.breachedPasswordCheck},
		},
		instance: {
			...master.instance,
			self_hosted: selfHosted,
		},
	};
}

describe('buildAPIConfigFromMaster optional outbound lookups', () => {
	let master: MasterConfig;
	beforeAll(async () => {
		master = await loadConfig();
	});

	it('keeps the lookup on when the instance is not self-hosted', () => {
		const config = buildAPIConfigFromMaster(withOptionalOutboundLookups(master, false));
		expect(config.breachedPasswordCheck.enabled).toBe(true);
	});

	it('leaves the lookup off on a self-hosted instance', () => {
		const config = buildAPIConfigFromMaster(withOptionalOutboundLookups(master, true));
		expect(config.breachedPasswordCheck.enabled).toBe(false);
	});

	it('lets a self-hosted operator switch the lookup on', () => {
		const config = buildAPIConfigFromMaster(withOptionalOutboundLookups(master, true, {breachedPasswordCheck: true}));
		expect(config.breachedPasswordCheck.enabled).toBe(true);
	});

	it('lets an operator switch the lookup off when the instance is not self-hosted', () => {
		const config = buildAPIConfigFromMaster(withOptionalOutboundLookups(master, false, {breachedPasswordCheck: false}));
		expect(config.breachedPasswordCheck.enabled).toBe(false);
	});
});

async function trustedCallersFromEnv(env: Record<string, string>) {
	for (const [key, value] of Object.entries(env)) {
		vi.stubEnv(key, value);
	}
	resetConfig();
	return buildAPIConfigFromMaster(await loadConfig()).internal.trustedCallers;
}

describe('buildAPIConfigFromMaster trusted callers', () => {
	const bugsKey = 'b'.repeat(32);
	const donationKey = 'd'.repeat(32);

	test('reads callers from FLUXER_API_TRUSTED_CALLERS', async () => {
		const callers = await trustedCallersFromEnv({
			FLUXER_API_TRUSTED_CALLERS: JSON.stringify([
				{name: 'bugs', key: bugsKey, buckets: ['oauth:token', 'oauth:revoke']},
			]),
		});
		expect(callers).toEqual([{name: 'bugs', key: bugsKey, buckets: ['oauth:token', 'oauth:revoke']}]);
	});

	test('turns FLUXER_API_DONATION_PROXY_KEY into a caller scoped to the donation buckets', async () => {
		const callers = await trustedCallersFromEnv({FLUXER_API_DONATION_PROXY_KEY: donationKey});
		expect(callers).toEqual([
			{
				name: 'donation',
				key: donationKey,
				buckets: ['donation:request_link', 'donation:manage', 'donation:checkout'],
			},
		]);
	});

	test('keeps both forms side by side', async () => {
		const callers = await trustedCallersFromEnv({
			FLUXER_API_DONATION_PROXY_KEY: donationKey,
			FLUXER_API_TRUSTED_CALLERS: JSON.stringify([{name: 'bugs', key: bugsKey, buckets: ['oauth:token']}]),
		});
		expect(callers.map((caller) => caller.name)).toEqual(['bugs', 'donation']);
	});

	test('tolerates bucket names and fields this build does not know', async () => {
		const callers = await trustedCallersFromEnv({
			FLUXER_API_TRUSTED_CALLERS: JSON.stringify([
				{name: 'bugs', key: bugsKey, buckets: ['oauth:token', 'future:bucket'], note: 'added later'},
			]),
		});
		expect(callers).toEqual([{name: 'bugs', key: bugsKey, buckets: ['oauth:token', 'future:bucket']}]);
	});

	test('fails at boot on a short key', async () => {
		await expect(
			trustedCallersFromEnv({
				FLUXER_API_TRUSTED_CALLERS: JSON.stringify([{name: 'bugs', key: 'short', buckets: ['oauth:token']}]),
			}),
		).rejects.toThrow('FLUXER_API_TRUSTED_CALLERS entry 1 key must be at least 32 characters');
	});

	test('fails at boot on an entry with no buckets', async () => {
		await expect(
			trustedCallersFromEnv({
				FLUXER_API_TRUSTED_CALLERS: JSON.stringify([{name: 'bugs', key: bugsKey, buckets: []}]),
			}),
		).rejects.toThrow('FLUXER_API_TRUSTED_CALLERS entry 1 buckets must be a non-empty list of bucket names');
	});

	test('fails at boot on a value that is not a JSON array', async () => {
		await expect(trustedCallersFromEnv({FLUXER_API_TRUSTED_CALLERS: '{"name":"bugs"}'})).rejects.toThrow(
			'FLUXER_API_TRUSTED_CALLERS must be a JSON array',
		);
	});
});
