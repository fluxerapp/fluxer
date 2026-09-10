// SPDX-License-Identifier: AGPL-3.0-or-later

import {loadConfig, resetConfig} from '@fluxer/config/src/ConfigLoader';
import type {MasterConfig} from '@fluxer/config/src/MasterConfig';
import {createServer} from '@fluxer/hono/src/Server';
import {Hono} from 'hono';
import {afterAll, afterEach, beforeAll, describe, expect, it, test, vi} from 'vitest';
import {buildAPIConfigFromMaster, buildAPIServerOptions} from './Config';

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

	test('carries the operator header timeout from the environment into the server', async () => {
		const server = await listenWithEnv({FLUXER_API_HEADERS_TIMEOUT_MS: '45000'});
		expect(server.headersTimeout).toBe(45_000);
		expect(server.requestTimeout).toBe(120_000);
	});

	test('carries the operator request timeout from the environment into the server', async () => {
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

	it('carries the retired stripe price map from master config onto the api config', () => {
		const legacyPrices = {
			monthly_brl: ['price_retired_monthly_brl'],
			yearly_brl: ['price_retired_yearly_brl_a', 'price_retired_yearly_brl_b'],
			monthly_try: ['price_1TMYpdFPC94Os7FdZVRx98Up'],
		};
		expect(buildAPIConfigFromMaster(withStripeLegacyPrices(master, legacyPrices)).stripe.legacyPrices).toEqual(
			legacyPrices,
		);
	});

	it('carries the retired price map even when no live prices are configured', () => {
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
