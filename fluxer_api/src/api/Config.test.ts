// SPDX-License-Identifier: AGPL-3.0-or-later

import {loadConfig, resetConfig} from '@fluxer/config/src/ConfigLoader';
import {createServer} from '@fluxer/hono/src/Server';
import {Hono} from 'hono';
import {afterAll, afterEach, describe, expect, test, vi} from 'vitest';
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
