// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Server} from 'node:http';
import {createServer} from '@fluxer/hono/src/Server';
import type {ServerType} from '@hono/node-server';
import {Hono} from 'hono';
import {afterEach, describe, expect, test} from 'vitest';

function closeServer(server: ServerType): Promise<void> {
	return new Promise((resolve, reject) => {
		server.close((error?: Error) => {
			if (error) {
				reject(error);
				return;
			}
			resolve();
		});
	});
}

function listen(options: Parameters<typeof createServer>[1]): Promise<{server: ServerType; port: number}> {
	return new Promise((resolve) => {
		const app = new Hono();
		app.get('/', (ctx) => ctx.text('OK'));
		const server = createServer(app, {
			...options,
			onListen: ({port}) => resolve({server, port}),
		});
	});
}

describe('Server limits', () => {
	let server: ServerType | null = null;
	afterEach(async () => {
		if (server) {
			await closeServer(server);
			server = null;
		}
	});

	test('applies bounded defaults instead of stock node timeouts', async () => {
		const listening = await listen({port: 0});
		server = listening.server;
		const httpServer = server as Server;
		expect(httpServer.requestTimeout).toBe(120_000);
		expect(httpServer.keepAliveTimeout).toBe(125_000);
		expect(httpServer.maxRequestsPerSocket).toBe(1_000);
		expect(httpServer.headersTimeout).toBeLessThanOrEqual(httpServer.requestTimeout);
	});

	test('honours explicit limit overrides', async () => {
		const listening = await listen({
			port: 0,
			requestTimeoutMs: 15_000,
			keepAliveTimeoutMs: 61_000,
			maxRequestsPerSocket: 250,
		});
		server = listening.server;
		const httpServer = server as Server;
		expect(httpServer.requestTimeout).toBe(15_000);
		expect(httpServer.keepAliveTimeout).toBe(61_000);
		expect(httpServer.maxRequestsPerSocket).toBe(250);
	});
});
