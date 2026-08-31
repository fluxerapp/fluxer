// SPDX-License-Identifier: AGPL-3.0-or-later

import {Hono} from 'hono';
import {afterEach, describe, expect, it} from 'vitest';
import {createServer} from '../Server';

const servers: Array<{close: (cb: () => void) => void}> = [];

afterEach(async () => {
	await Promise.all(servers.splice(0).map((s) => new Promise<void>((r) => s.close(() => r()))));
});

function listen(options: Parameters<typeof createServer>[1]) {
	const app = new Hono();
	app.post('/echo', async (ctx) => ctx.text(String((await ctx.req.text()).length)));
	const server = createServer(app, options) as never as {
		close: (cb: () => void) => void;
		address: () => {port: number};
		headersTimeout: number;
		requestTimeout: number;
	};
	servers.push(server);
	return server;
}

describe('createServer timeouts', () => {
	it('separates the header timeout from the total request timeout', () => {
		const server = listen({port: 0});
		expect(server.headersTimeout).toBe(30_000);
		expect(server.requestTimeout).toBe(120_000);
	});

	it('lets both be overridden', () => {
		const server = listen({port: 0, headersTimeoutMs: 1_000, requestTimeoutMs: 2_000});
		expect(server.headersTimeout).toBe(1_000);
		expect(server.requestTimeout).toBe(2_000);
	});

	it('never lets the header timeout exceed the request timeout', () => {
		const server = listen({port: 0, requestTimeoutMs: 5_000});
		expect(server.requestTimeout).toBe(5_000);
		expect(server.headersTimeout).toBeLessThanOrEqual(server.requestTimeout);
	});
});
