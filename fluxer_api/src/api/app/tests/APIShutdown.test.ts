// SPDX-License-Identifier: AGPL-3.0-or-later

import {once} from 'node:events';
import http from 'node:http';
import net, {type AddressInfo} from 'node:net';
import {closeHttpServers} from '@app/api/app/APIShutdown';
import type {LoggerInterface} from '@fluxer/logger/src/LoggerInterface';
import {describe, expect, it, vi} from 'vitest';

function recordingLogger() {
	const warn = vi.fn();
	const error = vi.fn();
	const logger = {warn, error, info: vi.fn(), debug: vi.fn(), trace: vi.fn(), fatal: vi.fn(), child: () => logger};
	return {logger: logger as unknown as LoggerInterface, warn, error};
}

async function listen(handler: http.RequestListener): Promise<{server: http.Server; port: number}> {
	const server = http.createServer(handler);
	server.listen(0, '127.0.0.1');
	await once(server, 'listening');
	return {server, port: (server.address() as AddressInfo).port};
}

describe('closeHttpServers', () => {
	it('closes idle servers within the budget', async () => {
		const {server} = await listen((_req, res) => res.end('ok'));
		const {logger, warn} = recordingLogger();
		await expect(closeHttpServers([server], 1_000, logger)).resolves.toBe(true);
		expect(server.listening).toBe(false);
		expect(warn).not.toHaveBeenCalled();
	});

	it('closes the connections of a request that outlives the budget', async () => {
		let arrived: () => void = () => {};
		const requestArrived = new Promise<void>((resolve) => {
			arrived = resolve;
		});
		const {server, port} = await listen(() => arrived());
		const client = net.connect(port, '127.0.0.1');
		client.write('GET /slow HTTP/1.1\r\nHost: 127.0.0.1\r\n\r\n');
		const clientClosed = once(client, 'close');
		await requestArrived;
		const {logger, warn} = recordingLogger();
		const closedEvent = once(server, 'close');
		await expect(closeHttpServers([server], 50, logger)).resolves.toBe(false);
		expect(warn).toHaveBeenCalledWith({budgetMs: 50}, 'API requests still open at shutdown, closing their connections');
		await closedEvent;
		await clientClosed;
	});

	it('keeps shutting down when a server fails to close', async () => {
		const failing = {close: (callback: (error?: Error) => void) => callback(new Error('not running'))};
		const {logger, error} = recordingLogger();
		await expect(closeHttpServers([failing], 1_000, logger)).resolves.toBe(true);
		expect(error).toHaveBeenCalledTimes(1);
	});
});
