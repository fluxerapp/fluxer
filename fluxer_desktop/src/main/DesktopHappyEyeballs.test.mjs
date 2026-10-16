// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import net from 'node:net';
import {describe, test} from 'node:test';

const {connectToFirstAnsweringAddress} = await import('./DesktopHappyEyeballs.ts');

function race(addresses, port, options) {
	return new Promise((resolve) => {
		connectToFirstAnsweringAddress(addresses, port, options, (error, socket) => resolve({error, socket}));
	});
}

describe('DesktopHappyEyeballs', () => {
	test('a slow first address keeps racing after the next one fails fast', async () => {
		const server = net.createServer((socket) => socket.end());
		await new Promise((resolve) => server.listen(0, '127.0.0.1', resolve));
		const {port} = server.address();
		const dial = (options) => {
			if (options.host !== '127.0.0.1') {
				return net.connect(options);
			}
			const socket = new net.Socket();
			setTimeout(() => socket.connect(options), 150);
			return socket;
		};
		try {
			const {error, socket} = await race(
				[
					{address: '127.0.0.1', family: 4},
					{address: '::1', family: 6},
				],
				port,
				{attemptDelayMs: 20, dial},
			);
			assert.equal(error, null, 'a connect slower than the attempt delay must not be abandoned');
			assert.equal(socket.remoteAddress, '127.0.0.1');
			socket.destroy();
		} finally {
			server.close();
		}
	});

	test('when every address fails the error names each attempt', async () => {
		const server = net.createServer();
		await new Promise((resolve) => server.listen(0, '127.0.0.1', resolve));
		const {port} = server.address();
		await new Promise((resolve) => server.close(resolve));
		const {error, socket} = await race([{address: '127.0.0.1', family: 4}], port, {});
		assert.equal(socket, null);
		assert.equal(error.code, 'ECONNREFUSED');
		assert.match(error.message, /ECONNREFUSED 127\.0\.0\.1/);
	});
});
