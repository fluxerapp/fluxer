// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import http from 'node:http';
import {Readable} from 'node:stream';
import {describe, test} from 'node:test';
import {installElectronStub} from './LocalAppTestSupport.test.mjs';

installElectronStub();

const {DesktopOriginTrust, DesktopOutboundHTTP, createPinnedHostLookup, readBoundedMessage} = await import(
	'./DesktopOutboundHTTP.ts'
);
const {DesktopSelectedInstanceClient} = await import('./SelectedInstanceFetch.ts');
const {getLaunchInstanceEndpointOverride} = await import('../common/DesktopConfig.ts');

const PNG_BYTES = Buffer.from('89504e470d0a1a0a', 'hex');

function stubResolver(table) {
	return async (hostname) => {
		const key = hostname.endsWith('.') ? hostname.slice(0, -1) : hostname;
		const addresses = table[key];
		if (addresses == null) {
			throw new Error(`no stubbed address for ${hostname}`);
		}
		return addresses;
	};
}

async function startServer(routes) {
	const requested = [];
	const server = http.createServer((request, response) => {
		const url = new URL(request.url, 'http://127.0.0.1');
		requested.push(url.pathname);
		const route = routes[url.pathname];
		if (route == null) {
			response.writeHead(404);
			response.end();
			return;
		}
		route(request, response);
	});
	await new Promise((resolve) => server.listen(0, '127.0.0.1', resolve));
	return {port: server.address().port, requested, server};
}

function sendImage(response, contentType, body, headers = {}) {
	response.writeHead(200, {'content-type': contentType, ...headers});
	response.end(body);
}

describe('DesktopOutboundHTTP origin anchoring', () => {
	test('a publicly reachable anchor refuses an advertised origin that resolves privately', async () => {
		const outboundHTTP = new DesktopOutboundHTTP({
			resolveHostAddresses: stubResolver({'media.internal': ['10.0.0.1'], 'public.example': ['93.184.216.34']}),
		});
		await outboundHTTP.registerAnchoredOrigins({
			anchorOrigin: 'https://public.example',
			origins: ['https://media.internal'],
		});
		await assert.rejects(
			outboundHTTP.request({
				body: null,
				expectedOrigin: 'https://media.internal',
				headers: null,
				method: 'GET',
				originTrust: DesktopOriginTrust.REGISTERED,
				serviceName: 'test',
				signal: null,
				timeoutMs: 500,
				url: 'https://media.internal/avatars/1.webp',
			}),
			/cannot authorize the privately resolving origin/,
		);
		outboundHTTP.cleanup();
	});

	test('an anchor that cannot resolve registers under the remembered requirement', async () => {
		let online = false;
		const outboundHTTP = new DesktopOutboundHTTP({
			resolveHostAddresses: async (hostname) => {
				if (!online) {
					throw Object.assign(new Error(`getaddrinfo EAI_AGAIN ${hostname}`), {
						code: 'EAI_AGAIN',
						syscall: 'getaddrinfo',
					});
				}
				return hostname.startsWith('media.internal') ? ['10.0.0.1'] : ['93.184.216.34'];
			},
		});
		const registration = {anchorOrigin: 'https://public.example', origins: ['https://media.internal']};
		await assert.rejects(outboundHTTP.registerAnchoredOrigins(registration), {code: 'EAI_AGAIN'});
		assert.equal(outboundHTTP.isRegisteredOrigin('https://public.example'), false);
		assert.equal(
			await outboundHTTP.registerAnchoredOrigins({...registration, unresolvedAnchorRequirement: 'public'}),
			'public',
		);
		assert.equal(outboundHTTP.isRegisteredOrigin('https://media.internal'), true);
		online = true;
		await assert.rejects(
			outboundHTTP.request({
				body: null,
				expectedOrigin: 'https://media.internal',
				headers: null,
				method: 'GET',
				originTrust: DesktopOriginTrust.REGISTERED,
				serviceName: 'test',
				signal: null,
				timeoutMs: 500,
				url: 'https://media.internal/avatars/1.webp',
			}),
			/cannot authorize the privately resolving origin/,
		);
		assert.equal(await outboundHTTP.registerAnchoredOrigins(registration), 'public');
		outboundHTTP.cleanup();
	});

	test('an anchor that resolves to no usable address never falls back to the remembered requirement', async () => {
		const outboundHTTP = new DesktopOutboundHTTP({
			resolveHostAddresses: stubResolver({'unusable.example': ['not-an-address']}),
		});
		await assert.rejects(
			outboundHTTP.registerAnchoredOrigins({
				anchorOrigin: 'https://unusable.example',
				origins: [],
				unresolvedAnchorRequirement: 'any',
			}),
			{name: 'DesktopOutboundHTTPEmptyResolutionError'},
		);
		assert.equal(outboundHTTP.isRegisteredOrigin('https://unusable.example'), false);
		outboundHTTP.cleanup();
	});

	test('a split-horizon anchor resolving to private and public addresses is anchored on its private address', async () => {
		const outboundHTTP = new DesktopOutboundHTTP({
			resolveHostAddresses: stubResolver({'chat.home.example': ['2606:4700:4700::1111', '192.168.1.20']}),
		});
		assert.equal(
			await outboundHTTP.registerAnchoredOrigins({anchorOrigin: 'http://chat.home.example', origins: []}),
			'any',
		);
		assert.equal(await outboundHTTP.requireCleartextTransportAddress('http://chat.home.example'), '192.168.1.20');
		outboundHTTP.cleanup();
	});

	test('a publicly reachable anchor refuses an advertised origin that also resolves privately', async () => {
		const outboundHTTP = new DesktopOutboundHTTP({
			resolveHostAddresses: stubResolver({
				'public.example': ['93.184.216.34'],
				'media.mixed.example': ['93.184.216.35', '10.0.0.1'],
			}),
		});
		await outboundHTTP.registerAnchoredOrigins({
			anchorOrigin: 'https://public.example',
			origins: ['https://media.mixed.example'],
		});
		await assert.rejects(
			outboundHTTP.request({
				body: null,
				expectedOrigin: 'https://media.mixed.example',
				headers: null,
				method: 'GET',
				originTrust: DesktopOriginTrust.REGISTERED,
				serviceName: 'test',
				signal: null,
				timeoutMs: 500,
				url: 'https://media.mixed.example/avatars/1.webp',
			}),
			/cannot authorize the privately resolving origin/,
		);
		outboundHTTP.cleanup();
	});

	test('only cleartext origins anchored on a private address are offered to the page policy', async () => {
		const outboundHTTP = new DesktopOutboundHTTP({
			resolveHostAddresses: stubResolver({
				'lan.example': ['192.168.1.20'],
				'public.example': ['93.184.216.34'],
			}),
		});
		await outboundHTTP.registerAnchoredOrigins({
			anchorOrigin: 'http://lan.example:48090',
			origins: ['http://lan.example:48091', 'https://lan.example'],
		});
		await outboundHTTP.registerAnchoredOrigins({
			anchorOrigin: 'https://public.example',
			origins: ['http://public.example'],
		});
		assert.deepEqual(outboundHTTP.registeredCleartextOrigins(), [
			'http://lan.example:48090',
			'http://lan.example:48091',
		]);
		outboundHTTP.cleanup();
	});

	test('a request that cannot be constructed releases its in-flight slot', async () => {
		const {port, server} = await startServer({
			'/ok': (_request, response) => sendImage(response, 'image/png', PNG_BYTES),
		});
		try {
			const outboundHTTP = new DesktopOutboundHTTP({
				resolveHostAddresses: stubResolver({'lan.example': ['127.0.0.1']}),
			});
			await outboundHTTP.registerAnchoredOrigins({
				anchorOrigin: `http://lan.example:${port}`,
				origins: [`http://lan.example:${port}`],
			});
			const send = (headers) =>
				outboundHTTP.request({
					body: null,
					expectedOrigin: `http://lan.example:${port}`,
					headers,
					method: 'GET',
					originTrust: DesktopOriginTrust.REGISTERED,
					serviceName: 'slot-leak',
					signal: null,
					timeoutMs: 2000,
					url: `http://lan.example:${port}/ok`,
				});

			for (let attempt = 0; attempt < 80; attempt += 1) {
				await assert.rejects(send({'x-bad': 'a\u0001b'}));
			}
			const message = await send(null);

			assert.equal(message.status, 200);
			message.message.resume();
			outboundHTTP.cleanup();
		} finally {
			server.close();
		}
	});

	test('a request whose signal already aborted is refused instead of sent', async () => {
		const seen = [];
		const {port, server} = await startServer({
			'/ok': (request, response) => {
				seen.push(request.url);
				sendImage(response, 'image/png', PNG_BYTES);
			},
		});
		try {
			const outboundHTTP = new DesktopOutboundHTTP({
				resolveHostAddresses: stubResolver({'lan.example': ['127.0.0.1']}),
			});
			await outboundHTTP.registerAnchoredOrigins({
				anchorOrigin: `http://lan.example:${port}`,
				origins: [`http://lan.example:${port}`],
			});
			const controller = new AbortController();
			controller.abort();

			await assert.rejects(
				outboundHTTP.request({
					body: null,
					expectedOrigin: `http://lan.example:${port}`,
					headers: null,
					method: 'GET',
					originTrust: DesktopOriginTrust.REGISTERED,
					serviceName: 'aborted',
					signal: controller.signal,
					timeoutMs: 2000,
					url: `http://lan.example:${port}/ok`,
				}),
			);

			assert.deepEqual(seen, []);
			outboundHTTP.cleanup();
		} finally {
			server.close();
		}
	});

	test('a LAN self-hosted instance whose anchor resolves privately is allowed', async () => {
		const {port, server} = await startServer({
			'/media/ok': (_request, response) => sendImage(response, 'image/png', PNG_BYTES),
		});
		try {
			const outboundHTTP = new DesktopOutboundHTTP({
				resolveHostAddresses: stubResolver({'lan.example': ['127.0.0.1'], 'media.lan.example': ['127.0.0.1']}),
			});
			await outboundHTTP.registerAnchoredOrigins({
				anchorOrigin: `http://lan.example:${port}`,
				origins: [`http://media.lan.example:${port}`],
			});
			const message = await outboundHTTP.request({
				body: null,
				expectedOrigin: `http://media.lan.example:${port}`,
				headers: null,
				method: 'GET',
				originTrust: DesktopOriginTrust.REGISTERED,
				serviceName: 'test',
				signal: null,
				timeoutMs: 2000,
				url: `http://media.lan.example:${port}/media/ok`,
			});
			assert.equal(message.status, 200);
			message.message.resume();
			outboundHTTP.cleanup();
		} finally {
			server.close();
		}
	});

	test('a private literal anchor binds and registers without a public requirement', async () => {
		const outboundHTTP = new DesktopOutboundHTTP({resolveHostAddresses: stubResolver({})});
		await outboundHTTP.registerAnchoredOrigins({anchorOrigin: 'http://192.168.1.10', origins: []});
		assert.equal(outboundHTTP.isRegisteredOrigin('http://192.168.1.10'), true);
		outboundHTTP.cleanup();
	});

	test('http is refused for a publicly resolving origin and accepted for a loopback one', async () => {
		const {port, server} = await startServer({
			'/ping': (_request, response) => sendImage(response, 'text/plain', 'ok'),
		});
		try {
			const outboundHTTP = new DesktopOutboundHTTP({
				resolveHostAddresses: stubResolver({'example.com': ['93.184.216.34'], localhost: ['127.0.0.1']}),
			});
			const client = new DesktopSelectedInstanceClient(outboundHTTP);
			await assert.rejects(
				client.fetch({expectedOrigin: 'http://example.com', timeoutMs: 500, url: 'http://example.com/ping'}),
				/requires https for the publicly routable origin/,
			);
			const loopback = await client.fetch({
				expectedOrigin: `http://localhost:${port}`,
				timeoutMs: 2000,
				url: `http://localhost:${port}/ping`,
			});
			assert.equal(loopback.status, 200);
			const literal = await client.fetch({
				expectedOrigin: `http://127.0.0.1:${port}`,
				timeoutMs: 2000,
				url: `http://127.0.0.1:${port}/ping`,
			});
			assert.equal(literal.status, 200);
			outboundHTTP.cleanup();
		} finally {
			server.close();
		}
	});

	test('cleartext transport addresses are pinned for loopback and LAN origins and refused for public ones', async () => {
		const outboundHTTP = new DesktopOutboundHTTP({
			resolveHostAddresses: stubResolver({
				'gateway.example.com': ['93.184.216.34'],
				'nas.lan': ['192.168.1.20'],
				localhost: ['::1', '127.0.0.1'],
			}),
		});
		assert.equal(await outboundHTTP.requireCleartextTransportAddress('http://localhost:8088'), '127.0.0.1');
		assert.equal(await outboundHTTP.requireCleartextTransportAddress('http://nas.lan'), '192.168.1.20');
		assert.equal(await outboundHTTP.requireCleartextTransportAddress('http://10.0.0.5:8080'), '10.0.0.5');
		await assert.rejects(
			outboundHTTP.requireCleartextTransportAddress('http://gateway.example.com'),
			/requires https for the publicly routable origin/,
		);
		await assert.rejects(
			outboundHTTP.requireCleartextTransportAddress('http://93.184.216.34'),
			/requires https for the publicly routable origin/,
		);
		outboundHTTP.cleanup();
	});

	test('a loopback host resolving to IPv6 first still reaches an IPv4 only server', async () => {
		const {port, server} = await startServer({
			'/ping': (_request, response) => sendImage(response, 'text/plain', 'ok'),
		});
		try {
			const outboundHTTP = new DesktopOutboundHTTP({
				resolveHostAddresses: async () => ['::1', '127.0.0.1'],
			});
			const client = new DesktopSelectedInstanceClient(outboundHTTP);
			const response = await client.fetch({
				expectedOrigin: `http://localdev.example:${port}`,
				timeoutMs: 2000,
				url: `http://localdev.example:${port}/ping`,
			});
			assert.equal(
				response.status,
				200,
				'macOS returns ::1 first for a loopback name and dev servers commonly bind IPv4 only, so pinning the first record makes every request fail with ECONNREFUSED',
			);
			outboundHTTP.cleanup();
		} finally {
			server.close();
		}
	});

	test('the pinned address survives a resolver that rebinds after the allow-list check', async () => {
		const {port, server} = await startServer({
			'/ping': (_request, response) => sendImage(response, 'text/plain', 'ok'),
		});
		try {
			let rebound = false;
			const outboundHTTP = new DesktopOutboundHTTP({
				resolveHostAddresses: async () => {
					if (rebound) {
						return ['203.0.113.7'];
					}
					return ['127.0.0.1'];
				},
			});
			const client = new DesktopSelectedInstanceClient(outboundHTTP);
			const first = await client.fetch({
				expectedOrigin: `http://rebind.example:${port}`,
				timeoutMs: 2000,
				url: `http://rebind.example:${port}/ping`,
			});
			assert.equal(first.status, 200);
			rebound = true;
			const second = await client.fetch({
				expectedOrigin: `http://rebind.example:${port}`,
				timeoutMs: 2000,
				url: `http://rebind.example:${port}/ping`,
			});
			assert.equal(second.status, 200);
			assert.equal(second.body.toString('utf8'), 'ok');
			outboundHTTP.cleanup();
		} finally {
			server.close();
		}
	});

	test('a pinned address that stops answering is resolved again instead of staying pinned for the process lifetime', async () => {
		const {port, server} = await startServer({});
		await new Promise((resolve) => server.close(resolve));
		let resolutions = 0;
		const outboundHTTP = new DesktopOutboundHTTP({
			resolveHostAddresses: async () => {
				resolutions += 1;
				return ['127.0.0.1'];
			},
		});
		const client = new DesktopSelectedInstanceClient(outboundHTTP);
		const request = {
			expectedOrigin: `http://moved.example:${port}`,
			timeoutMs: 2000,
			url: `http://moved.example:${port}/ping`,
		};
		await assert.rejects(client.fetch(request), /ECONNREFUSED/);
		assert.equal(resolutions, 1);
		const revived = http.createServer((_request, response) => sendImage(response, 'text/plain', 'ok'));
		await new Promise((resolve) => revived.listen(port, '127.0.0.1', resolve));
		try {
			const response = await client.fetch(request);
			assert.equal(response.status, 200);
			assert.equal(
				resolutions,
				2,
				'an address that stopped answering leaves the origin unreachable until the app restarts unless the failed binding is released',
			);
			outboundHTTP.cleanup();
		} finally {
			revived.close();
		}
	});

	test('a released binding refuses a re-resolution that changes the address scope', async () => {
		const {port, server} = await startServer({});
		await new Promise((resolve) => server.close(resolve));
		let rebound = false;
		const outboundHTTP = new DesktopOutboundHTTP({
			resolveHostAddresses: async () => {
				if (rebound) {
					return ['93.184.216.34'];
				}
				return ['127.0.0.1'];
			},
		});
		const client = new DesktopSelectedInstanceClient(outboundHTTP);
		const request = {
			expectedOrigin: `http://scope.example:${port}`,
			timeoutMs: 2000,
			url: `http://scope.example:${port}/ping`,
		};
		await assert.rejects(client.fetch(request), /ECONNREFUSED/);
		rebound = true;
		await assert.rejects(client.fetch(request), /resolved to both public and non-public addresses/);
		outboundHTTP.cleanup();
	});

	test('a chosen origin resolving to both public and non-public addresses is reached on its non-public address', async () => {
		const {port, requested, server} = await startServer({
			'/ping': (_request, response) => sendImage(response, 'text/plain', 'ok'),
		});
		try {
			const outboundHTTP = new DesktopOutboundHTTP({
				resolveHostAddresses: stubResolver({'mixed.example': ['93.184.216.34', '127.0.0.1']}),
			});
			const response = await outboundHTTP.request({
				body: null,
				expectedOrigin: `http://mixed.example:${port}`,
				headers: null,
				method: 'GET',
				originTrust: DesktopOriginTrust.BOUND,
				serviceName: 'test',
				signal: null,
				timeoutMs: 2000,
				url: `http://mixed.example:${port}/ping`,
			});
			response.message.resume();
			assert.equal(response.status, 200);
			assert.deepEqual(requested, ['/ping']);
			outboundHTTP.cleanup();
		} finally {
			server.close();
		}
	});

	test('a target on a different port than its declared origin is refused before any connection', async () => {
		const declared = await startServer({'/ping': (_request, response) => sendImage(response, 'text/plain', 'ok')});
		const other = await startServer({'/steal': (_request, response) => sendImage(response, 'text/plain', 'stolen')});
		try {
			const outboundHTTP = new DesktopOutboundHTTP({resolveHostAddresses: stubResolver({})});
			await assert.rejects(
				outboundHTTP.request({
					body: null,
					expectedOrigin: `http://127.0.0.1:${declared.port}`,
					headers: null,
					method: 'GET',
					originTrust: DesktopOriginTrust.BOUND,
					serviceName: 'test',
					signal: null,
					timeoutMs: 2000,
					url: `http://127.0.0.1:${other.port}/steal`,
				}),
				/not a canonical URL under its declared origin/,
			);
			assert.deepEqual(other.requested, [], 'a target outside the declared origin must never be contacted');
			assert.deepEqual(declared.requested, []);
			outboundHTTP.cleanup();
		} finally {
			declared.server.close();
			other.server.close();
		}
	});

	test('a keepalive socket the peer closed between requests is retried exactly once', async () => {
		let seen = 0;
		const server = http.createServer((request, response) => {
			seen += 1;
			if (seen === 2) {
				request.socket.destroy();
				return;
			}
			response.writeHead(200, {'content-type': 'text/plain'});
			response.end(`attempt-${seen}`);
		});
		await new Promise((resolve) => server.listen(0, '127.0.0.1', resolve));
		const {port} = server.address();
		try {
			const outboundHTTP = new DesktopOutboundHTTP({resolveHostAddresses: stubResolver({})});
			const send = () =>
				outboundHTTP.request({
					body: null,
					expectedOrigin: `http://127.0.0.1:${port}`,
					headers: null,
					method: 'GET',
					originTrust: DesktopOriginTrust.BOUND,
					serviceName: 'test',
					signal: null,
					timeoutMs: 5000,
					url: `http://127.0.0.1:${port}/ping`,
				});
			const readBody = async (message) =>
				(
					await readBoundedMessage({
						declaredBytes: null,
						description: 'test body',
						maxBytes: 1024,
						maxChunks: 16,
						message: message.message,
					})
				).toString('utf8');

			assert.equal(await readBody(await send()), 'attempt-1');
			assert.equal(await readBody(await send()), 'attempt-3');
			assert.equal(seen, 3);
			outboundHTTP.cleanup();
		} finally {
			server.close();
		}
	});

	test('a stale reused socket is not replayed for a request whose body cannot be replayed', async () => {
		let seen = 0;
		const server = http.createServer((request, response) => {
			seen += 1;
			if (seen === 2) {
				request.socket.destroy();
				return;
			}
			request.resume();
			request.on('end', () => {
				response.writeHead(200, {'content-type': 'text/plain'});
				response.end(`attempt-${seen}`);
			});
		});
		await new Promise((resolve) => server.listen(0, '127.0.0.1', resolve));
		const {port} = server.address();
		try {
			const outboundHTTP = new DesktopOutboundHTTP({resolveHostAddresses: stubResolver({})});
			const send = (body) =>
				outboundHTTP.request({
					body,
					expectedOrigin: `http://127.0.0.1:${port}`,
					headers: {'content-type': 'text/plain'},
					method: 'PUT',
					originTrust: DesktopOriginTrust.BOUND,
					serviceName: 'test',
					signal: null,
					timeoutMs: 5000,
					url: `http://127.0.0.1:${port}/upload`,
				});
			const streamBody = () => Readable.toWeb(Readable.from([Buffer.from('payload')]));

			const first = await send(streamBody());
			await readBoundedMessage({
				declaredBytes: null,
				description: 'test body',
				maxBytes: 1024,
				maxChunks: 16,
				message: first.message,
			});
			await assert.rejects(send(streamBody()), (error) => error.code === 'ECONNRESET');
			assert.equal(seen, 2, 'a streamed body must never be replayed onto a fresh socket');
			outboundHTTP.cleanup();
		} finally {
			server.close();
		}
	});

	test('the pinned lookup answers after its caller returns', async () => {
		const lookup = createPinnedHostLookup('pinned.example', {address: '127.0.0.1', family: 4});
		const order = [];
		const answered = new Promise((resolve) =>
			lookup('pinned.example', {all: true}, (error, addresses) => {
				order.push('answered');
				resolve({addresses, error});
			}),
		);
		order.push('returned');
		const {addresses, error} = await answered;
		assert.deepEqual(order, ['returned', 'answered']);
		assert.equal(error, null);
		assert.deepEqual(addresses, [{address: '127.0.0.1', family: 4}]);
	});

	test('a pinned address with no route fails the request instead of the process', async () => {
		const outboundHTTP = new DesktopOutboundHTTP({
			resolveHostAddresses: stubResolver({'unrouted.example': ['255.255.255.255']}),
		});
		const uncaught = [];
		const onUncaught = (error) => uncaught.push(error);
		process.on('uncaughtException', onUncaught);
		try {
			await outboundHTTP.registerAnchoredOrigins({anchorOrigin: 'https://unrouted.example', origins: []});
			await assert.rejects(
				outboundHTTP.request({
					body: null,
					expectedOrigin: 'https://unrouted.example',
					headers: null,
					method: 'GET',
					originTrust: DesktopOriginTrust.REGISTERED,
					serviceName: 'test',
					signal: null,
					timeoutMs: 2000,
					url: 'https://unrouted.example/',
				}),
				(error) => typeof error.code === 'string' && error.syscall === 'connect',
			);
			await new Promise((resolve) => setImmediate(resolve));
			assert.deepEqual(uncaught, []);
		} finally {
			process.off('uncaughtException', onUncaught);
			outboundHTTP.cleanup();
		}
	});

	test('the pinned lookup offers every validated address for happy eyeballs', async () => {
		const lookup = createPinnedHostLookup('dual.example', [
			{address: '2606:4700:4700::1111', family: 6},
			{address: '93.184.216.34', family: 4},
		]);
		const all = await new Promise((resolve) =>
			lookup('dual.example', {all: true}, (error, addresses) => resolve({addresses, error})),
		);
		assert.equal(all.error, null);
		assert.deepEqual(all.addresses, [
			{address: '2606:4700:4700::1111', family: 6},
			{address: '93.184.216.34', family: 4},
		]);
		const single = await new Promise((resolve) =>
			lookup('dual.example', {}, (error, address, family) => resolve({address, error, family})),
		);
		assert.equal(single.address, '2606:4700:4700::1111');
		assert.equal(single.family, 6);
	});

	test('a registered origin whose IPv6 address refuses connects over IPv4 without waiting', async () => {
		const {port, requested, server} = await startServer({
			'/ping': (_request, response) => sendImage(response, 'text/plain', 'ok'),
		});
		try {
			const outboundHTTP = new DesktopOutboundHTTP({
				resolveHostAddresses: stubResolver({'dual.example': ['::1', '127.0.0.1']}),
			});
			await outboundHTTP.registerAnchoredOrigins({anchorOrigin: `http://dual.example:${port}`, origins: []});
			const startedAt = Date.now();
			const response = await outboundHTTP.request({
				body: null,
				expectedOrigin: `http://dual.example:${port}`,
				headers: null,
				method: 'GET',
				originTrust: DesktopOriginTrust.REGISTERED,
				serviceName: 'test',
				signal: null,
				timeoutMs: 2000,
				url: `http://dual.example:${port}/ping`,
			});
			response.message.resume();
			assert.equal(response.status, 200, 'the refused IPv6 address must fall back to the IPv4 one like Chromium does');
			assert.ok(Date.now() - startedAt < 1000);
			assert.deepEqual(requested, ['/ping']);
			outboundHTTP.cleanup();
		} finally {
			server.close();
		}
	});

	test('a registered origin whose first address never answers is reached on the next one', async () => {
		const {port, requested, server} = await startServer({
			'/ping': (_request, response) => sendImage(response, 'text/plain', 'ok'),
		});
		try {
			const outboundHTTP = new DesktopOutboundHTTP({
				resolveHostAddresses: stubResolver({'multi.example': ['127.0.0.2', '127.0.0.1']}),
			});
			await outboundHTTP.registerAnchoredOrigins({anchorOrigin: `http://multi.example:${port}`, origins: []});
			const startedAt = Date.now();
			const response = await outboundHTTP.request({
				body: null,
				expectedOrigin: `http://multi.example:${port}`,
				headers: null,
				method: 'GET',
				originTrust: DesktopOriginTrust.REGISTERED,
				serviceName: 'test',
				signal: null,
				timeoutMs: 3000,
				url: `http://multi.example:${port}/ping`,
			});
			response.message.resume();
			assert.equal(response.status, 200, 'a dead first address must not stop the connection from trying the others');
			assert.ok(Date.now() - startedAt < 1500);
			assert.deepEqual(requested, ['/ping']);
			outboundHTTP.cleanup();
		} finally {
			server.close();
		}
	});

	test('a public requirement still refuses an answer that includes a private address', async () => {
		const outboundHTTP = new DesktopOutboundHTTP({
			resolveHostAddresses: stubResolver({'mixed.example': ['2606:4700:4700::1111', '127.0.0.1']}),
		});
		const client = new DesktopSelectedInstanceClient(outboundHTTP);
		await assert.rejects(
			client.fetch({expectedOrigin: 'https://mixed.example', timeoutMs: 2000, url: 'https://mixed.example/ping'}),
		);
		outboundHTTP.cleanup();
	});

	test('the pinned lookup refuses a hostname it was not created for', async () => {
		const lookup = createPinnedHostLookup('pinned.example', {address: '127.0.0.1', family: 4});
		const mismatch = await new Promise((resolve) => lookup('rebound.example', {}, resolve));
		assert.equal(mismatch.name, 'PinnedLookupHostnameMismatchError');
		const match = await new Promise((resolve) =>
			lookup('PINNED.example.', {}, (error, address) => resolve({address, error})),
		);
		assert.equal(match.error, null);
		assert.equal(match.address, '127.0.0.1');
	});
});

describe('DesktopSelectedInstanceClient', () => {
	test('a redirect on the instance path is refused instead of followed', async () => {
		const {port, requested, server} = await startServer({
			'/api/v1/auth/login': (_request, response) => {
				response.writeHead(302, {location: '/api/v1/elsewhere'});
				response.end();
			},
			'/api/v1/elsewhere': (_request, response) => sendImage(response, 'text/plain', 'followed'),
		});
		try {
			const outboundHTTP = new DesktopOutboundHTTP({
				resolveHostAddresses: stubResolver({'self.example': ['127.0.0.1']}),
			});
			const client = new DesktopSelectedInstanceClient(outboundHTTP);
			await assert.rejects(
				client.fetch({
					expectedOrigin: `http://self.example:${port}`,
					method: 'POST',
					timeoutMs: 2000,
					url: `http://self.example:${port}/api/v1/auth/login`,
				}),
				/was answered with an HTTP 302 redirect, which is never followed/,
			);
			assert.deepEqual(requested, ['/api/v1/auth/login']);
			outboundHTTP.cleanup();
		} finally {
			server.close();
		}
	});

	test('a foreign-origin URL throws', async () => {
		const outboundHTTP = new DesktopOutboundHTTP({resolveHostAddresses: stubResolver({'self.example': ['127.0.0.1']})});
		const client = new DesktopSelectedInstanceClient(outboundHTTP);
		await assert.rejects(
			client.fetch({
				expectedOrigin: 'http://self.example',
				timeoutMs: 500,
				url: 'http://other.example/api/v1/users/@me',
			}),
			/is not a canonical URL under its declared origin/,
		);
		outboundHTTP.cleanup();
	});

	test('a response beyond the one mebibyte bound aborts', async () => {
		const {port, server} = await startServer({
			'/big': (_request, response) => {
				response.writeHead(200, {'content-type': 'application/json'});
				const chunk = Buffer.alloc(256 * 1024, 0x61);
				for (let index = 0; index < 8; index += 1) {
					response.write(chunk);
				}
				response.end();
			},
		});
		try {
			const outboundHTTP = new DesktopOutboundHTTP({
				resolveHostAddresses: stubResolver({'self.example': ['127.0.0.1']}),
			});
			const client = new DesktopSelectedInstanceClient(outboundHTTP);
			await assert.rejects(
				client.fetch({
					expectedOrigin: `http://self.example:${port}`,
					timeoutMs: 5000,
					url: `http://self.example:${port}/big`,
				}),
				/Instance response exceeds 1048576 bytes/,
			);
			outboundHTTP.cleanup();
		} finally {
			server.close();
		}
	});
});

describe('DesktopOutboundHTTP system proxy routes', () => {
	function proxiedOutboundHTTP() {
		const sent = [];
		const resolved = [];
		const outboundHTTP = new DesktopOutboundHTTP({
			resolveHostAddresses: async (hostname) => {
				resolved.push(hostname);
				throw Object.assign(new Error(`getaddrinfo ENOTFOUND ${hostname}`), {syscall: 'getaddrinfo'});
			},
			resolveProxy: async (url) =>
				url.startsWith('https://direct.example') ? 'DIRECT' : 'PROXY 127.0.0.1:7890; DIRECT',
			sendThroughSession: async (request) => {
				sent.push({method: request.method, url: request.url.href, headers: request.headers});
				return {
					headers: {'content-type': 'image/png'},
					message: Readable.from([PNG_BYTES]),
					status: 200,
					statusText: 'OK',
				};
			},
		});
		return {outboundHTTP, resolved, sent};
	}

	test('an instance behind the system proxy is anchored and fetched without a local lookup', async () => {
		const {outboundHTTP, resolved, sent} = proxiedOutboundHTTP();
		const requirement = await outboundHTTP.registerAnchoredOrigins({
			anchorOrigin: 'https://api.blocked.example',
			origins: ['https://media.blocked.example'],
		});
		assert.equal(requirement, 'public');
		const message = await outboundHTTP.request({
			body: null,
			expectedOrigin: 'https://media.blocked.example',
			headers: {Accept: 'image/png'},
			method: 'GET',
			originTrust: DesktopOriginTrust.REGISTERED,
			serviceName: 'test',
			signal: null,
			timeoutMs: 500,
			url: 'https://media.blocked.example/avatar.png',
		});
		assert.deepEqual(
			await readBoundedMessage({
				declaredBytes: null,
				description: 'proxied',
				maxBytes: 64,
				maxChunks: 4,
				message: message.message,
			}),
			PNG_BYTES,
		);
		assert.deepEqual(resolved, []);
		assert.deepEqual(sent, [
			{method: 'GET', url: 'https://media.blocked.example/avatar.png', headers: {Accept: 'image/png'}},
		]);
		outboundHTTP.cleanup();
	});

	test('a proxied download goes through the session and a direct one keeps the pinned lookup', async () => {
		const {outboundHTTP, resolved, sent} = proxiedOutboundHTTP();
		const message = await outboundHTTP.get({
			context: 'test',
			timeoutMs: 500,
			url: new URL('https://cdn.blocked.example/icon.png'),
		});
		message.message.resume();
		await assert.rejects(
			outboundHTTP.get({context: 'test', timeoutMs: 500, url: new URL('https://direct.example/icon.png')}),
			/could not be reached/,
		);
		assert.deepEqual(
			sent.map((entry) => entry.url),
			['https://cdn.blocked.example/icon.png'],
		);
		assert.deepEqual(resolved, ['direct.example']);
		outboundHTTP.cleanup();
	});

	test('a proxied route keeps the cleartext and private address rules', async () => {
		const {outboundHTTP, sent} = proxiedOutboundHTTP();
		await outboundHTTP.registerAnchoredOrigins({
			anchorOrigin: 'https://api.blocked.example',
			origins: ['http://10.0.0.1'],
		});
		const request = (url, originTrust = DesktopOriginTrust.BOUND) => ({
			body: null,
			expectedOrigin: new URL(url).origin,
			headers: null,
			method: 'GET',
			originTrust,
			serviceName: 'test',
			signal: null,
			timeoutMs: 500,
			url,
		});
		await assert.rejects(outboundHTTP.request(request('http://plain.blocked.example/x')), /requires https/);
		await assert.rejects(
			outboundHTTP.request(request('http://10.0.0.1/x', DesktopOriginTrust.REGISTERED)),
			/cannot authorize the privately resolving origin/,
		);
		await assert.rejects(
			outboundHTTP.get({context: 'test', timeoutMs: 500, url: new URL('https://10.0.0.1/x')}),
			/could not be reached/,
		);
		assert.deepEqual(sent, []);
		outboundHTTP.cleanup();
	});
});

describe('desktop instance launch surfaces', () => {
	test('--fluxer-instance is a development-only API endpoint flag', () => {
		const originalDefaultApp = process.defaultApp;
		try {
			process.defaultApp = undefined;
			assert.equal(getLaunchInstanceEndpointOverride(['electron', '.']), null);
			assert.throws(
				() => getLaunchInstanceEndpointOverride(['--fluxer-instance', 'https://self.example/api']),
				/only accepted in a development build/,
			);
			process.defaultApp = true;
			assert.equal(
				getLaunchInstanceEndpointOverride(['--fluxer-instance', 'https://self.example/api/']),
				'https://self.example/api',
			);
			assert.equal(
				getLaunchInstanceEndpointOverride(['--fluxer-instance=https://self.example:8443/api']),
				'https://self.example:8443/api',
			);
			assert.throws(
				() => getLaunchInstanceEndpointOverride(['--fluxer-instance', '/api']),
				/absolute http\(s\) API endpoint/,
			);
			assert.throws(() => getLaunchInstanceEndpointOverride(['--fluxer-instance']), /absolute http\(s\) API endpoint/);
		} finally {
			process.defaultApp = originalDefaultApp;
		}
	});
});
