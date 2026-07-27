import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import http from 'node:http';
import { createRequire } from 'node:module';
import { describe, test } from 'node:test';
import { fileURLToPath } from 'node:url';
import vm from 'node:vm';
import { EventEmitter } from 'node:events';

const require = createRequire(import.meta.url);
const ts = require('/home/antigravity/fkill/node_modules/typescript');

const sourcePath = fileURLToPath(new URL('./RpcServer.ts', import.meta.url));
const rawSource = readFileSync(sourcePath, 'utf8');

let transformedSource = ts.transpileModule(rawSource, {
	compilerOptions: {
		module: ts.ModuleKind.CommonJS,
		target: ts.ScriptTarget.ES2022
	}
}).outputText;

transformedSource += '\nexports._getCurrentActivity = () => currentActivity;\nexports._requestHandler = requestHandler;\n';

class MockWebContents extends EventEmitter {
	send(channel, ...args) {
		this.emit(channel, ...args);
	}
}

class MockMainWindow {
	constructor() {
		this.webContents = new MockWebContents();
		this.destroyed = false;
	}
	isDestroyed() {
		return this.destroyed;
	}
}

const mockMainWindow = new MockMainWindow();

function loadRpcServerModule() {
	const module = { exports: {} };

	const requireMock = (id) => {
		if (id === 'node:http' || id === 'http') return require('node:http');
		if (id === 'node:net' || id === 'net') return require('node:net');
		if (id === '@electron/common/BuildChannel') return { BUILD_CHANNEL: 'stable' };
		if (id === '@electron/common/BuildVariant') return { DESKTOP_BUILD_VARIANT: 'default' };
		if (id === '@electron/common/Constants') return {
			STABLE_APP_URL: 'https://web.fluxer.app',
			CANARY_APP_URL: 'https://web.canary.fluxer.app'
		};
		if (id === '@electron/common/DesktopConfig') return { getCustomAppUrl: () => null };
		if (id === '@electron/main/Window') return {
			getMainWindow: () => mockMainWindow,
			showWindow: () => {}
		};
		if (id === 'electron') return { app: { getVersion: () => '1.0.0' } };
		if (id === 'electron-log') return {
			info: () => {},
			warn: () => {},
			error: () => {}
		};
		return require(id);
	};

	const context = vm.createContext({
		module,
		exports: module.exports,
		require: requireMock,
		mockMainWindow,
		console,
		process,
		Buffer,
		setTimeout,
		clearTimeout,
		Promise
	});

	vm.runInContext(transformedSource, context, { filename: sourcePath });
	return module.exports;
}

const rpcServer = loadRpcServerModule();
const { startRpcServer, stopRpcServer, _getCurrentActivity, _requestHandler } = rpcServer;

const PORT = 21863;

const makeRequest = (options, postData, rawData) => {
	return new Promise((resolve, reject) => {
		const req = http.request({
			host: '127.0.0.1',
			port: PORT,
			...options
		}, (res) => {
			let body = '';
			res.on('data', (chunk) => body += chunk);
			res.on('end', () => resolve({ statusCode: res.statusCode, headers: res.headers, body, reqSocket: req.socket }));
		});
		req.on('error', reject);
		if (rawData !== undefined) {
			req.write(rawData);
		} else if (postData !== undefined) {
			req.write(typeof postData === 'string' ? postData : JSON.stringify(postData));
		}
		req.end();
	});
};

describe('RpcServer P4 Empirical Stress Test Suite', () => {

	test('0. Setup Server', async () => {
		await startRpcServer();
	});

	describe('Category 1: Unauthorized External IPs', () => {
		test('1.1 Remote socket non-loopback IP (e.g. 192.168.1.100, 10.0.0.1, 8.8.8.8)', async () => {
			const mockReq = { socket: { remoteAddress: '192.168.1.100' }, headers: {} };
			let statusCode = null;
			const mockRes = {
				writeHead: (code) => { statusCode = code; },
				end: () => {}
			};
			await _requestHandler(mockReq, mockRes);
			assert.equal(statusCode, 403, 'External IP 192.168.1.100 must be rejected with HTTP 403');
		});

		test('1.2 Remote socket undefined or null IP', async () => {
			const mockReq = { socket: { remoteAddress: undefined }, headers: {} };
			let statusCode = null;
			const mockRes = {
				writeHead: (code) => { statusCode = code; },
				end: () => {}
			};
			await _requestHandler(mockReq, mockRes);
			assert.equal(statusCode, 403, 'Undefined remoteAddress must be rejected with HTTP 403');
		});

		test('1.3 X-Forwarded-For header spoofing from loopback socket', async () => {
			const res = await makeRequest({
				path: '/health',
				method: 'GET',
				headers: { 'X-Forwarded-For': '8.8.8.8' }
			});
			assert.equal(res.statusCode, 200, 'Loopback connection with X-Forwarded-For header should use socket IP and pass');
		});

		test('1.4 External socket sending X-Forwarded-For: 127.0.0.1', async () => {
			const mockReq = { socket: { remoteAddress: '203.0.113.5' }, headers: { 'x-forwarded-for': '127.0.0.1' } };
			let statusCode = null;
			const mockRes = {
				writeHead: (code) => { statusCode = code; },
				end: () => {}
			};
			await _requestHandler(mockReq, mockRes);
			assert.equal(statusCode, 403, 'X-Forwarded-For spoofing from non-loopback IP must be rejected with HTTP 403');
		});
	});

	describe('Category 2: Invalid Origin & Referer Headers', () => {
		test('2.1 Disallowed Origin header (https://evil.com)', async () => {
			const res = await makeRequest({
				path: '/activity',
				method: 'POST',
				headers: { 'Origin': 'https://evil.com', 'Content-Type': 'application/json' }
			}, { activity: { name: 'Hack Game' } });
			assert.equal(res.statusCode, 403);
		});

		test('2.2 Disallowed Origin header (null or file://)', async () => {
			const res1 = await makeRequest({
				path: '/health',
				method: 'GET',
				headers: { 'Origin': 'null' }
			});
			assert.equal(res1.statusCode, 403);

			const res2 = await makeRequest({
				path: '/health',
				method: 'GET',
				headers: { 'Origin': 'file://' }
			});
			assert.equal(res2.statusCode, 403);
		});

		test('2.3 Referer Subdomain/Suffix Prefix Bypass Flaw (https://web.fluxer.app.attacker.com)', async () => {
			const res = await makeRequest({
				path: '/health',
				method: 'GET',
				headers: { 'Referer': 'https://web.fluxer.app.attacker.com/exploit' }
			});

			console.log('   [EMPIRICAL OBSERVATION] Referer prefix bypass test status:', res.statusCode);
			if (res.statusCode === 200) {
				console.log('   🚨 VULNERABILITY DETECTED: Referer prefix matching allowed malicious domain https://web.fluxer.app.attacker.com!');
			}
		});

		test('2.4 Referer mismatch with allowed Origin', async () => {
			const res = await makeRequest({
				path: '/health',
				method: 'GET',
				headers: {
					'Origin': 'https://web.fluxer.app',
					'Referer': 'https://web.canary.fluxer.app/page'
				}
			});
			assert.equal(res.statusCode, 403, 'Mismatch between Origin and Referer must return HTTP 403');
		});
	});

	describe('Category 3: Malformed JSON Bodies', () => {
		test('3.1 Truncated / Invalid JSON Syntax on POST /activity', async () => {
			await makeRequest({
				path: '/activity',
				method: 'POST',
				headers: { 'Content-Type': 'application/json' }
			}, { activity: { name: 'Valid Game' } });

			const res = await makeRequest({
				path: '/activity',
				method: 'POST',
				headers: { 'Content-Type': 'application/json' }
			}, undefined, '{"cmd": "SET_ACTIVITY", "args": {');

			console.log('   [EMPIRICAL OBSERVATION] Truncated JSON response code:', res.statusCode, 'body:', res.body);
			console.log('   [EMPIRICAL OBSERVATION] currentActivity after malformed JSON:', _getCurrentActivity());
			
			if (res.statusCode === 200 && _getCurrentActivity() === null) {
				console.log('   🚨 BUG DETECTED: Malformed JSON syntax cleared active presence and returned HTTP 200 OK instead of HTTP 400!');
			}
		});

		test('3.2 JSON Primitive Types (String, Number, Boolean)', async () => {
			const resString = await makeRequest({
				path: '/activity',
				method: 'POST',
				headers: { 'Content-Type': 'application/json' }
			}, undefined, '"hello"');
			console.log('   [EMPIRICAL OBSERVATION] JSON String payload status:', resString.statusCode, 'body:', resString.body);

			const resNum = await makeRequest({
				path: '/activity',
				method: 'POST',
				headers: { 'Content-Type': 'application/json' }
			}, undefined, '12345');
			console.log('   [EMPIRICAL OBSERVATION] JSON Number payload status:', resNum.statusCode, 'body:', resNum.body);

			const resBool = await makeRequest({
				path: '/activity',
				method: 'POST',
				headers: { 'Content-Type': 'application/json' }
			}, undefined, 'true');
			console.log('   [EMPIRICAL OBSERVATION] JSON Boolean payload status:', resBool.statusCode, 'body:', resBool.body);

			if (resString.statusCode === 500 || resNum.statusCode === 500 || resBool.statusCode === 500) {
				console.log('   🚨 BUG DETECTED: JSON primitives cause uncaught TypeError in extractActivity resulting in HTTP 500!');
			}
		});

		test('3.3 Empty Body POST request', async () => {
			const res = await makeRequest({
				path: '/activity',
				method: 'POST',
				headers: { 'Content-Type': 'application/json' }
			}, undefined, '');
			assert.equal(res.statusCode, 200);
			const json = JSON.parse(res.body);
			assert.equal(json.data.activity, null);
		});
	});

	describe('Category 4: Payload Size Limits (>1MB)', () => {
		test('4.1 Payload exceeding 1MB (1.5MB JSON)', async () => {
			const largeString = 'a'.repeat(1.5 * 1024 * 1024);
			const largeJson = JSON.stringify({ activity: { name: largeString } });

			const res = await makeRequest({
				path: '/activity',
				method: 'POST',
				headers: { 'Content-Type': 'application/json' }
			}, undefined, largeJson);

			console.log('   [EMPIRICAL OBSERVATION] >1MB Payload response status:', res.statusCode, 'body:', res.body);

			if (res.statusCode === 200) {
				console.log('   🚨 BUG DETECTED: Payload >1MB returned HTTP 200 OK with null activity instead of HTTP 413 Payload Too Large!');
			}
		});

		test('4.2 Multi-chunk payload >1MB stream behavior', async () => {
			const chunk1 = 'a'.repeat(600 * 1024);
			const chunk2 = 'b'.repeat(600 * 1024);

			const res = await new Promise((resolve, reject) => {
				const req = http.request({
					host: '127.0.0.1',
					port: PORT,
					path: '/activity',
					method: 'POST',
					headers: { 'Content-Type': 'application/json' }
				}, (res) => {
					let body = '';
					res.on('data', (c) => body += c);
					res.on('end', () => resolve({ statusCode: res.statusCode, body }));
				});
				req.on('error', reject);
				req.write(chunk1);
				setTimeout(() => {
					req.write(chunk2);
					req.end();
				}, 50);
			});

			console.log('   [EMPIRICAL OBSERVATION] Multi-chunk >1MB stream response status:', res.statusCode, 'body:', res.body);
		});
	});

	describe('Category 5: Client Disconnect Cleanup & Listener Leaks', () => {
		test('5.1 Short HTTP Connection Disconnect (<50ms after response finish)', async () => {
			let lastActivityUpdate = 'initial';
			const listener = (act) => { lastActivityUpdate = act; };
			mockMainWindow.webContents.on('rpc-activity-update', listener);

			const activity = { name: 'Fast Disconnect Game' };
			const reqRes = await makeRequest({
				path: '/activity',
				method: 'POST',
				headers: { 'Content-Type': 'application/json' }
			}, { activity });

			assert.equal(reqRes.statusCode, 200);
			assert.equal(lastActivityUpdate.name, activity.name);

			reqRes.reqSocket.destroy();
			await new Promise((r) => setTimeout(r, 100));

			console.log('   [EMPIRICAL OBSERVATION] Activity after socket destroy within 5ms of response finish:', lastActivityUpdate.name);
			console.log('   [EMPIRICAL OBSERVATION] currentActivity in server state:', _getCurrentActivity() != null);

			mockMainWindow.webContents.removeListener('rpc-activity-update', listener);
		});

		test('5.2 Keep-Alive TCP Socket Listener Accumulation (EventEmitter Leak)', async () => {
			const keepAliveAgent = new http.Agent({ keepAlive: true });
			let socketRef = null;

			for (let i = 0; i < 15; i++) {
				await new Promise((resolve, reject) => {
					const req = http.request({
						host: '127.0.0.1',
						port: PORT,
						path: '/activity',
						method: 'POST',
						agent: keepAliveAgent,
						headers: { 'Content-Type': 'application/json' }
					}, (res) => {
						res.on('data', () => {});
						res.on('end', () => resolve());
					});
					req.on('error', reject);
					if (i === 0) {
						req.on('socket', (sock) => { socketRef = sock; });
					}
					req.write(JSON.stringify({ activity: { name: `KeepAlive Activity ${i}` } }));
					req.end();
				});
			}

			if (socketRef) {
				const closeListeners = socketRef.listenerCount('close');
				console.log('   [EMPIRICAL OBSERVATION] Number of close listeners on Keep-Alive socket after 15 requests:', closeListeners);
				if (closeListeners >= 15) {
					console.log('   🚨 MEMORY LEAK DETECTED: Each request on persistent Keep-Alive socket adds a new close listener without cleanup!');
				}
			}

			keepAliveAgent.destroy();
		});

		test('5.3 Socket Overwrite: Client A vs Client B disconnect', async () => {
			let lastActivityUpdate = 'initial';
			const listener = (act) => { lastActivityUpdate = act; };
			mockMainWindow.webContents.on('rpc-activity-update', listener);

			const agent1 = new http.Agent({ keepAlive: true });
			const agent2 = new http.Agent({ keepAlive: true });

			let sock1 = null;
			await new Promise((resolve) => {
				const req = http.request({
					host: '127.0.0.1',
					port: PORT,
					path: '/activity',
					method: 'POST',
					agent: agent1,
					headers: { 'Content-Type': 'application/json' }
				}, (res) => {
					res.on('data', () => {});
					res.on('end', resolve);
				});
				req.on('socket', (s) => sock1 = s);
				req.write(JSON.stringify({ activity: { name: 'Activity A' } }));
				req.end();
			});

			let sock2 = null;
			await new Promise((resolve) => {
				const req = http.request({
					host: '127.0.0.1',
					port: PORT,
					path: '/activity',
					method: 'POST',
					agent: agent2,
					headers: { 'Content-Type': 'application/json' }
				}, (res) => {
					res.on('data', () => {});
					res.on('end', resolve);
				});
				req.on('socket', (s) => sock2 = s);
				req.write(JSON.stringify({ activity: { name: 'Activity B' } }));
				req.end();
			});

			assert.equal(_getCurrentActivity().name, 'Activity B');

			sock1.destroy();
			await new Promise((r) => setTimeout(r, 100));

			assert.equal(_getCurrentActivity().name, 'Activity B', 'Disconnecting Client 1 socket must not clear Client 2 activity');

			sock2.destroy();
			await new Promise((r) => setTimeout(r, 100));

			assert.equal(_getCurrentActivity(), null, 'Disconnecting active Client 2 socket must clear activity');

			agent1.destroy();
			agent2.destroy();
			mockMainWindow.webContents.removeListener('rpc-activity-update', listener);
		});

		test('5.4 Server Stop Cleanup', async () => {
			await stopRpcServer();
			assert.equal(_getCurrentActivity(), null);
		});
	});
});
