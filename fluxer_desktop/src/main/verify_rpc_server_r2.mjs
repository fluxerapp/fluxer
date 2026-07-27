import assert from 'node:assert';
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

const transformedSource = ts.transpileModule(rawSource, {
	compilerOptions: {
		module: ts.ModuleKind.CommonJS,
		target: ts.ScriptTarget.ES2022
	}
}).outputText;

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

const { startRpcServer, stopRpcServer } = loadRpcServerModule();

const PORT = 21863;

const makeRequest = (options, postData) => {
	return new Promise((resolve, reject) => {
		const req = http.request({
			host: '127.0.0.1',
			port: PORT,
			...options
		}, (res) => {
			let body = '';
			res.on('data', (chunk) => body += chunk);
			res.on('end', () => resolve({ statusCode: res.statusCode, headers: res.headers, body }));
		});
		req.on('error', reject);
		if (postData !== undefined) {
			req.write(typeof postData === 'string' ? postData : JSON.stringify(postData));
		}
		req.end();
	});
};

describe('RpcServer R2 Verification Suite', () => {
	test('1. Server Lifecycle & GET /health Endpoint', async () => {
		await startRpcServer();

		const res = await makeRequest({ path: '/health', method: 'GET' });
		assert.equal(res.statusCode, 200);
		const json = JSON.parse(res.body);
		assert.equal(json.success, true);
		assert.equal(json.data.status, 'ok');
		assert.equal(json.data.has_activity, false);
	});

	test('2. Authorization Checks (Loopback IP, Origin & Referer Validation)', async () => {
		// Allowed origin
		const res1 = await makeRequest({
			path: '/health',
			method: 'GET',
			headers: { 'Origin': 'https://web.fluxer.app' }
		});
		assert.equal(res1.statusCode, 200);

		// Allowed canary origin
		const res2 = await makeRequest({
			path: '/health',
			method: 'GET',
			headers: { 'Origin': 'https://web.canary.fluxer.app' }
		});
		assert.equal(res2.statusCode, 200);

		// Disallowed origin
		const res3 = await makeRequest({
			path: '/health',
			method: 'GET',
			headers: { 'Origin': 'https://malicious-attacker.com' }
		});
		assert.equal(res3.statusCode, 403);

		// Disallowed referer
		const res4 = await makeRequest({
			path: '/health',
			method: 'GET',
			headers: { 'Referer': 'https://evil.com/phishing' }
		});
		assert.equal(res4.statusCode, 403);

		// Mismatched origin and referer
		const res5 = await makeRequest({
			path: '/health',
			method: 'GET',
			headers: {
				'Origin': 'https://web.fluxer.app',
				'Referer': 'https://malicious-site.com/app'
			}
		});
		assert.equal(res5.statusCode, 403);
	});

	test('3. Discord-compatible SET_ACTIVITY Commands & POST /activity Requests', async () => {
		let lastActivityUpdate = null;
		const onActivityUpdate = (act) => {
			lastActivityUpdate = act;
		};
		mockMainWindow.webContents.on('rpc-activity-update', onActivityUpdate);

		// Format 1: Direct POST /activity payload with activity field
		const activity1 = { name: 'Cyberpunk 2077', details: 'Night City', state: 'In Game' };
		const res1 = await makeRequest({
			path: '/activity',
			method: 'POST',
			headers: { 'Content-Type': 'application/json' }
		}, { activity: activity1 });

		assert.equal(res1.statusCode, 200);
		const json1 = JSON.parse(res1.body);
		assert.equal(json1.success, true);
		assert.deepEqual(json1.data.activity, activity1);
		assert.deepEqual(lastActivityUpdate, activity1);

		// Format 2: Discord RPC cmd: SET_ACTIVITY with args.activity
		const activity2 = { name: 'Spotify', details: 'Listening to Synthwave' };
		const res2 = await makeRequest({
			path: '/rpc',
			method: 'POST',
			headers: { 'Content-Type': 'application/json' }
		}, { cmd: 'SET_ACTIVITY', args: { activity: activity2 } });

		assert.equal(res2.statusCode, 200);
		const json2 = JSON.parse(res2.body);
		assert.equal(json2.success, true);
		assert.deepEqual(json2.data.activity, activity2);
		assert.deepEqual(lastActivityUpdate, activity2);

		// Format 3: Discord RPC method: SET_ACTIVITY with params.activity
		const activity3 = { name: 'Elden Ring', details: 'Shadow of the Erdtree' };
		const res3 = await makeRequest({
			path: '/activity',
			method: 'POST',
			headers: { 'Content-Type': 'application/json' }
		}, { method: 'SET_ACTIVITY', params: { activity: activity3 } });

		assert.equal(res3.statusCode, 200);
		const json3 = JSON.parse(res3.body);
		assert.equal(json3.success, true);
		assert.deepEqual(json3.data.activity, activity3);
		assert.deepEqual(lastActivityUpdate, activity3);

		// Format 4: Direct POST /activity with raw activity body
		const activity4 = { name: 'Minecraft', state: 'Building' };
		const res4 = await makeRequest({
			path: '/activity',
			method: 'POST',
			headers: { 'Content-Type': 'application/json' }
		}, activity4);

		assert.equal(res4.statusCode, 200);
		const json4 = JSON.parse(res4.body);
		assert.equal(json4.success, true);
		assert.deepEqual(json4.data.activity, activity4);
		assert.deepEqual(lastActivityUpdate, activity4);

		mockMainWindow.webContents.removeListener('rpc-activity-update', onActivityUpdate);
	});

	test('4. Clear Activity Behavior (activity: null)', async () => {
		let lastActivityUpdate = 'initial';
		const onActivityUpdate = (act) => {
			lastActivityUpdate = act;
		};
		mockMainWindow.webContents.on('rpc-activity-update', onActivityUpdate);

		const res = await makeRequest({
			path: '/activity',
			method: 'POST',
			headers: { 'Content-Type': 'application/json' }
		}, { activity: null });

		assert.equal(res.statusCode, 200);
		const json = JSON.parse(res.body);
		assert.equal(json.success, true);
		assert.equal(json.data.activity, null);
		assert.equal(lastActivityUpdate, null);

		// Verify /health reports no activity
		const healthRes = await makeRequest({ path: '/health', method: 'GET' });
		const healthJson = JSON.parse(healthRes.body);
		assert.equal(healthJson.data.has_activity, false);

		mockMainWindow.webContents.removeListener('rpc-activity-update', onActivityUpdate);
	});

	test('5. Client Disconnect Presence Clearing', async () => {
		let lastActivityUpdate = 'initial';
		const onActivityUpdate = (act) => {
			lastActivityUpdate = act;
		};
		mockMainWindow.webContents.on('rpc-activity-update', onActivityUpdate);

		const activity = { name: 'VALORANT', state: 'Competitive' };
		const keepAliveAgent = new http.Agent({ keepAlive: true });

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
				res.on('end', () => {
					setTimeout(() => {
						req.socket.destroy();
						setTimeout(resolve, 100);
					}, 100);
				});
			});
			req.on('error', reject);
			req.write(JSON.stringify({ activity }));
			req.end();
		});

		// Verify presence was cleared on disconnect
		assert.equal(lastActivityUpdate, null);

		const healthRes = await makeRequest({ path: '/health', method: 'GET' });
		const healthJson = JSON.parse(healthRes.body);
		assert.equal(healthJson.data.has_activity, false);

		keepAliveAgent.destroy();
		mockMainWindow.webContents.removeListener('rpc-activity-update', onActivityUpdate);
	});

	test('6. Server Cleanup on Stop', async () => {
		await stopRpcServer();
	});
});
