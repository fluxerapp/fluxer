// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {Buffer} from 'node:buffer';
import {createHash} from 'node:crypto';
import {existsSync, mkdirSync, mkdtempSync, readFileSync, writeFileSync} from 'node:fs';
import {rm} from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';
import {after, describe, test} from 'node:test';
import zlib from 'node:zlib';
import {installElectronStub} from './LocalAppTestSupport.test.mjs';

const BLOCK = 512;
const SHELL_VERSION = '2026.823.1';
const RELEASE_CHANNEL = 'canary';
const SOURCE_SHA = 'a'.repeat(40);
const LOCAL_APP_SCHEME = 'fluxer-app';
const IMMUTABLE_CACHE_CONTROL = 'public, max-age=31536000, immutable';
const MODULE_ONLY_ASSET = 'assets/deadbeefcafebabe.js';
const MODULE_ONLY_BODY = 'module-store renderer chunk';
const ASAR_ONLY_ASSET = 'assets/00000000feedface.js';
const ASAR_ONLY_BODY = 'asar renderer chunk';
const GRAMMAR_ONLY_ASSET = 'assets/grammars/rust.wasm';
const GRAMMAR_ONLY_BODY = 'on-demand rust grammar';

const temporaryRoots = [];

function createRoot(prefix) {
	const root = mkdtempSync(path.join(os.tmpdir(), prefix));
	temporaryRoots.push(root);
	return root;
}

after(async () => {
	for (const root of temporaryRoots) {
		await rm(root, {recursive: true, force: true});
	}
});

const appDataRoot = createRoot('local-app-module-appdata-');
const appPathRoot = createRoot('local-app-module-asar-');
const rendererRoot = path.join(appPathRoot, 'dist', 'renderer');
mkdirSync(path.join(rendererRoot, 'assets'), {recursive: true});
writeFileSync(path.join(rendererRoot, 'index.html'), '<html lang="en"><head></head><body>asar</body></html>');
writeFileSync(path.join(rendererRoot, ASAR_ONLY_ASSET), ASAR_ONLY_BODY);

const protocolHandlers = new Map();
const electronPaths = new Map([['appData', appDataRoot]]);

const registry = installElectronStub({
	app: {
		isReady: () => true,
		getAppPath: () => appPathRoot,
		getPath: (name) => electronPaths.get(name) ?? path.join(appDataRoot, name),
		setPath: (name, value) => electronPaths.set(name, value),
		getVersion: () => SHELL_VERSION,
	},
	protocol: {
		registerSchemesAsPrivileged: () => undefined,
		handle: (scheme, handler) => {
			protocolHandlers.set(scheme, handler);
		},
		unhandle: (scheme) => {
			protocolHandlers.delete(scheme);
		},
		isProtocolHandled: (scheme) => protocolHandlers.has(scheme),
	},
});

const {configureUserDataPath} = await import('@electron/common/UserDataPath');
const {getModuleStoreRoot, getModuleStoreTreeRoot, ModuleStore} = await import('@electron/main/ModuleStore');
const {setCommittedModuleFiles} = await import('@electron/main/ModuleBootHandoff');
const {createOnDemandModuleInstaller} = await import('@electron/main/ModuleOnDemand');
const {getDesktopLocalAppAuthorization} = await import('@electron/main/LocalAppProtocolAuthorization');
const LOCAL_APP_PROTOCOL_MODULE_URL = new URL('./LocalAppProtocol.ts', import.meta.url).href;
const mainAppProtocol = await import(LOCAL_APP_PROTOCOL_MODULE_URL);
const bundledOnlyProtocol = await import(`${LOCAL_APP_PROTOCOL_MODULE_URL}?instance=bundled-only`);

function sha256(data) {
	return createHash('sha256').update(data).digest('hex');
}

function octal(value, width) {
	return `${value.toString(8).padStart(width - 1, '0')}\0`;
}

function ustarHeader(name, size) {
	const header = Buffer.alloc(BLOCK);
	header.write(name, 0, 100, 'utf8');
	header.write(octal(0o644, 8), 100, 8, 'ascii');
	header.write(octal(0, 8), 108, 8, 'ascii');
	header.write(octal(0, 8), 116, 8, 'ascii');
	header.write(octal(size, 12), 124, 12, 'ascii');
	header.write(octal(0, 12), 136, 12, 'ascii');
	header.write('0', 156, 1, 'ascii');
	header.write('ustar\0', 257, 6, 'ascii');
	header.write('00', 263, 2, 'ascii');
	header.fill(0x20, 148, 156);
	let sum = 0;
	for (const byte of header) {
		sum += byte;
	}
	header.write(octal(sum, 8), 148, 8, 'ascii');
	return header;
}

function pad(size) {
	const remainder = size % BLOCK;
	return remainder === 0 ? Buffer.alloc(0) : Buffer.alloc(BLOCK - remainder);
}

function packModule(moduleName, entries) {
	const files = entries.map(([relative, body]) => ({
		path: relative,
		sha256: sha256(Buffer.from(body, 'utf8')),
		bytes: Buffer.byteLength(body, 'utf8'),
	}));
	const manifest = Buffer.from(
		`${JSON.stringify(
			{
				module: moduleName,
				build_version: SHELL_VERSION,
				release_channel: RELEASE_CHANNEL,
				source_sha: SOURCE_SHA,
				files,
			},
			null,
			'\t',
		)}\n`,
		'utf8',
	);
	const chunks = [ustarHeader('module.json', manifest.length), manifest, pad(manifest.length)];
	for (const [relative, body] of entries) {
		const payload = Buffer.from(body, 'utf8');
		chunks.push(ustarHeader(`files/${relative}`, payload.length), payload, pad(payload.length));
	}
	chunks.push(Buffer.alloc(BLOCK * 2));
	const tar = Buffer.concat(chunks);
	const packed = zlib.brotliCompressSync(tar, {
		params: {[zlib.constants.BROTLI_PARAM_QUALITY]: 5, [zlib.constants.BROTLI_PARAM_SIZE_HINT]: tar.length},
	});
	return {module: moduleName, packed, sha256: sha256(packed)};
}

function streamOf(buffer) {
	return {
		offset: 0,
		chunks: (async function* stream() {
			for (let offset = 0; offset < buffer.length; offset += 4096) {
				yield buffer.subarray(offset, Math.min(offset + 4096, buffer.length));
			}
		})(),
	};
}

const userData = configureUserDataPath();
const authorization = getDesktopLocalAppAuthorization();

const rendererWebContents = {id: 1, isDestroyed: () => false, once: () => undefined};
const rendererFrame = {
	processId: 1,
	routingId: 1,
	url: `${LOCAL_APP_SCHEME}://app/`,
	origin: `${LOCAL_APP_SCHEME}://app`,
	detached: false,
	parent: null,
	isDestroyed: () => false,
	owner: rendererWebContents,
};
rendererFrame.top = rendererFrame;
rendererWebContents.mainFrame = rendererFrame;
registry.contents.set(rendererWebContents.id, rendererWebContents);
registry.frames.set(`${rendererFrame.processId}:${rendererFrame.routingId}`, rendererFrame);
authorization.authorize(rendererWebContents);

function requestAsset(pathname) {
	const url = `${LOCAL_APP_SCHEME}://app/${pathname}`;
	const headers = authorization.applyRequestHeaders(
		{url, resourceType: 'script', frame: rendererFrame, webContentsId: rendererWebContents.id},
		{},
	);
	const handler = protocolHandlers.get(LOCAL_APP_SCHEME);
	assert.ok(handler != null, 'no protocol handler is registered');
	return handler(new Request(url, {headers}));
}

async function commitRendererModule() {
	const store = await ModuleStore.open({
		root: getModuleStoreRoot(userData.base),
		shellVersion: SHELL_VERSION,
		releaseChannel: RELEASE_CHANNEL,
	});
	const packaged = packModule('fluxer_renderer', [
		[MODULE_ONLY_ASSET, MODULE_ONLY_BODY],
		['index.html', '<html lang="en"><head></head><body>module</body></html>'],
	]);
	await store.installModule({
		module: packaged.module,
		sha256: packaged.sha256,
		download: () => streamOf(packaged.packed),
	});
	await store.commit({fluxer_renderer: packaged.sha256});
	return {store, sha256: packaged.sha256};
}

describe('the committed module index reaches the renderer through the real protocol handler', () => {
	test('with nothing committed the protocol serves the offline renderer untouched, the only path an offline build takes', async () => {
		const bundledOnly = bundledOnlyProtocol.getDesktopLocalAppProtocol();
		bundledOnly.register();
		const response = await requestAsset(ASAR_ONLY_ASSET);
		assert.equal(response.status, 200);
		assert.equal(await response.text(), ASAR_ONLY_BODY);
		const missing = await requestAsset(MODULE_ONLY_ASSET);
		assert.equal(missing.status, 404);
		await bundledOnlyProtocol.cleanupDesktopLocalAppProtocol();
		assert.equal(protocolHandlers.has(LOCAL_APP_SCHEME), false);
	});

	test('a committed module asset is served from the store with an immutable cache policy', async () => {
		const {store, sha256: digest} = await commitRendererModule();
		assert.equal(store.storeRoot, getModuleStoreTreeRoot(userData.base));

		setCommittedModuleFiles(store.storeRoot, await store.buildModuleIndex());

		mainAppProtocol.getDesktopLocalAppProtocol().register();

		const storePath = path.join(store.storeRoot, 'fluxer_renderer', digest, MODULE_ONLY_ASSET);
		assert.equal(readFileSync(storePath, 'utf8'), MODULE_ONLY_BODY);
		assert.equal(existsSync(path.join(rendererRoot, MODULE_ONLY_ASSET)), false);

		const response = await requestAsset(MODULE_ONLY_ASSET);
		assert.equal(response.status, 200);
		assert.equal(response.headers.get('Cache-Control'), IMMUTABLE_CACHE_CONTROL);
		assert.equal(response.headers.get('Content-Type'), 'application/javascript; charset=utf-8');
		assert.equal(response.headers.get('Content-Length'), String(Buffer.byteLength(MODULE_ONLY_BODY)));
		assert.equal(await response.text(), MODULE_ONLY_BODY);
	});

	test('an asset the module does not carry falls through to the offline renderer, which a modules build does not have', async () => {
		const response = await requestAsset(ASAR_ONLY_ASSET);
		assert.equal(response.status, 200);
		assert.equal(await response.text(), ASAR_ONLY_BODY);
	});

	test('a module committed after the handler registered is served on the very next request', async () => {
		const missing = await requestAsset(GRAMMAR_ONLY_ASSET);
		assert.equal(missing.status, 404);

		const store = await ModuleStore.open({
			root: getModuleStoreRoot(userData.base),
			shellVersion: SHELL_VERSION,
			releaseChannel: RELEASE_CHANNEL,
		});
		const packaged = packModule('fluxer_grammars', [[GRAMMAR_ONLY_ASSET, GRAMMAR_ONLY_BODY]]);
		const install = createOnDemandModuleInstaller({
			ensure: async (moduleName) => {
				await store.installModule({
					module: moduleName,
					sha256: packaged.sha256,
					download: () => streamOf(packaged.packed),
				});
				await store.commit({...store.getCommitted(), [moduleName]: packaged.sha256});
				return {module: moduleName, status: 'installed'};
			},
			refresh: async () => {
				setCommittedModuleFiles(store.storeRoot, await store.buildModuleIndex());
			},
		});

		assert.deepEqual(await install('fluxer_grammars'), {module: 'fluxer_grammars', status: 'installed'});
		assert.equal(existsSync(path.join(rendererRoot, GRAMMAR_ONLY_ASSET)), false);

		const response = await requestAsset(GRAMMAR_ONLY_ASSET);
		assert.equal(response.status, 200);
		assert.equal(response.headers.get('Content-Type'), 'application/wasm');
		assert.equal(await response.text(), GRAMMAR_ONLY_BODY);

		const renderer = await requestAsset(MODULE_ONLY_ASSET);
		assert.equal(renderer.status, 200);
		assert.equal(await renderer.text(), MODULE_ONLY_BODY);
	});

	test('a background activation keeps serving the running build until the renderer reloads into the new one', async () => {
		const store = await ModuleStore.open({
			root: getModuleStoreRoot(userData.base),
			shellVersion: SHELL_VERSION,
			releaseChannel: RELEASE_CHANNEL,
		});
		const live = {...store.getCommitted()};
		const nextAsset = 'assets/next-renderer.js';
		const packaged = packModule('fluxer_renderer', [
			[nextAsset, 'next renderer'],
			['index.html', '<html lang="en"><head></head><body>next</body></html>'],
		]);
		await store.installModule({
			module: packaged.module,
			sha256: packaged.sha256,
			download: () => streamOf(packaged.packed),
		});

		const activated = await store.activateMergedForRendererReload({fluxer_renderer: packaged.sha256});
		assert.notEqual(activated, null);
		assert.equal(store.getCommitted().fluxer_renderer, packaged.sha256);

		setCommittedModuleFiles(store.storeRoot, await store.buildModuleIndex(live));
		const running = await requestAsset(MODULE_ONLY_ASSET);
		assert.equal(running.status, 200);
		assert.equal(await running.text(), MODULE_ONLY_BODY);
		assert.equal((await requestAsset(nextAsset)).status, 404);

		setCommittedModuleFiles(store.storeRoot, await store.buildModuleIndex(activated.committed));
		const reloaded = await requestAsset(nextAsset);
		assert.equal(reloaded.status, 200);
		assert.equal(await reloaded.text(), 'next renderer');
		assert.equal((await requestAsset(MODULE_ONLY_ASSET)).status, 404);
	});
});
