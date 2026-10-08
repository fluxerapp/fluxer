// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {Buffer} from 'node:buffer';
import {createHash} from 'node:crypto';
import {mkdirSync, mkdtempSync, writeFileSync} from 'node:fs';
import {rm} from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';
import {after, describe, test} from 'node:test';
import './LocalAppTestSupport.test.mjs';

const {ModuleStore, getModuleStoreRoot} = await import('@electron/main/ModuleStore');
const {ModuleUpdater} = await import('@electron/main/ModuleUpdater');

const SHELL_VERSION = '2026.823.1';
const RELEASE_CHANNEL = 'canary';
const PLATFORM = 'darwin';
const ARCH = 'arm64';
const PACKAGE_ORIGIN = 'https://pkgs.invalid';
const SOURCE_SHA = 'a'.repeat(40);
const NOW = Date.parse('2026-08-23T12:00:00.000Z');

const temporaryRoots = [];

after(async () => {
	for (const root of temporaryRoots) {
		await rm(root, {recursive: true, force: true});
	}
});

function sha256(data) {
	return createHash('sha256').update(data).digest('hex');
}

function createUserData() {
	const root = mkdtempSync(path.join(os.tmpdir(), 'module-updater-'));
	temporaryRoots.push(root);
	return root;
}

function installModuleOnDisk(storeRoot, moduleName, digest, entries) {
	const directory = path.join(storeRoot, 'store', moduleName, digest);
	mkdirSync(directory, {recursive: true});
	const files = entries.map(([relative, body]) => {
		writeFileSync(path.join(directory, relative), body);
		return {path: relative, sha256: sha256(Buffer.from(body, 'utf8')), bytes: Buffer.byteLength(body, 'utf8')};
	});
	writeFileSync(
		path.join(directory, 'module.json'),
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
	);
	return directory;
}

function manifestBytes(
	modules,
	{
		shellLatest = SHELL_VERSION,
		shellMinimum = SHELL_VERSION,
		required = null,
		bounds = {},
		metadataVersion = 1,
		platform = PLATFORM,
	} = {},
) {
	return Buffer.from(
		JSON.stringify({
			manifest_version: 1,
			release_channel: RELEASE_CHANNEL,
			platform,
			arch: ARCH,
			build_version: SHELL_VERSION,
			pub_date: '2026-08-23T00:00:00.000Z',
			metadata_version: metadataVersion,
			shell: {latest_version: shellLatest, minimum_version: shellMinimum},
			modules: Object.fromEntries(
				Object.entries(modules).map(([moduleName, digest]) => [
					moduleName,
					{
						sha256: digest,
						bytes: 1024,
						url: `${PACKAGE_ORIGIN}/desktop/${RELEASE_CHANNEL}/modules/${moduleName}/${digest}/package.br`,
						...(bounds[moduleName] ?? {}),
					},
				]),
			),
			required_modules: required ?? Object.keys(modules),
		}),
		'utf8',
	);
}

async function openStore(userDataPath) {
	return await ModuleStore.open({
		root: getModuleStoreRoot(userDataPath),
		shellVersion: SHELL_VERSION,
		releaseChannel: RELEASE_CHANNEL,
	});
}

function createUpdater(store, modules, options = {}) {
	const bytes = manifestBytes(modules, options);
	const requests = [];
	const sleeps = [];
	const reports = [];
	const updater = new ModuleUpdater({
		store,
		shellVersion: SHELL_VERSION,
		releaseChannel: RELEASE_CHANNEL,
		platform: options.platform ?? PLATFORM,
		arch: ARCH,
		packageOrigin: PACKAGE_ORIGIN,
		hasOfflineRenderer: false,
		forceStartupUpdate: options.forceStartupUpdate ?? false,
		selfUpdateShellFirst: options.selfUpdateShellFirst,
		onState: options.onState,
		fetch: async (url, init) => {
			requests.push(url);
			const response = options.respond == null ? null : await options.respond(url, init);
			return response ?? new Response(bytes, {status: 200, headers: {etag: '"manifest-1"'}});
		},
		sleep: async (ms) => {
			sleeps.push(ms);
		},
		random: () => 0,
		now: () => NOW,
		report: (report) => {
			reports.push(report);
		},
	});
	return {updater, requests, sleeps, reports};
}

function failNthInstallationCheck(store, nth, failure) {
	let checks = 0;
	return new Proxy(store, {
		get(target, property) {
			const value = Reflect.get(target, property, target);
			if (typeof value !== 'function') {
				return value;
			}
			if (property !== 'isInstalled') {
				return value.bind(target);
			}
			return async (moduleName, digest) => {
				checks += 1;
				if (checks === nth) {
					throw failure;
				}
				return await value.call(target, moduleName, digest);
			};
		},
	});
}

const RENDERER_SHA = sha256('renderer-one');
const NEXT_RENDERER_SHA = sha256('renderer-two');
const OVERLAY_SHA = sha256('overlay-one');
const NEXT_OVERLAY_SHA = sha256('overlay-two');

function failInstallModule(store, failure) {
	return new Proxy(store, {
		get(target, property) {
			const value = Reflect.get(target, property, target);
			if (typeof value !== 'function') {
				return value;
			}
			if (property !== 'installModule') {
				return value.bind(target);
			}
			return async () => {
				throw failure;
			};
		},
	});
}

function failInstallModuleFor(store, moduleName, failure) {
	return new Proxy(store, {
		get(target, property) {
			const value = Reflect.get(target, property, target);
			if (typeof value !== 'function') {
				return value;
			}
			if (property !== 'installModule') {
				return value.bind(target);
			}
			return async (request) => {
				if (request.module === moduleName) {
					throw failure;
				}
				return await value.call(target, request);
			};
		},
	});
}

function mergeDuringInstall(store, entries) {
	let merged = false;
	return new Proxy(store, {
		get(target, property) {
			const value = Reflect.get(target, property, target);
			if (typeof value !== 'function') {
				return value;
			}
			if (property !== 'installModule') {
				return value.bind(target);
			}
			return async (request) => {
				if (!merged) {
					merged = true;
					await target.mergeCommitted(entries);
				}
				return await value.call(target, request);
			};
		},
	});
}

function recordInstalls(store, installs) {
	return new Proxy(store, {
		get(target, property) {
			const value = Reflect.get(target, property, target);
			if (typeof value !== 'function') {
				return value;
			}
			if (property !== 'installModule') {
				return value.bind(target);
			}
			return async (request) => {
				installs.push(request.module);
				return await value.call(target, request);
			};
		},
	});
}

function unreachableFeed() {
	return async () => {
		throw new Error('module feed is unreachable');
	};
}

function missingPackages() {
	return async (url) => (url.endsWith('.br') ? new Response('', {status: 404}) : null);
}

function seedCachedPackage(store, digest) {
	const packagePath = store.getPackagePath(digest);
	mkdirSync(path.dirname(packagePath), {recursive: true});
	writeFileSync(packagePath, 'cached');
}

describe('ModuleUpdater.run', () => {
	test('launches the committed set when the manifest asks for nothing new', async () => {
		const userDataPath = createUserData();
		const store = await openStore(userDataPath);
		installModuleOnDisk(getModuleStoreRoot(userDataPath), 'fluxer_renderer', RENDERER_SHA, [['index.js', 'renderer']]);
		await store.commit({fluxer_renderer: RENDERER_SHA});
		const {updater, requests, sleeps} = createUpdater(store, {fluxer_renderer: RENDERER_SHA});

		const outcome = await updater.run();

		assert.equal(outcome.status, 'launching');
		assert.deepEqual(outcome.committed, {fluxer_renderer: RENDERER_SHA});
		assert.equal(outcome.rolledBack, false);
		assert.deepEqual(outcome.belowFloor, []);
		assert.equal(Object.hasOwn(outcome, 'installed'), false);
		assert.equal(requests.length, 1);
		assert.deepEqual(sleeps, []);
		assert.deepEqual(store.getState().floor, {fluxer_renderer: RENDERER_SHA});
		assert.equal(store.getState().boot_attempt, 1);
	});

	test('a store failure at the floor check is retried instead of escaping run', async () => {
		const userDataPath = createUserData();
		const store = await openStore(userDataPath);
		installModuleOnDisk(getModuleStoreRoot(userDataPath), 'fluxer_renderer', RENDERER_SHA, [['index.js', 'renderer']]);
		await store.commit({fluxer_renderer: RENDERER_SHA});
		const failure = Object.assign(new Error('failed to inspect installed module path'), {
			name: 'ModuleStoreIOError',
		});
		const {updater, sleeps, reports} = createUpdater(failNthInstallationCheck(store, 2, failure), {
			fluxer_renderer: RENDERER_SHA,
		});

		const outcome = await updater.run();

		assert.equal(outcome.status, 'launching');
		assert.deepEqual(outcome.committed, {fluxer_renderer: RENDERER_SHA});
		assert.deepEqual(sleeps, [500]);
		assert.deepEqual(
			reports.map((report) => [report.type, report.message]),
			[['network-error', failure.message]],
		);
	});

	test('a manifest that no longer carries the committed module blocks instead of launching nothing', async () => {
		const userDataPath = createUserData();
		const store = await openStore(userDataPath);
		installModuleOnDisk(getModuleStoreRoot(userDataPath), 'fluxer_renderer', RENDERER_SHA, [['index.js', 'renderer']]);
		await store.commit({fluxer_renderer: RENDERER_SHA});
		const {updater} = createUpdater(store, {});

		const outcome = await updater.run();

		assert.equal(outcome.status, 'blocked-update-required');
		assert.equal(outcome.reason, 'nothing-installed');
	});

	test('a shell floor above this shell blocks on the shell update instead of installing modules', async () => {
		const userDataPath = createUserData();
		const store = await openStore(userDataPath);
		installModuleOnDisk(getModuleStoreRoot(userDataPath), 'fluxer_renderer', RENDERER_SHA, [['index.js', 'renderer']]);
		await store.commit({fluxer_renderer: RENDERER_SHA});
		const {updater, requests} = createUpdater(
			store,
			{fluxer_renderer: NEXT_RENDERER_SHA},
			{shellLatest: '2026.824.0', shellMinimum: '2026.824.0'},
		);

		const outcome = await updater.run();

		assert.equal(outcome.status, 'blocked-shell-update');
		assert.equal(outcome.latestVersion, '2026.824.0');
		assert.equal(outcome.minimumVersion, '2026.824.0');
		assert.equal(outcome.requiredSecurityUpdate, false);
		assert.deepEqual(requests, [`${PACKAGE_ORIGIN}/desktop/${RELEASE_CHANNEL}/${PLATFORM}/${ARCH}/modules.json`]);
		assert.deepEqual(store.getCommitted(), {fluxer_renderer: RENDERER_SHA});
	});

	test('a required module that outruns this shell blocks on the shell update', async () => {
		const userDataPath = createUserData();
		const store = await openStore(userDataPath);
		installModuleOnDisk(getModuleStoreRoot(userDataPath), 'fluxer_renderer', RENDERER_SHA, [['index.js', 'renderer']]);
		await store.commit({fluxer_renderer: RENDERER_SHA});
		const {updater, requests} = createUpdater(
			store,
			{fluxer_renderer: NEXT_RENDERER_SHA},
			{bounds: {fluxer_renderer: {minimum_shell_version: '2026.824.0'}}},
		);

		const outcome = await updater.run();

		assert.equal(outcome.status, 'blocked-shell-update');
		assert.equal(outcome.latestVersion, SHELL_VERSION);
		assert.equal(outcome.minimumVersion, SHELL_VERSION);
		assert.equal(requests.length, 1, 'a shell block must not start downloading packages');
	});

	test('an optional module this shell has outgrown keeps its committed copy and still launches', async () => {
		const userDataPath = createUserData();
		const storeRoot = getModuleStoreRoot(userDataPath);
		const store = await openStore(userDataPath);
		installModuleOnDisk(storeRoot, 'fluxer_renderer', RENDERER_SHA, [['index.js', 'renderer']]);
		installModuleOnDisk(storeRoot, 'fluxer_overlay', OVERLAY_SHA, [['index.js', 'overlay']]);
		await store.commit({fluxer_overlay: OVERLAY_SHA, fluxer_renderer: RENDERER_SHA});
		const {updater, requests} = createUpdater(
			store,
			{fluxer_overlay: NEXT_OVERLAY_SHA, fluxer_renderer: RENDERER_SHA},
			{required: ['fluxer_renderer'], bounds: {fluxer_overlay: {minimum_shell_version: '2026.824.0'}}},
		);

		const outcome = await updater.run();

		assert.equal(outcome.status, 'launching');
		assert.deepEqual(outcome.committed, {fluxer_overlay: OVERLAY_SHA, fluxer_renderer: RENDERER_SHA});
		assert.equal(requests.length, 1, 'a module this shell cannot run must never be downloaded');
	});

	test('an unreachable feed with no recorded fetch blocks instead of launching whatever is on disk', async () => {
		const userDataPath = createUserData();
		const store = await openStore(userDataPath);
		installModuleOnDisk(getModuleStoreRoot(userDataPath), 'fluxer_renderer', RENDERER_SHA, [['index.js', 'renderer']]);
		await store.commit({fluxer_renderer: RENDERER_SHA});
		const {updater, sleeps} = createUpdater(store, {fluxer_renderer: RENDERER_SHA}, {respond: unreachableFeed()});

		const outcome = await updater.run();

		assert.equal(outcome.status, 'blocked-update-required');
		assert.equal(outcome.reason, 'nothing-installed');
		assert.deepEqual(sleeps, [500, 1000, 2000, 4000, 8000]);
		assert.equal(store.getState().boot_attempt, 0);
	});

	test('an unreachable feed launches the set a previous boot already verified without waiting out the backoff', async () => {
		const userDataPath = createUserData();
		const store = await openStore(userDataPath);
		installModuleOnDisk(getModuleStoreRoot(userDataPath), 'fluxer_renderer', RENDERER_SHA, [['index.js', 'renderer']]);
		await store.commit({fluxer_renderer: RENDERER_SHA});
		assert.equal((await createUpdater(store, {fluxer_renderer: RENDERER_SHA}).updater.run()).status, 'launching');
		const {updater, sleeps} = createUpdater(store, {fluxer_renderer: RENDERER_SHA}, {respond: unreachableFeed()});

		const outcome = await updater.run();

		assert.equal(outcome.status, 'unreachable-launch');
		assert.deepEqual(sleeps, []);
		assert.deepEqual(outcome.committed, {fluxer_renderer: RENDERER_SHA});
		assert.equal(outcome.launchAttempt.committed.fluxer_renderer, RENDERER_SHA);
		assert.equal(store.getState().boot_attempt, 2);
	});

	test('a manifest older than the one already seen launches the verified set without retrying', async () => {
		const userDataPath = createUserData();
		const store = await openStore(userDataPath);
		installModuleOnDisk(getModuleStoreRoot(userDataPath), 'fluxer_renderer', RENDERER_SHA, [['index.js', 'renderer']]);
		await store.commit({fluxer_renderer: RENDERER_SHA});
		const seen = createUpdater(store, {fluxer_renderer: RENDERER_SHA}, {metadataVersion: 5});
		assert.equal((await seen.updater.run()).status, 'launching');
		const {updater, sleeps, reports} = createUpdater(store, {fluxer_renderer: NEXT_RENDERER_SHA}, {metadataVersion: 4});

		const outcome = await updater.run();

		assert.equal(outcome.status, 'unreachable-launch');
		assert.deepEqual(outcome.committed, {fluxer_renderer: RENDERER_SHA});
		assert.deepEqual(sleeps, []);
		assert.deepEqual(
			reports.map((report) => report.type),
			['manifest-rollback'],
		);
	});

	test('a package the server no longer has demotes an optional module to its committed digest', async () => {
		const userDataPath = createUserData();
		const storeRoot = getModuleStoreRoot(userDataPath);
		const store = await openStore(userDataPath);
		installModuleOnDisk(storeRoot, 'fluxer_renderer', RENDERER_SHA, [['index.js', 'renderer']]);
		installModuleOnDisk(storeRoot, 'fluxer_overlay', OVERLAY_SHA, [['index.js', 'overlay']]);
		await store.commit({fluxer_overlay: OVERLAY_SHA, fluxer_renderer: RENDERER_SHA});
		const {updater, reports} = createUpdater(
			store,
			{fluxer_overlay: NEXT_OVERLAY_SHA, fluxer_renderer: RENDERER_SHA},
			{required: ['fluxer_renderer'], respond: missingPackages()},
		);

		const outcome = await updater.run();

		assert.equal(outcome.status, 'launching');
		assert.deepEqual(outcome.committed, {fluxer_overlay: OVERLAY_SHA, fluxer_renderer: RENDERER_SHA});
		assert.deepEqual(
			reports.filter((report) => report.type !== 'network-error').map((report) => [report.type, report.module]),
			[['package-missing', 'fluxer_overlay']],
		);
	});

	test('an install that runs out of disk space reports the storage error and commits nothing', async () => {
		const userDataPath = createUserData();
		const store = await openStore(userDataPath);
		installModuleOnDisk(getModuleStoreRoot(userDataPath), 'fluxer_renderer', RENDERER_SHA, [['index.js', 'renderer']]);
		await store.commit({fluxer_renderer: RENDERER_SHA});
		const failure = Object.assign(new Error('no space left on device'), {code: 'ENOSPC'});
		const {updater, reports} = createUpdater(failInstallModule(store, failure), {
			fluxer_renderer: NEXT_RENDERER_SHA,
		});

		const outcome = await updater.run();

		assert.equal(outcome.status, 'blocked-update-required');
		assert.equal(outcome.reason, 'unreachable-below-floor');
		assert.equal(outcome.message, 'no space left to install fluxer_renderer: no space left on device');
		assert.equal(outcome.updateServerUnreachable, false);
		assert.deepEqual(store.getCommitted(), {fluxer_renderer: RENDERER_SHA});
		assert.deepEqual(
			[...new Set(reports.filter((report) => report.type === 'storage-error').map((report) => report.module))],
			['fluxer_renderer', null],
		);
		assert.equal(updater.getLastState().detail, 'Not enough disk space to install the update');
	});
});

describe('ModuleUpdater checks without downloading and installs only when asked', () => {
	async function storeOnRenderer() {
		const userDataPath = createUserData();
		const storeRoot = getModuleStoreRoot(userDataPath);
		const store = await openStore(userDataPath);
		installModuleOnDisk(storeRoot, 'fluxer_renderer', RENDERER_SHA, [['index.js', 'renderer']]);
		await store.commit({fluxer_renderer: RENDERER_SHA});
		return {store, storeRoot};
	}

	test('a check reads only the manifest and leaves the committed set, the floor and the boot attempt alone', async () => {
		const {store} = await storeOnRenderer();
		const before = store.getState();
		const installs = [];
		const {updater, requests} = createUpdater(recordInstalls(store, installs), {fluxer_renderer: NEXT_RENDERER_SHA});

		const check = await updater.checkForUpdate();

		assert.deepEqual(check, {shellLatestVersion: SHELL_VERSION, shellNewer: false, modulesChanged: true});
		assert.deepEqual(
			requests.map((url) => String(url).split('/').pop()),
			['modules.json'],
		);
		assert.deepEqual(installs, []);
		assert.deepEqual(store.getCommitted(), {fluxer_renderer: RENDERER_SHA});
		assert.equal(store.getState().boot_attempt, before.boot_attempt);
		assert.deepEqual(store.getState().floor, before.floor);
		assert.equal(store.getState().last_manifest_fetch, before.last_manifest_fetch);
	});

	test('a check reports a newer shell and an unchanged module set', async () => {
		const {store} = await storeOnRenderer();
		const {updater} = createUpdater(store, {fluxer_renderer: RENDERER_SHA}, {shellLatest: '2026.824.1'});

		assert.deepEqual(await updater.checkForUpdate(), {
			shellLatestVersion: '2026.824.1',
			shellNewer: true,
			modulesChanged: false,
		});
	});

	test('a module that needs the next shell reads as a shell update, not as a module update', async () => {
		const {store} = await storeOnRenderer();
		const {updater} = createUpdater(
			store,
			{fluxer_renderer: NEXT_RENDERER_SHA},
			{shellLatest: '2026.824.1', bounds: {fluxer_renderer: {minimum_shell_version: '2026.824.1'}}},
		);

		assert.deepEqual(await updater.checkForUpdate(), {
			shellLatestVersion: '2026.824.1',
			shellNewer: true,
			modulesChanged: false,
		});
	});

	test('the click installs the new set, reports progress and arms a launch attempt for the next window', async () => {
		const {store, storeRoot} = await storeOnRenderer();
		installModuleOnDisk(storeRoot, 'fluxer_renderer', NEXT_RENDERER_SHA, [['index.js', 'renderer-two']]);
		seedCachedPackage(store, NEXT_RENDERER_SHA);
		const statuses = [];
		const {updater} = createUpdater(
			store,
			{fluxer_renderer: NEXT_RENDERER_SHA},
			{onState: (state) => statuses.push(state.status)},
		);

		const attempt = await updater.installPending();

		assert.deepEqual(attempt?.committed, {fluxer_renderer: NEXT_RENDERER_SHA});
		assert.deepEqual(store.getCommitted(), {fluxer_renderer: NEXT_RENDERER_SHA});
		assert.equal(store.getState().boot_attempt, 1);
		assert.equal(statuses[0], 'checking');
		assert.equal(statuses.at(-1), 'verifying');
		await updater.markLaunchSucceeded(attempt);
		assert.equal(store.getState().boot_attempt, 0);
	});

	test('the click keeps a module installed on demand while it was downloading', async () => {
		const {store, storeRoot} = await storeOnRenderer();
		installModuleOnDisk(storeRoot, 'fluxer_renderer', NEXT_RENDERER_SHA, [['index.js', 'renderer-two']]);
		installModuleOnDisk(storeRoot, 'fluxer_overlay', OVERLAY_SHA, [['index.js', 'overlay']]);
		seedCachedPackage(store, NEXT_RENDERER_SHA);
		const {updater} = createUpdater(mergeDuringInstall(store, {fluxer_overlay: OVERLAY_SHA}), {
			fluxer_renderer: NEXT_RENDERER_SHA,
		});

		const attempt = await updater.installPending();

		assert.deepEqual(attempt?.committed, {fluxer_overlay: OVERLAY_SHA, fluxer_renderer: NEXT_RENDERER_SHA});
		assert.deepEqual(store.getCommitted(), {fluxer_overlay: OVERLAY_SHA, fluxer_renderer: NEXT_RENDERER_SHA});
	});

	test('the click installs nothing when the manifest now needs a newer shell', async () => {
		const {store} = await storeOnRenderer();
		const installs = [];
		const {updater} = createUpdater(
			recordInstalls(store, installs),
			{fluxer_renderer: NEXT_RENDERER_SHA},
			{shellLatest: '2026.824.1', shellMinimum: '2026.824.1'},
		);

		assert.equal(await updater.installPending(), null);
		assert.deepEqual(installs, []);
		assert.deepEqual(store.getCommitted(), {fluxer_renderer: RENDERER_SHA});
	});

	test('a module the manifest advertises but the CDN never got does not strand the rest of the update', async () => {
		const userDataPath = createUserData();
		const storeRoot = getModuleStoreRoot(userDataPath);
		const store = await openStore(userDataPath);
		installModuleOnDisk(storeRoot, 'fluxer_overlay', OVERLAY_SHA, [['index.js', 'overlay']]);
		installModuleOnDisk(storeRoot, 'fluxer_renderer', RENDERER_SHA, [['index.js', 'renderer']]);
		await store.commit({fluxer_overlay: OVERLAY_SHA, fluxer_renderer: RENDERER_SHA});
		installModuleOnDisk(storeRoot, 'fluxer_renderer', NEXT_RENDERER_SHA, [['index.js', 'renderer-two']]);
		seedCachedPackage(store, NEXT_RENDERER_SHA);
		const {updater, reports} = createUpdater(
			store,
			{fluxer_overlay: NEXT_OVERLAY_SHA, fluxer_renderer: NEXT_RENDERER_SHA},
			{
				required: ['fluxer_renderer'],
				respond: async (url) =>
					url.endsWith(`${NEXT_OVERLAY_SHA}/package.br`) ? new Response('', {status: 404}) : null,
			},
		);

		const attempt = await updater.installPending();

		assert.deepEqual(attempt?.committed, {fluxer_overlay: OVERLAY_SHA, fluxer_renderer: NEXT_RENDERER_SHA});
		assert.deepEqual(
			[...new Set(reports.filter((report) => report.type === 'package-missing').map((report) => report.module))],
			['fluxer_overlay'],
		);
	});

	test('a committed optional module whose tree is gone and whose package 404s still lets the renderer activate', async () => {
		const userDataPath = createUserData();
		const storeRoot = getModuleStoreRoot(userDataPath);
		const store = await openStore(userDataPath);
		installModuleOnDisk(storeRoot, 'fluxer_overlay', OVERLAY_SHA, [['index.js', 'overlay']]);
		installModuleOnDisk(storeRoot, 'fluxer_renderer', RENDERER_SHA, [['index.js', 'renderer']]);
		await store.commit({fluxer_overlay: OVERLAY_SHA, fluxer_renderer: RENDERER_SHA});
		await rm(path.join(storeRoot, 'store', 'fluxer_overlay', OVERLAY_SHA), {recursive: true, force: true});
		installModuleOnDisk(storeRoot, 'fluxer_renderer', NEXT_RENDERER_SHA, [['index.js', 'renderer-two']]);
		seedCachedPackage(store, NEXT_RENDERER_SHA);
		const {updater} = createUpdater(
			store,
			{fluxer_overlay: OVERLAY_SHA, fluxer_renderer: NEXT_RENDERER_SHA},
			{
				required: ['fluxer_renderer'],
				respond: async (url) => (url.endsWith(`${OVERLAY_SHA}/package.br`) ? new Response('', {status: 404}) : null),
			},
		);

		await updater.installPending();

		assert.deepEqual(store.getCommitted(), {fluxer_renderer: NEXT_RENDERER_SHA});
	});

	test('a package failure with no deterministic cause rejects the click and keeps the installed set', async () => {
		const userDataPath = createUserData();
		const storeRoot = getModuleStoreRoot(userDataPath);
		const store = await openStore(userDataPath);
		installModuleOnDisk(storeRoot, 'fluxer_overlay', OVERLAY_SHA, [['index.js', 'overlay']]);
		installModuleOnDisk(storeRoot, 'fluxer_renderer', RENDERER_SHA, [['index.js', 'renderer']]);
		await store.commit({fluxer_overlay: OVERLAY_SHA, fluxer_renderer: RENDERER_SHA});
		installModuleOnDisk(storeRoot, 'fluxer_overlay', NEXT_OVERLAY_SHA, [['index.js', 'overlay-two']]);
		seedCachedPackage(store, NEXT_OVERLAY_SHA);
		const {updater} = createUpdater(
			store,
			{fluxer_overlay: NEXT_OVERLAY_SHA, fluxer_renderer: NEXT_RENDERER_SHA},
			{
				required: ['fluxer_renderer'],
				respond: async (url) =>
					url.endsWith(`${NEXT_RENDERER_SHA}/package.br`) ? new Response('', {status: 503}) : null,
			},
		);

		await assert.rejects(updater.installPending(), /failed to download module fluxer_renderer/u);

		assert.deepEqual(store.getCommitted(), {fluxer_overlay: OVERLAY_SHA, fluxer_renderer: RENDERER_SHA});
	});

	test('a click that runs out of disk space aborts instead of activating the part that fit', async () => {
		const userDataPath = createUserData();
		const storeRoot = getModuleStoreRoot(userDataPath);
		const store = await openStore(userDataPath);
		installModuleOnDisk(storeRoot, 'fluxer_overlay', OVERLAY_SHA, [['index.js', 'overlay']]);
		installModuleOnDisk(storeRoot, 'fluxer_renderer', RENDERER_SHA, [['index.js', 'renderer']]);
		await store.commit({fluxer_overlay: OVERLAY_SHA, fluxer_renderer: RENDERER_SHA});
		installModuleOnDisk(storeRoot, 'fluxer_overlay', NEXT_OVERLAY_SHA, [['index.js', 'overlay-two']]);
		seedCachedPackage(store, NEXT_OVERLAY_SHA);
		const failure = Object.assign(new Error('no space left on device'), {code: 'ENOSPC'});
		const {updater} = createUpdater(
			failInstallModuleFor(store, 'fluxer_renderer', failure),
			{fluxer_overlay: NEXT_OVERLAY_SHA, fluxer_renderer: NEXT_RENDERER_SHA},
			{required: ['fluxer_renderer']},
		);

		await assert.rejects(updater.installPending(), /no space left on device/u);

		assert.deepEqual(store.getCommitted(), {fluxer_overlay: OVERLAY_SHA, fluxer_renderer: RENDERER_SHA});
	});

	test('a boot with a newer shell updates the shell before downloading any module', async () => {
		const {store, storeRoot} = await storeOnRenderer();
		installModuleOnDisk(storeRoot, 'fluxer_renderer', NEXT_RENDERER_SHA, [['index.js', 'renderer-two']]);
		seedCachedPackage(store, NEXT_RENDERER_SHA);
		const steps = [];
		const {updater} = createUpdater(
			recordInstalls(store, steps),
			{fluxer_renderer: NEXT_RENDERER_SHA},
			{
				shellLatest: '2026.824.1',
				forceStartupUpdate: true,
				selfUpdateShellFirst: async (latestVersion) => {
					steps.push(`shell ${latestVersion}`);
				},
			},
		);

		const outcome = await updater.run();

		assert.equal(outcome.status, 'launching');
		assert.deepEqual(steps, ['shell 2026.824.1', 'fluxer_renderer']);
	});

	test('a boot with the newest shell never runs the shell update', async () => {
		const {store, storeRoot} = await storeOnRenderer();
		installModuleOnDisk(storeRoot, 'fluxer_renderer', NEXT_RENDERER_SHA, [['index.js', 'renderer-two']]);
		seedCachedPackage(store, NEXT_RENDERER_SHA);
		const steps = [];
		const {updater} = createUpdater(
			recordInstalls(store, steps),
			{fluxer_renderer: NEXT_RENDERER_SHA},
			{
				forceStartupUpdate: true,
				selfUpdateShellFirst: async (latestVersion) => {
					steps.push(`shell ${latestVersion}`);
				},
			},
		);

		await updater.run();

		assert.deepEqual(steps, ['fluxer_renderer']);
	});

	test('a Linux boot right after a shell update fetches the modules instead of launching the old set', async () => {
		const optional = await storeOnRenderer();
		const quiet = createUpdater(optional.store, {fluxer_renderer: NEXT_RENDERER_SHA}, {platform: 'linux'});
		const skipped = await quiet.updater.run();
		assert.equal(skipped.status, 'launching');
		assert.deepEqual(skipped.committed, {fluxer_renderer: RENDERER_SHA});
		assert.deepEqual(quiet.requests, []);

		const forced = await storeOnRenderer();
		installModuleOnDisk(forced.storeRoot, 'fluxer_renderer', NEXT_RENDERER_SHA, [['index.js', 'renderer-two']]);
		seedCachedPackage(forced.store, NEXT_RENDERER_SHA);
		const {updater, requests} = createUpdater(
			forced.store,
			{fluxer_renderer: NEXT_RENDERER_SHA},
			{platform: 'linux', forceStartupUpdate: true},
		);
		const outcome = await updater.run();

		assert.equal(outcome.status, 'launching');
		assert.deepEqual(outcome.committed, {fluxer_renderer: NEXT_RENDERER_SHA});
		assert.ok(requests.some((url) => String(url).endsWith('/modules.json')));
	});

	test('a Linux boot forced only by a shell change launches the installed set at once when the download fails', async () => {
		const {store} = await storeOnRenderer();
		const {updater, sleeps} = createUpdater(
			store,
			{fluxer_renderer: NEXT_RENDERER_SHA},
			{
				platform: 'linux',
				forceStartupUpdate: true,
				respond: async (url) => (String(url).endsWith('package.br') ? new Response('', {status: 503}) : null),
			},
		);

		const outcome = await updater.run();

		assert.equal(outcome.status, 'unreachable-launch');
		assert.deepEqual(outcome.committed, {fluxer_renderer: RENDERER_SHA});
		assert.deepEqual(
			sleeps,
			[],
			'a package or deb upgrade never waits through the backoff for a module it can live without',
		);
	});
});

describe('ModuleUpdater explains why a launch is blocked', () => {
	test('a first launch that cannot reach the feed blocks as unreachable rather than as a missing update', async () => {
		const userDataPath = createUserData();
		const store = await openStore(userDataPath);
		const {updater} = createUpdater(store, {fluxer_renderer: RENDERER_SHA}, {respond: unreachableFeed()});

		const outcome = await updater.run();

		assert.equal(outcome.status, 'blocked-update-required');
		assert.equal(outcome.reason, 'nothing-installed');
		assert.equal(outcome.updateServerUnreachable, true);
	});

	test('a package url the policy refuses names its cause in the block and in a report', async () => {
		const userDataPath = createUserData();
		const store = await openStore(userDataPath);
		const url = `http://192.168.139.107:48780/desktop/canary/modules/fluxer_renderer/${RENDERER_SHA}/package.br`;
		const {updater, reports} = createUpdater(
			store,
			{fluxer_renderer: RENDERER_SHA},
			{bounds: {fluxer_renderer: {url}}},
		);

		const outcome = await updater.run();

		assert.equal(outcome.status, 'blocked-update-required');
		assert.equal(outcome.reason, 'required-module-unavailable');
		assert.equal(outcome.updateServerUnreachable, false);
		assert.match(outcome.message, /not https: http:\/\/192\.168\.139\.107/u);
		const rejected = reports.find((report) => report.type === 'package-rejected');
		assert.equal(rejected?.module, 'fluxer_renderer');
		assert.match(rejected?.message ?? '', /192\.168\.139\.107/u);
		const blocked = reports.find((report) => report.type === 'blocked');
		assert.notEqual(blocked?.error, null);
	});
});

describe('ModuleUpdater after a crash-loop rollback', () => {
	async function rollBackFrom(store, modules) {
		for (let launch = 0; launch < 2; launch += 1) {
			assert.equal((await createUpdater(store, modules).updater.run()).status, 'launching');
		}
		const reverted = await createUpdater(store, modules).updater.run();
		assert.equal(reverted.rolledBack, true);
		return reverted;
	}

	test('the next boot keeps the reverted set instead of reinstalling the build that crashed', async () => {
		const userDataPath = createUserData();
		const storeRoot = getModuleStoreRoot(userDataPath);
		const store = await openStore(userDataPath);
		installModuleOnDisk(storeRoot, 'fluxer_renderer', RENDERER_SHA, [['index.js', 'renderer']]);
		installModuleOnDisk(storeRoot, 'fluxer_renderer', NEXT_RENDERER_SHA, [['index.js', 'renderer two']]);
		await store.commit({fluxer_renderer: RENDERER_SHA});
		await store.commit({fluxer_renderer: NEXT_RENDERER_SHA});
		const modules = {fluxer_renderer: NEXT_RENDERER_SHA};
		await rollBackFrom(store, modules);

		const {updater, requests} = createUpdater(store, modules);
		const outcome = await updater.run();

		assert.equal(outcome.status, 'launching');
		assert.equal(outcome.rolledBack, false);
		assert.deepEqual(outcome.committed, {fluxer_renderer: RENDERER_SHA});
		assert.deepEqual(
			requests.filter((url) => String(url).endsWith('.br')),
			[],
		);
	});

	test('a manifest that moves past the rejected build clears the rejection', async () => {
		const userDataPath = createUserData();
		const storeRoot = getModuleStoreRoot(userDataPath);
		const store = await openStore(userDataPath);
		installModuleOnDisk(storeRoot, 'fluxer_renderer', RENDERER_SHA, [['index.js', 'renderer']]);
		installModuleOnDisk(storeRoot, 'fluxer_renderer', NEXT_RENDERER_SHA, [['index.js', 'renderer two']]);
		await store.commit({fluxer_renderer: RENDERER_SHA});
		await store.commit({fluxer_renderer: NEXT_RENDERER_SHA});
		await rollBackFrom(store, {fluxer_renderer: NEXT_RENDERER_SHA});
		assert.equal(store.isRejected('fluxer_renderer', NEXT_RENDERER_SHA), true);

		await createUpdater(store, {fluxer_renderer: OVERLAY_SHA}, {metadataVersion: 2, respond: missingPackages()})
			.updater.run()
			.catch(() => undefined);

		assert.equal(store.isRejected('fluxer_renderer', NEXT_RENDERER_SHA), false);
	});
});
