// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {Buffer} from 'node:buffer';
import {createHash} from 'node:crypto';
import {mkdirSync, mkdtempSync, rmSync, writeFileSync} from 'node:fs';
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

function installModuleOnDisk(storeRoot, moduleName, digest, entries, buildVersion = SHELL_VERSION) {
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
				build_version: buildVersion,
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
		buildVersion = SHELL_VERSION,
		extra = {},
	} = {},
) {
	return Buffer.from(
		JSON.stringify({
			manifest_version: 1,
			release_channel: RELEASE_CHANNEL,
			platform,
			arch: ARCH,
			build_version: buildVersion,
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
			...extra,
		}),
		'utf8',
	);
}

async function openStore(userDataPath, shellVersion = SHELL_VERSION) {
	return await ModuleStore.open({
		root: getModuleStoreRoot(userDataPath),
		shellVersion,
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
		shellVersion: options.shellVersion ?? SHELL_VERSION,
		releaseChannel: RELEASE_CHANNEL,
		platform: options.platform ?? PLATFORM,
		arch: ARCH,
		packageOrigin: PACKAGE_ORIGIN,
		hasOfflineRenderer: options.bundledRendererVersion != null,
		bundledRendererVersion: options.bundledRendererVersion ?? null,
		forceStartupUpdate: options.forceStartupUpdate ?? false,
		startupUpdateRequested: options.startupUpdateRequested ?? false,
		moduleUpdatesDisabled: options.moduleUpdatesDisabled ?? false,
		updateOnDemand: options.updateOnDemand ?? false,
		selfUpdateShellFirst: options.selfUpdateShellFirst,
		onState: options.onState,
		fetch: async (url, init) => {
			requests.push(url);
			const response = options.respond == null ? null : await options.respond(url, init);
			if (response != null) {
				return response;
			}
			if (String(url).endsWith('/latest.json')) {
				const version = options.shellFeed ?? options.shellLatest ?? SHELL_VERSION;
				return new Response(JSON.stringify({version}), {status: 200, headers: {etag: `"feed-${version}"`}});
			}
			return new Response(bytes, {status: 200, headers: {etag: '"manifest-1"'}});
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

	test('a manifest older than the one already seen launches on the last accepted manifest without retrying', async () => {
		const userDataPath = createUserData();
		const store = await openStore(userDataPath);
		installModuleOnDisk(getModuleStoreRoot(userDataPath), 'fluxer_renderer', RENDERER_SHA, [['index.js', 'renderer']]);
		await store.commit({fluxer_renderer: RENDERER_SHA});
		const seen = createUpdater(store, {fluxer_renderer: RENDERER_SHA}, {metadataVersion: 5});
		assert.equal((await seen.updater.run()).status, 'launching');
		const {updater, sleeps, reports} = createUpdater(store, {fluxer_renderer: NEXT_RENDERER_SHA}, {metadataVersion: 4});

		const outcome = await updater.run();

		assert.equal(outcome.status, 'launching');
		assert.deepEqual(outcome.committed, {fluxer_renderer: RENDERER_SHA});
		assert.deepEqual(sleeps, []);
		assert.deepEqual(reports, []);
		assert.equal(store.getState().manifest_high_water[0].metadataVersion, 5);
	});

	test('an older manifest with no accepted copy to fall back on retries like an outage instead of blocking', async () => {
		const userDataPath = createUserData();
		const storeRoot = getModuleStoreRoot(userDataPath);
		const store = await openStore(userDataPath);
		installModuleOnDisk(storeRoot, 'fluxer_renderer', RENDERER_SHA, [['index.js', 'renderer']]);
		await store.commit({fluxer_renderer: RENDERER_SHA});
		const seen = createUpdater(store, {fluxer_renderer: RENDERER_SHA}, {metadataVersion: 5});
		assert.equal((await seen.updater.run()).status, 'launching');
		rmSync(path.join(storeRoot, 'manifest.json'));
		const {updater, sleeps, reports} = createUpdater(store, {fluxer_renderer: NEXT_RENDERER_SHA}, {metadataVersion: 4});

		const outcome = await updater.run();

		assert.equal(outcome.status, 'unreachable-launch');
		assert.deepEqual(outcome.committed, {fluxer_renderer: RENDERER_SHA});
		assert.ok(sleeps.length > 0, 'a stale edge copy clears within a minute, so the launch retries before giving up');
		assert.ok(reports.every((report) => report.type === 'manifest-rollback'));
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

describe('ModuleUpdater with module updates disabled', () => {
	test('launches the verified set without fetching even when the manifest demands a shell update', async () => {
		const userDataPath = createUserData();
		const store = await openStore(userDataPath);
		installModuleOnDisk(getModuleStoreRoot(userDataPath), 'fluxer_renderer', RENDERER_SHA, [['index.js', 'renderer']]);
		await store.commit({fluxer_renderer: RENDERER_SHA});
		assert.equal((await createUpdater(store, {fluxer_renderer: RENDERER_SHA}).updater.run()).status, 'launching');
		const statuses = [];
		let shellUpdates = 0;
		const {updater, requests, sleeps} = createUpdater(
			store,
			{fluxer_renderer: NEXT_RENDERER_SHA},
			{
				shellLatest: '2026.824.0',
				shellMinimum: '2026.824.0',
				forceStartupUpdate: true,
				moduleUpdatesDisabled: true,
				onState: (state) => statuses.push(state.status),
				selfUpdateShellFirst: async () => {
					shellUpdates += 1;
					return {reason: 'no-update'};
				},
			},
		);

		const outcome = await updater.run();

		assert.equal(outcome.status, 'unreachable-launch');
		assert.deepEqual(outcome.committed, {fluxer_renderer: RENDERER_SHA});
		assert.deepEqual(requests, []);
		assert.deepEqual(sleeps, []);
		assert.equal(shellUpdates, 0);
		assert.deepEqual(statuses, ['unreachable-launch']);
		assert.equal(outcome.launchAttempt.committed.fluxer_renderer, RENDERER_SHA);
	});

	test('launches the bundled renderer on a fresh store without fetching', async () => {
		const store = await openStore(createUserData());
		const {updater, requests} = createUpdater(
			store,
			{fluxer_renderer: NEXT_RENDERER_SHA},
			{bundledRendererVersion: SHELL_VERSION, moduleUpdatesDisabled: true},
		);

		const outcome = await updater.run();

		assert.equal(outcome.status, 'unreachable-launch');
		assert.deepEqual(outcome.committed, {});
		assert.deepEqual(requests, []);
	});

	test('blocks at once on the unreachable splash without fetching when nothing is installed', async () => {
		const store = await openStore(createUserData());
		const {updater, requests, sleeps} = createUpdater(
			store,
			{fluxer_renderer: RENDERER_SHA},
			{moduleUpdatesDisabled: true},
		);

		const outcome = await updater.run();

		assert.equal(outcome.status, 'blocked-update-required');
		assert.equal(outcome.reason, 'nothing-installed');
		assert.equal(outcome.updateServerUnreachable, true);
		assert.deepEqual(requests, []);
		assert.deepEqual(sleeps, []);
	});

	test('ignores an explicit update request', async () => {
		const userDataPath = createUserData();
		const store = await openStore(userDataPath);
		installModuleOnDisk(getModuleStoreRoot(userDataPath), 'fluxer_renderer', RENDERER_SHA, [['index.js', 'renderer']]);
		await store.commit({fluxer_renderer: RENDERER_SHA});
		assert.equal((await createUpdater(store, {fluxer_renderer: RENDERER_SHA}).updater.run()).status, 'launching');
		const {updater, requests} = createUpdater(
			store,
			{fluxer_renderer: NEXT_RENDERER_SHA},
			{moduleUpdatesDisabled: true, startupUpdateRequested: true},
		);

		const outcome = await updater.run();

		assert.equal(outcome.status, 'unreachable-launch');
		assert.deepEqual(outcome.committed, {fluxer_renderer: RENDERER_SHA});
		assert.deepEqual(requests, []);
	});
});

describe('ModuleUpdater with updates on demand', () => {
	async function storeOnRenderer() {
		const userDataPath = createUserData();
		const storeRoot = getModuleStoreRoot(userDataPath);
		const store = await openStore(userDataPath);
		installModuleOnDisk(storeRoot, 'fluxer_renderer', RENDERER_SHA, [['index.js', 'renderer']]);
		await store.commit({fluxer_renderer: RENDERER_SHA});
		return {store, storeRoot};
	}

	test('launches the verified set without fetching when an automatic check left an update waiting', async () => {
		const {store} = await storeOnRenderer();
		assert.equal((await createUpdater(store, {fluxer_renderer: RENDERER_SHA}).updater.run()).status, 'launching');
		const statuses = [];
		let shellUpdates = 0;
		const {updater, requests, sleeps} = createUpdater(
			store,
			{fluxer_renderer: NEXT_RENDERER_SHA},
			{
				shellLatest: '2026.824.0',
				forceStartupUpdate: true,
				updateOnDemand: true,
				onState: (state) => statuses.push(state.status),
				selfUpdateShellFirst: async () => {
					shellUpdates += 1;
					return {reason: 'no-update'};
				},
			},
		);

		const outcome = await updater.run();

		assert.equal(outcome.status, 'unreachable-launch');
		assert.deepEqual(outcome.committed, {fluxer_renderer: RENDERER_SHA});
		assert.deepEqual(requests, []);
		assert.deepEqual(sleeps, []);
		assert.equal(shellUpdates, 0);
		assert.deepEqual(statuses, ['launching']);
	});

	test('installs the update the user asked for before launch', async () => {
		const {store, storeRoot} = await storeOnRenderer();
		installModuleOnDisk(storeRoot, 'fluxer_renderer', NEXT_RENDERER_SHA, [['index.js', 'renderer-two']]);
		seedCachedPackage(store, NEXT_RENDERER_SHA);
		const steps = [];
		const {updater, requests} = createUpdater(
			recordInstalls(store, steps),
			{fluxer_renderer: NEXT_RENDERER_SHA},
			{
				shellLatest: '2026.824.1',
				updateOnDemand: true,
				startupUpdateRequested: true,
				selfUpdateShellFirst: async (latestVersion) => {
					steps.push(`shell ${latestVersion}`);
					return {reason: 'install-failed'};
				},
			},
		);

		const outcome = await updater.run();

		assert.equal(outcome.status, 'launching');
		assert.deepEqual(outcome.committed, {fluxer_renderer: NEXT_RENDERER_SHA});
		assert.deepEqual(steps, ['shell 2026.824.1', 'fluxer_renderer']);
		assert.ok(requests.some((url) => String(url).endsWith('/modules.json')));
	});

	test('goes to the update server when nothing can launch', async () => {
		const store = await openStore(createUserData());
		const {updater, requests} = createUpdater(
			store,
			{fluxer_renderer: RENDERER_SHA},
			{updateOnDemand: true, respond: missingPackages()},
		);

		const outcome = await updater.run();

		assert.equal(outcome.status, 'blocked-update-required');
		assert.equal(outcome.reason, 'required-module-unavailable');
		assert.equal(outcome.updateServerUnreachable, false);
		assert.ok(requests.some((url) => String(url).endsWith('/modules.json')));
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
			['modules.json', 'latest.json'],
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

	test('a shell feed ahead of the manifest offers the newer shell', async () => {
		const {store} = await storeOnRenderer();
		const {updater} = createUpdater(store, {fluxer_renderer: RENDERER_SHA}, {shellFeed: '2026.1012.154032'});

		assert.deepEqual(await updater.checkForUpdate(), {
			shellLatestVersion: '2026.1012.154032',
			shellNewer: true,
			modulesChanged: false,
		});
	});

	test('a shell feed behind the manifest never hides the shell the manifest names', async () => {
		const {store} = await storeOnRenderer();
		const {updater} = createUpdater(
			store,
			{fluxer_renderer: RENDERER_SHA},
			{shellLatest: '2026.824.1', shellFeed: SHELL_VERSION},
		);

		assert.deepEqual(await updater.checkForUpdate(), {
			shellLatestVersion: '2026.824.1',
			shellNewer: true,
			modulesChanged: false,
		});
	});

	test('an unreachable shell feed still answers from the manifest', async () => {
		const {store} = await storeOnRenderer();
		const {updater} = createUpdater(
			store,
			{fluxer_renderer: NEXT_RENDERER_SHA},
			{respond: async (url) => (String(url).endsWith('/latest.json') ? new Response('', {status: 503}) : null)},
		);

		assert.deepEqual(await updater.checkForUpdate(), {
			shellLatestVersion: SHELL_VERSION,
			shellNewer: false,
			modulesChanged: true,
		});
	});

	test('repeat checks revalidate both feeds and reuse them when nothing changed', async () => {
		const {store} = await storeOnRenderer();
		const conditions = [];
		const {updater} = createUpdater(
			store,
			{fluxer_renderer: NEXT_RENDERER_SHA},
			{
				shellFeed: '2026.824.1',
				respond: async (url, init) => {
					const etag = init?.headers?.['if-none-match'] ?? null;
					conditions.push([String(url).split('/').pop(), etag]);
					return etag == null ? null : new Response(null, {status: 304});
				},
			},
		);
		const first = await updater.checkForUpdate();

		const second = await updater.checkForUpdate();

		assert.deepEqual(second, first);
		assert.deepEqual(second, {shellLatestVersion: '2026.824.1', shellNewer: true, modulesChanged: true});
		assert.deepEqual(conditions.slice(-2), [
			['modules.json', '"manifest-1"'],
			['latest.json', '"feed-2026.824.1"'],
		]);
	});

	test('an older manifest during a background check reads as nothing new instead of failing', async () => {
		const {store} = await storeOnRenderer();
		let metadataVersion = 6;
		const {updater} = createUpdater(
			store,
			{fluxer_renderer: NEXT_RENDERER_SHA},
			{
				respond: async (url) =>
					String(url).endsWith('/modules.json')
						? new Response(manifestBytes({fluxer_renderer: NEXT_RENDERER_SHA}, {metadataVersion}), {status: 200})
						: null,
			},
		);
		const fresh = await updater.checkForUpdate();
		metadataVersion = 5;

		const stale = await updater.checkForUpdate();

		assert.deepEqual(stale, fresh);
		assert.deepEqual(stale, {shellLatestVersion: SHELL_VERSION, shellNewer: false, modulesChanged: true});
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
					return {reason: 'install-failed'};
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
					return {reason: 'install-failed'};
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

	test('a forced Linux boot with a transient package error launches the committed set', async () => {
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

describe('ModuleUpdater with a renderer bundled in the shell', () => {
	const OLDER_VERSION = '2026.822.9';

	async function storeWithOlderRenderer() {
		const store = await openStore(createUserData());
		installModuleOnDisk(store.root, 'fluxer_renderer', RENDERER_SHA, [['index.html', 'renderer one']], OLDER_VERSION);
		await store.commit({fluxer_renderer: RENDERER_SHA});
		await store.recordManifestFetch({
			etag: null,
			fetchedAt: new Date(NOW).toISOString(),
			manifest: {
				feed: {releaseChannel: RELEASE_CHANNEL, platform: PLATFORM, arch: ARCH},
				metadataVersion: 1,
				manifestSha256: 'c'.repeat(64),
			},
			floor: {fluxer_renderer: NEXT_RENDERER_SHA},
		});
		return store;
	}

	test('a new shell over an older installed renderer launches the bundle with the feed down', async () => {
		const store = await storeWithOlderRenderer();
		const {updater, reports} = createUpdater(
			store,
			{},
			{bundledRendererVersion: SHELL_VERSION, forceStartupUpdate: true, respond: unreachableFeed()},
		);

		const outcome = await updater.run();

		assert.equal(outcome.status, 'unreachable-launch');
		assert.deepEqual(outcome.committed, {});
		assert.deepEqual(store.getCommitted(), {});
		assert.deepEqual(store.getState().previous, {});
		assert.equal(
			reports.some((report) => report.type === 'bundled-renderer-preferred'),
			true,
		);
		assert.deepEqual((await updater.selectServedModules(outcome.committed)).renderer, {
			source: 'bundled',
			version: SHELL_VERSION,
			bundledVersion: SHELL_VERSION,
		});
	});

	test('a feed renderer the bundle already covers is never downloaded', async () => {
		const store = await storeWithOlderRenderer();
		const {updater, requests} = createUpdater(
			store,
			{fluxer_renderer: NEXT_RENDERER_SHA},
			{bundledRendererVersion: SHELL_VERSION, forceStartupUpdate: true, metadataVersion: 2},
		);

		const outcome = await updater.run();

		assert.equal(outcome.status, 'launching');
		assert.deepEqual(outcome.committed, {});
		assert.equal(
			requests.some((url) => String(url).endsWith('package.br')),
			false,
		);
		assert.deepEqual(store.getState().floor, {});
		assert.deepEqual(await updater.checkForUpdate(), {
			shellLatestVersion: SHELL_VERSION,
			shellNewer: false,
			modulesChanged: false,
		});
	});

	test('a feed renderer newer than the bundle is still fetched', async () => {
		const store = await openStore(createUserData());
		const {updater, requests} = createUpdater(
			store,
			{fluxer_renderer: NEXT_RENDERER_SHA},
			{bundledRendererVersion: OLDER_VERSION, respond: missingPackages()},
		);

		await updater.run();

		assert.equal(
			requests.some((url) => String(url).endsWith(`${NEXT_RENDERER_SHA}/package.br`)),
			true,
		);
	});

	test('a renderer module that keeps failing to boot is dropped for the bundle, never relaunched forever', async () => {
		const store = await openStore(createUserData());
		installModuleOnDisk(store.root, 'fluxer_renderer', NEXT_RENDERER_SHA, [['index.html', 'broken']], '2026.900.1');
		await store.commit({fluxer_renderer: NEXT_RENDERER_SHA});
		const served = [];
		for (let launch = 0; launch < 4; launch += 1) {
			const {updater} = createUpdater(
				store,
				{},
				{bundledRendererVersion: SHELL_VERSION, platform: 'linux', respond: unreachableFeed()},
			);
			const outcome = await updater.run();
			served.push((await updater.selectServedModules(outcome.committed)).renderer.source);
		}

		assert.deepEqual(served, ['module', 'module', 'bundled', 'bundled']);
		assert.equal(store.isRejected('fluxer_renderer', NEXT_RENDERER_SHA), true);
	});

	test('a committed on-demand module whose files are gone never blocks an offline launch', async () => {
		for (const platform of ['darwin', 'linux']) {
			const store = await openStore(createUserData());
			const directory = installModuleOnDisk(store.root, 'fluxer_fonts_jp', OVERLAY_SHA, [['a.woff2', 'font']], '0.0.0');
			await store.commit({fluxer_fonts_jp: OVERLAY_SHA});
			await rm(directory, {recursive: true, force: true});
			const {updater} = createUpdater(
				store,
				{},
				{bundledRendererVersion: SHELL_VERSION, platform, respond: unreachableFeed()},
			);

			const outcome = await updater.run();

			assert.equal(outcome.status, platform === 'linux' ? 'launching' : 'unreachable-launch', platform);
			assert.deepEqual(outcome.committed, {}, platform);
		}
	});

	test('a newer feed renderer whose package is missing launches the bundle instead of blocking', async () => {
		const store = await openStore(createUserData());
		const {updater} = createUpdater(
			store,
			{fluxer_renderer: NEXT_RENDERER_SHA},
			{bundledRendererVersion: OLDER_VERSION, respond: missingPackages()},
		);

		const outcome = await updater.run();

		assert.equal(outcome.status, 'launching');
		assert.deepEqual(outcome.committed, {});
		assert.deepEqual(await updater.checkForUpdate(), {
			shellLatestVersion: SHELL_VERSION,
			shellNewer: false,
			modulesChanged: true,
		});
	});

	test('a packaged bundle with no readable version still outranks an installed module', async () => {
		const store = await openStore(createUserData());
		installModuleOnDisk(store.root, 'fluxer_renderer', RENDERER_SHA, [['index.html', 'old']], OLDER_VERSION);
		await store.commit({fluxer_renderer: RENDERER_SHA});
		const updater = new ModuleUpdater({
			store,
			shellVersion: SHELL_VERSION,
			releaseChannel: RELEASE_CHANNEL,
			platform: 'linux',
			arch: ARCH,
			packageOrigin: PACKAGE_ORIGIN,
			hasOfflineRenderer: true,
			bundledRendererVersion: 'dev',
			preferUnversionedBundle: true,
			fetch: unreachableFeed(),
			sleep: async () => {},
			random: () => 0,
			now: () => NOW,
		});

		const outcome = await updater.run();

		assert.equal((await updater.selectServedModules(outcome.committed)).renderer.source, 'bundled');
		assert.deepEqual(store.getCommitted(), {});
	});
});

describe('ModuleUpdater keeps every shell on modules it can run', () => {
	const REPORTER_SHELL = '2026.1008.32323';
	const LIVE_SHELL = '2026.1009.45608';
	const DICTIONARY_SHA = sha256('dictionary-one');
	const NEXT_DICTIONARY_SHA = sha256('dictionary-two');
	const FONT_SHA = sha256('font-one');
	const AHEAD = {fluxer_renderer: {minimum_shell_version: LIVE_SHELL}};

	function feedAhead(overrides = {}) {
		return {
			shellVersion: REPORTER_SHELL,
			shellLatest: LIVE_SHELL,
			shellMinimum: '0.0.0',
			buildVersion: LIVE_SHELL,
			bounds: AHEAD,
			...overrides,
		};
	}

	async function storeFor(shellVersion = REPORTER_SHELL) {
		const userDataPath = createUserData();
		const store = await openStore(userDataPath, shellVersion);
		return {store, storeRoot: getModuleStoreRoot(userDataPath)};
	}

	test('a Linux shell below the renderer floor launches its bundled renderer instead of asking for a shell update', async () => {
		const {store, storeRoot} = await storeFor();
		installModuleOnDisk(storeRoot, 'fluxer_dict_en', DICTIONARY_SHA, [['en.dic', 'words']], LIVE_SHELL);
		seedCachedPackage(store, DICTIONARY_SHA);
		const statuses = [];
		const {updater, requests} = createUpdater(
			store,
			{fluxer_renderer: NEXT_RENDERER_SHA, fluxer_dict_en: DICTIONARY_SHA},
			feedAhead({
				platform: 'linux',
				bundledRendererVersion: REPORTER_SHELL,
				forceStartupUpdate: true,
				onState: (state) => statuses.push(state.status),
			}),
		);

		const outcome = await updater.run();

		assert.equal(outcome.status, 'launching');
		assert.deepEqual(outcome.committed, {fluxer_dict_en: DICTIONARY_SHA});
		assert.equal(
			requests.some((url) => String(url).includes(NEXT_RENDERER_SHA)),
			false,
		);
		assert.equal(statuses.includes('blocked-shell-update'), false);
		assert.equal((await updater.selectServedModules(outcome.committed)).renderer.source, 'bundled');
	});

	test('a Flatpak shell Flathub has not updated yet keeps its renderer and still takes compatible modules', async () => {
		const flathubShell = '2026.1012.154028';
		const liveShell = '2026.1013.15209';
		const {store, storeRoot} = await storeFor(flathubShell);
		installModuleOnDisk(storeRoot, 'fluxer_renderer', RENDERER_SHA, [['index.js', 'renderer']], flathubShell);
		installModuleOnDisk(storeRoot, 'fluxer_dict_en', DICTIONARY_SHA, [['en.dic', 'words']], flathubShell);
		installModuleOnDisk(storeRoot, 'fluxer_dict_en', NEXT_DICTIONARY_SHA, [['en.dic', 'more words']], liveShell);
		seedCachedPackage(store, NEXT_DICTIONARY_SHA);
		await store.commit({fluxer_renderer: RENDERER_SHA, fluxer_dict_en: DICTIONARY_SHA});
		const statuses = [];
		const {updater, requests} = createUpdater(
			store,
			{fluxer_renderer: NEXT_RENDERER_SHA, fluxer_dict_en: NEXT_DICTIONARY_SHA},
			feedAhead({
				shellVersion: flathubShell,
				shellLatest: liveShell,
				buildVersion: liveShell,
				bounds: {fluxer_renderer: {minimum_shell_version: liveShell}},
				platform: 'linux',
				forceStartupUpdate: true,
				required: ['fluxer_renderer'],
				onState: (state) => statuses.push(state.status),
			}),
		);

		const outcome = await updater.run();

		assert.equal(outcome.status, 'launching');
		assert.deepEqual(outcome.committed, {fluxer_dict_en: NEXT_DICTIONARY_SHA, fluxer_renderer: RENDERER_SHA});
		assert.equal(
			requests.some((url) => String(url).includes(NEXT_RENDERER_SHA)),
			false,
		);
		assert.equal(statuses.includes('blocked-shell-update'), false);
		assert.deepEqual(await updater.checkForUpdate(), {
			shellLatestVersion: liveShell,
			shellNewer: true,
			modulesChanged: false,
		});
	});

	test('a Linux shell behind the floor keeps launching its committed compatible renderer', async () => {
		const {store, storeRoot} = await storeFor();
		installModuleOnDisk(storeRoot, 'fluxer_renderer', RENDERER_SHA, [['index.js', 'renderer']], REPORTER_SHELL);
		await store.commit({fluxer_renderer: RENDERER_SHA});
		const {updater, requests} = createUpdater(
			store,
			{fluxer_renderer: NEXT_RENDERER_SHA},
			feedAhead({platform: 'linux', forceStartupUpdate: true}),
		);

		const outcome = await updater.run();

		assert.equal(outcome.status, 'launching');
		assert.deepEqual(outcome.committed, {fluxer_renderer: RENDERER_SHA});
		assert.equal(
			requests.some((url) => String(url).endsWith('package.br')),
			false,
		);
	});

	test('a renderer asset module is not replaced while the served renderer is older than the feed', async () => {
		const {store, storeRoot} = await storeFor();
		installModuleOnDisk(storeRoot, 'fluxer_renderer', RENDERER_SHA, [['index.js', 'renderer']], REPORTER_SHELL);
		installModuleOnDisk(storeRoot, 'fluxer_overlay', OVERLAY_SHA, [['overlay.js', 'overlay']], REPORTER_SHELL);
		await store.commit({fluxer_renderer: RENDERER_SHA, fluxer_overlay: OVERLAY_SHA});
		const {updater, requests} = createUpdater(
			store,
			{fluxer_renderer: NEXT_RENDERER_SHA, fluxer_overlay: NEXT_OVERLAY_SHA},
			feedAhead({platform: 'linux', forceStartupUpdate: true, required: ['fluxer_renderer']}),
		);

		const outcome = await updater.run();

		assert.equal(outcome.status, 'launching');
		assert.deepEqual(outcome.committed, {fluxer_overlay: OVERLAY_SHA, fluxer_renderer: RENDERER_SHA});
		assert.equal(
			requests.some((url) => String(url).endsWith('package.br')),
			false,
		);
		assert.deepEqual(await updater.checkForUpdate(), {
			shellLatestVersion: LIVE_SHELL,
			shellNewer: true,
			modulesChanged: false,
		});
	});

	test('an on-demand renderer asset is unavailable while the served renderer is older than the feed', async () => {
		const {store} = await storeFor();
		const {updater, requests} = createUpdater(
			store,
			{fluxer_renderer: NEXT_RENDERER_SHA, fluxer_fonts_jp: FONT_SHA},
			feedAhead({platform: 'linux', bundledRendererVersion: REPORTER_SHELL, required: ['fluxer_renderer']}),
		);

		assert.deepEqual(await updater.ensureModule('fluxer_fonts_jp'), {module: 'fluxer_fonts_jp', status: 'unavailable'});
		assert.equal(
			requests.some((url) => String(url).endsWith('package.br')),
			false,
		);
	});

	test('a committed renderer asset stays installed while the served renderer is older than the feed', async () => {
		const {store, storeRoot} = await storeFor();
		installModuleOnDisk(storeRoot, 'fluxer_renderer', RENDERER_SHA, [['index.js', 'renderer']], REPORTER_SHELL);
		installModuleOnDisk(storeRoot, 'fluxer_fonts_jp', FONT_SHA, [['font.woff2', 'font']], REPORTER_SHELL);
		await store.commit({fluxer_renderer: RENDERER_SHA, fluxer_fonts_jp: FONT_SHA});
		const {updater} = createUpdater(
			store,
			{fluxer_renderer: NEXT_RENDERER_SHA, fluxer_fonts_jp: sha256('font-two')},
			feedAhead({platform: 'linux', required: ['fluxer_renderer']}),
		);

		assert.deepEqual(await updater.ensureModule('fluxer_fonts_jp'), {
			module: 'fluxer_fonts_jp',
			status: 'already-installed',
		});
	});

	test('an on-demand dictionary installs while the renderer needs a newer shell', async () => {
		const {store, storeRoot} = await storeFor();
		installModuleOnDisk(storeRoot, 'fluxer_dict_en', DICTIONARY_SHA, [['en.dic', 'words']], LIVE_SHELL);
		seedCachedPackage(store, DICTIONARY_SHA);
		const {updater} = createUpdater(
			store,
			{fluxer_renderer: NEXT_RENDERER_SHA, fluxer_dict_en: DICTIONARY_SHA},
			feedAhead({platform: 'linux', bundledRendererVersion: REPORTER_SHELL, required: ['fluxer_renderer']}),
		);

		assert.deepEqual(await updater.ensureModule('fluxer_dict_en'), {module: 'fluxer_dict_en', status: 'installed'});
		assert.deepEqual(store.getCommitted(), {fluxer_dict_en: DICTIONARY_SHA});
		assert.deepEqual(store.getShellRange('fluxer_dict_en', DICTIONARY_SHA), {
			minimum_shell_version: null,
			maximum_shell_version: null,
		});
	});

	test('an AppImage updates its shell first on a forced boot and launches the compatible set when that fails', async () => {
		const {store, storeRoot} = await storeFor();
		installModuleOnDisk(storeRoot, 'fluxer_renderer', RENDERER_SHA, [['index.js', 'renderer']], REPORTER_SHELL);
		await store.commit({fluxer_renderer: RENDERER_SHA});
		const attempts = [];
		const {updater} = createUpdater(
			store,
			{fluxer_renderer: NEXT_RENDERER_SHA},
			feedAhead({
				platform: 'linux',
				forceStartupUpdate: true,
				selfUpdateShellFirst: async (latestVersion) => {
					attempts.push(latestVersion);
					return {reason: 'download-failed'};
				},
			}),
		);

		const outcome = await updater.run();

		assert.deepEqual(attempts, [LIVE_SHELL]);
		assert.equal(outcome.status, 'launching');
		assert.deepEqual(outcome.committed, {fluxer_renderer: RENDERER_SHA});
	});

	test('a Windows shell below the floor launches the compatible set when the shell feed has no newer shell', async () => {
		const {store, storeRoot} = await storeFor();
		installModuleOnDisk(storeRoot, 'fluxer_renderer', RENDERER_SHA, [['index.js', 'renderer']], REPORTER_SHELL);
		await store.commit({fluxer_renderer: RENDERER_SHA});
		const {updater} = createUpdater(
			store,
			{fluxer_renderer: NEXT_RENDERER_SHA},
			feedAhead({platform: 'win32', selfUpdateShellFirst: async () => ({reason: 'no-update'})}),
		);

		const outcome = await updater.run();

		assert.equal(outcome.status, 'launching');
		assert.deepEqual(outcome.committed, {fluxer_renderer: RENDERER_SHA});
		assert.deepEqual(store.getState().floor, {});
	});

	test('a Windows shell below the floor installs a compatible module and launches when the shell feed has no newer shell', async () => {
		const {store, storeRoot} = await storeFor();
		installModuleOnDisk(storeRoot, 'fluxer_renderer', RENDERER_SHA, [['index.js', 'renderer']], REPORTER_SHELL);
		installModuleOnDisk(storeRoot, 'fluxer_dict_en', DICTIONARY_SHA, [['en.dic', 'words']], LIVE_SHELL);
		seedCachedPackage(store, DICTIONARY_SHA);
		await store.commit({fluxer_renderer: RENDERER_SHA});
		const attempts = [];
		const {updater} = createUpdater(
			store,
			{fluxer_renderer: NEXT_RENDERER_SHA, fluxer_dict_en: DICTIONARY_SHA},
			feedAhead({
				platform: 'win32',
				selfUpdateShellFirst: async (latestVersion) => {
					attempts.push(latestVersion);
					return {reason: 'no-update'};
				},
			}),
		);

		const outcome = await updater.run();

		assert.deepEqual(attempts, [LIVE_SHELL]);
		assert.equal(outcome.status, 'launching');
		assert.deepEqual(outcome.committed, {fluxer_dict_en: DICTIONARY_SHA, fluxer_renderer: RENDERER_SHA});
	});

	test('a Windows shell below the floor whose self update fails is blocked', async () => {
		const {store, storeRoot} = await storeFor();
		installModuleOnDisk(storeRoot, 'fluxer_renderer', RENDERER_SHA, [['index.js', 'renderer']], REPORTER_SHELL);
		await store.commit({fluxer_renderer: RENDERER_SHA});
		const {updater} = createUpdater(
			store,
			{fluxer_renderer: NEXT_RENDERER_SHA},
			feedAhead({platform: 'win32', selfUpdateShellFirst: async () => ({reason: 'download-failed'})}),
		);

		const outcome = await updater.run();

		assert.equal(outcome.status, 'blocked-shell-update');
		assert.equal(outcome.latestVersion, LIVE_SHELL);
		assert.deepEqual(store.getCommitted(), {fluxer_renderer: RENDERER_SHA});
	});

	test('a Windows floor keeps the renderer it already enforced when the feed raises it past this shell', async () => {
		const {store, storeRoot} = await storeFor();
		installModuleOnDisk(storeRoot, 'fluxer_renderer', RENDERER_SHA, [['index.js', 'renderer']], REPORTER_SHELL);
		seedCachedPackage(store, RENDERER_SHA);
		const first = createUpdater(
			store,
			{fluxer_renderer: RENDERER_SHA},
			feedAhead({
				platform: 'win32',
				shellLatest: REPORTER_SHELL,
				buildVersion: REPORTER_SHELL,
				bounds: {},
				required: ['fluxer_renderer'],
			}),
		);
		assert.equal((await first.updater.run()).status, 'launching');
		assert.deepEqual(store.getState().floor, {fluxer_renderer: RENDERER_SHA});
		const raised = createUpdater(
			store,
			{fluxer_renderer: NEXT_RENDERER_SHA},
			feedAhead({
				platform: 'win32',
				metadataVersion: 2,
				required: ['fluxer_renderer'],
				selfUpdateShellFirst: async () => ({reason: 'download-failed'}),
			}),
		);

		assert.equal((await raised.updater.run()).status, 'blocked-shell-update');
		assert.deepEqual(store.getState().floor, {fluxer_renderer: RENDERER_SHA});
	});

	test('a renderer installed for a newer shell is retired after a shell downgrade and the bundle serves', async () => {
		const {store, storeRoot} = await storeFor();
		installModuleOnDisk(storeRoot, 'fluxer_renderer', NEXT_RENDERER_SHA, [['index.js', 'renderer']], LIVE_SHELL);
		await store.commit({fluxer_renderer: NEXT_RENDERER_SHA});
		await store.recordShellRanges([
			{module: 'fluxer_renderer', sha256: NEXT_RENDERER_SHA, minimum: LIVE_SHELL, maximum: null},
		]);
		const {updater, reports} = createUpdater(
			store,
			{},
			{shellVersion: REPORTER_SHELL, platform: 'linux', bundledRendererVersion: REPORTER_SHELL},
		);

		const outcome = await updater.run();

		assert.equal(outcome.status, 'launching');
		assert.deepEqual(outcome.committed, {});
		assert.deepEqual(
			reports.filter((report) => report.type === 'shell-incompatible-retired').map((report) => report.module),
			['fluxer_renderer'],
		);
		assert.equal((await updater.selectServedModules(outcome.committed)).renderer.source, 'bundled');
	});

	test('a renderer with no recorded range is retired when the cached manifest says this shell cannot run it', async () => {
		const {store, storeRoot} = await storeFor();
		installModuleOnDisk(storeRoot, 'fluxer_renderer', NEXT_RENDERER_SHA, [['index.js', 'renderer']], LIVE_SHELL);
		await store.commit({fluxer_renderer: NEXT_RENDERER_SHA});
		writeFileSync(
			path.join(store.root, 'manifest.json'),
			manifestBytes({fluxer_renderer: NEXT_RENDERER_SHA}, feedAhead({platform: 'linux'})),
		);
		const {updater, reports} = createUpdater(
			store,
			{},
			{shellVersion: REPORTER_SHELL, platform: 'linux', bundledRendererVersion: REPORTER_SHELL},
		);

		const outcome = await updater.run();

		assert.deepEqual(outcome.committed, {});
		assert.equal(
			reports.some((report) => report.type === 'shell-incompatible-retired'),
			true,
		);
	});

	test('a crash-loop rollback never restores a renderer this shell cannot run', async () => {
		const {store, storeRoot} = await storeFor();
		installModuleOnDisk(storeRoot, 'fluxer_renderer', NEXT_RENDERER_SHA, [['index.js', 'ahead']], LIVE_SHELL);
		installModuleOnDisk(storeRoot, 'fluxer_renderer', RENDERER_SHA, [['index.js', 'current']], LIVE_SHELL);
		await store.commit({fluxer_renderer: NEXT_RENDERER_SHA});
		await store.commit({fluxer_renderer: RENDERER_SHA});
		await store.recordShellRanges([
			{module: 'fluxer_renderer', sha256: NEXT_RENDERER_SHA, minimum: LIVE_SHELL, maximum: null},
		]);
		await store.recordLaunchAttempt();
		await store.recordLaunchAttempt();
		const {updater} = createUpdater(
			store,
			{},
			{shellVersion: REPORTER_SHELL, platform: 'linux', bundledRendererVersion: REPORTER_SHELL},
		);

		const outcome = await updater.run();

		assert.equal(outcome.status, 'launching');
		assert.equal(outcome.committed.fluxer_renderer, undefined);
		assert.equal(store.getState().previous.fluxer_renderer, undefined);
		assert.equal(store.isRejected('fluxer_renderer', RENDERER_SHA), true);
	});

	test('a check offers compatible modules while the renderer needs a newer shell', async () => {
		const {store, storeRoot} = await storeFor();
		installModuleOnDisk(storeRoot, 'fluxer_dict_en', DICTIONARY_SHA, [['en.dic', 'words']], REPORTER_SHELL);
		await store.commit({fluxer_dict_en: DICTIONARY_SHA});
		const {updater} = createUpdater(
			store,
			{fluxer_renderer: NEXT_RENDERER_SHA, fluxer_dict_en: NEXT_DICTIONARY_SHA},
			feedAhead({platform: 'linux', bundledRendererVersion: REPORTER_SHELL}),
		);

		assert.deepEqual(await updater.checkForUpdate(), {
			shellLatestVersion: LIVE_SHELL,
			shellNewer: true,
			modulesChanged: true,
		});
	});

	test('a Linux launch never blocks on linux_security_minimum in the manifest', async () => {
		const {store, storeRoot} = await storeFor();
		installModuleOnDisk(storeRoot, 'fluxer_renderer', RENDERER_SHA, [['index.js', 'renderer']], REPORTER_SHELL);
		await store.commit({fluxer_renderer: RENDERER_SHA});
		const {updater} = createUpdater(
			store,
			{fluxer_renderer: NEXT_RENDERER_SHA},
			feedAhead({
				platform: 'linux',
				forceStartupUpdate: true,
				shellMinimum: LIVE_SHELL,
				extra: {linux_security_minimum: {version: LIVE_SHELL, required_modules: ['fluxer_renderer']}},
			}),
		);

		const outcome = await updater.run();

		assert.equal(outcome.status, 'launching');
		assert.deepEqual(outcome.committed, {fluxer_renderer: RENDERER_SHA});
		assert.equal(Object.hasOwn(updater.getLastState(), 'requiredSecurityUpdate'), false);
	});
});
