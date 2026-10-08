// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {Buffer} from 'node:buffer';
import {createHash} from 'node:crypto';
import {
	cpSync,
	existsSync,
	mkdirSync,
	mkdtempSync,
	readdirSync,
	readFileSync,
	symlinkSync,
	utimesSync,
	writeFileSync,
} from 'node:fs';
import fsPromises, {rm} from 'node:fs/promises';
import {registerHooks} from 'node:module';
import os from 'node:os';
import path from 'node:path';
import {after, describe, test} from 'node:test';
import zlib from 'node:zlib';

const DESKTOP_SRC = new URL('../', import.meta.url);
const PACKAGES = new URL('../../../packages/', import.meta.url);

registerHooks({
	resolve(specifier, context, nextResolve) {
		if (specifier.startsWith('@electron/')) {
			return {shortCircuit: true, url: new URL(`${specifier.slice('@electron/'.length)}.ts`, DESKTOP_SRC).href};
		}
		if (specifier.startsWith('@fluxer/')) {
			return {shortCircuit: true, url: new URL(`${specifier.slice('@fluxer/'.length)}.ts`, PACKAGES).href};
		}
		return nextResolve(specifier, context);
	},
});

const {
	MODULE_FILE_STAMPS_NAME,
	MODULE_INCOMING_STORE_PREFIX,
	MODULE_PACKAGE_MAX_AGE_MS,
	MODULE_STATE_FILE_NAME,
	MODULE_STATE_VERSION,
	ModuleManifestEquivocationError,
	ModuleManifestRollbackError,
	ModuleStore,
	ModuleStoreDownloadHashMismatchError,
	ModuleStoreGarbageCollectionOrderError,
	ModuleStoreInstallationMissingError,
	getModuleStoreRoot,
} = await import('./ModuleStore.ts');
const {ModulePackageHashMismatchError} = await import('./ModulePackage.ts');

const BLOCK = 512;
const SHELL_VERSION = '2026.823.1';
const RELEASE_CHANNEL = 'canary';
const SOURCE_SHA = 'a'.repeat(40);
const MANIFEST_FEED = {releaseChannel: RELEASE_CHANNEL, platform: 'darwin', arch: 'arm64'};
const MANIFEST_FETCHED_AT = '2026-08-23T12:00:00.000Z';

const temporaryRoots = [];

after(async () => {
	for (const root of temporaryRoots) {
		await rm(root, {recursive: true, force: true});
	}
});

function createUserData() {
	const root = mkdtempSync(path.join(os.tmpdir(), 'module-store-'));
	temporaryRoots.push(root);
	return root;
}

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

function packModule(moduleName, entries, {declaredSha256 = {}} = {}) {
	const files = entries.map(([relative, body]) => ({
		path: relative,
		sha256: declaredSha256[relative] ?? sha256(Buffer.from(body, 'utf8')),
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
	return {module: moduleName, packed, sha256: sha256(packed), files};
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

async function openStore(userDataPath) {
	return await ModuleStore.open({
		root: getModuleStoreRoot(userDataPath),
		shellVersion: SHELL_VERSION,
		releaseChannel: RELEASE_CHANNEL,
	});
}

function deferred() {
	let resolve;
	const promise = new Promise((resolvePromise) => {
		resolve = resolvePromise;
	});
	return {promise, resolve};
}

async function markLaunched(store) {
	return await store.markLaunchAttemptSucceeded(await store.recordLaunchAttempt());
}

async function install(store, packageFile) {
	const installed = await store.installModule({
		module: packageFile.module,
		sha256: packageFile.sha256,
		download: () => streamOf(packageFile.packed),
	});
	await store.settleInstalledModules();
	return installed;
}

function manifestObservation(metadataVersion, manifestSha256) {
	return {feed: MANIFEST_FEED, metadataVersion, manifestSha256};
}

async function recordManifest(store, metadataVersion, manifestSha256) {
	return await store.recordManifestFetch({
		etag: null,
		fetchedAt: MANIFEST_FETCHED_AT,
		manifest: manifestObservation(metadataVersion, manifestSha256),
	});
}

function readState(userDataPath) {
	return JSON.parse(readFileSync(path.join(getModuleStoreRoot(userDataPath), MODULE_STATE_FILE_NAME), 'utf8'));
}

const RENDERER_V1 = packModule('fluxer_renderer', [
	['assets/index-v1.js', 'renderer one'],
	['assets/style-v1.css', 'body{}'],
]);
const RENDERER_V2 = packModule('fluxer_renderer', [['assets/index-v2.js', 'renderer two']]);
const GRAMMARS_V1 = packModule('fluxer_grammars', [['assets/rust-v1.wasm', 'grammar one']]);
const RENDERER_BAD_MEMBER = packModule('fluxer_renderer', [['assets/index-v1.js', 'renderer one']], {
	declaredSha256: {'assets/index-v1.js': 'e'.repeat(64)},
});

describe('ModuleStore', () => {
	test('lives beside the account store, never inside it', () => {
		const userDataPath = createUserData();
		const root = getModuleStoreRoot(userDataPath);
		assert.equal(root, path.join(userDataPath, 'modules'));
		assert.equal(path.dirname(root), userDataPath);
		assert.equal(existsSync(path.join(userDataPath, 'desktop-app-store.sqlite3')), false);
	});

	test('says when the shell changed version since the store was last opened', async () => {
		const userDataPath = createUserData();
		assert.equal((await openStore(userDataPath)).shellVersionChanged, false);
		assert.equal((await openStore(userDataPath)).shellVersionChanged, false);

		const upgraded = await ModuleStore.open({
			root: getModuleStoreRoot(userDataPath),
			shellVersion: '2026.824.1',
			releaseChannel: RELEASE_CHANNEL,
		});

		assert.equal(upgraded.shellVersionChanged, true);
		assert.notEqual(
			readState(userDataPath).shell_version,
			'2026.824.1',
			'a quit or crash before the modules converge has to force the update again on the next boot',
		);
		const reopened = await ModuleStore.open({
			root: getModuleStoreRoot(userDataPath),
			shellVersion: '2026.824.1',
			releaseChannel: RELEASE_CHANNEL,
		});
		assert.equal(reopened.shellVersionChanged, true);

		await reopened.recordShellVersionConverged();

		assert.equal(readState(userDataPath).shell_version, '2026.824.1');
		const converged = await ModuleStore.open({
			root: getModuleStoreRoot(userDataPath),
			shellVersion: '2026.824.1',
			releaseChannel: RELEASE_CHANNEL,
		});
		assert.equal(converged.shellVersionChanged, false);
	});

	test('writes state.json atomically and leaves no temporary behind', async () => {
		const userDataPath = createUserData();
		const store = await openStore(userDataPath);
		await install(store, RENDERER_V1);
		await install(store, GRAMMARS_V1);
		const before = readState(userDataPath);
		assert.deepEqual(before.committed, {});

		await store.commit({fluxer_renderer: RENDERER_V1.sha256, fluxer_grammars: GRAMMARS_V1.sha256});

		const after = readState(userDataPath);
		assert.equal(after.state_version, MODULE_STATE_VERSION);
		assert.deepEqual(after.committed, {
			fluxer_grammars: GRAMMARS_V1.sha256,
			fluxer_renderer: RENDERER_V1.sha256,
		});
		assert.deepEqual(after.previous, {});
		assert.equal(after.boot_attempt, 0);
		const leftovers = readdirSync(getModuleStoreRoot(userDataPath)).filter((name) => name.endsWith('.tmp'));
		assert.deepEqual(leftovers, []);

		const reopened = await openStore(userDataPath);
		assert.deepEqual(reopened.getCommitted(), after.committed);
	});

	test('moves the old committed map to previous on the next commit', async () => {
		const userDataPath = createUserData();
		const store = await openStore(userDataPath);
		await install(store, RENDERER_V1);
		await store.commit({fluxer_renderer: RENDERER_V1.sha256});
		await install(store, RENDERER_V2);
		await store.commit({fluxer_renderer: RENDERER_V2.sha256});

		const state = readState(userDataPath);
		assert.deepEqual(state.committed, {fluxer_renderer: RENDERER_V2.sha256});
		assert.deepEqual(state.previous, {fluxer_renderer: RENDERER_V1.sha256});
	});

	test('a crash before the commit point leaves the old set committed', async () => {
		const userDataPath = createUserData();
		const first = await openStore(userDataPath);
		await install(first, RENDERER_V1);
		await first.commit({fluxer_renderer: RENDERER_V1.sha256});

		const crashing = await openStore(userDataPath);
		const staged = await install(crashing, RENDERER_V2);
		assert.equal(existsSync(path.join(staged.directory, 'module.json')), true);

		const rebooted = await openStore(userDataPath);
		assert.deepEqual(rebooted.getCommitted(), {fluxer_renderer: RENDERER_V1.sha256});
		const index = await rebooted.buildModuleIndex();
		assert.deepEqual([...index.keys()].sort(), ['assets/index-v1.js', 'assets/style-v1.css']);
		assert.equal(existsSync(staged.directory), true);
	});

	test('a download hash mismatch fails that module only and caches nothing', async () => {
		const userDataPath = createUserData();
		const store = await openStore(userDataPath);
		await assert.rejects(
			store.installModule({
				module: 'fluxer_renderer',
				sha256: RENDERER_V1.sha256,
				download: () => streamOf(Buffer.from('not the package', 'utf8')),
			}),
			ModuleStoreDownloadHashMismatchError,
		);
		assert.equal(existsSync(store.getPackagePath(RENDERER_V1.sha256)), false);
		assert.deepEqual(readdirSync(store.incomingDownloadRoot), []);

		const installed = await install(store, GRAMMARS_V1);
		assert.equal(existsSync(path.join(installed.directory, 'module.json')), true);
		await store.commit({fluxer_grammars: GRAMMARS_V1.sha256});
		assert.deepEqual(readState(userDataPath).committed, {fluxer_grammars: GRAMMARS_V1.sha256});
	});

	test('a member digest mismatch leaves the verified package cached for every later attempt', async () => {
		const userDataPath = createUserData();
		const store = await openStore(userDataPath);
		let downloads = 0;
		const attempt = () =>
			store.installModule({
				module: 'fluxer_renderer',
				sha256: RENDERER_BAD_MEMBER.sha256,
				download: () => {
					downloads += 1;
					return streamOf(RENDERER_BAD_MEMBER.packed);
				},
			});
		const memberMismatch = (error) =>
			error instanceof ModulePackageHashMismatchError && error.message === 'hash mismatch: assets/index-v1.js';
		const packagePath = store.getPackagePath(RENDERER_BAD_MEMBER.sha256);

		await assert.rejects(attempt(), memberMismatch);
		assert.equal(downloads, 1);
		assert.equal(existsSync(packagePath), true);

		await assert.rejects(attempt(), memberMismatch);
		assert.equal(existsSync(packagePath), true);
		await assert.rejects(attempt(), memberMismatch);

		assert.equal(downloads, 1);
		assert.deepEqual(readdirSync(store.storeRoot), []);
	});

	test('a cached package whose own digest no longer matches is dropped and downloaded again', async () => {
		const userDataPath = createUserData();
		const store = await openStore(userDataPath);
		let downloads = 0;
		const attempt = () =>
			store.installModule({
				module: 'fluxer_renderer',
				sha256: RENDERER_BAD_MEMBER.sha256,
				download: () => {
					downloads += 1;
					return streamOf(RENDERER_BAD_MEMBER.packed);
				},
			});
		const packagePath = store.getPackagePath(RENDERER_BAD_MEMBER.sha256);

		await assert.rejects(attempt(), ModulePackageHashMismatchError);
		assert.equal(downloads, 1);
		writeFileSync(packagePath, 'not the package');

		await assert.rejects(
			attempt(),
			(error) =>
				error instanceof ModulePackageHashMismatchError && error.message.startsWith('package sha256 mismatch:'),
		);
		assert.equal(existsSync(packagePath), false);

		await assert.rejects(attempt(), ModulePackageHashMismatchError);
		assert.equal(downloads, 2);
		assert.equal(existsSync(packagePath), true);
	});

	test('commit refuses when a store directory is missing', async () => {
		const userDataPath = createUserData();
		const store = await openStore(userDataPath);
		const installed = await install(store, RENDERER_V1);
		await rm(installed.directory, {recursive: true, force: true});

		await assert.rejects(store.commit({fluxer_renderer: RENDERER_V1.sha256}), ModuleStoreInstallationMissingError);
		assert.deepEqual(readState(userDataPath).committed, {});
	});

	test('commit refuses to re-affirm a committed module whose directory was deleted', async () => {
		const userDataPath = createUserData();
		const store = await openStore(userDataPath);
		const installed = await install(store, RENDERER_V1);
		await store.commit({fluxer_renderer: RENDERER_V1.sha256});
		await rm(installed.directory, {recursive: true, force: true});

		await assert.rejects(store.commit({fluxer_renderer: RENDERER_V1.sha256}), ModuleStoreInstallationMissingError);
		assert.deepEqual(readState(userDataPath).committed, {fluxer_renderer: RENDERER_V1.sha256});
	});

	test('commit refuses when module.json is unreadable', async () => {
		const userDataPath = createUserData();
		const store = await openStore(userDataPath);
		const installed = await install(store, RENDERER_V1);
		writeFileSync(path.join(installed.directory, 'module.json'), 'not json');

		await assert.rejects(store.commit({fluxer_renderer: RENDERER_V1.sha256}), ModuleStoreInstallationMissingError);
	});

	test('garbage collection only runs after a successful launch', async () => {
		const userDataPath = createUserData();
		const store = await openStore(userDataPath);
		await assert.rejects(store.collectGarbage(), ModuleStoreGarbageCollectionOrderError);
	});

	test('garbage collection preserves committed and previous', async () => {
		const userDataPath = createUserData();
		const earlierRun = await openStore(userDataPath);
		const v1 = await install(earlierRun, RENDERER_V1);
		await earlierRun.commit({fluxer_renderer: RENDERER_V1.sha256});
		const v2 = await install(earlierRun, RENDERER_V2);
		await earlierRun.commit({fluxer_renderer: RENDERER_V2.sha256});
		const orphan = await install(earlierRun, GRAMMARS_V1);
		const store = await openStore(userDataPath);

		const incoming = path.join(store.storeRoot, `${MODULE_INCOMING_STORE_PREFIX}${'b'.repeat(64)}`);
		mkdirSync(incoming, {recursive: true});
		const stale = path.join(store.downloadRoot, `${'c'.repeat(64)}.br`);
		writeFileSync(stale, 'stale');
		const staleSeconds = (Date.now() - MODULE_PACKAGE_MAX_AGE_MS - 60_000) / 1000;
		utimesSync(stale, staleSeconds, staleSeconds);
		const fresh = store.getPackagePath(RENDERER_V2.sha256);
		assert.equal(existsSync(fresh), true);

		await markLaunched(store);
		const result = await store.collectGarbage();

		assert.equal(existsSync(v1.directory), true);
		assert.equal(existsSync(v2.directory), true);
		assert.equal(existsSync(orphan.directory), false);
		assert.equal(existsSync(incoming), false);
		assert.equal(existsSync(stale), false);
		assert.equal(existsSync(fresh), true);
		assert.equal(result.removedDirectories.includes(incoming), true);
		assert.equal(result.removedPackages.includes(stale), true);
	});

	test('garbage collection keeps an installation that has not been committed yet', async () => {
		const userDataPath = createUserData();
		const store = await openStore(userDataPath);
		await install(store, RENDERER_V1);
		await store.commit({fluxer_renderer: RENDERER_V1.sha256});
		await markLaunched(store);
		const pending = await install(store, GRAMMARS_V1);

		await store.collectGarbage();

		assert.equal(existsSync(path.join(pending.directory, 'module.json')), true);
		await store.mergeCommitted({fluxer_grammars: GRAMMARS_V1.sha256});
		assert.deepEqual(readState(userDataPath).committed, {
			fluxer_grammars: GRAMMARS_V1.sha256,
			fluxer_renderer: RENDERER_V1.sha256,
		});
	});

	test('garbage collection waits for an in-flight install instead of deleting its staging tree', async () => {
		const userDataPath = createUserData();
		const store = await openStore(userDataPath);
		await install(store, RENDERER_V1);
		await store.commit({fluxer_renderer: RENDERER_V1.sha256});
		await markLaunched(store);

		const staging = path.join(store.storeRoot, `${MODULE_INCOMING_STORE_PREFIX}${GRAMMARS_V1.sha256}`);
		const rename = fsPromises.rename;
		const staged = deferred();
		const release = deferred();
		fsPromises.rename = async (from, to) => {
			if (from === staging) {
				staged.resolve();
				await release.promise;
			}
			return await rename(from, to);
		};
		try {
			const installing = install(store, GRAMMARS_V1);
			await staged.promise;
			const collecting = store.collectGarbage();
			await new Promise((resolve) => setTimeout(resolve, 50));

			assert.equal(existsSync(staging), true);
			release.resolve();
			const installed = await installing;
			const result = await collecting;
			assert.equal(existsSync(path.join(installed.directory, 'module.json')), true);
			assert.equal(result.removedDirectories.includes(staging), false);
		} finally {
			fsPromises.rename = rename;
		}
	});

	test('garbage collection keeps a cached package that an install is already using', async () => {
		const userDataPath = createUserData();
		const store = await openStore(userDataPath);
		await install(store, RENDERER_V1);
		await store.commit({fluxer_renderer: RENDERER_V1.sha256});
		await markLaunched(store);
		const reclaimed = await install(store, GRAMMARS_V1);
		await rm(reclaimed.directory, {recursive: true, force: true});
		const cached = store.getPackagePath(GRAMMARS_V1.sha256);
		const staleSeconds = (Date.now() - MODULE_PACKAGE_MAX_AGE_MS - 60_000) / 1000;
		utimesSync(cached, staleSeconds, staleSeconds);

		const readdir = fsPromises.readdir;
		const sweeping = deferred();
		const release = deferred();
		let held = false;
		fsPromises.readdir = async (directory, options) => {
			if (directory === store.storeRoot && !held) {
				held = true;
				sweeping.resolve();
				await release.promise;
			}
			return await readdir(directory, options);
		};
		try {
			const collecting = store.collectGarbage();
			await sweeping.promise;
			const installing = install(store, GRAMMARS_V1);
			await new Promise((resolve) => setTimeout(resolve, 50));
			release.resolve();
			const result = await collecting;
			const installed = await installing;

			assert.equal(existsSync(cached), true);
			assert.equal(result.removedPackages.includes(cached), false);
			assert.equal(existsSync(path.join(installed.directory, 'module.json')), true);
		} finally {
			fsPromises.readdir = readdir;
		}
	});

	test('a second rollback onto the same set reports no rollback, so the update path still runs', async () => {
		const userDataPath = createUserData();
		const store = await openStore(userDataPath);
		await install(store, RENDERER_V1);
		await store.commit({fluxer_renderer: RENDERER_V1.sha256});
		await install(store, RENDERER_V2);
		await store.commit({fluxer_renderer: RENDERER_V2.sha256});

		for (let boot = 0; boot < 2; boot += 1) {
			const failing = await openStore(userDataPath);
			await failing.beginBootAttempt();
			await failing.recordLaunchAttempt();
		}
		const rolledBackStore = await openStore(userDataPath);
		const rollback = await rolledBackStore.beginBootAttempt();
		assert.equal(rollback.rolledBack, true);
		assert.deepEqual(rollback.committed, {fluxer_renderer: RENDERER_V1.sha256});
		await rolledBackStore.recordLaunchAttempt();

		for (let boot = 0; boot < 1; boot += 1) {
			const failing = await openStore(userDataPath);
			await failing.beginBootAttempt();
			await failing.recordLaunchAttempt();
		}
		const secondRollback = await (await openStore(userDataPath)).beginBootAttempt();

		assert.equal(secondRollback.rolledBack, false);
		assert.equal(secondRollback.bootAttempt, 0);
		assert.deepEqual(secondRollback.committed, {fluxer_renderer: RENDERER_V1.sha256});
		assert.equal(readState(userDataPath).boot_attempt, 0);
	});

	test('a first-ever committed set is never rolled back into an empty committed map', async () => {
		const userDataPath = createUserData();
		const store = await openStore(userDataPath);
		await install(store, RENDERER_V1);
		await store.commit({fluxer_renderer: RENDERER_V1.sha256});

		for (let boot = 0; boot < 2; boot += 1) {
			const failing = await openStore(userDataPath);
			await failing.beginBootAttempt();
			await failing.recordLaunchAttempt();
		}
		const rollback = await (await openStore(userDataPath)).beginBootAttempt();

		assert.equal(rollback.rolledBack, false);
		assert.equal(rollback.bootAttempt, 0);
		assert.deepEqual(rollback.committed, {fluxer_renderer: RENDERER_V1.sha256});
		assert.deepEqual(readState(userDataPath).committed, {fluxer_renderer: RENDERER_V1.sha256});
	});

	test('rolls back to the previous set after two failed boots', async () => {
		const userDataPath = createUserData();
		const store = await openStore(userDataPath);
		await install(store, RENDERER_V1);
		await store.commit({fluxer_renderer: RENDERER_V1.sha256});
		await install(store, RENDERER_V2);
		await store.commit({fluxer_renderer: RENDERER_V2.sha256});

		const firstStore = await openStore(userDataPath);
		const firstBoot = await firstStore.beginBootAttempt();
		assert.deepEqual(firstBoot, {
			rolledBack: false,
			bootAttempt: 0,
			committed: {fluxer_renderer: RENDERER_V2.sha256},
		});
		await firstStore.recordLaunchAttempt();
		assert.equal(readState(userDataPath).boot_attempt, 1);

		const secondStore = await openStore(userDataPath);
		const secondBoot = await secondStore.beginBootAttempt();
		assert.equal(secondBoot.rolledBack, false);
		assert.equal(secondBoot.bootAttempt, 1);
		await secondStore.recordLaunchAttempt();
		assert.equal(readState(userDataPath).boot_attempt, 2);

		const thirdStore = await openStore(userDataPath);
		const thirdBoot = await thirdStore.beginBootAttempt();
		assert.equal(thirdBoot.rolledBack, true);
		assert.equal(thirdBoot.bootAttempt, 0);
		assert.deepEqual(thirdBoot.committed, {fluxer_renderer: RENDERER_V1.sha256});
		assert.deepEqual(readState(userDataPath).committed, {fluxer_renderer: RENDERER_V1.sha256});

		const index = await thirdStore.buildModuleIndex();
		assert.deepEqual([...index.keys()].sort(), ['assets/index-v1.js', 'assets/style-v1.css']);

		await markLaunched(thirdStore);
		assert.equal(readState(userDataPath).boot_attempt, 0);
		const fourthBoot = await (await openStore(userDataPath)).beginBootAttempt();
		assert.equal(fourthBoot.rolledBack, false);
		assert.equal(fourthBoot.bootAttempt, 0);
	});

	test('a boot that never launches leaves the attempt counter alone, so blocking cannot downgrade anyone', async () => {
		const userDataPath = createUserData();
		const store = await openStore(userDataPath);
		await install(store, RENDERER_V1);
		await store.commit({fluxer_renderer: RENDERER_V1.sha256});
		await install(store, RENDERER_V2);
		await store.commit({fluxer_renderer: RENDERER_V2.sha256});

		for (let attempt = 0; attempt < 5; attempt += 1) {
			const blocked = await openStore(userDataPath);
			assert.equal((await blocked.beginBootAttempt()).rolledBack, false);
		}

		assert.equal(readState(userDataPath).boot_attempt, 0);
		assert.deepEqual(readState(userDataPath).committed, {fluxer_renderer: RENDERER_V2.sha256});
	});

	test('the attempt counter is fsynced before the handoff, not after it', async () => {
		const userDataPath = createUserData();
		const store = await openStore(userDataPath);
		await install(store, RENDERER_V1);
		await store.commit({fluxer_renderer: RENDERER_V1.sha256});

		await store.recordLaunchAttempt();

		assert.equal(readState(userDataPath).boot_attempt, 1);
		assert.equal(store.getState().boot_attempt, 1);
	});

	test('builds an index map across every committed module', async () => {
		const userDataPath = createUserData();
		const store = await openStore(userDataPath);
		const renderer = await install(store, RENDERER_V1);
		const grammars = await install(store, GRAMMARS_V1);
		await install(store, RENDERER_V2);
		await store.commit({fluxer_renderer: RENDERER_V1.sha256, fluxer_grammars: GRAMMARS_V1.sha256});

		const index = await store.buildModuleIndex();
		assert.equal(index.size, 3);
		assert.equal(index.get('assets/index-v1.js'), path.join(renderer.directory, 'assets', 'index-v1.js'));
		assert.equal(index.get('assets/style-v1.css'), path.join(renderer.directory, 'assets', 'style-v1.css'));
		assert.equal(index.get('assets/rust-v1.wasm'), path.join(grammars.directory, 'assets', 'rust-v1.wasm'));
		assert.equal(index.has('assets/index-v2.js'), false);
		for (const absolute of index.values()) {
			assert.equal(existsSync(absolute), true);
		}
		assert.equal(readFileSync(index.get('assets/rust-v1.wasm'), 'utf8'), 'grammar one');
	});

	test('builds the index from one committed generation even when a merge lands mid-scan', async () => {
		const userDataPath = createUserData();
		const store = await openStore(userDataPath);
		const renderer = await install(store, RENDERER_V1);
		await install(store, GRAMMARS_V1);
		await install(store, RENDERER_V2);
		await store.commit({fluxer_renderer: RENDERER_V1.sha256, fluxer_grammars: GRAMMARS_V1.sha256});

		const grammarsDirectory = store.getModuleDirectory('fluxer_grammars', GRAMMARS_V1.sha256);
		const lstat = fsPromises.lstat;
		const scanning = deferred();
		const release = deferred();
		let held = false;
		fsPromises.lstat = async (target, ...rest) => {
			if (target === grammarsDirectory && !held) {
				held = true;
				scanning.resolve();
				await release.promise;
			}
			return await lstat(target, ...rest);
		};
		try {
			const building = store.buildModuleIndex();
			await scanning.promise;
			await store.mergeCommitted({fluxer_renderer: RENDERER_V2.sha256});
			release.resolve();
			const index = await building;

			assert.deepEqual([...index.keys()].sort(), ['assets/index-v1.js', 'assets/rust-v1.wasm', 'assets/style-v1.css']);
			assert.equal(index.get('assets/index-v1.js'), path.join(renderer.directory, 'assets', 'index-v1.js'));
		} finally {
			fsPromises.lstat = lstat;
		}
	});

	test('a served file swapped for different bytes of the same length is no longer installed', async () => {
		const userDataPath = createUserData();
		const store = await openStore(userDataPath);
		const installed = await install(store, RENDERER_V1);
		await store.commit({fluxer_renderer: RENDERER_V1.sha256});
		const served = path.join(installed.directory, 'assets', 'index-v1.js');
		writeFileSync(served, 'RENDERER ONE');
		assert.equal(readFileSync(served).length, Buffer.byteLength('renderer one', 'utf8'));

		assert.equal(await store.isInstalled('fluxer_renderer', RENDERER_V1.sha256), false);
		await assert.rejects(store.buildModuleIndex(), ModuleStoreInstallationMissingError);
		await assert.rejects(store.commit({fluxer_renderer: RENDERER_V1.sha256}), ModuleStoreInstallationMissingError);
	});

	test('a repair reinstall keeps the committed tree served until the replacement is renamed in', async () => {
		const userDataPath = createUserData();
		const store = await openStore(userDataPath);
		const installed = await install(store, RENDERER_V1);
		await store.commit({fluxer_renderer: RENDERER_V1.sha256});
		const corrupted = path.join(installed.directory, 'assets', 'index-v1.js');
		const untouched = path.join(installed.directory, 'assets', 'style-v1.css');
		writeFileSync(corrupted, 'RENDERER ONE');
		assert.equal(await store.isInstalled('fluxer_renderer', RENDERER_V1.sha256), false);

		const incoming = path.join(store.storeRoot, `${MODULE_INCOMING_STORE_PREFIX}${RENDERER_V1.sha256}`);
		const mkdir = fsPromises.mkdir;
		const servedDuringExtraction = [];
		fsPromises.mkdir = async (target, ...rest) => {
			if (typeof target === 'string' && (target === incoming || target.startsWith(`${incoming}${path.sep}`))) {
				servedDuringExtraction.push(existsSync(untouched));
			}
			return await mkdir(target, ...rest);
		};
		let repaired;
		try {
			repaired = await install(store, RENDERER_V1);
		} finally {
			fsPromises.mkdir = mkdir;
		}

		assert.equal(servedDuringExtraction.length > 0, true);
		assert.deepEqual(
			servedDuringExtraction.filter((present) => !present),
			[],
		);
		assert.equal(repaired.directory, installed.directory);
		assert.equal(readFileSync(corrupted, 'utf8'), 'renderer one');
		assert.equal(await store.isInstalled('fluxer_renderer', RENDERER_V1.sha256), true);
		assert.deepEqual(
			readdirSync(store.storeRoot).filter((name) => name.startsWith('.')),
			[],
		);
		const index = await store.buildModuleIndex();
		assert.deepEqual([...index.keys()].sort(), ['assets/index-v1.js', 'assets/style-v1.css']);
	});

	test('a manifest below the highest metadata_version seen for its feed is refused as a downgrade', async () => {
		const userDataPath = createUserData();
		const store = await openStore(userDataPath);
		await recordManifest(store, 7, 'd'.repeat(64));

		assert.throws(
			() => store.requireManifestFresh(manifestObservation(6, 'e'.repeat(64))),
			ModuleManifestRollbackError,
		);
		await assert.rejects(recordManifest(store, 6, 'e'.repeat(64)), ModuleManifestRollbackError);

		const highWater = readState(userDataPath).manifest_high_water;
		assert.equal(highWater.length, 1);
		assert.equal(highWater[0].metadata_version, 7);
		assert.equal(highWater[0].manifest_sha256, 'd'.repeat(64));
		await recordManifest(store, 8, 'f'.repeat(64));
		assert.equal(readState(userDataPath).manifest_high_water[0].metadata_version, 8);
	});

	test('a second manifest body at an already seen metadata_version is refused as equivocation', async () => {
		const userDataPath = createUserData();
		const store = await openStore(userDataPath);
		await recordManifest(store, 7, 'd'.repeat(64));

		assert.throws(
			() => store.requireManifestFresh(manifestObservation(7, 'e'.repeat(64))),
			ModuleManifestEquivocationError,
		);
		await assert.rejects(recordManifest(store, 7, 'e'.repeat(64)), ModuleManifestEquivocationError);

		store.requireManifestFresh(manifestObservation(7, 'd'.repeat(64)));
		await recordManifest(store, 7, 'd'.repeat(64));
		const highWater = readState(userDataPath).manifest_high_water;
		assert.equal(highWater.length, 1);
		assert.equal(highWater[0].manifest_sha256, 'd'.repeat(64));
	});

	test('a superseded launch attempt cannot clear the boot counter that drives rollback', async () => {
		const userDataPath = createUserData();
		const store = await openStore(userDataPath);
		await install(store, RENDERER_V1);
		await store.commit({fluxer_renderer: RENDERER_V1.sha256});
		const superseded = await store.recordLaunchAttempt();
		const active = await store.recordLaunchAttempt();

		assert.equal(await store.markLaunchAttemptSucceeded(superseded), false);
		assert.equal(readState(userDataPath).boot_attempt, 2);
		assert.equal(store.getState().boot_attempt, 2);

		assert.equal(await store.markLaunchAttemptSucceeded(active), true);
		assert.equal(readState(userDataPath).boot_attempt, 0);
	});

	test('a store directory whose module.json names another module is not an installation of it', async () => {
		const userDataPath = createUserData();
		const store = await openStore(userDataPath);
		const renderer = await install(store, RENDERER_V1);
		const impostor = store.getModuleDirectory('fluxer_grammars', RENDERER_V1.sha256);
		mkdirSync(path.dirname(impostor), {recursive: true});
		cpSync(renderer.directory, impostor, {recursive: true});

		assert.equal(await store.isInstalled('fluxer_grammars', RENDERER_V1.sha256), false);
		assert.equal(await store.getInstalledManifest('fluxer_grammars', RENDERER_V1.sha256), null);
		await assert.rejects(store.commit({fluxer_grammars: RENDERER_V1.sha256}), ModuleStoreInstallationMissingError);
	});

	test('a symlinked store directory is never served, even when it points at a real installation', async () => {
		const userDataPath = createUserData();
		const store = await openStore(userDataPath);
		const renderer = await install(store, RENDERER_V1);
		const outside = path.join(userDataPath, 'outside-the-store');
		cpSync(renderer.directory, outside, {recursive: true});
		const linkedSha256 = 'b'.repeat(64);
		const linked = store.getModuleDirectory('fluxer_renderer', linkedSha256);
		symlinkSync(outside, linked, 'dir');
		assert.equal(existsSync(path.join(linked, 'module.json')), true);

		assert.equal(await store.isInstalled('fluxer_renderer', linkedSha256), false);
		await assert.rejects(store.commit({fluxer_renderer: linkedSha256}), ModuleStoreInstallationMissingError);
	});

	test('an index built over a deleted committed module fails loudly', async () => {
		const userDataPath = createUserData();
		const store = await openStore(userDataPath);
		const installed = await install(store, RENDERER_V1);
		await store.commit({fluxer_renderer: RENDERER_V1.sha256});
		await rm(installed.directory, {recursive: true, force: true});

		await assert.rejects(store.buildModuleIndex(), ModuleStoreInstallationMissingError);
	});
});

function chunksOf(buffer) {
	return (async function* stream() {
		for (let offset = 0; offset < buffer.length; offset += 4096) {
			yield buffer.subarray(offset, Math.min(offset + 4096, buffer.length));
		}
	})();
}

function interruptedAfter(buffer, bytes) {
	return (async function* stream() {
		yield buffer.subarray(0, bytes);
		throw new Error('connection reset');
	})();
}

describe('ModuleStore resumable package downloads', () => {
	test('an interrupted download keeps its bytes and the next attempt resumes from them', async () => {
		const userDataPath = createUserData();
		const store = await openStore(userDataPath);
		const cut = Math.floor(RENDERER_V1.packed.length / 2);
		const offsets = [];
		await assert.rejects(
			store.installModule({
				module: 'fluxer_renderer',
				sha256: RENDERER_V1.sha256,
				download: (resumeFrom) => {
					offsets.push(resumeFrom);
					return {offset: 0, chunks: interruptedAfter(RENDERER_V1.packed, cut)};
				},
			}),
		);
		assert.equal(readFileSync(store.getPartialPackagePath(RENDERER_V1.sha256)).length, cut);

		const installed = await store.installModule({
			module: 'fluxer_renderer',
			sha256: RENDERER_V1.sha256,
			download: (resumeFrom) => {
				offsets.push(resumeFrom);
				return {offset: resumeFrom, chunks: chunksOf(RENDERER_V1.packed.subarray(resumeFrom))};
			},
		});

		assert.deepEqual(offsets, [0, cut]);
		assert.equal(existsSync(path.join(installed.directory, 'module.json')), true);
		assert.equal(existsSync(store.getPartialPackagePath(RENDERER_V1.sha256)), false);
		assert.equal(existsSync(store.getPackagePath(RENDERER_V1.sha256)), true);
	});

	test('a server that ignores the range restarts the download from the first byte', async () => {
		const userDataPath = createUserData();
		const store = await openStore(userDataPath);
		mkdirSync(store.incomingDownloadRoot, {recursive: true});
		writeFileSync(store.getPartialPackagePath(RENDERER_V1.sha256), RENDERER_V1.packed.subarray(0, 100));

		const installed = await store.installModule({
			module: 'fluxer_renderer',
			sha256: RENDERER_V1.sha256,
			download: () => ({offset: 0, chunks: chunksOf(RENDERER_V1.packed)}),
		});

		assert.equal(existsSync(path.join(installed.directory, 'module.json')), true);
	});

	test('a resumed partial that does not hash to the package is dropped so the retry starts clean', async () => {
		const userDataPath = createUserData();
		const store = await openStore(userDataPath);
		mkdirSync(store.incomingDownloadRoot, {recursive: true});
		const partial = store.getPartialPackagePath(RENDERER_V1.sha256);
		writeFileSync(partial, Buffer.alloc(100, 7));

		await assert.rejects(
			store.installModule({
				module: 'fluxer_renderer',
				sha256: RENDERER_V1.sha256,
				download: (resumeFrom) => ({offset: resumeFrom, chunks: chunksOf(RENDERER_V1.packed.subarray(resumeFrom))}),
			}),
			ModuleStoreDownloadHashMismatchError,
		);
		assert.equal(existsSync(partial), false);
	});

	test('a download that resumes at an offset nobody asked for is refused', async () => {
		const userDataPath = createUserData();
		const store = await openStore(userDataPath);

		await assert.rejects(
			store.installModule({
				module: 'fluxer_renderer',
				sha256: RENDERER_V1.sha256,
				download: () => ({offset: 10, chunks: chunksOf(RENDERER_V1.packed.subarray(10))}),
			}),
			/resumed at byte 10, expected 0/u,
		);
	});
});

describe('ModuleStore rejected module sets', () => {
	async function bootTwiceWithoutReady(userDataPath) {
		for (let boot = 0; boot < 2; boot += 1) {
			const store = await openStore(userDataPath);
			await store.beginBootAttempt();
			await store.recordLaunchAttempt();
		}
		return await openStore(userDataPath);
	}

	test('a rollback that reverts remembers the module set it rejected', async () => {
		const userDataPath = createUserData();
		const store = await openStore(userDataPath);
		await install(store, RENDERER_V1);
		await store.commit({fluxer_renderer: RENDERER_V1.sha256});
		await install(store, RENDERER_V2);
		await store.commit({fluxer_renderer: RENDERER_V2.sha256});

		const rebooted = await bootTwiceWithoutReady(userDataPath);
		const attempt = await rebooted.beginBootAttempt();

		assert.equal(attempt.rolledBack, true);
		assert.deepEqual(readState(userDataPath).rejected, {fluxer_renderer: RENDERER_V2.sha256});
		assert.equal(rebooted.isRejected('fluxer_renderer', RENDERER_V2.sha256), true);
		assert.equal(rebooted.isRejected('fluxer_renderer', RENDERER_V1.sha256), false);
	});

	test('the rejection holds while the manifest still advertises that build and clears once it moves on', async () => {
		const userDataPath = createUserData();
		const store = await openStore(userDataPath);
		await install(store, RENDERER_V1);
		await store.commit({fluxer_renderer: RENDERER_V1.sha256});
		await install(store, RENDERER_V2);
		await store.commit({fluxer_renderer: RENDERER_V2.sha256});
		const rebooted = await bootTwiceWithoutReady(userDataPath);
		await rebooted.beginBootAttempt();

		await rebooted.recordManifestFetch({
			etag: null,
			fetchedAt: MANIFEST_FETCHED_AT,
			manifest: manifestObservation(1, 'a'.repeat(64)),
			advertised: {fluxer_renderer: RENDERER_V2.sha256},
		});
		assert.equal(rebooted.isRejected('fluxer_renderer', RENDERER_V2.sha256), true);

		await rebooted.recordManifestFetch({
			etag: null,
			fetchedAt: MANIFEST_FETCHED_AT,
			manifest: manifestObservation(2, 'b'.repeat(64)),
			advertised: {fluxer_renderer: 'c'.repeat(64)},
		});
		assert.equal(rebooted.isRejected('fluxer_renderer', RENDERER_V2.sha256), false);
		assert.deepEqual(readState(userDataPath).rejected, {});
	});

	test('a state file written before rejections existed still opens', async () => {
		const userDataPath = createUserData();
		const store = await openStore(userDataPath);
		const statePath = path.join(store.root, MODULE_STATE_FILE_NAME);
		const legacy = readState(userDataPath);
		delete legacy.rejected;
		writeFileSync(statePath, JSON.stringify(legacy));

		const reopened = await openStore(userDataPath);

		assert.deepEqual(reopened.getState().rejected, {});
	});
});

describe('ModuleStore on Windows sharing violations', () => {
	test('a rename that an antivirus scanner briefly locks is retried instead of failing the install', async () => {
		const userDataPath = createUserData();
		const store = await openStore(userDataPath);
		const platform = Object.getOwnPropertyDescriptor(process, 'platform');
		const rename = fsPromises.rename;
		let failures = 0;
		Object.defineProperty(process, 'platform', {...platform, value: 'win32'});
		fsPromises.rename = async (...args) => {
			if (failures < 2) {
				failures += 1;
				throw Object.assign(new Error('operation not permitted'), {code: 'EPERM'});
			}
			return await rename(...args);
		};
		try {
			await store.recordLaunchAttempt();
		} finally {
			fsPromises.rename = rename;
			Object.defineProperty(process, 'platform', platform);
		}

		assert.equal(failures, 2);
		assert.equal(readState(userDataPath).boot_attempt, 1);
	});

	test('the same lock on another platform is not retried', async () => {
		const userDataPath = createUserData();
		const store = await openStore(userDataPath);
		const rename = fsPromises.rename;
		let calls = 0;
		fsPromises.rename = async () => {
			calls += 1;
			throw Object.assign(new Error('operation not permitted'), {code: 'EPERM'});
		};
		try {
			await assert.rejects(store.recordLaunchAttempt(), /failed to publish/u);
		} finally {
			fsPromises.rename = rename;
		}

		assert.equal(calls, process.platform === 'win32' ? 7 : 1);
	});
});

describe('ModuleStore boot checks without rehashing', () => {
	async function countServedFileOpens(directory, operation) {
		const open = fsPromises.open;
		const opened = [];
		fsPromises.open = async (target, ...rest) => {
			if (
				typeof target === 'string' &&
				target.startsWith(`${directory}${path.sep}`) &&
				path.basename(target) !== 'module.json' &&
				!path.basename(target).startsWith(MODULE_FILE_STAMPS_NAME)
			) {
				opened.push(target);
			}
			return await open(target, ...rest);
		};
		try {
			return {result: await operation(), opened};
		} finally {
			fsPromises.open = open;
		}
	}

	test('an installed module is trusted from its persisted file stamps without reading a served file', async () => {
		const userDataPath = createUserData();
		const first = await openStore(userDataPath);
		const installed = await install(first, RENDERER_V1);
		await first.commit({fluxer_renderer: RENDERER_V1.sha256});
		assert.equal(existsSync(path.join(installed.directory, MODULE_FILE_STAMPS_NAME)), true);

		const store = await openStore(userDataPath);
		const {result, opened} = await countServedFileOpens(installed.directory, async () => {
			const isInstalled = await store.isInstalled('fluxer_renderer', RENDERER_V1.sha256);
			const index = await store.buildModuleIndex();
			return {isInstalled, index};
		});

		assert.equal(result.isInstalled, true);
		assert.deepEqual([...result.index.keys()].sort(), ['assets/index-v1.js', 'assets/style-v1.css']);
		assert.deepEqual(opened, []);
	});

	test('a fresh install launches from its in-memory stamps and persists them only once the tree is durable', async () => {
		const userDataPath = createUserData();
		const store = await openStore(userDataPath);
		const installed = await store.installModule({
			module: RENDERER_V1.module,
			sha256: RENDERER_V1.sha256,
			download: () => streamOf(RENDERER_V1.packed),
		});
		const stampsPath = path.join(installed.directory, MODULE_FILE_STAMPS_NAME);
		assert.equal(existsSync(stampsPath), false);

		const {result, opened} = await countServedFileOpens(installed.directory, async () => {
			await store.commit({fluxer_renderer: RENDERER_V1.sha256});
			return await store.buildModuleIndex();
		});
		assert.deepEqual([...result.keys()].sort(), ['assets/index-v1.js', 'assets/style-v1.css']);
		assert.deepEqual(opened, []);
		assert.equal(existsSync(stampsPath), false);

		const attempt = await store.recordLaunchAttempt();
		assert.equal(await store.markLaunchAttemptSucceeded(attempt), true);
		await store.settleInstalledModules();
		assert.equal(existsSync(stampsPath), true);
		const reopened = await openStore(userDataPath);
		const later = await countServedFileOpens(installed.directory, () =>
			reopened.isInstalled('fluxer_renderer', RENDERER_V1.sha256),
		);
		assert.equal(later.result, true);
		assert.deepEqual(later.opened, []);
	});

	test('a served file rewritten with the same length and its old mtime is still caught', async () => {
		const userDataPath = createUserData();
		const store = await openStore(userDataPath);
		const installed = await install(store, RENDERER_V1);
		await store.commit({fluxer_renderer: RENDERER_V1.sha256});
		const served = path.join(installed.directory, 'assets', 'index-v1.js');
		const before = await fsPromises.stat(served);
		await new Promise((resolve) => setTimeout(resolve, 20));
		writeFileSync(served, 'RENDERER ONE');
		utimesSync(served, before.atime, before.mtime);

		assert.equal(await store.isInstalled('fluxer_renderer', RENDERER_V1.sha256), false);
		await assert.rejects(store.buildModuleIndex(), ModuleStoreInstallationMissingError);
	});

	test('a deleted served file is caught without hashing the rest of the module', async () => {
		const userDataPath = createUserData();
		const store = await openStore(userDataPath);
		const installed = await install(store, RENDERER_V1);
		await store.commit({fluxer_renderer: RENDERER_V1.sha256});
		await rm(path.join(installed.directory, 'assets', 'style-v1.css'));

		assert.equal(await store.isInstalled('fluxer_renderer', RENDERER_V1.sha256), false);
	});

	test('an installation without stamps is hashed once and then trusted from the stamps it wrote', async () => {
		const userDataPath = createUserData();
		const store = await openStore(userDataPath);
		const installed = await install(store, RENDERER_V1);
		await store.commit({fluxer_renderer: RENDERER_V1.sha256});
		const stampsPath = path.join(installed.directory, MODULE_FILE_STAMPS_NAME);
		await rm(stampsPath);

		const firstPass = await countServedFileOpens(installed.directory, () =>
			store.isInstalled('fluxer_renderer', RENDERER_V1.sha256),
		);
		assert.equal(firstPass.result, true);
		assert.equal(firstPass.opened.length, 2);
		assert.equal(existsSync(stampsPath), true);

		const secondPass = await countServedFileOpens(installed.directory, () =>
			store.isInstalled('fluxer_renderer', RENDERER_V1.sha256),
		);
		assert.equal(secondPass.result, true);
		assert.deepEqual(secondPass.opened, []);
	});

	test('unreadable or foreign stamps fall back to hashing instead of trusting the tree', async () => {
		const userDataPath = createUserData();
		const store = await openStore(userDataPath);
		const installed = await install(store, RENDERER_V1);
		await store.commit({fluxer_renderer: RENDERER_V1.sha256});
		const stampsPath = path.join(installed.directory, MODULE_FILE_STAMPS_NAME);
		const stamps = JSON.parse(readFileSync(stampsPath, 'utf8'));
		const served = path.join(installed.directory, 'assets', 'index-v1.js');
		writeFileSync(served, 'RENDERER ONE');
		const tampered = await fsPromises.lstat(served);
		const forged = {
			...stamps,
			manifest_sha256: 'f'.repeat(64),
			files: {
				...stamps.files,
				'assets/index-v1.js': [tampered.size, tampered.mtimeMs, tampered.ctimeMs, tampered.ino],
			},
		};
		writeFileSync(stampsPath, JSON.stringify(forged));

		assert.equal(await store.isInstalled('fluxer_renderer', RENDERER_V1.sha256), false);

		writeFileSync(stampsPath, 'not json');
		assert.equal(await store.isInstalled('fluxer_renderer', RENDERER_V1.sha256), false);
	});
});
