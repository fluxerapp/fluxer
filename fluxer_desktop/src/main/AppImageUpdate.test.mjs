// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {createHash} from 'node:crypto';
import {
	chmodSync,
	existsSync,
	mkdirSync,
	mkdtempSync,
	readdirSync,
	readFileSync,
	statSync,
	symlinkSync,
	writeFileSync,
} from 'node:fs';
import {createServer} from 'node:http';
import {createRequire} from 'node:module';
import {tmpdir} from 'node:os';
import {join} from 'node:path';
import {after, before, describe, test} from 'node:test';
import {fileURLToPath} from 'node:url';
import vm from 'node:vm';

const require = createRequire(import.meta.url);
const esbuild = require('esbuild');

const sourcePath = fileURLToPath(new URL('./AppImageUpdate.ts', import.meta.url));
const source = readFileSync(sourcePath, 'utf8');
const transformedSource = esbuild.transformSync(source, {
	loader: 'ts',
	format: 'cjs',
	platform: 'node',
	target: 'node20',
}).code;

function loadAppImageUpdate(env = {}, overrides = {}, requireImpl = require) {
	const module = {exports: {}};
	const context = vm.createContext({
		module,
		exports: module.exports,
		require: requireImpl,
		console,
		Buffer,
		fetch,
		Error,
		Number,
		process: {...process, ...overrides, env: {...env}},
	});
	vm.runInContext(transformedSource, context, {filename: sourcePath});
	return module.exports;
}

const INSTALLED_NAME = 'My Fluxer.AppImage';
const OLD_BYTES = Buffer.from('old-appimage-payload');
const NEW_BYTES = Buffer.alloc(400_000, 7);
const NEW_SHA256 = createHash('sha256').update(NEW_BYTES).digest('hex');

let server;
let baseUrl;
let requestCount = 0;

before(async () => {
	server = createServer((request, response) => {
		requestCount += 1;
		if (request.url === '/appimage') {
			response.writeHead(200, {'content-type': 'application/octet-stream', 'content-length': NEW_BYTES.length});
			response.end(NEW_BYTES);
			return;
		}
		if (request.url === '/corrupt') {
			const corrupt = Buffer.from(NEW_BYTES);
			corrupt[0] = corrupt[0] ^ 0xff;
			response.writeHead(200, {'content-type': 'application/octet-stream', 'content-length': corrupt.length});
			response.end(corrupt);
			return;
		}
		response.writeHead(404);
		response.end('missing');
	});
	await new Promise((resolve) => server.listen(0, '127.0.0.1', resolve));
	baseUrl = `http://127.0.0.1:${server.address().port}`;
});

after(async () => {
	await new Promise((resolve) => server.close(resolve));
});

function createInstall() {
	const root = mkdtempSync(join(tmpdir(), 'fluxer-appimage-test-'));
	const applications = join(root, 'Applications');
	mkdirSync(applications);
	const installedPath = join(applications, INSTALLED_NAME);
	writeFileSync(installedPath, OLD_BYTES, {mode: 0o755});
	return {root, applications, installedPath};
}

function stagingLeftovers(directory) {
	return readdirSync(directory).filter((entry) => entry.startsWith('.fluxer-update-'));
}

function assertUnavailable(result, reason) {
	assert.equal(result.ok, false);
	assert.equal(result.reason, reason);
}

function createAppDir(name) {
	const root = mkdtempSync(join(tmpdir(), 'fluxer-appdir-test-'));
	const dir = join(root, name);
	mkdirSync(dir);
	writeFileSync(join(dir, 'AppRun'), '#!/usr/bin/env bash\n', {mode: 0o755});
	const binary = join(dir, 'fluxer-canary');
	writeFileSync(binary, 'elf', {mode: 0o755});
	return {dir, binary};
}

function stagingPrefixFor(installedPath) {
	return `.fluxer-update-${createHash('sha256').update(installedPath).digest('hex').slice(0, 8)}-`;
}

describe('AppImageUpdate process provenance', () => {
	test('accepts the process the AppImage runtime execed out of its own mount', () => {
		const {isRunningFromAppImage} = loadAppImageUpdate({});
		const mounted = createAppDir('.mount_My FluAKcenK');

		assert.equal(isRunningFromAppImage({APPDIR: mounted.dir}, mounted.binary), true);
		assert.equal(isRunningFromAppImage({APPDIR: `${mounted.dir}/`}, mounted.binary), true);
		assert.equal(isRunningFromAppImage({APPDIR: mounted.dir}, join(mounted.dir, 'usr', 'bin', 'fluxer-canary')), true);
	});

	test('refuses an extracted AppDir, whose APPIMAGE belongs to whatever launched it', () => {
		const {isRunningFromAppImage} = loadAppImageUpdate({});
		const extracted = createAppDir('squashfs-root');
		const launcher = createAppDir('.mount_My FluOjOANH');

		assert.equal(isRunningFromAppImage({}, extracted.binary), false);
		assert.equal(
			isRunningFromAppImage({APPDIR: launcher.dir}, extracted.binary),
			false,
			'the generated AppRun assigns APPDIR without exporting it, so an APPDIR that reaches this process and does not contain it came from another AppImage',
		);
	});

	test('refuses an installed package that inherited APPIMAGE from the terminal it was started in', () => {
		const {isRunningFromAppImage} = loadAppImageUpdate({});
		const terminal = createAppDir('.mount_Alacritty7fJk2L');

		assert.equal(isRunningFromAppImage({}, '/opt/Fluxer/fluxer'), false);
		assert.equal(isRunningFromAppImage({APPDIR: ''}, '/opt/Fluxer/fluxer'), false);
		assert.equal(isRunningFromAppImage({APPDIR: terminal.dir}, '/opt/Fluxer/fluxer'), false);
	});

	test('refuses a mount path that only prefixes this executable', () => {
		const {isRunningFromAppImage} = loadAppImageUpdate({});
		const mounted = createAppDir('.mount_My FluAKcenK');

		assert.equal(isRunningFromAppImage({APPDIR: mounted.dir}, `${mounted.dir}-other/fluxer-canary`), false);
	});

	test('refuses an APPDIR the runtime cannot have mounted', () => {
		const {isRunningFromAppImage} = loadAppImageUpdate({});
		const installDir = mkdtempSync(join(tmpdir(), 'fluxer-opt-test-'));
		const binary = join(installDir, 'fluxer-canary');
		writeFileSync(binary, 'elf', {mode: 0o755});

		assert.equal(isRunningFromAppImage({APPDIR: installDir}, binary), false);
		writeFileSync(join(installDir, 'AppRun'), '#!/usr/bin/env bash\n', {mode: 0o755});
		assert.equal(isRunningFromAppImage({APPDIR: installDir}, binary), true);
	});
});

describe('AppImageUpdate target resolution', () => {
	test('falls back when APPIMAGE is unset', () => {
		const {resolveAppImageTarget} = loadAppImageUpdate({});

		assertUnavailable(resolveAppImageTarget(), 'appimage-path-missing');
		assertUnavailable(resolveAppImageTarget(''), 'appimage-path-missing');
		assertUnavailable(resolveAppImageTarget('   '), 'appimage-path-missing');
	});

	test('falls back when the AppImage moved since launch', () => {
		const install = createInstall();
		const {resolveAppImageTarget} = loadAppImageUpdate({APPIMAGE: join(install.applications, 'Gone.AppImage')});

		assertUnavailable(resolveAppImageTarget(), 'appimage-path-gone');
	});

	test('falls back when the path is not a regular file', () => {
		const install = createInstall();
		const {resolveAppImageTarget} = loadAppImageUpdate({APPIMAGE: install.applications});

		assertUnavailable(resolveAppImageTarget(), 'appimage-path-not-a-file');
	});

	test('falls back when the containing directory is not writable', {skip: process.getuid?.() === 0}, () => {
		const install = createInstall();
		chmodSync(install.applications, 0o500);
		try {
			const {resolveAppImageTarget} = loadAppImageUpdate({APPIMAGE: install.installedPath});

			assertUnavailable(resolveAppImageTarget(), 'directory-not-writable');
		} finally {
			chmodSync(install.applications, 0o755);
		}
	});

	test('resolves a writable AppImage to its own path', () => {
		const install = createInstall();
		const {resolveAppImageTarget} = loadAppImageUpdate({APPIMAGE: install.installedPath});

		const resolved = resolveAppImageTarget();
		assert.equal(resolved.ok, true);
		assert.equal(resolved.target.installedPath, install.installedPath);
		assert.equal(resolved.target.directory, install.applications);
	});
});

describe('AppImageUpdate in-place replacement', () => {
	test('replaces the running AppImage and keeps the user filename', async () => {
		const install = createInstall();
		const appImageUpdate = loadAppImageUpdate({APPIMAGE: install.installedPath});
		const resolved = appImageUpdate.resolveAppImageTarget();
		assert.equal(resolved.ok, true);

		const progress = [];
		const staged = await appImageUpdate.stageAppImageUpdate({
			target: resolved.target,
			url: `${baseUrl}/appimage`,
			expectedSha256: NEW_SHA256,
			onProgress: (sample) => progress.push(sample),
		});

		assert.equal(staged.stagedPath, join(staged.stagingDirectory, INSTALLED_NAME));
		assert.equal(readFileSync(install.installedPath).equals(OLD_BYTES), true);
		assert.ok(progress.length > 0);
		assert.equal(progress.at(-1).transferred, NEW_BYTES.length);
		assert.equal(progress.at(-1).total, NEW_BYTES.length);

		appImageUpdate.applyStagedAppImageUpdate(resolved.target, staged);

		assert.deepEqual(readdirSync(install.applications), [INSTALLED_NAME]);
		assert.equal(readFileSync(install.installedPath).equals(NEW_BYTES), true);
		assert.equal(createHash('sha256').update(readFileSync(install.installedPath)).digest('hex'), NEW_SHA256);
		assert.equal(statSync(install.installedPath).mode & 0o111, 0o111);
	});

	test('stages inside the install directory so the rename cannot cross filesystems', async () => {
		const install = createInstall();
		const appImageUpdate = loadAppImageUpdate({APPIMAGE: install.installedPath});
		const resolved = appImageUpdate.resolveAppImageTarget();

		const staged = await appImageUpdate.stageAppImageUpdate({
			target: resolved.target,
			url: `${baseUrl}/appimage`,
			expectedSha256: NEW_SHA256,
		});

		assert.equal(staged.stagingDirectory.startsWith(join(install.applications, '.fluxer-update-')), true);
		appImageUpdate.discardStagedAppImageUpdate(staged);
		assert.deepEqual(stagingLeftovers(install.applications), []);
	});

	test('refuses to swap when the download does not match the published checksum', async () => {
		const install = createInstall();
		const appImageUpdate = loadAppImageUpdate({APPIMAGE: install.installedPath});
		const resolved = appImageUpdate.resolveAppImageTarget();

		await assert.rejects(
			appImageUpdate.stageAppImageUpdate({
				target: resolved.target,
				url: `${baseUrl}/corrupt`,
				expectedSha256: NEW_SHA256,
			}),
			(error) => {
				assert.equal(error instanceof appImageUpdate.AppImageChecksumError, true);
				assert.match(error.message, /checksum mismatch/);
				return true;
			},
		);

		assert.equal(readFileSync(install.installedPath).equals(OLD_BYTES), true);
		assert.deepEqual(readdirSync(install.applications), [INSTALLED_NAME]);
	});

	test('refuses a checksum the release feed never published', async () => {
		const install = createInstall();
		const appImageUpdate = loadAppImageUpdate({APPIMAGE: install.installedPath});
		const resolved = appImageUpdate.resolveAppImageTarget();
		const before = requestCount;

		await assert.rejects(
			appImageUpdate.stageAppImageUpdate({
				target: resolved.target,
				url: `${baseUrl}/appimage`,
				expectedSha256: '',
			}),
			/checksum is missing or malformed/,
		);

		assert.equal(requestCount, before);
		assert.deepEqual(readdirSync(install.applications), [INSTALLED_NAME]);
	});

	test('cleans up and leaves the install untouched when the download fails', async () => {
		const install = createInstall();
		const appImageUpdate = loadAppImageUpdate({APPIMAGE: install.installedPath});
		const resolved = appImageUpdate.resolveAppImageTarget();

		await assert.rejects(
			appImageUpdate.stageAppImageUpdate({
				target: resolved.target,
				url: `${baseUrl}/nothing-here`,
				expectedSha256: NEW_SHA256,
			}),
			/Update download failed: 404/,
		);

		assert.equal(readFileSync(install.installedPath).equals(OLD_BYTES), true);
		assert.deepEqual(readdirSync(install.applications), [INSTALLED_NAME]);
	});

	test('finishes a short write instead of installing a truncated AppImage', async () => {
		const install = createInstall();
		let shortWrites = 0;
		const shortWritingRequire = (specifier) => {
			const real = require(specifier);
			if (specifier !== 'node:fs/promises') {
				return real;
			}
			return {
				...real,
				open: async (...args) => {
					const handle = await real.open(...args);
					return {
						close: () => handle.close(),
						datasync: () => handle.datasync(),
						write: (buffer, offset, length) => {
							const capped = shortWrites === 0 ? Math.min(length, 4096) : length;
							shortWrites += 1;
							return handle.write(buffer, offset, capped);
						},
					};
				},
			};
		};
		const appImageUpdate = loadAppImageUpdate({APPIMAGE: install.installedPath}, {}, shortWritingRequire);
		const resolved = appImageUpdate.resolveAppImageTarget();

		const staged = await appImageUpdate.stageAppImageUpdate({
			target: resolved.target,
			url: `${baseUrl}/appimage`,
			expectedSha256: NEW_SHA256,
		});

		assert.ok(shortWrites > 1, 'the write loop has to come back for the remainder of a short write');
		assert.equal(statSync(staged.stagedPath).size, NEW_BYTES.length);
		assert.equal(createHash('sha256').update(readFileSync(staged.stagedPath)).digest('hex'), NEW_SHA256);
		appImageUpdate.discardStagedAppImageUpdate(staged);
	});

	test('checksums the staged file from disk, not the stream it was read from', async () => {
		const install = createInstall();
		const droppingRequire = (specifier) => {
			const real = require(specifier);
			if (specifier !== 'node:fs/promises') {
				return real;
			}
			return {
				...real,
				open: async (...args) => {
					const handle = await real.open(...args);
					return {
						close: () => handle.close(),
						datasync: () => handle.datasync(),
						write: async (buffer, offset, length) => {
							const kept = Math.max(0, length - 1024);
							if (kept > 0) {
								await handle.write(buffer, offset, kept);
							}
							return {bytesWritten: length};
						},
					};
				},
			};
		};
		const appImageUpdate = loadAppImageUpdate({APPIMAGE: install.installedPath}, {}, droppingRequire);
		const resolved = appImageUpdate.resolveAppImageTarget();

		await assert.rejects(
			appImageUpdate.stageAppImageUpdate({
				target: resolved.target,
				url: `${baseUrl}/appimage`,
				expectedSha256: NEW_SHA256,
			}),
			(error) => {
				assert.equal(error instanceof appImageUpdate.AppImageChecksumError, true);
				return true;
			},
		);

		assert.equal(readFileSync(install.installedPath).equals(OLD_BYTES), true);
		assert.deepEqual(readdirSync(install.applications), [INSTALLED_NAME]);
	});

	test('flushes the staged file to disk before it can be renamed over the install', async () => {
		const install = createInstall();
		let flushes = 0;
		const countingRequire = (specifier) => {
			const real = require(specifier);
			if (specifier !== 'node:fs/promises') {
				return real;
			}
			return {
				...real,
				open: async (...args) => {
					const handle = await real.open(...args);
					return {
						close: () => handle.close(),
						datasync: () => {
							flushes += 1;
							return handle.datasync();
						},
						write: (buffer, offset, length) => handle.write(buffer, offset, length),
					};
				},
			};
		};
		const appImageUpdate = loadAppImageUpdate({APPIMAGE: install.installedPath}, {}, countingRequire);
		const resolved = appImageUpdate.resolveAppImageTarget();

		const staged = await appImageUpdate.stageAppImageUpdate({
			target: resolved.target,
			url: `${baseUrl}/appimage`,
			expectedSha256: NEW_SHA256,
		});

		assert.equal(flushes, 1);
		appImageUpdate.discardStagedAppImageUpdate(staged);
	});

	test('gives up when the staged bytes cannot be flushed to disk', async () => {
		const install = createInstall();
		const failingFlushRequire = (specifier) => {
			const real = require(specifier);
			if (specifier !== 'node:fs/promises') {
				return real;
			}
			return {
				...real,
				open: async (...args) => {
					const handle = await real.open(...args);
					return {
						close: () => handle.close(),
						datasync: async () => {
							const error = new Error('ENOSPC: no space left on device, fsync');
							error.code = 'ENOSPC';
							throw error;
						},
						write: (buffer, offset, length) => handle.write(buffer, offset, length),
					};
				},
			};
		};
		const appImageUpdate = loadAppImageUpdate({APPIMAGE: install.installedPath}, {}, failingFlushRequire);
		const resolved = appImageUpdate.resolveAppImageTarget();

		await assert.rejects(
			appImageUpdate.stageAppImageUpdate({
				target: resolved.target,
				url: `${baseUrl}/appimage`,
				expectedSha256: NEW_SHA256,
			}),
			(error) => {
				assert.equal(error instanceof appImageUpdate.AppImageStagingError, true);
				assert.equal(error.code, 'ENOSPC');
				return true;
			},
		);

		assert.deepEqual(readdirSync(install.applications), [INSTALLED_NAME]);
	});

	test('retries a body that stops short of content-length instead of calling it a bad build', async () => {
		const install = createInstall();
		const appImageUpdate = loadAppImageUpdate({APPIMAGE: install.installedPath});
		const resolved = appImageUpdate.resolveAppImageTarget();
		const truncatingFetch = async () => {
			let sent = false;
			return {
				ok: true,
				status: 200,
				headers: {get: (name) => (name === 'content-length' ? String(NEW_BYTES.length) : null)},
				body: {
					cancel: async () => {},
					getReader: () => ({
						cancel: async () => {},
						read: async () => {
							if (sent) {
								return {done: true, value: undefined};
							}
							sent = true;
							return {done: false, value: NEW_BYTES.subarray(0, 1024)};
						},
					}),
				},
			};
		};

		await assert.rejects(
			appImageUpdate.stageAppImageUpdate({
				target: resolved.target,
				url: `${baseUrl}/appimage`,
				expectedSha256: NEW_SHA256,
				fetchImpl: truncatingFetch,
			}),
			(error) => {
				assert.equal(error instanceof appImageUpdate.AppImageChecksumError, false);
				assert.equal(error instanceof appImageUpdate.AppImageStagingError, false);
				assert.match(error.message, new RegExp(`ended after 1024 of ${NEW_BYTES.length} bytes`));
				return true;
			},
		);

		assert.deepEqual(readdirSync(install.applications), [INSTALLED_NAME]);
	});

	test('refuses to stage when the install filesystem cannot hold the download', async () => {
		const install = createInstall();
		const appImageUpdate = loadAppImageUpdate({APPIMAGE: install.installedPath});
		const resolved = appImageUpdate.resolveAppImageTarget();
		const hugeFetch = async () => ({
			ok: true,
			status: 200,
			headers: {get: (name) => (name === 'content-length' ? String(Number.MAX_SAFE_INTEGER) : null)},
			body: {
				cancel: async () => {},
				getReader: () => ({cancel: async () => {}, read: async () => ({done: true, value: undefined})}),
			},
		});

		await assert.rejects(
			appImageUpdate.stageAppImageUpdate({
				target: resolved.target,
				url: `${baseUrl}/appimage`,
				expectedSha256: NEW_SHA256,
				fetchImpl: hugeFetch,
			}),
			(error) => {
				assert.equal(error instanceof appImageUpdate.AppImageStagingError, true);
				assert.equal(error.code, 'ENOSPC');
				return true;
			},
		);

		assert.deepEqual(readdirSync(install.applications), [INSTALLED_NAME]);
	});

	test('marks a local write failure as non-retryable', async () => {
		const install = createInstall();
		const appImageUpdate = loadAppImageUpdate({APPIMAGE: install.installedPath});
		const resolved = appImageUpdate.resolveAppImageTarget();
		const failingFetch = async () => ({
			ok: true,
			status: 200,
			headers: {get: () => null},
			body: {
				cancel: async () => {},
				getReader: () => ({
					cancel: async () => {},
					read: async () => {
						const error = new Error('ENOSPC: no space left on device, write');
						error.code = 'ENOSPC';
						throw error;
					},
				}),
			},
		});

		await assert.rejects(
			appImageUpdate.stageAppImageUpdate({
				target: resolved.target,
				url: `${baseUrl}/appimage`,
				expectedSha256: NEW_SHA256,
				fetchImpl: failingFetch,
			}),
			(error) => {
				assert.equal(error instanceof appImageUpdate.AppImageStagingError, true);
				assert.equal(error.code, 'ENOSPC');
				return true;
			},
		);

		assert.deepEqual(readdirSync(install.applications), [INSTALLED_NAME]);
	});

	test('refuses to install when the AppImage moved between download and install', async () => {
		const install = createInstall();
		const appImageUpdate = loadAppImageUpdate({APPIMAGE: install.installedPath});
		const resolved = appImageUpdate.resolveAppImageTarget();
		const staged = await appImageUpdate.stageAppImageUpdate({
			target: resolved.target,
			url: `${baseUrl}/appimage`,
			expectedSha256: NEW_SHA256,
		});

		const relocated = {installedPath: join(install.applications, 'Moved.AppImage'), directory: install.applications};
		assert.throws(
			() => appImageUpdate.applyStagedAppImageUpdate(relocated, staged),
			/cannot be replaced in place: appimage-path-gone/,
		);

		assert.equal(readFileSync(install.installedPath).equals(OLD_BYTES), true);
		assert.deepEqual(stagingLeftovers(install.applications), []);
	});
});

describe('AppImageUpdate abandoned staging cleanup', () => {
	test('reclaims staged updates left behind by a process that never quit cleanly', async () => {
		const install = createInstall();
		const appImageUpdate = loadAppImageUpdate({APPIMAGE: install.installedPath});
		const resolved = appImageUpdate.resolveAppImageTarget();
		const abandoned = await appImageUpdate.stageAppImageUpdate({
			target: resolved.target,
			url: `${baseUrl}/appimage`,
			expectedSha256: NEW_SHA256,
		});

		assert.equal(stagingLeftovers(install.applications).length, 1);

		const reclaimed = appImageUpdate.sweepAbandonedAppImageUpdates(resolved.target);

		assert.deepEqual([...reclaimed], [abandoned.stagingDirectory]);
		assert.deepEqual(readdirSync(install.applications), [INSTALLED_NAME]);
		assert.equal(readFileSync(install.installedPath).equals(OLD_BYTES), true);
	});

	test('leaves a staged update belonging to another AppImage in the same directory alone', async () => {
		const install = createInstall();
		const siblingPath = join(install.applications, 'Fluxer Canary.AppImage');
		writeFileSync(siblingPath, OLD_BYTES, {mode: 0o755});
		const appImageUpdate = loadAppImageUpdate({APPIMAGE: install.installedPath});
		const resolved = appImageUpdate.resolveAppImageTarget();
		const sibling = appImageUpdate.resolveAppImageTarget(siblingPath);
		const siblingStaged = await appImageUpdate.stageAppImageUpdate({
			target: sibling.target,
			url: `${baseUrl}/appimage`,
			expectedSha256: NEW_SHA256,
		});

		const reclaimed = appImageUpdate.sweepAbandonedAppImageUpdates(resolved.target);

		assert.deepEqual([...reclaimed], []);
		assert.equal(existsSync(siblingStaged.stagedPath), true);
		appImageUpdate.discardStagedAppImageUpdate(siblingStaged);
		assert.deepEqual(stagingLeftovers(install.applications), []);
	});

	test('unlinks a staging-named symlink instead of following it out of the install directory', () => {
		const install = createInstall();
		const outside = mkdtempSync(join(tmpdir(), 'fluxer-outside-test-'));
		writeFileSync(join(outside, 'keep-me'), 'not ours to delete');
		const planted = join(install.applications, `${stagingPrefixFor(install.installedPath)}link`);
		symlinkSync(outside, planted);
		const appImageUpdate = loadAppImageUpdate({APPIMAGE: install.installedPath});
		const resolved = appImageUpdate.resolveAppImageTarget();

		const reclaimed = appImageUpdate.sweepAbandonedAppImageUpdates(resolved.target);

		assert.deepEqual([...reclaimed], [planted]);
		assert.deepEqual(readdirSync(install.applications), [INSTALLED_NAME]);
		assert.deepEqual(readdirSync(outside), ['keep-me']);
	});

	test('leaves everything that is not one of this AppImage staging directories alone', () => {
		const install = createInstall();
		const neighbours = ['Fluxer Canary.AppImage', '.fluxer-update-notes.txt', '.fluxer-update-'];
		for (const name of neighbours) {
			writeFileSync(join(install.applications, name), 'keep');
		}
		const appImageUpdate = loadAppImageUpdate({APPIMAGE: install.installedPath});
		const resolved = appImageUpdate.resolveAppImageTarget();

		assert.deepEqual([...appImageUpdate.sweepAbandonedAppImageUpdates(resolved.target)], []);
		assert.deepEqual(readdirSync(install.applications).sort(), [...neighbours, INSTALLED_NAME].sort());
	});

	test('reports nothing when the install directory holds no staged update', () => {
		const install = createInstall();
		const appImageUpdate = loadAppImageUpdate({APPIMAGE: install.installedPath});
		const resolved = appImageUpdate.resolveAppImageTarget();

		assert.deepEqual([...appImageUpdate.sweepAbandonedAppImageUpdates(resolved.target)], []);
		assert.deepEqual(readdirSync(install.applications), [INSTALLED_NAME]);
	});
});
