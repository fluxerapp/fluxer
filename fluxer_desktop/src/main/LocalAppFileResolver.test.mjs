// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {Buffer} from 'node:buffer';
import {mkdirSync, mkdtempSync, symlinkSync, writeFileSync} from 'node:fs';
import fsPromises, {rm} from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';
import {after, describe, test} from 'node:test';
import {installElectronStub} from './LocalAppTestSupport.test.mjs';

installElectronStub();

const {DesktopLocalAppFiles, LocalAppResourceBudget} = await import('./LocalAppFileResolver.ts');

const REACTION_BURST_SIZE = 300;
const REWRITTEN_FILE_MAX_BYTES = 4 * 1024 * 1024;
const OPEN_SHELL_RESPONSES = 16;
const WAIT_GIVE_UP_MS = 250;
const LARGE_ASSET_BODY = 'x'.repeat(512 * 1024);

const temporaryRoots = [];

function createRoot(prefix) {
	const root = mkdtempSync(path.join(os.tmpdir(), prefix));
	temporaryRoots.push(root);
	return root;
}

function createRendererRoot() {
	const root = createRoot('local-app-renderer-');
	mkdirSync(path.join(root, 'assets'));
	mkdirSync(path.join(root, 'web'));
	writeFileSync(path.join(root, 'index.html'), '<html lang="en"><head></head><body></body></html>');
	writeFileSync(path.join(root, 'assets', 'deadbeefdeadbeef.js'), 'console.log(1)');
	writeFileSync(path.join(root, 'assets', 'cafed00dcafed00d.css'), '');
	writeFileSync(path.join(root, 'assets', 'f00dfacef00dface.js'), LARGE_ASSET_BODY);
	writeFileSync(path.join(root, 'web', 'favicon-32x32.png'), 'png-bytes');
	writeFileSync(path.join(root, 'manifest.json'), '{}');
	return root;
}

const rendererRoot = createRendererRoot();
const outsideRoot = createRoot('local-app-outside-');
writeFileSync(path.join(outsideRoot, 'secret.txt'), 'secret');
symlinkSync(path.join(outsideRoot, 'secret.txt'), path.join(rendererRoot, 'escape.txt'));

const files = new DesktopLocalAppFiles({rendererRoot});

function resolvePath(pathname) {
	return files.resolve(`fluxer-app://app${pathname}`);
}

function neverAborted() {
	return new AbortController().signal;
}

async function drain(stream) {
	let text = '';
	for await (const chunk of stream) {
		text += Buffer.from(chunk).toString('utf8');
	}
	return text;
}

function tick() {
	return new Promise((resolve) => {
		setImmediate(resolve);
	});
}

const STILL_WAITING = Symbol('still-waiting');

async function settleOrGiveUp(promise) {
	let timer;
	const gaveUp = new Promise((resolve) => {
		timer = setTimeout(() => resolve(STILL_WAITING), WAIT_GIVE_UP_MS);
	});
	promise.catch(() => {});
	try {
		return await Promise.race([promise, gaveUp]);
	} finally {
		clearTimeout(timer);
	}
}

after(async () => {
	for (const root of temporaryRoots) {
		await rm(root, {recursive: true, force: true});
	}
});

describe('renderer file resolution', () => {
	test('extensionless routes fall back to index.html', async () => {
		for (const pathname of ['/', '/channels/@me', '/login', '/settings/user']) {
			const resolved = await resolvePath(pathname);
			assert.equal(resolved.filePath, path.join(rendererRoot, 'index.html'), pathname);
		}
	});

	test('a dotted route segment still falls back to index.html, not 404', async () => {
		for (const pathname of ['/invite/abc.def', '/users/1.2.3', '/theme/my.custom.theme']) {
			const resolved = await resolvePath(pathname);
			assert.equal(resolved.filePath, path.join(rendererRoot, 'index.html'), pathname);
		}
	});

	test('a present asset resolves to itself with the right metadata', async () => {
		const resolved = await resolvePath('/assets/deadbeefdeadbeef.js');
		assert.equal(resolved.type, 'file');
		assert.equal(resolved.filePath, path.join(rendererRoot, 'assets', 'deadbeefdeadbeef.js'));
		assert.equal(resolved.size, 14);
		assert.equal(resolved.contentType, 'application/javascript; charset=utf-8');
		assert.equal(resolved.cacheControl, 'public, max-age=31536000, immutable');
	});

	test('a missing asset is 404, never the SPA shell', async () => {
		const resolved = await resolvePath('/assets/missing.js');
		assert.equal(resolved.type, 'not-found');
	});
});

describe('renderer file resolution refuses to leave the bundle', () => {
	test('path escapes in every encoding are blocked', async () => {
		const escapes = [
			'/..%2f..%2fetc%2fpasswd',
			'/%2e%2e%2f%2e%2e%2fx',
			'/..\\..\\x',
			'/assets/..%5c..%5cx',
			'/x%00.js',
			'/C:\\x',
			'/%ZZ',
		];
		for (const pathname of escapes) {
			const resolved = await resolvePath(pathname);
			assert.equal(resolved.type, 'blocked', `${pathname} -> ${JSON.stringify(resolved)}`);
		}
	});

	test('a dot-segment path is normalised by the URL parser before it can escape', async () => {
		const resolved = await resolvePath('/../etc/passwd');
		assert.equal(resolved.filePath, path.join(rendererRoot, 'index.html'));
	});

	test('a symlink pointing outside the bundle is not served', async () => {
		const resolved = await resolvePath('/escape.txt');
		assert.equal(resolved.type, 'not-found');
	});

	test('a request that is not on the local app origin is blocked', async () => {
		assert.equal((await files.resolve('https://web.fluxer.app/')).reason, 'invalid-origin');
		assert.equal((await files.resolve('data:text/html,x')).reason, 'invalid-origin');
		assert.equal((await files.resolve('fluxer-app://evil/')).reason, 'invalid-origin');
		assert.equal((await files.resolve('fluxer-app://app:8080/')).reason, 'invalid-origin');
	});
});

describe('renderer file reads', () => {
	test('a served asset comes back byte for byte', async () => {
		const resolved = await resolvePath('/assets/deadbeefdeadbeef.js');
		const stream = await files.openStream(resolved.filePath, resolved.size, neverAborted());
		assert.equal(await drain(stream), 'console.log(1)');
	});

	test('an asset too large to buffer is streamed and still comes back whole', async () => {
		const resolved = await resolvePath('/assets/f00dfacef00dface.js');
		assert.equal(resolved.size, LARGE_ASSET_BODY.length);
		const stream = await files.openStream(resolved.filePath, resolved.size, neverAborted());
		assert.equal(await drain(stream), LARGE_ASSET_BODY);
	});

	test('an empty asset is an empty body, not a failure', async () => {
		const resolved = await resolvePath('/assets/cafed00dcafed00d.css');
		assert.equal(resolved.size, 0);
		const stream = await files.openStream(resolved.filePath, resolved.size, neverAborted());
		assert.equal(await drain(stream), '');
	});

	test('readForResponse hands back the bytes and enforces the size ceiling', async () => {
		const resolved = await resolvePath('/assets/deadbeefdeadbeef.js');
		const file = await files.readForResponse(resolved.filePath, 1024, neverAborted());
		try {
			assert.equal(file.contents.toString('utf8'), 'console.log(1)');
		} finally {
			file.release();
		}
		await assert.rejects(() => files.readForResponse(resolved.filePath, 4, neverAborted()), /exceeds 4 bytes/u);
	});

	test('openStream streams the file and refuses a size that changed under it', async () => {
		const resolved = await resolvePath('/assets/deadbeefdeadbeef.js');
		const controller = new AbortController();
		const stream = await files.openStream(resolved.filePath, resolved.size, controller.signal);
		assert.equal(await drain(stream), 'console.log(1)');
		await assert.rejects(
			() => files.openStream(resolved.filePath, resolved.size + 1, controller.signal),
			(error) => error.name === 'LocalRendererFileChangedDuringOpenError',
		);
		controller.abort();
		await assert.rejects(() => files.openStream(resolved.filePath, resolved.size, controller.signal));
	});

	test('a buffered body the renderer never reads is torn down when the request aborts', async () => {
		const resolved = await resolvePath('/assets/deadbeefdeadbeef.js');
		const controller = new AbortController();
		const stream = await files.openStream(resolved.filePath, resolved.size, controller.signal);
		controller.abort(new Error('renderer went away'));
		await assert.rejects(() => drain(stream), /renderer went away/u);
	});

	test('isIndexFile only matches the shell document', async () => {
		assert.equal(files.isIndexFile(path.join(rendererRoot, 'index.html')), true);
		assert.equal(files.isIndexFile(path.join(rendererRoot, 'manifest.json')), false);
	});

	test('a symlinked path handed straight to a read never yields what it points at', async () => {
		const symlinkPath = path.join(rendererRoot, 'escape.txt');
		await assert.rejects(
			() => files.readForResponse(symlinkPath, 1024, neverAborted()),
			(error) => error.code === 'ELOOP',
			'readForResponse followed a symlink out of the bundle',
		);
		await assert.rejects(
			() => files.openStream(symlinkPath, 'secret'.length, neverAborted()),
			(error) => error.code === 'ELOOP',
			'openStream followed a symlink out of the bundle',
		);
	});
});

describe('a reaction burst is served rather than refused', () => {
	test('every request of a burst far past the old in-flight ceiling gets its bytes', async () => {
		const resolved = await resolvePath('/assets/deadbeefdeadbeef.js');
		const signal = neverAborted();
		const streams = await Promise.all(
			Array.from({length: REACTION_BURST_SIZE}, () => files.openStream(resolved.filePath, resolved.size, signal)),
		);
		const bodies = await Promise.all(streams.map((stream) => drain(stream)));
		assert.deepEqual(bodies, new Array(REACTION_BURST_SIZE).fill('console.log(1)'));
	});

	test('a burst of shell rewrites is served one after another instead of being turned away', async () => {
		const resolved = await resolvePath('/index.html');
		const signal = neverAborted();
		const bodies = await Promise.all(
			Array.from({length: 32}, async () => {
				const file = await files.readForResponse(resolved.filePath, REWRITTEN_FILE_MAX_BYTES, signal);
				try {
					return file.contents.toString('utf8');
				} finally {
					file.release();
				}
			}),
		);
		assert.deepEqual(bodies, new Array(32).fill('<html lang="en"><head></head><body></body></html>'));
	});

	test('a shell rewrite never waits on shell responses that are still open', async () => {
		const resolved = await resolvePath('/index.html');
		const signal = neverAborted();
		const open = [];
		try {
			for (let index = 0; index < OPEN_SHELL_RESPONSES; index += 1) {
				const file = await settleOrGiveUp(files.readForResponse(resolved.filePath, REWRITTEN_FILE_MAX_BYTES, signal));
				assert.notEqual(file, STILL_WAITING, `shell response ${index} was made to wait on the ones before it`);
				open.push(file);
			}
		} finally {
			for (const file of open) {
				file.release();
			}
		}
		assert.equal(open.length, OPEN_SHELL_RESPONSES);
	});

	test('a shell document past the rewrite ceiling still fails loudly', async () => {
		const resolved = await resolvePath('/index.html');
		await assert.rejects(
			() => files.readForResponse(resolved.filePath, 4, neverAborted()),
			(error) => error.name === 'LocalRendererFileTooLargeError',
		);
	});
});

describe('the resource budget queues instead of refusing', () => {
	test('a burst is admitted in order and never exceeds the limit', async () => {
		const budget = new LocalAppResourceBudget(2);
		const signal = neverAborted();
		const admitted = [];
		let peak = 0;
		await Promise.all(
			Array.from({length: 10}, (_unused, index) =>
				budget.acquire(1, signal).then(async (permit) => {
					admitted.push(index);
					peak = Math.max(peak, budget.inUse);
					await tick();
					permit.release();
				}),
			),
		);
		assert.deepEqual(admitted, [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]);
		assert.equal(peak, 2);
		assert.equal(budget.inUse, 0);
		assert.equal(budget.waitingCount, 0);
	});

	test('a reservation larger than the whole budget still runs once the budget frees up', async () => {
		const budget = new LocalAppResourceBudget(4);
		const signal = neverAborted();
		const held = await budget.acquire(4, signal);
		const oversized = budget.acquire(16, signal);
		held.release();
		const permit = await oversized;
		assert.equal(budget.inUse, 4);
		permit.release();
		assert.equal(budget.inUse, 0);
	});

	test('a waiter that aborts leaves the queue and reserves nothing', async () => {
		const budget = new LocalAppResourceBudget(1);
		const held = await budget.acquire(1, neverAborted());
		const controller = new AbortController();
		const queued = budget.acquire(1, controller.signal);
		assert.equal(budget.waitingCount, 1);
		controller.abort(new Error('renderer went away'));
		await assert.rejects(() => queued, /renderer went away/u);
		assert.equal(budget.waitingCount, 0);
		assert.equal(budget.inUse, 1);
		held.release();
		assert.equal(budget.inUse, 0);
	});

	test('a waiter aborted in the same turn it is admitted hands its permit straight back', async () => {
		const budget = new LocalAppResourceBudget(1);
		const held = await budget.acquire(1, neverAborted());
		const controller = new AbortController();
		controller.signal.addEventListener('abort', () => held.release());
		const queued = budget.acquire(1, controller.signal);
		controller.abort(new Error('renderer went away'));
		await assert.rejects(() => queued, /renderer went away/u);
		assert.equal(budget.inUse, 0);
		assert.equal(budget.waitingCount, 0);
		const permit = await budget.acquire(1, neverAborted());
		assert.equal(budget.inUse, 1);
		permit.release();
	});

	test('an acquire that is already aborted never joins the queue', async () => {
		const budget = new LocalAppResourceBudget(1);
		const controller = new AbortController();
		controller.abort(new Error('gone'));
		await assert.rejects(() => budget.acquire(1, controller.signal), /gone/u);
		assert.equal(budget.inUse, 0);
		assert.equal(budget.waitingCount, 0);
	});

	test('a waiter that aborts wakes the waiter queued behind it', async () => {
		const budget = new LocalAppResourceBudget(10);
		const kept = await budget.acquire(5, neverAborted());
		const released = await budget.acquire(5, neverAborted());
		const controller = new AbortController();
		const blocked = budget.acquire(8, controller.signal);
		const successor = budget.acquire(4, neverAborted());
		released.release();
		assert.equal(budget.waitingCount, 2);
		controller.abort(new Error('renderer went away'));
		await assert.rejects(() => blocked, /renderer went away/u);
		const permit = await settleOrGiveUp(successor);
		assert.notEqual(permit, STILL_WAITING, 'the successor was never woken after the aborted waiter left');
		assert.equal(budget.inUse, 9);
		permit.release();
		kept.release();
		assert.equal(budget.inUse, 0);
		assert.equal(budget.waitingCount, 0);
	});

	test('a nonsensical reservation is a real error, not a capacity refusal', async () => {
		const budget = new LocalAppResourceBudget(4);
		await assert.rejects(() => budget.acquire(0, neverAborted()), /not a positive integer/u);
		await assert.rejects(() => budget.acquire(1.5, neverAborted()), /not a positive integer/u);
	});
});

describe('a renderer root with no index.html', () => {
	test('rejects with MissingLocalRendererIndexError, and the rejection is memoised', async () => {
		const emptyRoot = createRoot('local-app-empty-');
		const emptyFiles = new DesktopLocalAppFiles({rendererRoot: emptyRoot});
		const isMissingIndex = (error) => error.name === 'MissingLocalRendererIndexError';
		await assert.rejects(() => emptyFiles.resolve('fluxer-app://app/'), isMissingIndex);
		writeFileSync(path.join(emptyRoot, 'index.html'), '<html lang="en"></html>');
		await assert.rejects(() => emptyFiles.resolve('fluxer-app://app/'), isMissingIndex);
	});
});

describe('a renderer root probe that fails', () => {
	test('is retried on the next request instead of poisoning every later one', async () => {
		const root = createRendererRoot();
		const indexPath = path.join(root, 'index.html');
		const probed = new DesktopLocalAppFiles({rendererRoot: root});
		const lstat = fsPromises.lstat;
		let failed = false;
		fsPromises.lstat = async (target, ...rest) => {
			if (target === indexPath && !failed) {
				failed = true;
				throw Object.assign(new Error('too many open files'), {code: 'EMFILE'});
			}
			return await lstat(target, ...rest);
		};
		try {
			await assert.rejects(
				() => probed.resolve('fluxer-app://app/channels/@me'),
				(error) => error.code === 'EMFILE',
			);
			const resolved = await probed.resolve('fluxer-app://app/channels/@me');
			assert.equal(resolved.filePath, indexPath);
		} finally {
			fsPromises.lstat = lstat;
		}
	});
});

const moduleStoreRoot = createRoot('local-app-modules-');
const moduleDirectory = path.join(moduleStoreRoot, 'store', 'fluxer_renderer', 'b'.repeat(64));
mkdirSync(path.join(moduleDirectory, 'assets'), {recursive: true});
writeFileSync(path.join(moduleDirectory, 'assets', 'cafebabecafebabe.wasm'), 'wasm-bytes');
writeFileSync(path.join(moduleDirectory, 'assets', 'deadbeefdeadbeef.js'), 'console.log(2)');
writeFileSync(path.join(moduleDirectory, 'index.html'), '<html lang="en"><body>module</body></html>');
symlinkSync(path.join(outsideRoot, 'secret.txt'), path.join(moduleDirectory, 'assets', 'aaaaaaaabbbbbbbb.css'));

const siblingStoreRoot = `${moduleStoreRoot}-sibling`;
temporaryRoots.push(siblingStoreRoot);
mkdirSync(siblingStoreRoot);
writeFileSync(path.join(siblingStoreRoot, 'secret.txt'), 'sibling-secret');

function moduleIndexEntries() {
	return [
		['assets/cafebabecafebabe.wasm', path.join(moduleDirectory, 'assets', 'cafebabecafebabe.wasm')],
		['assets/deadbeefdeadbeef.js', path.join(moduleDirectory, 'assets', 'deadbeefdeadbeef.js')],
		['assets/aaaaaaaabbbbbbbb.css', path.join(moduleDirectory, 'assets', 'aaaaaaaabbbbbbbb.css')],
		['index.html', path.join(moduleDirectory, 'index.html')],
	];
}

function createModuleFiles(entries = moduleIndexEntries()) {
	const moduleFiles = new DesktopLocalAppFiles({rendererRoot: createRendererRoot()});
	moduleFiles.setModuleIndex({root: moduleStoreRoot, files: new Map(entries)});
	return moduleFiles;
}

describe('the module root chain', () => {
	test('a committed module file wins over the asar copy', async () => {
		const moduleFiles = createModuleFiles();
		const resolved = await moduleFiles.resolve('fluxer-app://app/assets/deadbeefdeadbeef.js');
		assert.equal(resolved.type, 'file');
		assert.equal(resolved.filePath, path.join(moduleDirectory, 'assets', 'deadbeefdeadbeef.js'));
		assert.equal(resolved.size, 14);
		assert.equal(resolved.contentType, 'application/javascript; charset=utf-8');
		const stream = await moduleFiles.openStream(resolved.filePath, resolved.size, neverAborted());
		assert.equal(await drain(stream), 'console.log(2)');
	});

	test('a store asset is cached from its resolved path, not its request path', async () => {
		const moduleFiles = createModuleFiles();
		const resolved = await moduleFiles.resolve('fluxer-app://app/assets/cafebabecafebabe.wasm');
		assert.equal(resolved.filePath, path.join(moduleDirectory, 'assets', 'cafebabecafebabe.wasm'));
		assert.equal(resolved.cacheControl, 'public, max-age=31536000, immutable');
		assert.equal(resolved.contentType, 'application/wasm');
		assert.equal(resolved.size, 10);
	});

	test('a missing module asset is 404, never the SPA shell', async () => {
		const moduleFiles = createModuleFiles();
		for (const pathname of ['/assets/missing.wasm', '/assets/missing.js', '/assets/missing.woff2']) {
			const resolved = await moduleFiles.resolve(`fluxer-app://app${pathname}`);
			assert.equal(resolved.type, 'not-found', pathname);
			assert.equal(resolved.filePath, undefined, pathname);
		}
	});

	test('a module entry whose file is gone falls back to the asar copy', async () => {
		const rendererRoot = createRendererRoot();
		const moduleFiles = new DesktopLocalAppFiles({rendererRoot});
		moduleFiles.setModuleIndex({
			root: moduleStoreRoot,
			files: new Map([['assets/deadbeefdeadbeef.js', path.join(moduleDirectory, 'assets', 'ghost.js')]]),
		});
		const resolved = await moduleFiles.resolve('fluxer-app://app/assets/deadbeefdeadbeef.js');
		assert.equal(resolved.filePath, path.join(rendererRoot, 'assets', 'deadbeefdeadbeef.js'));
		assert.equal(resolved.size, 14);
	});

	test('a module entry pointing outside the store root is blocked', async () => {
		const moduleFiles = createModuleFiles([['evil.txt', path.join(outsideRoot, 'secret.txt')]]);
		const resolved = await moduleFiles.resolve('fluxer-app://app/evil.txt');
		assert.equal(resolved.type, 'blocked');
		assert.equal(resolved.reason, 'path-escape');
	});

	test('a module entry in a sibling directory that only shares the store root name prefix is blocked', async () => {
		const moduleFiles = createModuleFiles([['evil.txt', path.join(siblingStoreRoot, 'secret.txt')]]);
		const resolved = await moduleFiles.resolve('fluxer-app://app/evil.txt');
		assert.equal(resolved.type, 'blocked');
		assert.equal(resolved.reason, 'path-escape');
	});

	test('a symlinked module file is not served and does not become the SPA shell', async () => {
		const moduleFiles = createModuleFiles();
		const resolved = await moduleFiles.resolve('fluxer-app://app/assets/aaaaaaaabbbbbbbb.css');
		assert.equal(resolved.type, 'not-found');
	});

	test('request path hardening still runs when an index is installed', async () => {
		const moduleFiles = createModuleFiles();
		for (const pathname of ['/x%00.js', '/..%2f..%2fetc%2fpasswd', '/assets/..%5c..%5cx']) {
			const resolved = await moduleFiles.resolve(`fluxer-app://app${pathname}`);
			assert.equal(resolved.type, 'blocked', pathname);
		}
	});

	test('the committed module supplies index.html for the shell and for SPA routes', async () => {
		const moduleFiles = createModuleFiles();
		for (const pathname of ['/', '/index.html', '/channels/@me', '/invite/abc.def']) {
			const resolved = await moduleFiles.resolve(`fluxer-app://app${pathname}`);
			assert.equal(resolved.filePath, path.join(moduleDirectory, 'index.html'), pathname);
		}
	});

	test('clearing the module index restores the asar chain', async () => {
		const rendererRoot = createRendererRoot();
		const moduleFiles = new DesktopLocalAppFiles({rendererRoot});
		moduleFiles.setModuleIndex({root: moduleStoreRoot, files: new Map(moduleIndexEntries())});
		moduleFiles.setModuleIndex(null);
		const resolved = await moduleFiles.resolve('fluxer-app://app/assets/deadbeefdeadbeef.js');
		assert.equal(resolved.filePath, path.join(rendererRoot, 'assets', 'deadbeefdeadbeef.js'));
		const route = await moduleFiles.resolve('fluxer-app://app/channels/@me');
		assert.equal(route.filePath, path.join(rendererRoot, 'index.html'));
	});
});
