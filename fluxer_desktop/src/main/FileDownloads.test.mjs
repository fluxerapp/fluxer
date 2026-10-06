// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {createHash} from 'node:crypto';
import {existsSync, mkdtempSync, readFileSync, writeFileSync} from 'node:fs';
import {createServer} from 'node:http';
import {createRequire} from 'node:module';
import {tmpdir} from 'node:os';
import {join} from 'node:path';
import {after, before, describe, test} from 'node:test';
import {fileURLToPath} from 'node:url';
import vm from 'node:vm';

const require = createRequire(import.meta.url);
const esbuild = require('esbuild');

function transform(name) {
	const url = new URL(`./${name}`, import.meta.url);
	const path = fileURLToPath(url);
	return {
		path,
		code: esbuild.transformSync(readFileSync(path, 'utf8'), {
			loader: 'ts',
			format: 'cjs',
			platform: 'node',
			target: 'node20',
			define: {'import.meta.url': JSON.stringify(url.href)},
		}).code,
	};
}

const outboundSource = transform('DesktopOutboundHTTP.ts');
const fileDownloadsSource = transform('FileDownloads.ts');

const PAYLOAD = Buffer.alloc(300_000, 9);
const PAYLOAD_SHA256 = createHash('sha256').update(PAYLOAD).digest('hex');
const CORRUPT = Buffer.concat([Buffer.from([0]), PAYLOAD.subarray(1)]);
const CORRUPT_SHA256 = createHash('sha256').update(CORRUPT).digest('hex');
const EMPTY_SHA256 = createHash('sha256').digest('hex');

let server;
let baseUrl;
let requests = 0;

before(async () => {
	server = createServer((request, response) => {
		requests += 1;
		if (request.url === '/payload' || request.url === '/redirected') {
			response.writeHead(200, {'content-type': 'application/octet-stream', 'content-length': PAYLOAD.length});
			response.end(PAYLOAD);
			return;
		}
		if (request.url === '/corrupt') {
			response.writeHead(200, {'content-type': 'application/octet-stream', 'content-length': CORRUPT.length});
			response.end(CORRUPT);
			return;
		}
		if (request.url === '/redirect') {
			response.writeHead(302, {location: '/redirected'});
			response.end();
			return;
		}
		if (request.url === '/empty') {
			response.writeHead(204);
			response.end();
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

function loadFileDownloads() {
	const stubs = {
		'@electron/common/DesktopConfig': {getAppUrl: () => baseUrl},
		'@electron/common/Logger': {
			createChildLogger: () => ({debug() {}, info() {}, warn() {}, error() {}}),
		},
	};
	const sandbox = {
		console,
		Buffer,
		URL,
		process,
		setTimeout,
		clearTimeout,
		require: (specifier) => stubs[specifier] ?? require(specifier),
	};
	const context = vm.createContext(sandbox);

	const outboundModule = {exports: {}};
	sandbox.module = outboundModule;
	sandbox.exports = outboundModule.exports;
	vm.runInContext(outboundSource.code, context, {filename: outboundSource.path});
	stubs['@electron/main/DesktopOutboundHTTP'] = outboundModule.exports;

	const fileDownloadsModule = {exports: {}};
	sandbox.module = fileDownloadsModule;
	sandbox.exports = fileDownloadsModule.exports;
	vm.runInContext(fileDownloadsSource.code, context, {filename: fileDownloadsSource.path});
	return fileDownloadsModule.exports;
}

function destination() {
	return join(mkdtempSync(join(tmpdir(), 'fluxer-download-test-')), 'Fluxer-linux-amd64.deb');
}

describe('downloadFile checksum verification', () => {
	test('keeps the file when the bytes match the published sha256', async () => {
		const {downloadFile} = loadFileDownloads();
		const destPath = destination();

		await downloadFile(`${baseUrl}/payload`, destPath, {sha256: PAYLOAD_SHA256});

		assert.equal(readFileSync(destPath).equals(PAYLOAD), true);
	});

	test('accepts an uppercase published sha256', async () => {
		const {downloadFile} = loadFileDownloads();
		const destPath = destination();

		await downloadFile(`${baseUrl}/payload`, destPath, {sha256: PAYLOAD_SHA256.toUpperCase()});

		assert.equal(readFileSync(destPath).equals(PAYLOAD), true);
	});

	test('verifies the bytes that survive a redirect', async () => {
		const {downloadFile} = loadFileDownloads();
		const destPath = destination();

		await downloadFile(`${baseUrl}/redirect`, destPath, {sha256: PAYLOAD_SHA256});

		assert.equal(readFileSync(destPath).equals(PAYLOAD), true);
	});

	test('deletes the download and reports both digests when the bytes do not match', async () => {
		const {downloadFile} = loadFileDownloads();
		const destPath = destination();

		await assert.rejects(downloadFile(`${baseUrl}/corrupt`, destPath, {sha256: PAYLOAD_SHA256}), (error) => {
			assert.equal(error.name, 'DownloadChecksumError');
			assert.match(error.message, new RegExp(PAYLOAD_SHA256));
			assert.match(error.message, new RegExp(CORRUPT_SHA256));
			return true;
		});

		assert.equal(existsSync(destPath), false);
	});

	test('leaves nothing behind when an earlier file sits at the destination', async () => {
		const {downloadFile} = loadFileDownloads();
		const destPath = destination();
		writeFileSync(destPath, Buffer.from('an older download'), {mode: 0o755});

		await assert.rejects(downloadFile(`${baseUrl}/corrupt`, destPath, {sha256: PAYLOAD_SHA256}));

		assert.equal(existsSync(destPath), false);
	});

	test('refuses an empty body when a checksum was published', async () => {
		const {downloadFile} = loadFileDownloads();
		const destPath = destination();

		await assert.rejects(downloadFile(`${baseUrl}/empty`, destPath, {sha256: PAYLOAD_SHA256}), (error) => {
			assert.equal(error.name, 'DownloadChecksumError');
			assert.match(error.message, new RegExp(EMPTY_SHA256));
			return true;
		});

		assert.equal(existsSync(destPath), false);
	});

	test('refuses a malformed checksum before asking for any bytes', async () => {
		const {downloadFile} = loadFileDownloads();
		const destPath = destination();
		const requestsBefore = requests;

		await assert.rejects(downloadFile(`${baseUrl}/payload`, destPath, {sha256: 'deb-2026.909.202036'}), (error) => {
			assert.equal(error.message, 'Download checksum is malformed');
			return true;
		});

		assert.equal(requests, requestsBefore);
		assert.equal(existsSync(destPath), false);
	});

	test('downloads without verifying when the feed published no checksum', async () => {
		const {downloadFile} = loadFileDownloads();
		const destPath = destination();

		await downloadFile(`${baseUrl}/corrupt`, destPath, {sha256: null});

		assert.equal(readFileSync(destPath).equals(CORRUPT), true);
	});
});
