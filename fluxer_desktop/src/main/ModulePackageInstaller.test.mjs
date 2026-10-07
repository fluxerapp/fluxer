// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {Buffer} from 'node:buffer';
import {createHash} from 'node:crypto';
import {mkdtempSync, writeFileSync} from 'node:fs';
import {rm} from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';
import {after, describe, test} from 'node:test';
import './LocalAppTestSupport.test.mjs';

const {ModulePackageIOError, verifyAndExtract} = await import('@electron/main/ModulePackage');
const {isDeterministicPackageFailure, ModulePackageInstaller} = await import('@electron/main/ModulePackageInstaller');

const temporaryRoots = [];

after(async () => {
	for (const root of temporaryRoots) {
		await rm(root, {recursive: true, force: true});
	}
});

function createRoot() {
	const root = mkdtempSync(path.join(os.tmpdir(), 'module-package-installer-'));
	temporaryRoots.push(root);
	return root;
}

function sha256(data) {
	return createHash('sha256').update(data).digest('hex');
}

describe('module package failure classification', () => {
	test('an EEXIST failure from the extractor is deterministic', async () => {
		const root = createRoot();
		const packed = Buffer.from('module package bytes', 'utf8');
		const packedPath = path.join(root, 'package.br');
		writeFileSync(packedPath, packed);
		const destinationDir = path.join(root, 'incoming');
		writeFileSync(destinationDir, 'occupied');
		const failure = await verifyAndExtract({packedPath, expectedSha256: sha256(packed), destinationDir}).then(
			() => null,
			(error) => error,
		);
		assert.equal(failure?.name, 'ModulePackageIOError');
		assert.equal(failure?.cause?.code, 'EEXIST');
		assert.equal(isDeterministicPackageFailure(failure), true);
	});

	test('a storage-full failure from the extractor stays non-deterministic', () => {
		const cause = Object.assign(new Error('no space left on device'), {code: 'ENOSPC'});
		const failure = new ModulePackageIOError('failed to write index.html', {cause});
		assert.equal(isDeterministicPackageFailure(failure), false);
	});
});

const PACKAGE_BYTES = Buffer.from('0123456789abcdefghijklmnopqrstuvwxyz', 'utf8');
const PACKAGE_ORIGIN = 'https://pkgs.example.test';

function planItem({
	url = `${PACKAGE_ORIGIN}/desktop/canary/modules/fluxer_renderer/${sha256(PACKAGE_BYTES)}/package.br`,
} = {}) {
	return {
		module: 'fluxer_renderer',
		entry: {sha256: sha256(PACKAGE_BYTES), bytes: PACKAGE_BYTES.length, url},
		requirement: 'required',
	};
}

async function collect(download) {
	const parts = [];
	for await (const chunk of download.chunks) {
		parts.push(Buffer.from(chunk));
	}
	return {offset: download.offset, bytes: Buffer.concat(parts)};
}

function createInstaller({respond, resumeFrom}) {
	const requests = [];
	const reports = [];
	const downloads = [];
	const installer = new ModulePackageInstaller({
		store: {
			installModule: async ({download}) => {
				downloads.push(await collect(await download(resumeFrom)));
			},
		},
		packageOrigin: PACKAGE_ORIGIN,
		fetch: async (url, init) => {
			requests.push({url, range: init.headers.range ?? null});
			return respond(requests.length, init);
		},
		onProgress: () => {},
		report: (report) => reports.push(report),
	});
	return {installer, requests, reports, downloads};
}

describe('resuming a module package download', () => {
	test('asks for the remaining bytes and hands back the range the server confirmed', async () => {
		const {installer, requests, downloads} = createInstaller({
			resumeFrom: 10,
			respond: () =>
				new Response(PACKAGE_BYTES.subarray(10), {
					status: 206,
					headers: {'content-range': `bytes 10-${PACKAGE_BYTES.length - 1}/${PACKAGE_BYTES.length}`},
				}),
		});

		await installer.install(planItem(), 1, 1);

		assert.deepEqual(
			requests.map((request) => request.range),
			['bytes=10-'],
		);
		assert.equal(downloads[0].offset, 10);
		assert.deepEqual(downloads[0].bytes, PACKAGE_BYTES.subarray(10));
	});

	test('a full response to a range request restarts from the first byte', async () => {
		const {installer, downloads} = createInstaller({
			resumeFrom: 10,
			respond: () => new Response(PACKAGE_BYTES, {status: 200}),
		});

		await installer.install(planItem(), 1, 1);

		assert.equal(downloads[0].offset, 0);
		assert.deepEqual(downloads[0].bytes, PACKAGE_BYTES);
	});

	test('an unsatisfiable range falls back to a plain request', async () => {
		const {installer, requests, downloads} = createInstaller({
			resumeFrom: 10,
			respond: (attempt) =>
				attempt === 1 ? new Response('', {status: 416}) : new Response(PACKAGE_BYTES, {status: 200}),
		});

		await installer.install(planItem(), 1, 1);

		assert.deepEqual(
			requests.map((request) => request.range),
			['bytes=10-', null],
		);
		assert.equal(downloads[0].offset, 0);
	});

	test('a partial response that starts somewhere else is discarded for a plain request', async () => {
		const {installer, requests, downloads} = createInstaller({
			resumeFrom: 10,
			respond: (attempt) =>
				attempt === 1
					? new Response(PACKAGE_BYTES.subarray(5), {
							status: 206,
							headers: {'content-range': `bytes 5-${PACKAGE_BYTES.length - 1}/${PACKAGE_BYTES.length}`},
						})
					: new Response(PACKAGE_BYTES, {status: 200}),
		});

		await installer.install(planItem(), 1, 1);

		assert.deepEqual(
			requests.map((request) => request.range),
			['bytes=10-', null],
		);
		assert.deepEqual(downloads[0].bytes, PACKAGE_BYTES);
	});

	test('a partial file that already holds every byte needs no request', async () => {
		const {installer, requests, downloads} = createInstaller({
			resumeFrom: PACKAGE_BYTES.length,
			respond: () => {
				throw new Error('unexpected request');
			},
		});

		await installer.install(planItem(), 1, 1);

		assert.equal(requests.length, 0);
		assert.equal(downloads[0].offset, PACKAGE_BYTES.length);
		assert.equal(downloads[0].bytes.length, 0);
	});

	test('a partial file longer than the package is downloaded again from the start', async () => {
		const {installer, requests} = createInstaller({
			resumeFrom: PACKAGE_BYTES.length + 1,
			respond: () => new Response(PACKAGE_BYTES, {status: 200}),
		});

		await installer.install(planItem(), 1, 1);

		assert.deepEqual(
			requests.map((request) => request.range),
			[null],
		);
	});
});

describe('reporting deterministic package failures', () => {
	test('a package url the policy refuses is reported with the url', async () => {
		const url = 'http://192.168.139.107:48780/package.br';
		const {installer, reports} = createInstaller({
			resumeFrom: 0,
			respond: () => {
				throw new Error('unexpected request');
			},
		});

		await assert.rejects(installer.install(planItem({url}), 1, 1), /not https/u);

		assert.equal(reports.length, 1);
		assert.equal(reports[0].type, 'package-rejected');
		assert.match(reports[0].message, /192\.168\.139\.107/u);
	});
});
