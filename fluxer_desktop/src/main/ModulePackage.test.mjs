// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {Buffer} from 'node:buffer';
import {createHash} from 'node:crypto';
import {mkdirSync, mkdtempSync, readdirSync, readFileSync, writeFileSync} from 'node:fs';
import {rm} from 'node:fs/promises';
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

const {verifyAndExtract} = await import('./ModulePackage.ts');
const {parseModuleVersion} = await import('@electron/main/ModuleVersion');

const BLOCK = 512;
const MODULE_NAME = 'fluxer_renderer';
const BUILD_VERSION = '2026.823.1';
const RELEASE_CHANNEL = 'canary';
const SOURCE_SHA = 'a'.repeat(40);

const temporaryRoots = [];

after(async () => {
	for (const root of temporaryRoots) {
		await rm(root, {recursive: true, force: true});
	}
});

function createRoot(prefix) {
	const root = mkdtempSync(path.join(os.tmpdir(), prefix));
	temporaryRoots.push(root);
	return root;
}

const workspace = createRoot('module-package-');
let counter = 0;

function packagePath(packed) {
	const file = path.join(workspace, `package-${(counter += 1)}.br`);
	writeFileSync(file, packed);
	return file;
}

function destination() {
	return path.join(workspace, `store-${(counter += 1)}`);
}

function sha256(data) {
	return createHash('sha256').update(data).digest('hex');
}

function octal(value, width) {
	return `${value.toString(8).padStart(width - 1, '0')}\0`;
}

function fixChecksum(header) {
	header.fill(0x20, 148, 156);
	let sum = 0;
	for (const byte of header) {
		sum += byte;
	}
	header.write(octal(sum, 8), 148, 8, 'ascii');
}

function ustarHeader({name, size, mode = 0o644, mtime = 0, typeflag = '0', prefix = ''}) {
	const header = Buffer.alloc(BLOCK);
	header.write(name, 0, 100, 'utf8');
	header.write(octal(mode, 8), 100, 8, 'ascii');
	header.write(octal(0, 8), 108, 8, 'ascii');
	header.write(octal(0, 8), 116, 8, 'ascii');
	header.write(octal(size, 12), 124, 12, 'ascii');
	header.write(octal(mtime, 12), 136, 12, 'ascii');
	header.write(typeflag, 156, 1, 'ascii');
	header.write('ustar\0', 257, 6, 'ascii');
	header.write('00', 263, 2, 'ascii');
	header.write(prefix, 345, 155, 'utf8');
	fixChecksum(header);
	return header;
}

function pad(size) {
	const remainder = size % BLOCK;
	return remainder === 0 ? Buffer.alloc(0) : Buffer.alloc(BLOCK - remainder);
}

function packArchive(manifestBytes, members) {
	const chunks = [
		ustarHeader({name: 'module.json', size: manifestBytes.length}),
		manifestBytes,
		pad(manifestBytes.length),
	];
	for (const member of members) {
		chunks.push(ustarHeader({name: member.name, size: member.body.length}), member.body, pad(member.body.length));
	}
	chunks.push(Buffer.alloc(BLOCK * 2));
	const tar = Buffer.concat(chunks);
	return repack(tar);
}

function repack(tar) {
	const packed = zlib.brotliCompressSync(tar, {
		params: {
			[zlib.constants.BROTLI_PARAM_QUALITY]: 5,
			[zlib.constants.BROTLI_PARAM_SIZE_HINT]: tar.length,
		},
	});
	return {tar, packed, sha256: sha256(packed)};
}

function manifestBytesFor(files) {
	return Buffer.from(
		`${JSON.stringify(
			{
				module: MODULE_NAME,
				build_version: BUILD_VERSION,
				release_channel: RELEASE_CHANNEL,
				source_sha: SOURCE_SHA,
				files,
			},
			null,
			'\t',
		)}\n`,
		'utf8',
	);
}

function walk(root, base = '') {
	const out = [];
	for (const entry of readdirSync(path.join(root, base), {withFileTypes: true})) {
		const relative = base ? path.posix.join(base, entry.name) : entry.name;
		if (entry.isDirectory()) {
			out.push(...walk(root, relative));
		} else if (entry.isFile()) {
			out.push(relative);
		}
	}
	return out.sort();
}

function packModule(sourceDir) {
	const files = [];
	const members = [];
	for (const relative of walk(sourceDir)) {
		const body = readFileSync(path.join(sourceDir, relative));
		files.push({path: relative, sha256: sha256(body), bytes: body.length});
		members.push({name: `files/${relative}`, body});
	}
	return packArchive(manifestBytesFor(files), members);
}

function forgeHeader(tar, targetName, mutate) {
	const forged = Buffer.from(tar);
	const at = forged.indexOf(Buffer.from(`${targetName}\0`, 'utf8'));
	if (at === -1) {
		throw new Error(`fixture header ${targetName} not found`);
	}
	const header = forged.subarray(at, at + BLOCK);
	mutate(header);
	fixChecksum(header);
	return forged;
}

function renameHeader(tar, oldName, newName) {
	return forgeHeader(tar, oldName, (header) => {
		header.fill(0, 0, 100);
		header.write(newName, 0, 100, 'utf8');
	});
}

const fixture = createRoot('module-package-fixture-');
mkdirSync(path.join(fixture, 'assets'));
mkdirSync(path.join(fixture, 'web'));
writeFileSync(path.join(fixture, 'index.html'), '<html lang="en"><head></head><body></body></html>');
writeFileSync(path.join(fixture, 'assets', 'deadbeefdeadbeef.js'), 'console.log(1)');
writeFileSync(path.join(fixture, 'web', 'favicon-32x32.png'), 'png-bytes');

const good = packModule(fixture);

describe('module package extraction', () => {
	test('a valid package verifies and lands every declared file', async () => {
		const destinationDir = destination();
		const manifest = await verifyAndExtract({
			packedPath: packagePath(good.packed),
			expectedSha256: good.sha256,
			destinationDir,
		});
		assert.equal(manifest.module, MODULE_NAME);
		assert.equal(manifest.build_version, BUILD_VERSION);
		assert.equal(manifest.release_channel, RELEASE_CHANNEL);
		assert.equal(manifest.source_sha, SOURCE_SHA);
		assert.deepEqual(
			manifest.files.map((file) => file.path),
			['assets/deadbeefdeadbeef.js', 'index.html', 'web/favicon-32x32.png'],
		);
		assert.equal(readFileSync(path.join(destinationDir, 'assets', 'deadbeefdeadbeef.js'), 'utf8'), 'console.log(1)');
		assert.equal(readFileSync(path.join(destinationDir, 'web', 'favicon-32x32.png'), 'utf8'), 'png-bytes');
		assert.equal(JSON.parse(readFileSync(path.join(destinationDir, 'module.json'), 'utf8')).module, MODULE_NAME);
	});

	test('a wrong package sha256 is rejected before anything is extracted', async () => {
		const destinationDir = destination();
		await assert.rejects(
			verifyAndExtract({
				packedPath: packagePath(good.packed),
				expectedSha256: '0'.repeat(64),
				destinationDir,
			}),
			{name: 'ModulePackageHashMismatchError'},
		);
		assert.throws(() => readdirSync(destinationDir));
	});

	test('a tampered payload fails its per-file sha256', async () => {
		const tar = Buffer.from(good.tar);
		const at = tar.indexOf(Buffer.from('console.log', 'utf8'));
		assert.notEqual(at, -1);
		tar[at] ^= 0xff;
		const tampered = repack(tar);
		await assert.rejects(
			verifyAndExtract({
				packedPath: packagePath(tampered.packed),
				expectedSha256: tampered.sha256,
				destinationDir: destination(),
			}),
			{name: 'ModulePackageHashMismatchError', message: 'hash mismatch: assets/deadbeefdeadbeef.js'},
		);
	});

	test('an archive member outside files/ is rejected even with a valid ustar checksum', async () => {
		const forged = repack(renameHeader(good.tar, 'files/index.html', '../../../etc/passwd'));
		await assert.rejects(
			verifyAndExtract({
				packedPath: packagePath(forged.packed),
				expectedSha256: forged.sha256,
				destinationDir: destination(),
			}),
			{name: 'ModulePackageMalformedError', message: 'path must start with files/: ../../../etc/passwd'},
		);
	});

	test('an undeclared member under files/ is rejected even with a valid ustar checksum', async () => {
		const forged = repack(renameHeader(good.tar, 'files/index.html', 'files/smuggled.js'));
		await assert.rejects(
			verifyAndExtract({
				packedPath: packagePath(forged.packed),
				expectedSha256: forged.sha256,
				destinationDir: destination(),
			}),
			{name: 'ModulePackageMalformedError', message: 'unexpected file in archive: not in manifest: files/smuggled.js'},
		);
	});

	test('a truncated brotli stream is rejected', async () => {
		const truncated = good.packed.subarray(0, Math.floor(good.packed.length / 2));
		await assert.rejects(
			verifyAndExtract({
				packedPath: packagePath(truncated),
				expectedSha256: sha256(truncated),
				destinationDir: destination(),
			}),
			{name: 'ModulePackageMalformedError'},
		);
	});

	test('an archive without module.json at the root is rejected', async () => {
		const forged = repack(renameHeader(good.tar, 'module.json', 'notmodule.json'));
		await assert.rejects(
			verifyAndExtract({
				packedPath: packagePath(forged.packed),
				expectedSha256: forged.sha256,
				destinationDir: destination(),
			}),
			{name: 'ModulePackageMalformedError', message: 'module.json missing from archive root'},
		);
	});

	test('a symlink typeflag is rejected even with a valid ustar checksum', async () => {
		const forged = repack(
			forgeHeader(good.tar, 'files/index.html', (header) => {
				header.write('2', 156, 1, 'ascii');
			}),
		);
		await assert.rejects(
			verifyAndExtract({
				packedPath: packagePath(forged.packed),
				expectedSha256: forged.sha256,
				destinationDir: destination(),
			}),
			{name: 'ModulePackageMalformedError'},
		);
	});

	test('a manifest declaring an escaping path is rejected', async () => {
		const body = Buffer.from('owned', 'utf8');
		const escaping = packArchive(
			manifestBytesFor([{path: '../../../etc/passwd', sha256: sha256(body), bytes: body.length}]),
			[{name: 'files/../../../etc/passwd', body}],
		);
		await assert.rejects(
			verifyAndExtract({
				packedPath: packagePath(escaping.packed),
				expectedSha256: escaping.sha256,
				destinationDir: destination(),
			}),
			{name: 'ModulePackageMalformedError', message: 'unsafe path in archive: ../../../etc/passwd'},
		);
	});

	test('a manifest declaring module.json is rejected instead of colliding with the extracted manifest', async () => {
		const body = Buffer.from('{"module":"smuggled"}', 'utf8');
		const reserved = packArchive(manifestBytesFor([{path: 'module.json', sha256: sha256(body), bytes: body.length}]), [
			{name: 'files/module.json', body},
		]);
		await assert.rejects(
			verifyAndExtract({
				packedPath: packagePath(reserved.packed),
				expectedSha256: reserved.sha256,
				destinationDir: destination(),
			}),
			{name: 'ModulePackageMalformedError', message: 'module.json declares the reserved path module.json'},
		);
	});

	test('a manifest declaring two paths that differ only in ASCII case is rejected', async () => {
		const first = Buffer.from('console.log(1)', 'utf8');
		const second = Buffer.from('console.log(2)', 'utf8');
		const colliding = packArchive(
			manifestBytesFor([
				{path: 'assets/App.js', sha256: sha256(first), bytes: first.length},
				{path: 'assets/app.js', sha256: sha256(second), bytes: second.length},
			]),
			[
				{name: 'files/assets/App.js', body: first},
				{name: 'files/assets/app.js', body: second},
			],
		);
		await assert.rejects(
			verifyAndExtract({
				packedPath: packagePath(colliding.packed),
				expectedSha256: colliding.sha256,
				destinationDir: destination(),
			}),
			{
				name: 'ModulePackageMalformedError',
				message: 'module.json declares colliding paths assets/App.js and assets/app.js',
			},
		);
	});

	test('a manifest declaring two paths that differ only in non-ASCII case is rejected', async () => {
		const first = Buffer.from('console.log(1)', 'utf8');
		const second = Buffer.from('console.log(2)', 'utf8');
		const colliding = packArchive(
			manifestBytesFor([
				{path: 'assets/\u00c9.js', sha256: sha256(first), bytes: first.length},
				{path: 'assets/\u00e9.js', sha256: sha256(second), bytes: second.length},
			]),
			[
				{name: 'files/assets/\u00c9.js', body: first},
				{name: 'files/assets/\u00e9.js', body: second},
			],
		);
		await assert.rejects(
			verifyAndExtract({
				packedPath: packagePath(colliding.packed),
				expectedSha256: colliding.sha256,
				destinationDir: destination(),
			}),
			{name: 'ModulePackageMalformedError'},
		);
	});

	test('a manifest declaring the same path in NFD and NFC is rejected', async () => {
		const decomposed = 'assets/e\u0301.js';
		const composed = 'assets/\u00e9.js';
		const first = Buffer.from('console.log(1)', 'utf8');
		const second = Buffer.from('console.log(2)', 'utf8');
		const colliding = packArchive(
			manifestBytesFor([
				{path: decomposed, sha256: sha256(first), bytes: first.length},
				{path: composed, sha256: sha256(second), bytes: second.length},
			]),
			[
				{name: `files/${decomposed}`, body: first},
				{name: `files/${composed}`, body: second},
			],
		);
		await assert.rejects(
			verifyAndExtract({
				packedPath: packagePath(colliding.packed),
				expectedSha256: colliding.sha256,
				destinationDir: destination(),
			}),
			{
				name: 'ModulePackageMalformedError',
				message: `module.json declares colliding paths ${decomposed} and ${composed}`,
			},
		);
	});

	test('a manifest declaring a file that is also a directory prefix is rejected', async () => {
		const first = Buffer.from('console.log(1)', 'utf8');
		const second = Buffer.from('console.log(2)', 'utf8');
		const colliding = packArchive(
			manifestBytesFor([
				{path: 'assets', sha256: sha256(first), bytes: first.length},
				{path: 'assets/app.js', sha256: sha256(second), bytes: second.length},
			]),
			[
				{name: 'files/assets', body: first},
				{name: 'files/assets/app.js', body: second},
			],
		);
		await assert.rejects(
			verifyAndExtract({
				packedPath: packagePath(colliding.packed),
				expectedSha256: colliding.sha256,
				destinationDir: destination(),
			}),
			{name: 'ModulePackageMalformedError', message: 'module.json declares colliding paths assets and assets/app.js'},
		);
	});

	test('a manifest declaring an absolute path is rejected', async () => {
		const body = Buffer.from('owned', 'utf8');
		const absolute = packArchive(manifestBytesFor([{path: '/etc/passwd', sha256: sha256(body), bytes: body.length}]), [
			{name: 'files//etc/passwd', body},
		]);
		await assert.rejects(
			verifyAndExtract({
				packedPath: packagePath(absolute.packed),
				expectedSha256: absolute.sha256,
				destinationDir: destination(),
			}),
			{name: 'ModulePackageMalformedError', message: 'unsafe path in archive: /etc/passwd'},
		);
	});
});

describe('module package build version', () => {
	function packWithBuildVersion(buildVersion) {
		const body = Buffer.from('console.log(1)', 'utf8');
		const files = [{path: 'index.html', sha256: sha256(body), bytes: body.length}];
		const manifestBytes = Buffer.from(
			`${JSON.stringify(
				{
					module: MODULE_NAME,
					build_version: buildVersion,
					release_channel: RELEASE_CHANNEL,
					source_sha: SOURCE_SHA,
					files,
				},
				null,
				'\t',
			)}\n`,
			'utf8',
		);
		return packArchive(manifestBytes, [{name: 'files/index.html', body}]);
	}

	test('a package whose build_version is not a semantic version is rejected at install', async () => {
		assert.throws(() => parseModuleVersion('2026.823.1.4', 'installed module fluxer_renderer build version'), {
			name: 'ModuleVersionMalformedError',
		});
		const forged = packWithBuildVersion('2026.823.1.4');
		const destinationDir = destination();
		await assert.rejects(
			verifyAndExtract({
				packedPath: packagePath(forged.packed),
				expectedSha256: forged.sha256,
				destinationDir,
			}),
			{
				name: 'ModulePackageMalformedError',
				message: 'module.json build_version is not a valid semantic version: 2026.823.1.4',
			},
		);
		assert.deepEqual(readdirSync(destinationDir), []);
	});

	test('a package whose build_version has a leading zero component is rejected at install', async () => {
		assert.throws(() => parseModuleVersion('2026.08.1', 'installed module fluxer_renderer build version'), {
			name: 'ModuleVersionMalformedError',
		});
		const forged = packWithBuildVersion('2026.08.1');
		await assert.rejects(
			verifyAndExtract({
				packedPath: packagePath(forged.packed),
				expectedSha256: forged.sha256,
				destinationDir: destination(),
			}),
			{
				name: 'ModulePackageMalformedError',
				message: 'module.json build_version is not a valid semantic version: 2026.08.1',
			},
		);
	});

	test('a package whose build_version carries a prerelease is rejected at install', async () => {
		const forged = packWithBuildVersion('2026.823.1-rc.1');
		await assert.rejects(
			verifyAndExtract({
				packedPath: packagePath(forged.packed),
				expectedSha256: forged.sha256,
				destinationDir: destination(),
			}),
			{
				name: 'ModulePackageMalformedError',
				message: 'module.json build_version must contain exactly three numeric components: 2026.823.1-rc.1',
			},
		);
	});
});
