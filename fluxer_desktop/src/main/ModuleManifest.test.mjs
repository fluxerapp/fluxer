// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {Buffer} from 'node:buffer';
import {registerHooks} from 'node:module';
import {describe, test} from 'node:test';

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

const {parseModuleUpdateManifest} = await import('./ModuleManifest.ts');

const SHELL_VERSION = '2026.823.1';
const RELEASE_CHANNEL = 'canary';
const PLATFORM = 'darwin';
const ARCH = 'arm64';
const RENDERER_SHA = 'a'.repeat(64);
const EXPECTED_TARGET = {releaseChannel: RELEASE_CHANNEL, platform: PLATFORM, arch: ARCH};

function manifestBytes(overrides = {}) {
	return Buffer.from(
		JSON.stringify({
			manifest_version: 1,
			release_channel: RELEASE_CHANNEL,
			platform: PLATFORM,
			arch: ARCH,
			build_version: SHELL_VERSION,
			pub_date: '2026-08-23T00:00:00.000Z',
			metadata_version: 4,
			shell: {latest_version: SHELL_VERSION, minimum_version: SHELL_VERSION},
			modules: {
				fluxer_renderer: {sha256: RENDERER_SHA, bytes: 1024, url: `https://api.invalid/dl/desktop/${RENDERER_SHA}.br`},
			},
			required_modules: ['fluxer_renderer'],
			...overrides,
		}),
		'utf8',
	);
}

describe('parseModuleUpdateManifest', () => {
	test('accepts a manifest cut for this shell channel, platform and arch', () => {
		const manifest = parseModuleUpdateManifest(manifestBytes(), EXPECTED_TARGET);

		assert.equal(manifest.releaseChannel, RELEASE_CHANNEL);
		assert.equal(manifest.platform, PLATFORM);
		assert.equal(manifest.arch, ARCH);
		assert.equal(manifest.metadataVersion, 4);
		assert.deepEqual(manifest.requiredModules, ['fluxer_renderer']);
		assert.equal(manifest.modules.fluxer_renderer.sha256, RENDERER_SHA);
	});

	test('refuses a manifest cut for another channel, another platform or another arch', () => {
		for (const overrides of [{release_channel: 'stable'}, {platform: 'win32'}, {arch: 'x64'}]) {
			assert.throws(() => parseModuleUpdateManifest(manifestBytes(overrides), EXPECTED_TARGET), {
				name: 'ModuleManifestMalformedError',
				message: /manifest targets/u,
			});
		}
	});

	test('refuses required_modules naming an Object.prototype property', () => {
		assert.throws(
			() =>
				parseModuleUpdateManifest(manifestBytes({required_modules: ['fluxer_renderer', 'toString']}), EXPECTED_TARGET),
			{
				name: 'ModuleManifestMalformedError',
				message: /required_modules names an undeclared module: toString/u,
			},
		);
	});

	test('refuses linux_security_minimum.required_modules naming an Object.prototype property', () => {
		const bytes = manifestBytes({
			platform: 'linux',
			linux_security_minimum: {version: SHELL_VERSION, required_modules: ['constructor']},
		});

		assert.throws(() => parseModuleUpdateManifest(bytes, {...EXPECTED_TARGET, platform: 'linux'}), {
			name: 'ModuleManifestMalformedError',
			message: /linux_security_minimum\.required_modules names an undeclared module: constructor/u,
		});
	});
});
