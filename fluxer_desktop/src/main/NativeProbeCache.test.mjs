// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {existsSync, mkdtempSync, readFileSync, writeFileSync} from 'node:fs';
import {rm} from 'node:fs/promises';
import {createRequire, registerHooks} from 'node:module';
import os from 'node:os';
import path from 'node:path';
import {after, afterEach, describe, test} from 'node:test';

const DESKTOP_SRC = new URL('../', import.meta.url);

registerHooks({
	resolve(specifier, context, nextResolve) {
		if (specifier.startsWith('@electron/')) {
			return {shortCircuit: true, url: new URL(`${specifier.slice('@electron/'.length)}.ts`, DESKTOP_SRC).href};
		}
		return nextResolve(specifier, context);
	},
});

const require = createRequire(import.meta.url);
const {loadNativeBinding, probeNativeBinary} = require('../../native/app-store/loader-diagnostics.cjs');
const {NATIVE_PROBE_CACHE_ENV, NATIVE_PROBE_CACHE_FILENAME, armNativeProbeCache} = await import(
	'./NativeProbeCache.ts'
);

const SKIP_ENV = 'FLUXER_TEST_SKIP_NATIVE_PROBE';
const roots = [];
const savedCacheEnv = process.env[NATIVE_PROBE_CACHE_ENV];

after(async () => {
	for (const root of roots) {
		await rm(root, {recursive: true, force: true});
	}
});

afterEach(() => {
	if (savedCacheEnv === undefined) {
		delete process.env[NATIVE_PROBE_CACHE_ENV];
	} else {
		process.env[NATIVE_PROBE_CACHE_ENV] = savedCacheEnv;
	}
});

function fixture({exitCode = null} = {}) {
	const root = mkdtempSync(path.join(os.tmpdir(), 'native-probe-cache-'));
	roots.push(root);
	const log = path.join(root, 'loads.log');
	const nativePath = path.join(root, 'binding.cjs');
	const exit = exitCode == null ? '' : `if (process.env.${SKIP_ENV} === '1') process.exit(${exitCode});`;
	writeFileSync(
		nativePath,
		`require('node:fs').appendFileSync(${JSON.stringify(log)}, 'load\\n');${exit}module.exports = {ok: true};\n`,
	);
	const cacheFile = path.join(root, 'userData', NATIVE_PROBE_CACHE_FILENAME);
	process.env[NATIVE_PROBE_CACHE_ENV] = cacheFile;
	return {root, log, nativePath, cacheFile};
}

function loads(log) {
	return existsSync(log) ? readFileSync(log, 'utf8').split('\n').filter(Boolean).length : 0;
}

function probe(nativePath) {
	return probeNativeBinary({
		moduleName: '@fluxer/test-binding',
		nativePath,
		nativeRoot: path.dirname(nativePath),
		packageDir: path.dirname(nativePath),
		skipNativeProbeEnv: SKIP_ENV,
	});
}

describe('native module probe cache', () => {
	test('a binary that passed its probe is not probed again on the next boot', () => {
		const {log, nativePath, cacheFile} = fixture();

		assert.equal(probe(nativePath), null);
		assert.equal(loads(log), 1);
		assert.equal(existsSync(cacheFile), true);

		assert.equal(probe(nativePath), null);
		assert.equal(loads(log), 1);
	});

	test('a replaced binary is probed again', () => {
		const {log, nativePath} = fixture();
		assert.equal(probe(nativePath), null);
		writeFileSync(nativePath, `${readFileSync(nativePath, 'utf8')}module.exports.changed = true;\n`);

		assert.equal(probe(nativePath), null);
		assert.equal(loads(log), 2);
	});

	test('a binary that crashes its probe is never cached as safe', () => {
		const {log, nativePath, cacheFile} = fixture({exitCode: 3});

		assert.notEqual(probe(nativePath), null);
		assert.notEqual(probe(nativePath), null);
		assert.equal(loads(log), 2);
		assert.equal(existsSync(cacheFile), false);
	});

	test('a binary that loaded in process counts as probed, so the next boot skips the child', () => {
		const {log, nativePath} = fixture();
		const loaded = loadNativeBinding({
			moduleName: '@fluxer/test-binding',
			nativePath,
			nativeRoot: path.dirname(nativePath),
			packageDir: path.dirname(nativePath),
			skipNativeProbeEnv: SKIP_ENV,
			probe: false,
		});
		assert.equal(loaded.loadError, null);
		assert.equal(loads(log), 1);

		assert.equal(probe(nativePath), null);
		assert.equal(loads(log), 1);
	});

	test('without a cache file the probe still runs every time', () => {
		const {log, nativePath} = fixture();
		delete process.env[NATIVE_PROBE_CACHE_ENV];

		probe(nativePath);
		probe(nativePath);
		assert.equal(loads(log), 2);
	});

	test('arming points the loaders at the user data directory and keeps an explicit override', () => {
		const env = {};
		armNativeProbeCache('/userData', env);
		assert.equal(env[NATIVE_PROBE_CACHE_ENV], path.join('/userData', NATIVE_PROBE_CACHE_FILENAME));
		armNativeProbeCache('/elsewhere', env);
		assert.equal(env[NATIVE_PROBE_CACHE_ENV], path.join('/userData', NATIVE_PROBE_CACHE_FILENAME));
	});
});
