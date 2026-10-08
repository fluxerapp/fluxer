// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {readFileSync} from 'node:fs';
import {createRequire} from 'node:module';
import {describe, test} from 'node:test';
import {fileURLToPath} from 'node:url';
import vm from 'node:vm';

const require = createRequire(import.meta.url);
const esbuild = require('esbuild');

const sourcePath = fileURLToPath(new URL('./OpenExternal.ts', import.meta.url));
const source = readFileSync(sourcePath, 'utf8');
const transformedSource = esbuild.transformSync(source, {
	loader: 'ts',
	format: 'cjs',
	platform: 'node',
	target: 'node20',
}).code;

function loadOpenExternal() {
	const openExternalCalls = [];
	const module = {exports: {}};
	const context = vm.createContext({
		module,
		exports: module.exports,
		console,
		Date,
		Error,
		Map,
		Set,
		URL,
		require: (specifier) => {
			if (specifier === '@electron/common/Constants') {
				return {APP_PROTOCOL: 'fluxer'};
			}
			if (specifier === 'electron') {
				return {
					shell: {
						openExternal: async (url) => {
							openExternalCalls.push(url);
						},
					},
				};
			}
			return require(specifier);
		},
	});
	vm.runInContext(transformedSource, context, {filename: sourcePath});
	return {...module.exports, openExternalCalls};
}

describe('OpenExternal URL validation', () => {
	test('allows standard and custom external protocols', () => {
		const {shouldOpenExternalUrl} = loadOpenExternal();

		assert.equal(shouldOpenExternalUrl('https://fluxer.app'), true);
		assert.equal(shouldOpenExternalUrl('http://fluxer.app'), true);
		assert.equal(shouldOpenExternalUrl('mailto:support@fluxer.com'), true);
		assert.equal(shouldOpenExternalUrl('tel:+15551234567'), true);
		assert.equal(shouldOpenExternalUrl('fluxer://invite/test'), true);
	});

	test('blocks dangerous or malformed external URLs', () => {
		const {shouldOpenExternalUrl} = loadOpenExternal();

		assert.equal(shouldOpenExternalUrl('file:///etc/passwd'), false);
		assert.equal(shouldOpenExternalUrl('javascript:alert(1)'), false);
		assert.equal(shouldOpenExternalUrl('data:text/html,hello'), false);
		assert.equal(shouldOpenExternalUrl('about:blank'), false);
		assert.equal(shouldOpenExternalUrl('chrome://settings'), false);
		assert.equal(shouldOpenExternalUrl('/relative/path'), false);
		assert.equal(shouldOpenExternalUrl('not a url'), false);
		assert.equal(shouldOpenExternalUrl('http://[::1'), false);
	});

	test('blocks schemes outside the allowlist (allowlist hardening)', () => {
		const {shouldOpenExternalUrl} = loadOpenExternal();

		assert.equal(shouldOpenExternalUrl('smb://server/share'), false);
		assert.equal(shouldOpenExternalUrl('search-ms:query=secret'), false);
		assert.equal(shouldOpenExternalUrl('ms-officecmd:foo'), false);
		assert.equal(shouldOpenExternalUrl('vbscript:msgbox(1)'), false);
		assert.equal(shouldOpenExternalUrl('ftp://example.com/x'), false);
	});

	test('opens normalized URLs once inside the dedupe window', async () => {
		const {openExternalDeduped, openExternalCalls} = loadOpenExternal();

		await openExternalDeduped('HTTPS://Fluxer.App/path');
		await openExternalDeduped('https://fluxer.app/path');

		assert.deepEqual(openExternalCalls, ['https://fluxer.app/path']);
	});

	test('rejects blocked URLs before calling Electron shell', async () => {
		const {openExternalDeduped, openExternalCalls} = loadOpenExternal();

		await assert.rejects(openExternalDeduped('javascript:alert(1)'), /External URL open request blocked/);
		assert.deepEqual(openExternalCalls, []);
	});
});
