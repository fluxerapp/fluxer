// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import fs from 'node:fs';
import {createRequire} from 'node:module';
import os from 'node:os';
import path from 'node:path';
import {after, describe, test} from 'node:test';
import {fileURLToPath} from 'node:url';
import vm from 'node:vm';

const require = createRequire(import.meta.url);
const esbuild = require('esbuild');

const sourcePath = fileURLToPath(new URL('./ThemeLinkedFileWatch.ts', import.meta.url));
const source = fs.readFileSync(sourcePath, 'utf8');
const transformedSource = esbuild.transformSync(source, {
	loader: 'ts',
	format: 'cjs',
	platform: 'node',
	target: 'node20',
}).code;

function loadThemeLinkedFileWatch() {
	const module = {exports: {}};
	const context = vm.createContext({
		module,
		exports: module.exports,
		require,
	});
	vm.runInContext(transformedSource, context, {filename: sourcePath});
	return module.exports;
}

const {readThemeCssFile} = loadThemeLinkedFileWatch();
const tempRoot = fs.mkdtempSync(path.join(os.tmpdir(), 'fluxer-theme-linked-'));
let fileCounter = 0;

after(() => {
	fs.rmSync(tempRoot, {recursive: true, force: true});
});

function nextFilePath() {
	fileCounter += 1;
	return path.join(tempRoot, `theme-${fileCounter}.css`);
}

describe('ThemeLinkedFileWatch', () => {
	test('rejects oversize files', async () => {
		const filePath = nextFilePath();
		fs.writeFileSync(filePath, 'a'.repeat(1024 * 1024 + 1));
		const change = await readThemeCssFile(filePath);
		assert.equal(change.css, undefined);
		assert.equal(change.error, 'too_large');
	});

	test('refuses to follow a symlink', async () => {
		const targetPath = nextFilePath();
		const linkPath = nextFilePath();
		fs.writeFileSync(targetPath, 'secret');
		fs.symlinkSync(targetPath, linkPath);
		const change = await readThemeCssFile(linkPath);
		assert.equal(change.css, undefined);
		assert.equal(change.error, 'not_file');
	});
});
