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

const {createThemeLinkedFileWatchSet, readThemeCssFile} = loadThemeLinkedFileWatch();
const tempRoot = fs.mkdtempSync(path.join(os.tmpdir(), 'fluxer-theme-linked-'));
let fileCounter = 0;

after(() => {
	fs.rmSync(tempRoot, {recursive: true, force: true});
});

function nextFilePath() {
	fileCounter += 1;
	return path.join(tempRoot, `theme-${fileCounter}.css`);
}

function createRecorder() {
	const changes = [];
	const waiters = [];
	const emit = (change) => {
		changes.push(change);
		for (const waiter of [...waiters]) {
			if (waiter.predicate(change)) {
				waiters.splice(waiters.indexOf(waiter), 1);
				clearTimeout(waiter.timer);
				waiter.resolve(change);
			}
		}
	};
	const waitFor = (predicate, timeoutMs = 3000) =>
		new Promise((resolve, reject) => {
			const waiter = {predicate, resolve, timer: null};
			waiter.timer = setTimeout(() => {
				waiters.splice(waiters.indexOf(waiter), 1);
				reject(new Error(`Timed out waiting for change, saw ${JSON.stringify(changes)}`));
			}, timeoutMs);
			waiters.push(waiter);
		});
	return {changes, emit, waitFor};
}

function delay(ms) {
	return new Promise((resolve) => setTimeout(resolve, ms));
}

async function writeLater(filePath, contents) {
	await delay(40);
	fs.writeFileSync(filePath, contents);
}

describe('ThemeLinkedFileWatch', () => {
	test('emits css on the first read', async () => {
		const filePath = nextFilePath();
		fs.writeFileSync(filePath, '.a{color:red}');
		const recorder = createRecorder();
		const watchSet = createThemeLinkedFileWatchSet(recorder.emit, 25);
		try {
			watchSet.replace([filePath]);
			const change = await recorder.waitFor((item) => item.path === filePath);
			assert.equal(change.css, '.a{color:red}');
		} finally {
			watchSet.dispose();
		}
	});

	test('emits new css after an in-place write', async () => {
		const filePath = nextFilePath();
		fs.writeFileSync(filePath, '.a{color:red}');
		const recorder = createRecorder();
		const watchSet = createThemeLinkedFileWatchSet(recorder.emit, 25);
		try {
			watchSet.replace([filePath]);
			await recorder.waitFor((item) => item.css === '.a{color:red}');
			await writeLater(filePath, '.a{color:blue;padding:1px}');
			await recorder.waitFor((item) => item.css === '.a{color:blue;padding:1px}');
		} finally {
			watchSet.dispose();
		}
	});

	test('emits new css after a temp file and rename save', async () => {
		const filePath = nextFilePath();
		fs.writeFileSync(filePath, '.a{color:red}');
		const recorder = createRecorder();
		const watchSet = createThemeLinkedFileWatchSet(recorder.emit, 25);
		try {
			watchSet.replace([filePath]);
			await recorder.waitFor((item) => item.css === '.a{color:red}');
			await delay(40);
			const tempPath = `${filePath}.tmp`;
			fs.writeFileSync(tempPath, '.a{color:green}');
			fs.renameSync(tempPath, filePath);
			await recorder.waitFor((item) => item.css === '.a{color:green}');
		} finally {
			watchSet.dispose();
		}
	});

	test('reports missing after unlink and css after the file returns', async () => {
		const filePath = nextFilePath();
		fs.writeFileSync(filePath, '.a{color:red}');
		const recorder = createRecorder();
		const watchSet = createThemeLinkedFileWatchSet(recorder.emit, 25);
		try {
			watchSet.replace([filePath]);
			await recorder.waitFor((item) => item.css === '.a{color:red}');
			await delay(40);
			fs.unlinkSync(filePath);
			await recorder.waitFor((item) => item.error === 'missing');
			await writeLater(filePath, '.a{color:purple}');
			await recorder.waitFor((item) => item.css === '.a{color:purple}');
		} finally {
			watchSet.dispose();
		}
	});

	test('strips a leading byte order mark', async () => {
		const filePath = nextFilePath();
		fs.writeFileSync(filePath, '﻿.a{color:red}');
		const change = await readThemeCssFile(filePath);
		assert.equal(change.css, '.a{color:red}');
	});

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

	test('reports missing for an absent file', async () => {
		const change = await readThemeCssFile(nextFilePath());
		assert.equal(change.error, 'missing');
	});

	test('stops emitting for a path dropped by replace', async () => {
		const keptPath = nextFilePath();
		const droppedPath = nextFilePath();
		fs.writeFileSync(keptPath, '.kept{}');
		fs.writeFileSync(droppedPath, '.dropped{}');
		const recorder = createRecorder();
		const watchSet = createThemeLinkedFileWatchSet(recorder.emit, 25);
		try {
			const initialReads = Promise.all([
				recorder.waitFor((item) => item.path === droppedPath),
				recorder.waitFor((item) => item.path === keptPath),
			]);
			watchSet.replace([keptPath, droppedPath]);
			await initialReads;
			watchSet.replace([keptPath]);
			await delay(40);
			fs.writeFileSync(droppedPath, '.dropped{color:red}');
			fs.writeFileSync(keptPath, '.kept{color:red}');
			await recorder.waitFor((item) => item.css === '.kept{color:red}');
			await delay(150);
			assert.equal(
				recorder.changes.some((item) => item.css === '.dropped{color:red}'),
				false,
			);
		} finally {
			watchSet.dispose();
		}
	});

	test('dispose stops every event', async () => {
		const filePath = nextFilePath();
		fs.writeFileSync(filePath, '.a{color:red}');
		const recorder = createRecorder();
		const watchSet = createThemeLinkedFileWatchSet(recorder.emit, 25);
		watchSet.replace([filePath]);
		await recorder.waitFor((item) => item.css === '.a{color:red}');
		watchSet.dispose();
		const count = recorder.changes.length;
		await delay(40);
		fs.writeFileSync(filePath, '.a{color:blue}');
		await delay(200);
		assert.equal(recorder.changes.length, count);
	});
});
