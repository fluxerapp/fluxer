// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {readFileSync} from 'node:fs';
import path from 'node:path';
import {describe, test} from 'node:test';
import {fileURLToPath} from 'node:url';

const source = readFileSync(path.join(path.dirname(fileURLToPath(import.meta.url)), 'index.ts'), 'utf8');

describe('main app entry point', () => {
	test('destroying the harvest window during startup never quits the app', () => {
		assert.match(
			source,
			/app\.on\('window-all-closed', \(\) => \{\n\t+if \(startupWindowsPending\) \{\n\t+log\.info\([^\n]*\);\n\t+return;\n\t+\}/,
			'The legacy origin harvest creates and destroys a hidden BrowserWindow before create-window runs. On the offline variant it is the only window in the process, so window-all-closed fires with no main window and no tray yet, and the unguarded quit tears the app down mid startup. The harvest markers then race the store close, so the next launch repeats it.',
		);
	});

	test('the startup window guard clears once create-window returns and when the startup chain rejects', () => {
		assert.match(
			source,
			/try \{\n\t+createWindow\(\{startHidden: isStartMinimizedLaunch\(\)\}\);\n\t+\} finally \{\n\t+startupWindowsPending = false;/,
			'A createWindow that throws halfway would otherwise leave the guard armed and the app could never quit again.',
		);
		assert.match(
			source,
			/\.catch\(\(error: unknown\) => \{\n\t+startupWindowsPending = false;\n\t+log\.error\('\[Startup\] whenReady chain rejected:', error\);/,
		);
	});

	test('the forwarded second instance launch is drained from the bootstrap buffer, never from a listener attached this late', () => {
		assert.doesNotMatch(
			source,
			/app\.on\('second-instance'/,
			'This bundle is only imported once the module update loop has resolved. A listener attached here misses every launch forwarded during the splash, and on the blocked update paths this bundle is never imported at all.',
		);
		assert.match(source, /armSecondInstanceForwarding\(\);\n\t\tsetSecondInstanceSink\(handleSecondInstance\);/);
	});

	test('the single instance lock the bootstrap already holds is reused rather than requested again', () => {
		assert.match(
			source,
			/const gotTheLock = app\.hasSingleInstanceLock\(\) \|\| app\.requestSingleInstanceLock\(\);/,
			'Bootstrap.ts takes the lock before it opens the module store, and this bundle is imported from inside that bootstrap, so a second request here would be asking for a lock this very process already owns.',
		);
	});

	test('open-url is drained from the bootstrap buffer, never from a listener attached this late', () => {
		assert.doesNotMatch(source, /app\.on\('open-url'/);
		assert.doesNotMatch(source, /app\.on\('ready'/);
		assert.match(source, /armOpenUrlForwarding\(\);\n\t\tsetOpenUrlSink\(handleOpenUrl\);/);
	});

	test('pre-ready Chromium setup goes through the shared idempotent helper', () => {
		assert.doesNotMatch(source, /app\.disableHardwareAcceleration\(\)/);
		assert.doesNotMatch(source, /appendSwitch\('autoplay-policy'/);
		assert.match(source, /applyPreReadyChromiumConfiguration\(userDataConfig\.channel, process\.argv\);/);
	});
});
