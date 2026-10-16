// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import fs from 'node:fs';
import {createRequire} from 'node:module';
import os from 'node:os';
import path from 'node:path';
import {after, describe, test} from 'node:test';

const require = createRequire(import.meta.url);
const DESKTOP_DIR = path.resolve(import.meta.dirname, '..', '..');
const PREFLIGHT_MODULES = path.join('src', 'main', 'NativeModulePreflightModules.json');
const temporaryRoots = [];

after(() => {
	for (const root of temporaryRoots) {
		fs.rmSync(root, {recursive: true, force: true});
	}
});

function loadMetadataVersion({version, rendererVersionJson}) {
	const root = fs.mkdtempSync(path.join(os.tmpdir(), 'builder-shell-version-'));
	temporaryRoots.push(root);
	fs.copyFileSync(
		path.join(DESKTOP_DIR, 'electron-builder.config.cjs'),
		path.join(root, 'electron-builder.config.cjs'),
	);
	fs.mkdirSync(path.join(root, 'src', 'main'), {recursive: true});
	fs.copyFileSync(path.join(DESKTOP_DIR, PREFLIGHT_MODULES), path.join(root, PREFLIGHT_MODULES));
	if (rendererVersionJson != null) {
		fs.mkdirSync(path.join(root, 'dist', 'renderer'), {recursive: true});
		fs.writeFileSync(path.join(root, 'dist', 'renderer', 'version.json'), rendererVersionJson);
	}
	const previous = process.env.VERSION;
	if (version == null) {
		delete process.env.VERSION;
	} else {
		process.env.VERSION = version;
	}
	try {
		return require(path.join(root, 'electron-builder.config.cjs')).extraMetadata.version;
	} finally {
		if (previous === undefined) {
			delete process.env.VERSION;
		} else {
			process.env.VERSION = previous;
		}
	}
}

describe('the packaged shell version', () => {
	test('comes from VERSION whenever it is set', () => {
		assert.equal(
			loadMetadataVersion({version: '2026.1014.2', rendererVersionJson: '{"version":"2026.1014.9"}'}),
			'2026.1014.2',
		);
	});

	test('falls back to the bundled renderer version when VERSION is unset', () => {
		assert.equal(loadMetadataVersion({rendererVersionJson: '{"version":"2026.1014.9"}'}), '2026.1014.9');
	});

	test('keeps the package.json version when the renderer has no numeric version', () => {
		assert.equal(loadMetadataVersion({rendererVersionJson: '{"version":"dev"}'}), undefined);
		assert.equal(loadMetadataVersion({rendererVersionJson: 'null'}), undefined);
		assert.equal(loadMetadataVersion({}), undefined);
	});
});
