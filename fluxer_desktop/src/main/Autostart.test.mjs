// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import {describe, test} from 'node:test';
import {loadTsModule} from './fixtures/TsModuleLoader.mjs';

const autostart = loadTsModule('@electron/main/Autostart', {
	stubs: {
		'node:fs': fs,
		'node:module': {createRequire: () => () => ({})},
		'node:os': os,
		'node:path': path,
		electron: {app: {}, ipcMain: {handle: () => {}}},
		'electron-log': {info: () => {}, warn: () => {}, error: () => {}},
		'@electron/common/BuildChannel': {BUILD_CHANNEL: 'stable'},
		'@electron/common/UserDataPath': {isPortableMode: () => false},
		'@electron/main/LinuxDesktopEntry': {GENERATED_MARKER: '# X-Generated-By=fluxer-desktop'},
		'@electron/main/LinuxLaunchPath': {getStableLinuxLaunchPath: () => '/opt/Fluxer/fluxer'},
		'@electron/main/LinuxSandbox': {isFlatpakRuntime: () => false},
		'@electron/main/MainI18n': {t: (key) => key},
	},
});

const LEGACY = [
	'[Desktop Entry]',
	'Type=Application',
	'Name=Fluxer',
	'Exec="/opt/Fluxer/fluxer" --autostart --ozone-platform=x11',
	'TryExec=/opt/Fluxer/fluxer',
	'StartupWMClass=fluxer',
	'X-GNOME-Autostart-enabled=false',
	'X-GNOME-Autostart-Delay=10',
	'',
	'[Desktop Action other]',
	'StartupWMClass=fluxer',
	'',
].join('\n');

describe('rewriteLegacyLinuxAutostartContents', () => {
	test('keeps user edits and only rewrites the window class when the command still exists', () => {
		const rewritten = autostart.rewriteLegacyLinuxAutostartContents(LEGACY, '/opt/Fluxer/fluxer', false);
		assert.equal(
			rewritten,
			LEGACY.replace('StartupWMClass=fluxer\nX-GNOME', 'StartupWMClass=app.fluxer.FluxerDesktop\nX-GNOME'),
		);
	});

	test('repoints Exec and TryExec when the old command is gone', () => {
		const rewritten = autostart.rewriteLegacyLinuxAutostartContents(LEGACY, '/home/u/Fluxer.AppImage', true);
		const lines = rewritten.split('\n');
		assert.ok(lines.includes('Exec="/home/u/Fluxer.AppImage" --autostart'));
		assert.ok(lines.includes('TryExec=/home/u/Fluxer.AppImage'));
		assert.ok(lines.includes('X-GNOME-Autostart-enabled=false'));
		assert.ok(lines.includes('X-GNOME-Autostart-Delay=10'));
		assert.equal(lines.filter((line) => line === 'StartupWMClass=fluxer').length, 1);
	});
});

describe('Linux autostart with an entry left under the previous desktop id', () => {
	function loadLinuxAutostart() {
		const handlers = new Map();
		const platform = Object.getOwnPropertyDescriptor(process, 'platform');
		Object.defineProperty(process, 'platform', {...platform, value: 'linux'});
		try {
			const module = loadTsModule('@electron/main/Autostart', {
				stubs: {
					'node:fs': fs,
					'node:module': {createRequire: () => () => ({})},
					'node:os': os,
					'node:path': path,
					electron: {app: {}, ipcMain: {handle: (channel, handler) => handlers.set(channel, handler)}},
					'electron-log': {info: () => {}, warn: () => {}, error: () => {}},
					'@electron/common/BuildChannel': {BUILD_CHANNEL: 'stable'},
					'@electron/common/UserDataPath': {isPortableMode: () => false},
					'@electron/main/LinuxDesktopEntry': {GENERATED_MARKER: '# X-Generated-By=fluxer-desktop'},
					'@electron/main/LinuxLaunchPath': {getStableLinuxLaunchPath: () => process.execPath},
					'@electron/main/LinuxSandbox': {isFlatpakRuntime: () => false},
					'@electron/main/MainI18n': {t: (key) => key},
				},
			});
			return {module, handlers};
		} finally {
			Object.defineProperty(process, 'platform', platform);
		}
	}

	test('a generated legacy entry the migration could not move still shows as on and turning it off removes it', async (t) => {
		const root = fs.mkdtempSync(path.join(os.tmpdir(), 'fluxer-autostart-'));
		const previousConfig = process.env.XDG_CONFIG_HOME;
		process.env.XDG_CONFIG_HOME = root;
		const dir = path.join(root, 'autostart');
		t.after(() => {
			fs.rmSync(root, {recursive: true, force: true});
			if (previousConfig === undefined) delete process.env.XDG_CONFIG_HOME;
			else process.env.XDG_CONFIG_HOME = previousConfig;
		});
		fs.mkdirSync(dir, {recursive: true});
		const legacyPath = path.join(dir, 'fluxer.desktop');
		fs.writeFileSync(
			legacyPath,
			[
				'[Desktop Entry]',
				`Exec="${process.execPath}" --autostart`,
				`TryExec=${process.execPath}`,
				'StartupWMClass=fluxer',
				'',
			].join('\n'),
		);
		fs.mkdirSync(path.join(dir, `app.fluxer.FluxerDesktop.desktop.${process.pid}.tmp`, 'blocker'), {recursive: true});
		const {module, handlers} = loadLinuxAutostart();
		module.registerAutostartHandlers();
		await new Promise((resolve) => setTimeout(resolve, 50));
		assert.equal(fs.existsSync(legacyPath), true);
		assert.equal(await handlers.get('autostart-is-enabled')(), true);
		await handlers.get('autostart-disable')();
		assert.equal(fs.existsSync(legacyPath), false);
		assert.equal(await handlers.get('autostart-is-enabled')(), false);
	});

	test('a hand-written legacy entry is left alone', async (t) => {
		const root = fs.mkdtempSync(path.join(os.tmpdir(), 'fluxer-autostart-'));
		const previousConfig = process.env.XDG_CONFIG_HOME;
		process.env.XDG_CONFIG_HOME = root;
		t.after(() => {
			fs.rmSync(root, {recursive: true, force: true});
			if (previousConfig === undefined) delete process.env.XDG_CONFIG_HOME;
			else process.env.XDG_CONFIG_HOME = previousConfig;
		});
		const dir = path.join(root, 'autostart');
		fs.mkdirSync(dir, {recursive: true});
		const legacyPath = path.join(dir, 'fluxer.desktop');
		fs.writeFileSync(legacyPath, ['[Desktop Entry]', `Exec="${process.execPath}" --autostart`, ''].join('\n'));
		const {module, handlers} = loadLinuxAutostart();
		module.registerAutostartHandlers();
		await new Promise((resolve) => setTimeout(resolve, 50));
		assert.equal(await handlers.get('autostart-is-enabled')(), false);
		await handlers.get('autostart-disable')();
		assert.equal(fs.existsSync(legacyPath), true);
	});
});
