// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {readFileSync} from 'node:fs';
import {createRequire} from 'node:module';
import {describe, test} from 'node:test';
import {fileURLToPath} from 'node:url';
import vm from 'node:vm';

const require = createRequire(import.meta.url);
const esbuild = require('esbuild');

const sourcePath = fileURLToPath(new URL('./DesktopUpdatePrompt.ts', import.meta.url));
const transformedSource = esbuild.transformSync(readFileSync(sourcePath, 'utf8'), {
	loader: 'ts',
	format: 'cjs',
	platform: 'node',
	target: 'node20',
}).code;

const DOWNLOAD_PAGE_URL = 'https://fluxer.app/download';
const FLATPAK_APP_ID = 'app.fluxer.FluxerDesktop';
const NEWER_SHELL = '2026.1013.15209';

function loadPrompt({plan, flatpakAppId = null, response = 0}) {
	const dialogs = [];
	const opened = [];
	const logger = {info: () => {}, warn: () => {}, error: () => {}};
	function requireStub(specifier) {
		switch (specifier) {
			case '@electron/common/DesktopIdentity':
				return {DESKTOP_APP_NAME: 'Fluxer'};
			case '@electron/common/ErrorMessage':
				return {errorMessage: (error) => String(error)};
			case '@electron/common/Logger':
				return {createChildLogger: () => logger};
			case '@electron/main/DesktopUpdateGate':
				return {
					checkDesktopUpdateNow: async () => ({available: false}),
					desktopUpdateReplacesShell: () => false,
					getDesktopUpdateState: () => ({available: false}),
					startDesktopUpdate: () => false,
				};
			case '@electron/main/LinuxSandbox':
				return {getFlatpakAppId: () => flatpakAppId, isFlatpakRuntime: () => flatpakAppId != null};
			case '@electron/main/MainI18n':
				return {t: (key) => key};
			case '@electron/main/OpenExternal':
				return {
					openExternalDeduped: async (url) => {
						opened.push(url);
					},
				};
			case '@electron/main/ShellUpdateCapability':
				return {
					resolveShellUpdatePlan: () => plan,
					ShellUpdateCapability: {
						SELF_UPDATE: 'self-update',
						MANUAL_DOWNLOAD: 'manual-download',
						MANAGED_PACKAGE: 'managed-package',
					},
				};
			case '@electron/main/Updater':
				return {findNewerManualShell: async () => ({version: NEWER_SHELL})};
			case '@electron/main/UpdaterDownloads':
				return {DOWNLOAD_PAGE_URL};
			case 'electron':
				return {
					app: {getVersion: () => '2026.1006.171735'},
					BrowserWindow: {getFocusedWindow: () => null},
					dialog: {
						showMessageBox: async (options) => {
							dialogs.push(options);
							return {response};
						},
					},
				};
			default:
				throw new Error(`Unexpected import: ${specifier}`);
		}
	}
	const module = {exports: {}};
	vm.runInContext(
		transformedSource,
		vm.createContext({require: requireStub, module, exports: module.exports, console}),
		{filename: sourcePath},
	);
	return {prompt: module.exports, dialogs, opened};
}

describe('the native update prompt for a shell that cannot update itself', () => {
	test('a Flatpak with a newer shell offers the software center', async () => {
		const {prompt, dialogs, opened} = loadPrompt({
			plan: {capability: 'managed-package'},
			flatpakAppId: FLATPAK_APP_ID,
		});

		await prompt.checkForUpdatesFromShell();

		assert.equal(dialogs.length, 1);
		assert.equal(dialogs[0].message, 'desktop.update.manualMessage');
		assert.equal(dialogs[0].detail, 'desktop.update.flatpakDetail');
		assert.deepEqual([...dialogs[0].buttons], ['desktop.update.openSoftwareCenter', 'desktop.update.later']);
		assert.deepEqual(opened, [`appstream://${FLATPAK_APP_ID}`]);
	});

	test('a deb install with a newer shell offers the download page', async () => {
		const {prompt, dialogs, opened} = loadPrompt({plan: {capability: 'managed-package'}});

		await prompt.checkForUpdatesFromShell();

		assert.equal(dialogs[0].detail, 'desktop.update.managedDetail');
		assert.deepEqual([...dialogs[0].buttons], ['desktop.update.download', 'desktop.update.later']);
		assert.deepEqual(opened, [DOWNLOAD_PAGE_URL]);
	});

	test('a portable build keeps the plain download wording', async () => {
		const {prompt, dialogs, opened} = loadPrompt({plan: {capability: 'manual-download', reason: 'platform'}});

		await prompt.checkForUpdatesFromShell();

		assert.equal(dialogs[0].detail, 'desktop.update.manualDetail');
		assert.deepEqual(opened, [DOWNLOAD_PAGE_URL]);
	});

	test('choosing later opens nothing', async () => {
		const {prompt, opened} = loadPrompt({
			plan: {capability: 'managed-package'},
			flatpakAppId: FLATPAK_APP_ID,
			response: 1,
		});

		await prompt.checkForUpdatesFromShell();

		assert.deepEqual(opened, []);
	});
});
