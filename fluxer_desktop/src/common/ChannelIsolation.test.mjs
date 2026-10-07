// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import fs from 'node:fs';
import {createRequire} from 'node:module';
import path from 'node:path';
import {describe, test} from 'node:test';

const require = createRequire(import.meta.url);
const DESKTOP_DIR = path.resolve(import.meta.dirname, '..', '..');
const CONFIG_PATH = path.join(DESKTOP_DIR, 'electron-builder.config.cjs');
const CHANNELS = ['stable', 'canary', 'development'];

function loadBuilderConfig(channel) {
	const previous = process.env.BUILD_CHANNEL;
	process.env.BUILD_CHANNEL = channel;
	try {
		delete require.cache[CONFIG_PATH];
		return require(CONFIG_PATH);
	} finally {
		delete require.cache[CONFIG_PATH];
		if (previous === undefined) {
			delete process.env.BUILD_CHANNEL;
		} else {
			process.env.BUILD_CHANNEL = previous;
		}
	}
}

const configs = Object.fromEntries(CHANNELS.map((channel) => [channel, loadBuilderConfig(channel)]));

function assertDistinct(label, select) {
	const values = CHANNELS.map((channel) => select(configs[channel]));
	assert.equal(new Set(values).size, values.length, `two channels share a ${label}: ${JSON.stringify(values)}`);
}

describe('installed channels stay apart', () => {
	test('each channel installs under its own bundle id, product name, package name and icon set', () => {
		assertDistinct('bundle id', (config) => config.appId);
		assertDistinct('product name', (config) => config.productName);
		assertDistinct('package name', (config) => config.extraMetadata.name);
		assertDistinct('mac icon', (config) => config.mac.icon);
		assertDistinct('protocol registration name', (config) => config.protocols[0].name);
	});

	test('the development app registers its own deep link scheme instead of fluxer://', () => {
		const schemes = configs.development.protocols.flatMap((protocol) => protocol.schemes);
		assert.deepEqual(schemes, ['fluxer-development']);
		assert.equal(configs.development.linux.desktop.entry.MimeType, 'x-scheme-handler/fluxer-development;');
		for (const channel of ['stable', 'canary']) {
			assert.deepEqual(
				configs[channel].protocols.flatMap((protocol) => protocol.schemes),
				['fluxer'],
			);
		}
	});

	test('the development app signs without notarization or a provisioning profile it does not have', () => {
		const mac = configs.development.mac;
		assert.equal(mac.notarize, false);
		assert.equal(mac.sign.provisioningProfile, undefined);
		const entitlements = fs.readFileSync(path.join(DESKTOP_DIR, mac.sign.entitlements), 'utf8');
		assert.doesNotMatch(entitlements, /com\.apple\.developer\.|com\.apple\.application-identifier/u);
		assert.ok(fs.existsSync(path.join(DESKTOP_DIR, mac.icon)), `missing ${mac.icon}`);
	});
});

describe('running channels stay apart', () => {
	test('each channel keeps its own profile directory, so locks, logs, stores and caches never collide', () => {
		const source = fs.readFileSync(path.join(DESKTOP_DIR, 'src/common/UserDataPath.ts'), 'utf8');
		const map = /channelStorageDirectoryMap: ChannelStorageDirectoryMap = \{([^}]+)\}/u.exec(source)?.[1] ?? '';
		const directories = CHANNELS.map((channel) => new RegExp(`${channel}: '([^']+)'`, 'u').exec(map)?.[1]);
		assert.deepEqual(directories, ['fluxer', 'fluxercanary', 'fluxerdevelopment']);
	});

	test('only a packaged app claims its deep link scheme, so a dev Electron run never takes it from an installed app', () => {
		const source = fs.readFileSync(path.join(DESKTOP_DIR, 'src/main/DeepLinks.ts'), 'utf8');
		assert.doesNotMatch(source, /setAsDefaultProtocolClient\(['"]/u);
		assert.doesNotMatch(source, /process\.defaultApp/u);
		assert.match(source, /if \(app\.isPackaged\) \{\n\t\tapp\.setAsDefaultProtocolClient\(APP_PROTOCOL\);/u);
	});
});
