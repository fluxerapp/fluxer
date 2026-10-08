// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import {afterEach, beforeEach, describe, test} from 'node:test';
import {loadTsModule} from './fixtures/TsModuleLoader.mjs';

const EXEC_PATH = '/opt/Fluxer/fluxer';
const DESKTOP_NAME = 'app.fluxer.FluxerDesktop.desktop';
const protocolRegistrations = [];
const inProcessRegistrations = [];
const childProcessCalls = [];
const loggedWarnings = [];
let registeredAsDefault = false;
const electronApp = {
	isPackaged: true,
	isDefaultProtocolClient: () => registeredAsDefault,
	setAsDefaultProtocolClient: (protocol) => inProcessRegistrations.push(protocol),
};
const childProcess = {
	execFile: (file, args, options, callback) => {
		childProcessCalls.push({file, args, options, callback});
		if (file === 'xdg-mime' && args[0] === 'default' && args[2].startsWith('x-scheme-handler/')) {
			protocolRegistrations.push(args[2].slice('x-scheme-handler/'.length));
		}
	},
};
const noopLogger = {debug: () => {}, info: () => {}, warn: (message) => loggedWarnings.push(message), error: () => {}};
const englishStrings = {
	'desktop.jumpList.openSettings': 'Open Settings',
	'desktop.jumpList.newDirectMessage': 'New Direct Message',
	'desktop.linuxEntry.genericName': 'Instant Messenger',
	'desktop.linuxEntry.comment': 'Instant messaging and VoIP',
};

const desktopEntry = loadTsModule('@electron/main/LinuxDesktopEntry', {
	stubs: {
		'node:child_process': childProcess,
		'node:fs': fs,
		'node:os': os,
		'node:path': path,
		electron: {app: electronApp},
		'@electron/common/BuildChannel': {BUILD_CHANNEL: 'stable'},
		'@electron/common/Logger': {createChildLogger: () => noopLogger},
		'@electron/main/JumpList': {TASK_ARG_PREFIX: '--fluxer-task='},
		'@electron/main/LinuxLaunchPath': {getStableLinuxLaunchPath: () => EXEC_PATH},
		'@electron/main/LinuxSandbox': {isFlatpakRuntime: () => false},
		'@electron/main/MainI18n': {getNativeLocale: () => 'en-US', t: (key) => englishStrings[key] ?? key},
	},
});

function parseMainGroup(contents) {
	const entries = new Map();
	for (const line of contents.split('\n')) {
		if (line.startsWith('[Desktop Action ')) break;
		const index = line.indexOf('=');
		if (index > 0 && !line.startsWith('#')) entries.set(line.slice(0, index), line.slice(index + 1));
	}
	return entries;
}

describe('LinuxDesktopEntry contents', () => {
	test('uses the reverse-DNS desktop id for window matching and keeps the packaged icon name', () => {
		const entry = parseMainGroup(desktopEntry.buildDesktopFileContents(EXEC_PATH, false));
		assert.equal(entry.get('StartupWMClass'), 'app.fluxer.FluxerDesktop');
		assert.equal(entry.get('Icon'), 'fluxer');
		assert.equal(entry.get('Exec'), `"${EXEC_PATH}" %U`);
		assert.equal(entry.get('TryExec'), EXEC_PATH);
		assert.equal(entry.get('MimeType'), 'x-scheme-handler/fluxer;');
		assert.equal(entry.has('Hidden'), false);
		assert.equal(entry.has('NoDisplay'), false);
	});

	test('a third-party launcher keeps ours out of menus without hiding it from lookups', () => {
		const contents = desktopEntry.buildDesktopFileContents(EXEC_PATH, true);
		const entry = parseMainGroup(contents);
		assert.equal(entry.get('NoDisplay'), 'true');
		assert.equal(entry.has('Hidden'), false);
		assert.ok(contents.includes(desktopEntry.GENERATED_MARKER));
	});
});

describe('ensureLinuxDesktopEntry', () => {
	let root;
	let previous;

	beforeEach(() => {
		root = fs.mkdtempSync(path.join(os.tmpdir(), 'fluxer-desktop-entry-'));
		previous = {
			platform: Object.getOwnPropertyDescriptor(process, 'platform'),
			resourcesPath: process.resourcesPath,
			dataHome: process.env.XDG_DATA_HOME,
			dataDirs: process.env.XDG_DATA_DIRS,
			disable: process.env.FLUXER_DISABLE_DESKTOP_FILE,
			desktopName: process.env.CHROME_DESKTOP,
		};
		Object.defineProperty(process, 'platform', {value: 'linux'});
		process.resourcesPath = path.join(root, 'resources');
		process.env.XDG_DATA_HOME = path.join(root, 'home');
		process.env.XDG_DATA_DIRS = path.join(root, 'system');
		delete process.env.FLUXER_DISABLE_DESKTOP_FILE;
		process.env.CHROME_DESKTOP = DESKTOP_NAME;
		protocolRegistrations.length = 0;
		inProcessRegistrations.length = 0;
		childProcessCalls.length = 0;
		loggedWarnings.length = 0;
		registeredAsDefault = false;
		electronApp.isPackaged = true;
	});

	afterEach(() => {
		Object.defineProperty(process, 'platform', previous.platform);
		process.resourcesPath = previous.resourcesPath;
		for (const [key, value] of [
			['XDG_DATA_HOME', previous.dataHome],
			['XDG_DATA_DIRS', previous.dataDirs],
			['FLUXER_DISABLE_DESKTOP_FILE', previous.disable],
			['CHROME_DESKTOP', previous.desktopName],
		]) {
			if (value === undefined) delete process.env[key];
			else process.env[key] = value;
		}
		fs.rmSync(root, {recursive: true, force: true});
	});

	const userDir = () => path.join(root, 'home', 'applications');
	const systemDir = () => path.join(root, 'system', 'applications');

	test('writes the new entry and removes only the generated legacy entry', () => {
		fs.mkdirSync(userDir(), {recursive: true});
		fs.writeFileSync(path.join(userDir(), 'fluxer.desktop'), `[Desktop Entry]\n${desktopEntry.GENERATED_MARKER}\n`);
		fs.writeFileSync(
			path.join(userDir(), 'fluxer-canary.desktop'),
			`[Desktop Entry]\n${desktopEntry.GENERATED_MARKER}\n`,
		);
		assert.equal(desktopEntry.ensureLinuxDesktopEntry(), true);
		assert.equal(fs.existsSync(path.join(userDir(), 'fluxer.desktop')), false);
		assert.equal(fs.existsSync(path.join(userDir(), 'fluxer-canary.desktop')), true);
		const written = fs.readFileSync(path.join(userDir(), 'app.fluxer.FluxerDesktop.desktop'), 'utf8');
		assert.equal(written, desktopEntry.buildDesktopFileContents(EXEC_PATH, false));
		assert.deepEqual(
			fs.readdirSync(userDir()).filter((name) => name.endsWith('.tmp')),
			[],
		);
		assert.deepEqual(protocolRegistrations, ['fluxer']);
	});

	test('leaves a hand-written legacy entry alone', () => {
		fs.mkdirSync(userDir(), {recursive: true});
		fs.writeFileSync(path.join(userDir(), 'fluxer.desktop'), '[Desktop Entry]\nExec=/somewhere/else\n');
		desktopEntry.ensureLinuxDesktopEntry();
		assert.equal(fs.existsSync(path.join(userDir(), 'fluxer.desktop')), true);
	});

	test('a system entry wins and removes a generated user copy', () => {
		fs.mkdirSync(systemDir(), {recursive: true});
		fs.writeFileSync(path.join(systemDir(), 'app.fluxer.FluxerDesktop.desktop'), '[Desktop Entry]\n');
		fs.mkdirSync(userDir(), {recursive: true});
		fs.writeFileSync(
			path.join(userDir(), 'app.fluxer.FluxerDesktop.desktop'),
			desktopEntry.buildDesktopFileContents(EXEC_PATH, false),
		);
		assert.equal(desktopEntry.ensureLinuxDesktopEntry(), true);
		assert.equal(fs.existsSync(path.join(userDir(), 'app.fluxer.FluxerDesktop.desktop')), false);
	});

	test('a system entry under the previous desktop id keeps the new user entry out of menus', () => {
		fs.mkdirSync(systemDir(), {recursive: true});
		fs.writeFileSync(path.join(systemDir(), 'fluxer.desktop'), '[Desktop Entry]\nExec=/usr/bin/fluxer\n');
		assert.equal(desktopEntry.ensureLinuxDesktopEntry(), true);
		const written = fs.readFileSync(path.join(userDir(), 'app.fluxer.FluxerDesktop.desktop'), 'utf8');
		assert.equal(written, desktopEntry.buildDesktopFileContents(EXEC_PATH, true));
		assert.equal(fs.existsSync(path.join(systemDir(), 'fluxer.desktop')), true);
	});

	test('FLUXER_DISABLE_DESKTOP_FILE leaves launchers and the scheme handler alone', () => {
		process.env.FLUXER_DISABLE_DESKTOP_FILE = '1';
		fs.mkdirSync(userDir(), {recursive: true});
		fs.writeFileSync(path.join(userDir(), 'fluxer.desktop'), `[Desktop Entry]\n${desktopEntry.GENERATED_MARKER}\n`);
		assert.equal(desktopEntry.ensureLinuxDesktopEntry(), false);
		assert.equal(fs.existsSync(path.join(userDir(), 'fluxer.desktop')), true);
		assert.equal(fs.existsSync(path.join(userDir(), 'app.fluxer.FluxerDesktop.desktop')), false);
		assert.deepEqual(protocolRegistrations, []);
	});

	test('FLUXER_DISABLE_DESKTOP_FILE still reports an entry the user installed', () => {
		process.env.FLUXER_DISABLE_DESKTOP_FILE = '1';
		fs.mkdirSync(userDir(), {recursive: true});
		fs.writeFileSync(path.join(userDir(), 'app.fluxer.FluxerDesktop.desktop'), '[Desktop Entry]\n');
		assert.equal(desktopEntry.ensureLinuxDesktopEntry(), true);
		assert.deepEqual(protocolRegistrations, []);
	});

	test('FLUXER_DISABLE_DESKTOP_FILE still points the scheme handler at a packaged entry', () => {
		process.env.FLUXER_DISABLE_DESKTOP_FILE = '1';
		fs.mkdirSync(systemDir(), {recursive: true});
		fs.writeFileSync(path.join(systemDir(), 'app.fluxer.FluxerDesktop.desktop'), '[Desktop Entry]\n');
		fs.mkdirSync(userDir(), {recursive: true});
		fs.writeFileSync(path.join(userDir(), 'fluxer.desktop'), `[Desktop Entry]\n${desktopEntry.GENERATED_MARKER}\n`);
		assert.equal(desktopEntry.ensureLinuxDesktopEntry(), true);
		assert.equal(fs.existsSync(path.join(userDir(), 'fluxer.desktop')), true);
		assert.deepEqual(protocolRegistrations, ['fluxer']);
	});

	const installSystemEntry = () => {
		fs.mkdirSync(systemDir(), {recursive: true});
		fs.writeFileSync(path.join(systemDir(), 'app.fluxer.FluxerDesktop.desktop'), '[Desktop Entry]\n');
	};

	test('the scheme handler is registered off the main thread with a bounded xdg-mime call', () => {
		installSystemEntry();
		assert.equal(desktopEntry.ensureLinuxDesktopEntry(), true);
		const registrations = childProcessCalls.filter((call) => call.file === 'xdg-mime');
		assert.equal(registrations.length, 1);
		assert.deepEqual(registrations[0].args, ['default', DESKTOP_NAME, 'x-scheme-handler/fluxer']);
		assert.equal(registrations[0].options.timeout, 5000);
		registrations[0].callback(null);
		assert.deepEqual(inProcessRegistrations, []);
		assert.deepEqual(loggedWarnings, []);
	});

	test('a scheme handler that already points at this app is not written again', () => {
		registeredAsDefault = true;
		installSystemEntry();
		assert.equal(desktopEntry.ensureLinuxDesktopEntry(), true);
		assert.deepEqual(childProcessCalls, []);
		assert.deepEqual(inProcessRegistrations, []);
	});

	test('a missing, failed or timed out xdg-mime falls back to registering in process', () => {
		installSystemEntry();
		assert.equal(desktopEntry.ensureLinuxDesktopEntry(), true);
		assert.deepEqual(inProcessRegistrations, []);
		const registration = childProcessCalls.find((call) => call.file === 'xdg-mime');
		registration.callback(Object.assign(new Error('spawn xdg-mime ENOENT'), {code: 'ENOENT'}));
		assert.deepEqual(inProcessRegistrations, ['fluxer']);
	});

	test('the scheme handler is left alone when the shell has no desktop name', () => {
		delete process.env.CHROME_DESKTOP;
		installSystemEntry();
		assert.equal(desktopEntry.ensureLinuxDesktopEntry(), true);
		assert.deepEqual(childProcessCalls, []);
		assert.deepEqual(inProcessRegistrations, []);
	});

	test('an unpackaged run leaves launchers and the scheme handler alone', () => {
		electronApp.isPackaged = false;
		assert.equal(desktopEntry.ensureLinuxDesktopEntry(), false);
		assert.equal(fs.existsSync(path.join(userDir(), 'app.fluxer.FluxerDesktop.desktop')), false);
		assert.deepEqual(protocolRegistrations, []);
	});
});
