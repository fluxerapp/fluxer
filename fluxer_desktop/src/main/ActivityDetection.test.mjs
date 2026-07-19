// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {readFileSync} from 'node:fs';
import {createRequire} from 'node:module';
import {describe, test} from 'node:test';
import {fileURLToPath} from 'node:url';
import vm from 'node:vm';

const require = createRequire(import.meta.url);
const esbuild = require('esbuild');

const sourcePath = fileURLToPath(new URL('./ActivityDetection.ts', import.meta.url));
const source = readFileSync(sourcePath, 'utf8');
const transformedSource = esbuild.transformSync(source, {
	loader: 'ts',
	format: 'cjs',
	platform: 'node',
	target: 'node20',
}).code;

const detectablesCatalog = [
	{
		name: 'Minecraft',
		icon: 'minecraft.png',
		executables: [
			{name: 'minecraft.windows.exe', os: 'win32'},
			{name: '>java', os: 'linux', arguments: 'net.minecraft.client.main.Main'},
		],
	},
	{
		name: 'osu!',
		icon: 'osu.png',
		executables: [{name: 'osu!', os: 'linux'}],
		presence_assets: {mode_0: 'osu/mode_0.png'},
	},
];

function plain(value) {
	return JSON.parse(JSON.stringify(value));
}

function loadActivityDetection({platform = 'linux'} = {}) {
	const module = {exports: {}};
	const context = vm.createContext({
		module,
		exports: module.exports,
		console,
		process: {platform},
		require: (specifier) => {
			if (specifier === './DetectablesCatalog.json') return detectablesCatalog;
			if (specifier === 'node:child_process') {
				return {execFile: () => {}};
			}
			return require(specifier);
		},
	});
	vm.runInContext(transformedSource, context, {filename: sourcePath});
	return module.exports.__activityDetectionTest;
}

describe('ActivityDetection', () => {
	test('matches shared runtimes only when command-line arguments match', () => {
		const detector = loadActivityDetection();
		const activities = detector.detectActivities(detectablesCatalog, [
			{name: '/usr/bin/java', pid: 123, path: '/usr/bin/java', arguments: '-cp game.jar net.minecraft.client.main.Main'},
			{name: '/usr/bin/java', pid: 456, path: '/usr/bin/java', arguments: '-jar server.jar'},
		]);

		assert.deepEqual(plain(activities), [
			{
				name: 'Minecraft',
				icon: 'minecraft.png',
				processes: [{name: 'java', pid: 123}],
			},
		]);
	});

	test('returns presence assets for matching detectables', () => {
		const detector = loadActivityDetection();
		const activities = detector.detectActivities(detectablesCatalog, [
			{name: '/usr/bin/osu!', pid: 789, path: '/usr/bin/osu!', arguments: 'osu!'},
		]);

		assert.deepEqual(plain(activities), [
			{
				name: 'osu!',
				icon: 'osu.png',
				presenceAssets: {mode_0: 'osu/mode_0.png'},
				processes: [{name: 'osu!', pid: 789}],
			},
		]);
	});

	test('matches Windows executable names case-insensitively', () => {
		const detector = loadActivityDetection({platform: 'win32'});
		const activities = detector.detectActivities(detectablesCatalog, [
			{
				name: 'Minecraft.Windows.exe',
				pid: 321,
				path: 'C:\\Program Files\\WindowsApps\\Minecraft.Windows.exe',
				arguments: null,
			},
		]);

		assert.equal(activities[0]?.name, 'Minecraft');
		assert.equal(activities[0]?.processes[0]?.name, 'minecraft.windows.exe');
	});

	test('parses POSIX ps output with command paths and arguments', () => {
		const detector = loadActivityDetection();
		const processes = detector.parsePosixProcesses(
			'  123 /usr/bin/java  /usr/bin/java -cp game.jar net.minecraft.client.main.Main\n',
		);

		assert.deepEqual(plain(processes), [
			{
				name: '/usr/bin/java',
				pid: 123,
				path: '/usr/bin/java',
				arguments: '/usr/bin/java -cp game.jar net.minecraft.client.main.Main',
			},
		]);
	});

	test('parses Windows process JSON from PowerShell', () => {
		const detector = loadActivityDetection({platform: 'win32'});
		const processes = detector.parseWindowsProcessJson(
			JSON.stringify({ProcessId: 42, Name: 'osu!.exe', ExecutablePath: 'C:\\osu!\\osu!.exe', CommandLine: '"osu!.exe"'}),
		);

		assert.deepEqual(plain(processes), [
			{name: 'osu!.exe', pid: 42, path: 'C:\\osu!\\osu!.exe', arguments: '"osu!.exe"'},
		]);
	});
});
