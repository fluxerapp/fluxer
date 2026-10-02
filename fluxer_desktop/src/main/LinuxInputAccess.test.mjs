// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {readFileSync} from 'node:fs';
import {createRequire} from 'node:module';
import {describe, test} from 'node:test';
import {fileURLToPath} from 'node:url';
import vm from 'node:vm';

const require = createRequire(import.meta.url);
const esbuild = require('esbuild');

const sourcePath = fileURLToPath(new URL('./LinuxInputAccess.ts', import.meta.url));
const source = readFileSync(sourcePath, 'utf8');
const transformedSource = esbuild.transformSync(source, {
	loader: 'ts',
	format: 'cjs',
	platform: 'node',
	target: 'node20',
}).code;

function loadLinuxInputAccess() {
	const module = {exports: {}};
	const context = vm.createContext({module, exports: module.exports});
	vm.runInContext(transformedSource, context, {filename: sourcePath});
	return module.exports;
}

const {isFullKeyboardKeyCapabilities, hasLinuxEvdevAccess} = loadLinuxInputAccess();

function probe(overrides) {
	return {
		totalEventDevices: 4,
		readableEventDevices: 0,
		keyboardDevices: 0,
		readableKeyboardDevices: 0,
		inInputGroup: false,
		...overrides,
	};
}

describe('isFullKeyboardKeyCapabilities', () => {
	test('accepts a real keyboard bitmap', () => {
		assert.equal(
			isFullKeyboardKeyCapabilities(
				'120013 0 0 0 0 0 0 0 0 1000000000007 ff9f207ac14057ff febeffdfffefffff fffffffffffffffe',
			),
			true,
		);
	});

	test('accepts a keyboard bitmap from a 32-bit kernel', () => {
		assert.equal(isFullKeyboardKeyCapabilities('ffefffff fffffffe'), true);
	});

	test('rejects a mouse bitmap', () => {
		assert.equal(isFullKeyboardKeyCapabilities('1f0000 0 0 0 0'), false);
	});

	test('rejects a gamepad bitmap', () => {
		assert.equal(isFullKeyboardKeyCapabilities('7fdb000000000000 0 0 0 0'), false);
	});

	test('rejects a media-key-only bitmap', () => {
		assert.equal(isFullKeyboardKeyCapabilities('10000 0 0 0 1c000000000000 0'), false);
	});

	test('rejects empty and malformed input', () => {
		assert.equal(isFullKeyboardKeyCapabilities(''), false);
		assert.equal(isFullKeyboardKeyCapabilities('zz'), false);
	});
});

describe('hasLinuxEvdevAccess', () => {
	test('a readable gamepad does not count when the keyboard is unreadable', () => {
		assert.equal(hasLinuxEvdevAccess(probe({readableEventDevices: 1, keyboardDevices: 1})), false);
	});

	test('every keyboard must be readable', () => {
		assert.equal(
			hasLinuxEvdevAccess(probe({readableEventDevices: 3, keyboardDevices: 2, readableKeyboardDevices: 1})),
			false,
		);
		assert.equal(
			hasLinuxEvdevAccess(probe({readableEventDevices: 4, keyboardDevices: 2, readableKeyboardDevices: 2})),
			true,
		);
	});

	test('input group membership grants access', () => {
		assert.equal(hasLinuxEvdevAccess(probe({keyboardDevices: 1, inInputGroup: true})), true);
	});

	test('falls back to any readable device when no keyboard is detected', () => {
		assert.equal(hasLinuxEvdevAccess(probe({readableEventDevices: 1})), true);
		assert.equal(hasLinuxEvdevAccess(probe({})), false);
	});
});
