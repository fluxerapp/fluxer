// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {readFileSync} from 'node:fs';
import path from 'node:path';
import {describe, test} from 'node:test';
import {fileURLToPath} from 'node:url';

const source = readFileSync(path.join(path.dirname(fileURLToPath(import.meta.url)), 'Window.ts'), 'utf8');

describe('window chrome', () => {
	test('macOS keeps the native frame behind the custom title bar', () => {
		assert.match(
			source,
			/titleBarStyle: useCustomChrome \? 'hidden' : undefined,[\s\S]*?frame: isMac \? true : !useCustomChrome,/,
			'A frameless macOS window loses the native border, rounded corners and window shadow.',
		);
	});

	test('window options never pass an explicit undefined shadow or traffic light position', () => {
		assert.doesNotMatch(source, /hasShadow: [^,\n]*undefined/);
		assert.doesNotMatch(source, /trafficLightPosition: isMac \? [^,\n]* : undefined/);
		assert.doesNotMatch(source, /hasShadow: getWindowHasShadow/);
	});

	test('the shadow is only removed for transparent Linux windows', () => {
		assert.match(
			source,
			/function getWindowShadowOptions\([\s\S]*?if \(process\.platform !== 'linux' \|\| !allowTransparency\) return \{\};\n\treturn \{hasShadow: false\};/,
		);
	});
});
