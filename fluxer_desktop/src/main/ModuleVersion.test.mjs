// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {describe, test} from 'node:test';
import './LocalAppTestSupport.test.mjs';

const {ModuleVersionMalformedError, compareModuleVersions, parseModuleVersion} = await import(
	'@electron/main/ModuleVersion'
);

function compare(left, right) {
	return compareModuleVersions(parseModuleVersion(left, 'left version'), parseModuleVersion(right, 'right version'));
}

const ASCENDING = [
	'1.0.0-0',
	'1.0.0-1',
	'1.0.0-11',
	'1.0.0-alpha',
	'1.0.0-alpha.1',
	'1.0.0-alpha.beta',
	'1.0.0-beta',
	'1.0.0-beta.2',
	'1.0.0-beta.11',
	'1.0.0-rc.1',
	'1.0.0',
	'1.0.1-alpha',
	'1.0.1',
	'1.1.0',
	'2.0.0',
	'99999999999999999999.0.0',
];

describe('compareModuleVersions', () => {
	test('a prerelease sorts below the release it leads to', () => {
		assert.equal(compare('1.0.0-rc.1', '1.0.0'), -1);
		assert.equal(compare('1.0.0', '1.0.0-rc.1'), 1);
		assert.equal(compare('2026.823.1-canary', '2026.823.1'), -1);
		assert.equal(compare('1.0.0', '1.0.0'), 0);
		assert.equal(compare('1.0.0-rc.1', '1.0.0-rc.1'), 0);
	});

	test('a prerelease does not lift a version over a higher release', () => {
		assert.equal(compare('1.0.1-rc.1', '1.0.0'), 1);
		assert.equal(compare('1.0.0-rc.1', '0.9.9'), 1);
	});

	test('numeric prerelease identifiers sort below textual ones', () => {
		assert.equal(compare('1.0.0-1', '1.0.0-alpha'), -1);
		assert.equal(compare('1.0.0-alpha', '1.0.0-1'), 1);
		assert.equal(compare('1.0.0-alpha.1', '1.0.0-alpha.beta'), -1);
		assert.equal(compare('1.0.0-alpha.beta', '1.0.0-alpha.1'), 1);
	});

	test('numeric prerelease identifiers compare by magnitude, not lexically', () => {
		assert.equal(compare('1.0.0-beta.2', '1.0.0-beta.11'), -1);
		assert.equal(compare('1.0.0-beta.11', '1.0.0-beta.2'), 1);
	});

	test('a longer prerelease sorts above its own prefix', () => {
		assert.equal(compare('1.0.0-alpha', '1.0.0-alpha.1'), -1);
		assert.equal(compare('1.0.0-alpha.1', '1.0.0-alpha'), 1);
	});

	test('the whole precedence chain is a total order', () => {
		for (let left = 0; left < ASCENDING.length; left += 1) {
			assert.equal(compare(ASCENDING[left], ASCENDING[left]), 0, `${ASCENDING[left]} equals itself`);
			for (let right = left + 1; right < ASCENDING.length; right += 1) {
				assert.equal(compare(ASCENDING[left], ASCENDING[right]), -1, `${ASCENDING[left]} < ${ASCENDING[right]}`);
				assert.equal(compare(ASCENDING[right], ASCENDING[left]), 1, `${ASCENDING[right]} > ${ASCENDING[left]}`);
			}
		}
	});

	test('a major beyond the safe integer range still orders by magnitude', () => {
		assert.equal(compare('99999999999999999999.0.0', '99999999999999999998.0.0'), 1);
		assert.equal(compare('99999999999999999998.0.0', '99999999999999999999.0.0'), -1);
	});

	test('build metadata takes no part in precedence', () => {
		assert.equal(compare('1.0.0+build.1', '1.0.0+build.2'), 0);
		assert.equal(compare('1.0.0-alpha+build.1', '1.0.0-alpha'), 0);
	});
});

describe('parseModuleVersion', () => {
	test('keeps the source it parsed', () => {
		const version = parseModuleVersion('2026.823.1-rc.2+sha.abc', 'shell version');
		assert.equal(version.source, '2026.823.1-rc.2+sha.abc');
		assert.equal(version.major, 2026n);
		assert.equal(version.minor, 823n);
		assert.equal(version.patch, 1n);
	});

	for (const value of ['2026.08.23', '1.0', '', '1', '1.0.0.0', '1.0.0-01', 'v1.0.0', ' 1.0.0', '1.0.0 ']) {
		test(`rejects ${JSON.stringify(value)}`, () => {
			assert.throws(
				() => parseModuleVersion(value, 'manifest shell.minimum_version'),
				(error) => {
					assert.ok(error instanceof ModuleVersionMalformedError);
					assert.equal(error.name, 'ModuleVersionMalformedError');
					assert.equal(error.message, `manifest shell.minimum_version is not a valid semantic version: ${value}`);
					return true;
				},
			);
		});
	}
});
