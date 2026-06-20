// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';

function generatePathVariations(normalizedPath) {
	const toCompare = [];
	const splitPath = normalizedPath.split('/');
	for (let i = 1; i <= splitPath.length; i++) {
		toCompare.push(splitPath.slice(-i).join('/'));
	}
	return toCompare;
}

const variations = generatePathVariations('usr/games/steam/steamapps/common/celeste/celeste');
assert.ok(variations.includes('celeste'));
assert.ok(variations.includes('common/celeste/celeste'));
console.log('LinuxProcessScanner path variation test passed');
