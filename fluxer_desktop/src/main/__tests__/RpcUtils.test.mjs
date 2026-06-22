// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {resolveListeningActivityName, shouldUseAppNameForListeningName} from '../rpc/RpcUtils.ts';

assert.equal(
	shouldUseAppNameForListeningName('Strawberry', 'Counting Worms', 'Knocked Loose - Counting Worms', 'Knocked Loose'),
	true,
);
assert.equal(
	shouldUseAppNameForListeningName('Strawberry', 'Bleed It Out - Live', 'Bleed It Out - Live', 'Linkin Park'),
	true,
);
assert.equal(shouldUseAppNameForListeningName('Spotify', 'Spotify', 'Artist - Song', 'Artist'), false);

assert.equal(
	resolveListeningActivityName('Strawberry', 'Counting Worms', 'Knocked Loose - Counting Worms', 'Knocked Loose', 2),
	'Strawberry',
);
assert.equal(resolveListeningActivityName('Spotify', 'Spotify', 'Artist - Song', 'Artist', 2), 'Spotify');
assert.equal(
	resolveListeningActivityName('Spotify', 'Hybrid Theory', 'Linkin Park - In the End', 'Linkin Park', 2),
	'Hybrid Theory',
);

console.log('RpcUtils test passed');
