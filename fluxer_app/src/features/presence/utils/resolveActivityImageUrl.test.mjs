// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {resolveActivityImageUrl} from './resolveActivityImageUrl.ts';

assert.equal(
	resolveActivityImageUrl('https://is1-ssl.mzstatic.com/image/thumb/foo.jpg'),
	'https://is1-ssl.mzstatic.com/image/thumb/foo.jpg',
);
assert.equal(resolveActivityImageUrl('mp:external/abc123/hash'), null);
assert.equal(resolveActivityImageUrl('cover-art', '1489544859718258779'), null);
assert.equal(
	resolveActivityImageUrl('fluxer-rpc-art://abc123/'),
	'fluxer-rpc-art://abc123/',
);
assert.equal(resolveActivityImageUrl(undefined), null);
console.log('resolveActivityImageUrl test passed');
