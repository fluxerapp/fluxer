// SPDX-License-Identifier: AGPL-3.0-or-later

import {readPasskeyBridgeReturn} from '@app/features/auth/passkey_migration/PasskeyBridgeReturn';
import {describe, expect, it} from 'vitest';

const CEREMONY_ID = 'a'.repeat(43);
const COMPLETION_CODE = 'B_-'.repeat(14).concat('c');

describe('readPasskeyBridgeReturn', () => {
	it('reads the ceremony id and completion code from the fragment', () => {
		expect(readPasskeyBridgeReturn(`#passkey-bridge=${CEREMONY_ID}.${COMPLETION_CODE}`)).toEqual({
			ceremonyId: CEREMONY_ID,
			completionCode: COMPLETION_CODE,
		});
	});

	it.each([
		['an empty fragment', ''],
		['another fragment', '#section'],
		['a missing completion code', `#passkey-bridge=${CEREMONY_ID}`],
		['an extra segment', `#passkey-bridge=${CEREMONY_ID}.${COMPLETION_CODE}.${COMPLETION_CODE}`],
		['a short ceremony id', `#passkey-bridge=abc.${COMPLETION_CODE}`],
		['characters outside base64url', `#passkey-bridge=${CEREMONY_ID}.${'+'.repeat(43)}`],
	])('rejects %s', (_label, hash) => {
		expect(readPasskeyBridgeReturn(hash)).toBeNull();
	});
});
