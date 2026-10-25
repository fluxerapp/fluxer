// SPDX-License-Identifier: AGPL-3.0-or-later

import {isTrustedMusicSender, parseParticipantUserId} from '@fluxer/music_protocol/src/Identity';
import {describe, expect, test} from 'vitest';

describe('Identity', () => {
	test('parses user ids', () => {
		expect(parseParticipantUserId('user_123_abc')).toBe('123');
		expect(parseParticipantUserId('admin_1')).toBeNull();
	});
	test('trusts only bot users', () => {
		expect(isTrustedMusicSender('user_9_c', (id) => id === '9')).toBe(true);
		expect(isTrustedMusicSender('user_8_c', (id) => id === '9')).toBe(false);
		expect(isTrustedMusicSender('weird', () => true)).toBe(false);
	});
});
