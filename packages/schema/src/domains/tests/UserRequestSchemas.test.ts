// SPDX-License-Identifier: AGPL-3.0-or-later

import {create} from '@bufbuild/protobuf';
import {MAX_GROUP_DM_OTHER_RECIPIENTS} from '@fluxer/constants/src/LimitConstants';
import {encodeSyncedPreferences, SyncedPreferencesSchema} from '@fluxer/schema/src/domains/user/SyncedPreferencesCodec';
import {
	CreatePrivateChannelRequest,
	CustomStatusPayload,
	UserSettingsUpdateRequest,
} from '@fluxer/schema/src/domains/user/UserRequestSchemas';
import {SearchEngineSettingsSchema} from '@fluxer/schema/src/gen/fluxer/user/preferences/v1/preferences_pb';
import {describe, expect, it} from 'vitest';

describe('CustomStatusPayload', () => {
	it('accepts single-codepoint unicode emoji names', () => {
		const result = CustomStatusPayload.safeParse({
			text: 'Coffee time',
			emoji_name: '☕',
		});
		expect(result.success).toBe(true);
		if (result.success) {
			expect(result.data.emoji_name).toBe('☕');
		}
	});
	it('ignores emoji_name when emoji_id is provided', () => {
		const result = CustomStatusPayload.safeParse({
			text: 'Custom emoji status',
			emoji_id: '123456789012345678',
			emoji_name: 'not-a-unicode-emoji-and-way-too-long-to-validate-in-this-path',
		});
		expect(result.success).toBe(true);
		if (result.success) {
			expect(result.data.emoji_id).toBe(123456789012345678n);
			expect(result.data.emoji_name).toBeUndefined();
		}
	});
});

describe('UserSettingsUpdateRequest synced_preferences', () => {
	it('accepts a base64-encoded snapshot', () => {
		const encoded = encodeSyncedPreferences(
			create(SyncedPreferencesSchema, {
				searchEngines: create(SearchEngineSettingsSchema, {textSearchEngineId: 'google'}),
			}),
		);
		const result = UserSettingsUpdateRequest.safeParse({synced_preferences: encoded});
		expect(result.success).toBe(true);
	});
	it('accepts the empty snapshot', () => {
		expect(UserSettingsUpdateRequest.safeParse({synced_preferences: ''}).success).toBe(true);
	});
	it('accepts null to clear the snapshot', () => {
		expect(UserSettingsUpdateRequest.safeParse({synced_preferences: null}).success).toBe(true);
	});
	it('rejects non-base64 strings', () => {
		expect(UserSettingsUpdateRequest.safeParse({synced_preferences: 'not_base64!!!'}).success).toBe(false);
	});
	it('rejects raw record/object payloads', () => {
		expect(UserSettingsUpdateRequest.safeParse({synced_preferences: {textSearchEngineId: 'google'}}).success).toBe(
			false,
		);
	});
	it('rejects strings exceeding the size cap', () => {
		const oversized = 'A'.repeat(400000);
		expect(UserSettingsUpdateRequest.safeParse({synced_preferences: oversized}).success).toBe(false);
	});
});

describe('CreatePrivateChannelRequest', () => {
	it('allows the maximum other recipients for a 50-member group DM', () => {
		const recipients = Array.from({length: MAX_GROUP_DM_OTHER_RECIPIENTS}, (_, index) =>
			String(100000000000000000n + BigInt(index)),
		);
		expect(CreatePrivateChannelRequest.safeParse({recipients}).success).toBe(true);
	});
	it('rejects group DM requests above the member limit', () => {
		const recipients = Array.from({length: MAX_GROUP_DM_OTHER_RECIPIENTS + 1}, (_, index) =>
			String(100000000000000000n + BigInt(index)),
		);
		expect(CreatePrivateChannelRequest.safeParse({recipients}).success).toBe(false);
	});
	it('allows DM requests to the system user id 0', () => {
		const parsed = CreatePrivateChannelRequest.safeParse({recipient_id: '0'});
		expect(parsed.success).toBe(true);
		if (parsed.success) {
			expect(parsed.data.recipient_id).toBe(0n);
		}
	});
});
