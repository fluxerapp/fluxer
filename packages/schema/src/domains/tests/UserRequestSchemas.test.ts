// SPDX-License-Identifier: AGPL-3.0-or-later

import {create} from '@bufbuild/protobuf';
import {MAX_GROUP_DM_OTHER_RECIPIENTS} from '@fluxer/constants/src/LimitConstants';
import {
	encodeSyncedPreferences,
	SYNCED_PREFERENCES_MAX_ENCODED_LENGTH,
	SyncedPreferencesSchema,
} from '@fluxer/schema/src/domains/user/SyncedPreferencesCodec';
import {
	CreatePrivateChannelRequest,
	CustomStatusPayload,
	UserSettingsUpdateRequest,
} from '@fluxer/schema/src/domains/user/UserRequestSchemas';
import {SearchEngineSettingsSchema} from '@fluxer/schema/src/gen/fluxer/user/preferences/v1/preferences_pb';
import {describe, expect, it} from 'vitest';

describe('CustomStatusPayload', () => {
	it('accepts single-codepoint unicode emoji names', () => {
		expect(
			CustomStatusPayload.parse({
				text: 'Coffee time',
				emoji_name: '☕',
			}),
		).toEqual({text: 'Coffee time', emoji_name: '☕'});
	});
	it('ignores emoji_name when emoji_id is provided', () => {
		expect(
			CustomStatusPayload.parse({
				text: 'Custom emoji status',
				emoji_id: '123456789012345678',
				emoji_name: 'not-a-unicode-emoji-and-way-too-long-to-validate-in-this-path',
			}),
		).toEqual({text: 'Custom emoji status', emoji_id: 123456789012345678n});
	});
});

describe('UserSettingsUpdateRequest synced_preferences', () => {
	it('accepts a base64-encoded snapshot', () => {
		const encoded = encodeSyncedPreferences(
			create(SyncedPreferencesSchema, {
				searchEngines: create(SearchEngineSettingsSchema, {textSearchEngineId: 'google'}),
			}),
		);
		expect(UserSettingsUpdateRequest.parse({synced_preferences: encoded})).toEqual({synced_preferences: encoded});
	});
	it.each([
		{name: 'empty snapshot', value: ''},
		{name: 'cleared snapshot', value: null},
		{name: 'exact size cap', value: 'A'.repeat(SYNCED_PREFERENCES_MAX_ENCODED_LENGTH)},
	])('preserves $name', ({value}) => {
		expect(UserSettingsUpdateRequest.parse({synced_preferences: value})).toEqual({synced_preferences: value});
	});
	it.each([
		{name: 'non-base64 string', value: 'not_base64!!!'},
		{name: 'raw object', value: {textSearchEngineId: 'google'}},
		{name: 'above size cap', value: 'A'.repeat(SYNCED_PREFERENCES_MAX_ENCODED_LENGTH + 1)},
	])('rejects $name', ({value}) => {
		expect(UserSettingsUpdateRequest.safeParse({synced_preferences: value}).success).toBe(false);
	});
});

describe('CreatePrivateChannelRequest', () => {
	it('allows the maximum other recipients for a 50-member group DM', () => {
		const recipients = Array.from({length: MAX_GROUP_DM_OTHER_RECIPIENTS}, (_, index) =>
			String(100000000000000000n + BigInt(index)),
		);
		expect(CreatePrivateChannelRequest.parse({recipients})).toEqual({recipients: recipients.map(BigInt)});
	});
	it('rejects group DM requests above the member limit', () => {
		const recipients = Array.from({length: MAX_GROUP_DM_OTHER_RECIPIENTS + 1}, (_, index) =>
			String(100000000000000000n + BigInt(index)),
		);
		expect(CreatePrivateChannelRequest.safeParse({recipients}).success).toBe(false);
	});
	it('allows DM requests to the system user id 0', () => {
		expect(CreatePrivateChannelRequest.parse({recipient_id: '0'})).toEqual({recipient_id: 0n});
	});
	it.each([{}, {recipient_id: '0', recipients: []}])('requires exactly one recipient form for %j', (input) => {
		expect(CreatePrivateChannelRequest.safeParse(input)).toMatchObject({
			success: false,
			error: {issues: [{message: 'Either recipient_id or recipients must be provided, but not both'}]},
		});
	});
});
