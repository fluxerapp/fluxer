// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import type {UserID} from '@app/api/BrandedTypes';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {UserRepository} from '@app/api/user/repositories/UserRepository';
import {fetchUserSettings, updateUserSettings} from '@app/api/user/tests/UserTestUtils';
import {create, equals} from '@bufbuild/protobuf';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {
	decodeSyncedPreferences,
	encodedSyncedPreferencesByteLength,
	encodeSyncedPreferences,
	SYNCED_PREFERENCES_MAX_BYTES,
	SYNCED_PREFERENCES_MAX_ENCODED_LENGTH,
	SyncedPreferencesSchema,
} from '@fluxer/schema/src/domains/user/SyncedPreferencesCodec';
import {AccessibilitySettingsSchema} from '@fluxer/schema/src/gen/fluxer/user/preferences/v1/accessibility_pb';
import {
	LocalUserSpamOverridesSchema,
	SearchEngineSettingsSchema,
} from '@fluxer/schema/src/gen/fluxer/user/preferences/v1/preferences_pb';
import {afterEach, beforeEach, describe, expect, test, vi} from 'vitest';

function snapshotHash(encoded: string): string {
	return createHash('sha256').update(Buffer.from(encoded, 'base64')).digest('hex');
}

function encodedFontSize(fontSize: number): string {
	return encodeSyncedPreferences(
		create(SyncedPreferencesSchema, {accessibility: create(AccessibilitySettingsSchema, {fontSize})}),
	);
}

describe('User Settings synced_preferences', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(() => {
		vi.restoreAllMocks();
	});
	test('round-trips an encoded snapshot', async () => {
		const account = await createTestAccount(harness);
		const snapshot = create(SyncedPreferencesSchema, {
			searchEngines: create(SearchEngineSettingsSchema, {textSearchEngineId: 'google'}),
			accessibility: create(AccessibilitySettingsSchema, {fontSize: 14, alwaysUnderlineLinks: true}),
			sanitizeUrls: true,
		});
		const encoded = encodeSyncedPreferences(snapshot);
		const {json} = await updateUserSettings(harness, account.token, {
			synced_preferences: encoded,
		});
		expect(typeof json.synced_preferences).toBe('string');
		expect(json.synced_preferences.length).toBeGreaterThan(0);
		expect(equals(SyncedPreferencesSchema, decodeSyncedPreferences(json.synced_preferences), snapshot)).toBe(true);
	});
	test('full-snapshot replacement removes prior fields', async () => {
		const account = await createTestAccount(harness);
		const initial = create(SyncedPreferencesSchema, {
			sanitizeUrls: true,
			accessibility: create(AccessibilitySettingsSchema, {fontSize: 14}),
			searchEngines: create(SearchEngineSettingsSchema, {textSearchEngineId: 'google'}),
		});
		await updateUserSettings(harness, account.token, {
			synced_preferences: encodeSyncedPreferences(initial),
		});
		const replacement = create(SyncedPreferencesSchema, {sanitizeUrls: true});
		const {json} = await updateUserSettings(harness, account.token, {
			synced_preferences: encodeSyncedPreferences(replacement),
		});
		expect(equals(SyncedPreferencesSchema, decodeSyncedPreferences(json.synced_preferences), replacement)).toBe(true);
	});
	test('null clears the snapshot entirely', async () => {
		const account = await createTestAccount(harness);
		await updateUserSettings(harness, account.token, {
			synced_preferences: encodeSyncedPreferences(create(SyncedPreferencesSchema, {sanitizeUrls: true})),
		});
		const {json} = await updateUserSettings(harness, account.token, {
			synced_preferences: null,
		});
		expect(json.synced_preferences).toBe('');
	});
	test('rejects garbage protobuf bytes with 400', async () => {
		const account = await createTestAccount(harness);
		await createBuilder(harness, account.token)
			.patch('/users/@me/settings')
			.body({synced_preferences: '////////'})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
	test('rejects oversized snapshots', async () => {
		const account = await createTestAccount(harness);
		const blob = 'x'.repeat(2048);
		const ids: Array<string> = [];
		const entries = Math.ceil(SYNCED_PREFERENCES_MAX_BYTES / blob.length) + 8;
		for (let i = 0; i < entries; i++) ids.push(`${i}-${blob}`);
		const big = create(SyncedPreferencesSchema, {
			localSpamOverrides: create(LocalUserSpamOverridesSchema, {spammerUserIds: ids}),
		});
		const encoded = encodeSyncedPreferences(big);
		const response = await createBuilder<{
			code: string;
			errors: Array<{
				path: string;
				code: string;
				message: string;
			}>;
		}>(harness, account.token)
			.patch('/users/@me/settings')
			.body({synced_preferences: encoded})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
		expect(response.code).toBe('INVALID_FORM_BODY');
		expect(response.errors[0]?.path).toBe('synced_preferences');
		expect(response.errors[0]?.code).toBe(ValidationErrorCodes.CONTENT_EXCEEDS_MAX_LENGTH);
	});
	test('reports TOO_LARGE for a snapshot inside the encoded-length bound but over the byte cap', async () => {
		const account = await createTestAccount(harness);
		const baseEntries = Math.floor((SYNCED_PREFERENCES_MAX_BYTES - 8192) / 1006);
		const build = (tailLength: number) =>
			encodeSyncedPreferences(
				create(SyncedPreferencesSchema, {
					localSpamOverrides: create(LocalUserSpamOverridesSchema, {
						spammerUserIds: [
							...Array.from({length: baseEntries}, (_, i) => `${i}-${'x'.repeat(1000)}`),
							'y'.repeat(tailLength),
						],
					}),
				}),
			);
		let tail = 1;
		let encoded = build(tail);
		while (encodedSyncedPreferencesByteLength(encoded) <= SYNCED_PREFERENCES_MAX_BYTES) {
			tail += 512;
			encoded = build(tail);
		}
		tail = Math.max(1, tail - 512);
		encoded = build(tail);
		while (encodedSyncedPreferencesByteLength(encoded) <= SYNCED_PREFERENCES_MAX_BYTES) {
			tail += 1;
			encoded = build(tail);
		}
		expect(encodedSyncedPreferencesByteLength(encoded)).toBeGreaterThan(SYNCED_PREFERENCES_MAX_BYTES);
		expect(encoded.length).toBeLessThanOrEqual(SYNCED_PREFERENCES_MAX_ENCODED_LENGTH);
		const response = await createBuilder<{
			code: string;
			errors: Array<{path: string; code: string; message: string}>;
		}>(harness, account.token)
			.patch('/users/@me/settings')
			.body({synced_preferences: encoded})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
		expect(response.errors[0]?.path).toBe('synced_preferences');
		expect(response.errors[0]?.code).toBe(ValidationErrorCodes.TOO_LARGE);
	});
	test('omitting the field leaves the existing snapshot intact', async () => {
		const account = await createTestAccount(harness);
		const initial = encodeSyncedPreferences(
			create(SyncedPreferencesSchema, {
				searchEngines: create(SearchEngineSettingsSchema, {textSearchEngineId: 'google'}),
			}),
		);
		await updateUserSettings(harness, account.token, {synced_preferences: initial});
		const {json} = await updateUserSettings(harness, account.token, {
			gif_auto_play: false,
		});
		expect(json.synced_preferences).toBe(initial);
		expect(json.gif_auto_play).toBe(false);
	});
	test('accepts a base hash that matches the stored snapshot, starting from an empty one', async () => {
		const account = await createTestAccount(harness);
		const first = await updateUserSettings(harness, account.token, {
			synced_preferences: encodedFontSize(14),
			synced_preferences_base_hash: snapshotHash(''),
		});
		const second = await updateUserSettings(harness, account.token, {
			synced_preferences: encodedFontSize(16),
			synced_preferences_base_hash: snapshotHash(first.json.synced_preferences),
		});
		expect(second.json.synced_preferences).toBe(encodedFontSize(16));
	});
	test('rejects a stale base hash with 409 and leaves the stored snapshot alone', async () => {
		const account = await createTestAccount(harness);
		const first = await updateUserSettings(harness, account.token, {synced_preferences: encodedFontSize(14)});
		await updateUserSettings(harness, account.token, {synced_preferences: encodedFontSize(16)});
		const response = await createBuilder<{code: string; message: string}>(harness, account.token)
			.patch('/users/@me/settings')
			.body({
				synced_preferences: encodedFontSize(18),
				synced_preferences_base_hash: snapshotHash(first.json.synced_preferences),
				gif_auto_play: false,
			})
			.expect(HTTP_STATUS.CONFLICT)
			.execute();
		expect(response.code).toBe(APIErrorCodes.SYNCED_PREFERENCES_CONFLICT);
		const {json} = await fetchUserSettings(harness, account.token);
		expect(json.synced_preferences).toBe(encodedFontSize(16));
		expect(json.gif_auto_play).toBe(true);
	});
	test('a patch without synced_preferences keeps a snapshot written while it was in flight', async () => {
		const account = await createTestAccount(harness);
		await updateUserSettings(harness, account.token, {synced_preferences: encodedFontSize(14)});
		const findSettings = UserRepository.prototype.findSettings;
		let concurrentWrite: Promise<unknown> | null = null;
		vi.spyOn(UserRepository.prototype, 'findSettings').mockImplementation(async function (
			this: UserRepository,
			userId: UserID,
		) {
			const settings = await findSettings.call(this, userId);
			if (concurrentWrite === null) {
				concurrentWrite = updateUserSettings(harness, account.token, {synced_preferences: encodedFontSize(16)});
				await concurrentWrite;
			}
			return settings;
		});
		await updateUserSettings(harness, account.token, {gif_auto_play: false});
		vi.restoreAllMocks();
		const {json} = await fetchUserSettings(harness, account.token);
		expect(json.synced_preferences).toBe(encodedFontSize(16));
		expect(json.gif_auto_play).toBe(false);
	});
});
