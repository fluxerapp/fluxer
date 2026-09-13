// SPDX-License-Identifier: AGPL-3.0-or-later

import {create, equals} from '@bufbuild/protobuf';
import {
	createEmptySyncedPreferences,
	decodeSyncedPreferences,
	decodeSyncedPreferencesLenient,
	EMPTY_SYNCED_PREFERENCES_ENCODED,
	encodedSyncedPreferencesByteLength,
	encodeSyncedPreferences,
	isEmptySyncedPreferencesEncoded,
	isValidSyncedPreferencesEncoding,
	SYNCED_PREFERENCES_MAX_ENCODED_LENGTH,
	SyncedPreferencesDecodeError,
	SyncedPreferencesSchema,
} from '@fluxer/schema/src/domains/user/SyncedPreferencesCodec';
import {AccessibilitySettingsSchema} from '@fluxer/schema/src/gen/fluxer/user/preferences/v1/accessibility_pb';
import {
	FavoriteGifSettingsSchema,
	SoundSettingsSchema,
} from '@fluxer/schema/src/gen/fluxer/user/preferences/v1/pickers_pb';
import {
	ChatInputSettingsSchema,
	ReactionEmojiSchema,
} from '@fluxer/schema/src/gen/fluxer/user/preferences/v1/preferences_pb';
import {describe, expect, it} from 'vitest';

describe('SyncedPreferencesCodec', () => {
	it('encodes the empty snapshot to the empty string', () => {
		expect(encodeSyncedPreferences(createEmptySyncedPreferences())).toBe(EMPTY_SYNCED_PREFERENCES_ENCODED);
		expect(EMPTY_SYNCED_PREFERENCES_ENCODED).toBe('');
	});
	it('decodes empty/null/undefined to an empty message', () => {
		const empty = createEmptySyncedPreferences();
		expect(equals(SyncedPreferencesSchema, decodeSyncedPreferences(''), empty)).toBe(true);
		expect(equals(SyncedPreferencesSchema, decodeSyncedPreferences(null), empty)).toBe(true);
		expect(equals(SyncedPreferencesSchema, decodeSyncedPreferences(undefined), empty)).toBe(true);
	});
	it('round-trips a populated accessibility sub-message', () => {
		const original = create(SyncedPreferencesSchema, {
			accessibility: create(AccessibilitySettingsSchema, {
				fontSize: 14,
				alwaysUnderlineLinks: true,
				saturationFactor: 0.75,
			}),
		});
		const decoded = decodeSyncedPreferences(encodeSyncedPreferences(original));
		expect(equals(SyncedPreferencesSchema, decoded, original)).toBe(true);
		expect(decoded.accessibility?.fontSize).toBe(14);
		expect(decoded.accessibility?.alwaysUnderlineLinks).toBe(true);
		expect(decoded.accessibility?.saturationFactor).toBeCloseTo(0.75);
	});
	it('round-trips a populated sound sub-message with map fields', () => {
		const original = create(SyncedPreferencesSchema, {
			sound: create(SoundSettingsSchema, {
				allSoundsDisabled: false,
				masterVolume: 0.5,
				disabledSounds: {ping: true, join: false},
				soundOverrides: {ping: 0.25},
			}),
		});
		const decoded = decodeSyncedPreferences(encodeSyncedPreferences(original));
		expect(equals(SyncedPreferencesSchema, decoded, original)).toBe(true);
		expect(decoded.sound?.disabledSounds).toEqual({ping: true, join: false});
		expect(decoded.sound?.soundOverrides).toEqual({ping: 0.25});
	});
	it('round-trips a populated favorites/gif sub-message and top-level scalar', () => {
		const original = create(SyncedPreferencesSchema, {
			sanitizeUrls: true,
			favoriteGifs: create(FavoriteGifSettingsSchema, {
				saveAsSavedMedia: true,
				seenFirstTimePrompt: true,
			}),
		});
		const decoded = decodeSyncedPreferences(encodeSyncedPreferences(original));
		expect(equals(SyncedPreferencesSchema, decoded, original)).toBe(true);
		expect(decoded.sanitizeUrls).toBe(true);
		expect(decoded.favoriteGifs?.saveAsSavedMedia).toBe(true);
	});
	it('round-trips chat input settings', () => {
		const original = create(SyncedPreferencesSchema, {
			chatInput: create(ChatInputSettingsSchema, {
				convertEmoticons: true,
			}),
		});
		const decoded = decodeSyncedPreferences(encodeSyncedPreferences(original));
		expect(equals(SyncedPreferencesSchema, decoded, original)).toBe(true);
		expect(decoded.chatInput?.convertEmoticons).toBe(true);
	});
	it('round-trips a unicode and a custom double tap reaction', () => {
		const unicode = create(SyncedPreferencesSchema, {
			doubleTapReaction: create(ReactionEmojiSchema, {name: '👍'}),
		});
		const decodedUnicode = decodeSyncedPreferences(encodeSyncedPreferences(unicode));
		expect(equals(SyncedPreferencesSchema, decodedUnicode, unicode)).toBe(true);
		expect(decodedUnicode.doubleTapReaction?.id).toBeUndefined();
		expect(decodedUnicode.doubleTapReaction?.name).toBe('👍');
		const custom = create(SyncedPreferencesSchema, {
			doubleTapReaction: create(ReactionEmojiSchema, {id: '123456789012345678', name: 'party'}),
		});
		const decodedCustom = decodeSyncedPreferences(encodeSyncedPreferences(custom));
		expect(equals(SyncedPreferencesSchema, decodedCustom, custom)).toBe(true);
		expect(decodedCustom.doubleTapReaction?.id).toBe('123456789012345678');
		expect(decodedCustom.doubleTapReaction?.name).toBe('party');
	});
	it('produces canonical encodings for equal inputs', () => {
		const a = create(SyncedPreferencesSchema, {
			accessibility: create(AccessibilitySettingsSchema, {fontSize: 14}),
		});
		const b = create(SyncedPreferencesSchema, {
			accessibility: create(AccessibilitySettingsSchema, {fontSize: 14}),
		});
		expect(encodeSyncedPreferences(a)).toBe(encodeSyncedPreferences(b));
	});
	it('rejects invalid base64 with a typed error', () => {
		expect(() => decodeSyncedPreferences('not_base64!!!')).toThrow(SyncedPreferencesDecodeError);
	});
	it('rejects invalid protobuf bytes with a typed error', () => {
		expect(() => decodeSyncedPreferences('////////')).toThrow(SyncedPreferencesDecodeError);
	});
	it('lenient decode swallows errors and returns an empty message', () => {
		const empty = createEmptySyncedPreferences();
		expect(equals(SyncedPreferencesSchema, decodeSyncedPreferencesLenient('not_base64!!!'), empty)).toBe(true);
		expect(equals(SyncedPreferencesSchema, decodeSyncedPreferencesLenient('////////'), empty)).toBe(true);
		expect(equals(SyncedPreferencesSchema, decodeSyncedPreferencesLenient(null), empty)).toBe(true);
	});
	it('reports byte length consistent with the underlying buffer', () => {
		const populated = create(SyncedPreferencesSchema, {
			accessibility: create(AccessibilitySettingsSchema, {fontSize: 14}),
		});
		const encoded = encodeSyncedPreferences(populated);
		expect(encoded).not.toBe('');
		expect(encodedSyncedPreferencesByteLength(encoded)).toBeGreaterThan(0);
		expect(encodedSyncedPreferencesByteLength('')).toBe(0);
	});
	it('isEmptySyncedPreferencesEncoded matches the empty sentinel', () => {
		const populated = create(SyncedPreferencesSchema, {sanitizeUrls: true});
		expect(isEmptySyncedPreferencesEncoded('')).toBe(true);
		expect(isEmptySyncedPreferencesEncoded(null)).toBe(true);
		expect(isEmptySyncedPreferencesEncoded(undefined)).toBe(true);
		expect(isEmptySyncedPreferencesEncoded(encodeSyncedPreferences(populated))).toBe(false);
	});
	it('EMPTY_SYNCED_PREFERENCES_ENCODED round-trips to an empty message', () => {
		const decoded = decodeSyncedPreferences(EMPTY_SYNCED_PREFERENCES_ENCODED);
		expect(equals(SyncedPreferencesSchema, decoded, createEmptySyncedPreferences())).toBe(true);
	});
	it('isValidSyncedPreferencesEncoding accepts standard and url-safe base64', () => {
		expect(isValidSyncedPreferencesEncoding('')).toBe(true);
		expect(isValidSyncedPreferencesEncoding('AAAA')).toBe(true);
		expect(isValidSyncedPreferencesEncoding('AA==')).toBe(true);
		expect(isValidSyncedPreferencesEncoding('-_AB')).toBe(true);
		expect(isValidSyncedPreferencesEncoding('not base64!')).toBe(false);
		expect(isValidSyncedPreferencesEncoding('a'.repeat(SYNCED_PREFERENCES_MAX_ENCODED_LENGTH + 4))).toBe(false);
	});
});
