// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {create} from '@bufbuild/protobuf';
import {afterEach, describe, expect, it, vi} from 'vitest';

const mocks = vi.hoisted(() => ({patch: vi.fn()}));

vi.mock('@lingui/core/macro', () => ({
	msg: (descriptor: unknown) => descriptor,
	t: (descriptor: unknown) => descriptor,
}));

vi.mock('@app/features/platform/transport/RestTransport', () => ({
	http: {get: vi.fn(), post: vi.fn(), patch: mocks.patch, put: vi.fn(), delete: vi.fn()},
}));

const SCOPE_A = 'official::100000000000000001';
const SCOPE_B = 'selfhost::200000000000000002';

const storage = await import('@app/features/platform/state/PersistentStorage');
await storage.initializeAppStorage({scoped: true});
await storage.activateAppStorageScope(SCOPE_B);
storage.default.setItem('Accessibility:customThemeCss', JSON.stringify('body { color: red; }'));
await storage.activateAppStorageScope(SCOPE_A);
await storage.flushAppStorageWrites();

const {default: UserSettings} = await import('@app/features/user/state/UserSettings');
const {default: Accessibility} = await import('@app/features/accessibility/state/Accessibility');
const {default: EmojiPicker} = await import('@app/features/emoji/state/EmojiPicker');
const {AccessibilitySettingsSchema} = await import(
	'@fluxer/schema/src/gen/fluxer/user/preferences/v1/accessibility_pb'
);
const {SyncedPreferencesSchema, decodeSyncedPreferences, encodeSyncedPreferences} = await import(
	'@fluxer/schema/src/domains/user/SyncedPreferencesCodec'
);

type SyncedPreferencesInit = Parameters<typeof create<typeof SyncedPreferencesSchema>>[1];

async function settle(ms = 0): Promise<void> {
	for (let round = 0; round < 5; round++) {
		for (let i = 0; i < 20; i++) await Promise.resolve();
		await new Promise((resolve) => setTimeout(resolve, round === 0 ? ms : 0));
	}
}

function settingsPayload(preferences: SyncedPreferencesInit | null): Record<string, unknown> {
	return {
		status: 'online',
		flags: 0,
		locale: 'en-US',
		developer_mode: false,
		message_display_compact: false,
		restricted_guilds: [],
		bot_restricted_guilds: [],
		guild_folders: [],
		trusted_domains: [],
		staff_dm_access_user_ids: [],
		suppress_unprivileged_self_mentions_bypass_user_ids: [],
		synced_preferences:
			preferences === null ? null : encodeSyncedPreferences(create(SyncedPreferencesSchema, preferences)),
	};
}

function accessibility(showFavorites: boolean): SyncedPreferencesInit {
	return {
		accessibility: create(AccessibilitySettingsSchema, {showFavorites, showMediaFavoriteButton: showFavorites}),
	};
}

function pushedPreferences() {
	return mocks.patch.mock.calls.map((call) =>
		decodeSyncedPreferences((call[1] as {body: {synced_preferences: string | null}}).body.synced_preferences),
	);
}

function echoPatch(): void {
	mocks.patch.mockImplementation(async (_url: string, options: {body: {synced_preferences: string | null}}) => ({
		ok: true,
		status: 200,
		body: {synced_preferences: options.body.synced_preferences},
	}));
}

describe('synced fields across an account switch', () => {
	afterEach(() => {
		UserSettings.handleAccountTransition();
		mocks.patch.mockReset();
	});

	it("keeps the outgoing account's accessibility out of the next account when scope activation changes local state", async () => {
		echoPatch();
		UserSettings.handleGatewayReady(settingsPayload(accessibility(true)));
		await settle();
		expect(Accessibility.showFavorites).toBe(true);
		mocks.patch.mockClear();

		UserSettings.handleAccountTransition();
		await storage.activateAppStorageScope(SCOPE_B);
		await settle();
		expect(Accessibility.customThemeCss).toBe('body { color: red; }');

		UserSettings.handleGatewayReady(settingsPayload(accessibility(false)));
		await settle();

		expect(pushedPreferences().filter((preferences) => preferences.accessibility?.showFavorites === true)).toEqual([]);
		expect(Accessibility.showFavorites).toBe(false);
		expect(Accessibility.showMediaFavoriteButton).toBe(false);
	});

	it('drops a debounced emoji change that fires after the account switched', async () => {
		echoPatch();
		UserSettings.handleGatewayReady(
			settingsPayload({emojiPicker: {favoriteEmojiIds: ['unicode:a_one'], usage: {}, collapsedCategoryIds: []}}),
		);
		await settle(50);
		expect([...EmojiPicker.favoriteEmojis]).toEqual(['unicode:a_one']);
		mocks.patch.mockClear();

		EmojiPicker.toggleFavorite('unicode:a_two');
		UserSettings.handleAccountTransition();
		await settle(1600);
		UserSettings.handleGatewayReady(
			settingsPayload({emojiPicker: {favoriteEmojiIds: ['unicode:b_one'], usage: {}, collapsedCategoryIds: []}}),
		);
		await settle(1600);

		expect(pushedPreferences()).toEqual([]);
		expect([...EmojiPicker.favoriteEmojis]).toEqual(['unicode:b_one']);
		expect(UserSettings.getSubPreference('emojiPicker')?.favoriteEmojiIds).toEqual(['unicode:b_one']);
	});

	it('resets optional fields to their defaults and pushes nothing until the next account edits', async () => {
		echoPatch();
		UserSettings.handleGatewayReady(settingsPayload(accessibility(false)));
		await settle();
		expect(Accessibility.showFavorites).toBe(false);
		mocks.patch.mockClear();

		UserSettings.handleAccountTransition();
		await settle();
		expect(Accessibility.showFavorites).toBe(true);
		expect(Accessibility.showMediaFavoriteButton).toBe(true);

		UserSettings.handleGatewayReady(settingsPayload(null));
		await settle();
		expect(mocks.patch).not.toHaveBeenCalled();

		Accessibility.updateSettings({alwaysUnderlineLinks: true});
		await settle();
		const pushed = pushedPreferences().at(-1)?.accessibility;
		expect(pushed?.alwaysUnderlineLinks).toBe(true);
		expect(pushed?.showFavorites).toBe(true);
		expect(pushed?.showMediaFavoriteButton).toBe(true);
	});
});
