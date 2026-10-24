// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import {afterEach, describe, expect, it, vi} from 'vitest';

const mocks = vi.hoisted(() => ({get: vi.fn(), patch: vi.fn()}));

vi.mock('@lingui/core/macro', () => ({
	msg: (descriptor: unknown) => descriptor,
	t: (descriptor: unknown) => descriptor,
}));

vi.mock('@app/features/platform/transport/RestTransport', () => ({
	http: {get: mocks.get, post: vi.fn(), patch: mocks.patch},
}));

const {create} = await import('@bufbuild/protobuf');
const {HttpError} = await import('@app/features/platform/types/EndpointError');
const {APIErrorCodes} = await import('@fluxer/constants/src/ApiErrorCodes');
const {SyncedPreferencesSchema, decodeSyncedPreferences, encodeSyncedPreferences} = await import(
	'@fluxer/schema/src/domains/user/SyncedPreferencesCodec'
);
const {default: UserSettings} = await import('@app/features/user/state/UserSettings');

async function settle(): Promise<void> {
	for (let round = 0; round < 5; round++) {
		for (let i = 0; i < 20; i++) await Promise.resolve();
		await new Promise((resolve) => setTimeout(resolve, 0));
	}
}

function settingsPayload(syncedPreferences: string): Record<string, unknown> {
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
		synced_preferences: syncedPreferences,
	};
}

function hashOf(encoded: string): string {
	return createHash('sha256').update(Buffer.from(encoded, 'base64')).digest('hex');
}

function favorites(ids: Array<string>): string {
	return encodeSyncedPreferences(
		create(SyncedPreferencesSchema, {emojiPicker: {favoriteEmojiIds: ids, usage: {}, collapsedCategoryIds: []}}),
	);
}

function serve(
	initial: string,
	options: {alwaysConflict?: boolean} = {},
): {stored: () => string; replace: (next: string) => void} {
	let stored = initial;
	mocks.get.mockImplementation(async () => ({ok: true, status: 200, body: settingsPayload(stored)}));
	mocks.patch.mockImplementation(
		async (
			_url: string,
			request: {body: {synced_preferences: string | null; synced_preferences_base_hash?: string}},
		) => {
			if (options.alwaysConflict || request.body.synced_preferences_base_hash !== hashOf(stored)) {
				throw new HttpError({
					method: 'PATCH',
					path: '/users/@me/settings',
					status: 409,
					body: {code: APIErrorCodes.SYNCED_PREFERENCES_CONFLICT, message: 'conflict'},
				});
			}
			stored = request.body.synced_preferences ?? '';
			return {ok: true, status: 200, body: settingsPayload(stored)};
		},
	);
	return {
		stored: () => stored,
		replace: (next) => {
			stored = next;
		},
	};
}

describe('synced preferences optimistic concurrency', () => {
	afterEach(() => {
		UserSettings.handleAccountTransition();
		mocks.get.mockReset();
		mocks.patch.mockReset();
	});

	it('refetches on a conflict and keeps both the remote change and the local edit', async () => {
		const server = serve(favorites(['unicode:one']));
		UserSettings.handleGatewayReady(settingsPayload(server.stored()));
		await settle();
		server.replace(favorites(['unicode:one', 'unicode:two']));

		const saved = UserSettings.setSanitizeUrls(!UserSettings.getSanitizeUrls());
		await settle();
		await expect(saved).resolves.toBeUndefined();

		const stored = decodeSyncedPreferences(server.stored());
		expect(stored.emojiPicker?.favoriteEmojiIds).toEqual(['unicode:one', 'unicode:two']);
		expect(stored.sanitizeUrls).toBe(UserSettings.getSanitizeUrls());
		expect(UserSettings.getSubPreference('emojiPicker')?.favoriteEmojiIds).toEqual(['unicode:one', 'unicode:two']);
	});

	it('gives up after a bounded number of conflicts without writing', async () => {
		const initial = favorites(['unicode:one']);
		const server = serve(initial, {alwaysConflict: true});
		UserSettings.handleGatewayReady(settingsPayload(initial));
		await settle();

		const saved = expect(UserSettings.setSanitizeUrls(!UserSettings.getSanitizeUrls())).rejects.toMatchObject({
			status: 409,
		});
		await settle();

		await saved;
		expect(server.stored()).toBe(initial);
	});
});
