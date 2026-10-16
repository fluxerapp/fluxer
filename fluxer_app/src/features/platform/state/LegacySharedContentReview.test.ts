// SPDX-License-Identifier: AGPL-3.0-or-later

import {beforeEach, describe, expect, test, vi} from 'vitest';

const REVIEW_KEY = 'fluxer:migration:shared-content-review';

const mocks = vi.hoisted(() => ({
	channels: new Set<string>(),
	unavailableGuilds: new Set<string>(),
	drafts: new Map<string, string>(),
	storage: new Map<string, string>(),
	scope: 'primary.test::200',
	hydration: Promise.resolve(),
	abandonUnreachableLastLocation: vi.fn<() => void>(),
}));

vi.mock('@app/features/channel/state/Channels', () => ({
	default: {getChannel: (id: string) => (mocks.channels.has(id) ? {id} : undefined)},
}));

vi.mock('@app/features/guild/state/GuildAvailability', () => ({
	default: {unavailableGuilds: mocks.unavailableGuilds},
}));

vi.mock('@app/features/messaging/state/MessagingDrafts', () => ({
	default: {
		getAllDrafts: () => [...mocks.drafts.entries()],
		deleteDraft: (channelId: string) => {
			mocks.drafts.delete(channelId);
		},
	},
}));

vi.mock('@app/features/navigation/utils/ChannelRouteReachability', () => ({
	abandonUnreachableLastLocation: mocks.abandonUnreachableLastLocation,
}));

vi.mock('@app/features/platform/state/PersistentStorage', () => ({
	default: {
		getItem: (key: string) => mocks.storage.get(key) ?? null,
		setItem: (key: string, value: string) => {
			mocks.storage.set(key, value);
		},
	},
	getAppStorageScope: () => mocks.scope,
}));

vi.mock('@app/features/platform/utils/MobXPersistence', () => ({
	awaitHydration: () => mocks.hydration,
}));

import {reviewLegacySharedContent} from '@app/features/platform/state/LegacySharedContentReview';

const SHARED_CHANNEL = '110000000000000001';
const FOREIGN_DM = '210000000000000001';

beforeEach(() => {
	mocks.channels.clear();
	mocks.unavailableGuilds.clear();
	mocks.drafts.clear();
	mocks.storage.clear();
	mocks.scope = 'primary.test::200';
	mocks.hydration = Promise.resolve();
	mocks.abandonUnreachableLastLocation.mockReset();
	mocks.channels.add(SHARED_CHANNEL);
	mocks.drafts.set(SHARED_CHANNEL, 'for a channel both accounts share');
	mocks.drafts.set(FOREIGN_DM, 'for a conversation only the other account has');
});

describe('reviewing shared content carried from the legacy client', () => {
	test('a pending account keeps the drafts it can open and drops the ones it cannot', async () => {
		mocks.storage.set(REVIEW_KEY, 'pending');

		await reviewLegacySharedContent();

		expect([...mocks.drafts.keys()]).toEqual([SHARED_CHANNEL]);
		expect(mocks.abandonUnreachableLastLocation).toHaveBeenCalledTimes(1);
		expect(mocks.storage.get(REVIEW_KEY)).toBe('done');
	});

	test('an account that received no legacy content is never reviewed', async () => {
		await reviewLegacySharedContent();

		expect([...mocks.drafts.keys()]).toEqual([SHARED_CHANNEL, FOREIGN_DM]);
		expect(mocks.abandonUnreachableLastLocation).not.toHaveBeenCalled();
		expect(mocks.storage.has(REVIEW_KEY)).toBe(false);
	});

	test('a reviewed account is never reviewed again', async () => {
		mocks.storage.set(REVIEW_KEY, 'done');

		await reviewLegacySharedContent();

		expect([...mocks.drafts.keys()]).toEqual([SHARED_CHANNEL, FOREIGN_DM]);
		expect(mocks.abandonUnreachableLastLocation).not.toHaveBeenCalled();
	});

	test('the review waits for a later READY while any guild is unavailable', async () => {
		mocks.storage.set(REVIEW_KEY, 'pending');
		mocks.unavailableGuilds.add('120000000000000001');

		await reviewLegacySharedContent();

		expect([...mocks.drafts.keys()]).toEqual([SHARED_CHANNEL, FOREIGN_DM]);
		expect(mocks.storage.get(REVIEW_KEY)).toBe('pending');

		mocks.unavailableGuilds.clear();
		await reviewLegacySharedContent();

		expect([...mocks.drafts.keys()]).toEqual([SHARED_CHANNEL]);
		expect(mocks.storage.get(REVIEW_KEY)).toBe('done');
	});

	test('a switch to another account while the stores hydrate leaves that account untouched', async () => {
		mocks.storage.set(REVIEW_KEY, 'pending');
		let release: () => void = () => {};
		mocks.hydration = new Promise<void>((resolve) => {
			release = resolve;
		});

		const review = reviewLegacySharedContent();
		mocks.scope = 'primary.test::100';
		release();
		await review;

		expect([...mocks.drafts.keys()]).toEqual([SHARED_CHANNEL, FOREIGN_DM]);
		expect(mocks.abandonUnreachableLastLocation).not.toHaveBeenCalled();
		expect(mocks.storage.get(REVIEW_KEY)).toBe('pending');
	});
});
