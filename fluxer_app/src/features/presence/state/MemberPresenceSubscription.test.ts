// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';

vi.mock('@app/features/gateway/transport/GatewayConnection', () => ({
	default: {
		socket: null,
	},
}));

type MemberPresenceSubscriptionInternals = {
	touchMember(guildId: string, userId: string): void;
	unsubscribe(guildId: string, userId: string): void;
	getSubscribedMembers(guildId: string): Array<string>;
	clearAll(): void;
	subscriptionRefs: Map<string, number>;
	unsubscribeTimeouts: Map<string, number>;
};

describe('MemberPresenceSubscription', () => {
	let store: MemberPresenceSubscriptionInternals | null = null;

	beforeEach(() => {
		vi.resetModules();
		vi.useFakeTimers();
	});

	afterEach(() => {
		store?.clearAll();
		store = null;
		vi.useRealTimers();
	});

	it('decrements refs when unsubscribe runs after LRU eviction', async () => {
		store = (await import('@app/features/presence/state/MemberPresenceSubscription')).default as unknown as MemberPresenceSubscriptionInternals;
		store.clearAll();

		const guildId = 'guild-1';
		const evictedUserId = 'user-0';

		for (let index = 0; index <= 100; index++) {
			store.touchMember(guildId, `user-${index}`);
		}

		expect(store.getSubscribedMembers(guildId)).not.toContain(evictedUserId);
		expect(store.subscriptionRefs.get(`${guildId}:${evictedUserId}`)).toBe(1);

		store.unsubscribe(guildId, evictedUserId);

		expect(store.subscriptionRefs.has(`${guildId}:${evictedUserId}`)).toBe(false);
		expect(store.unsubscribeTimeouts.has(`${guildId}:${evictedUserId}`)).toBe(false);

		store.touchMember(guildId, evictedUserId);
		store.unsubscribe(guildId, evictedUserId);
		vi.advanceTimersByTime(1_500);

		expect(store.subscriptionRefs.has(`${guildId}:${evictedUserId}`)).toBe(false);
		expect(store.getSubscribedMembers(guildId)).not.toContain(evictedUserId);
	});
});
