// SPDX-License-Identifier: AGPL-3.0-or-later

import {createChannelID} from '@app/api/BrandedTypes';
import {withChannelFollowLock} from '@app/api/channel/services/ChannelFollowers';
import {InMemoryProvider} from '@pkgs/cache/src/providers/InMemoryProvider';
import {describe, expect, it} from 'vitest';

describe('withChannelFollowLock', () => {
	it('keeps a long conversion lock alive past its ttl', async () => {
		const cache = new InMemoryProvider();
		const channelId = createChannelID(1n);
		const holder = withChannelFollowLock(
			cache,
			channelId,
			() => new Promise((resolve) => setTimeout(resolve, 1500)),
			0.6,
		);
		await new Promise((resolve) => setTimeout(resolve, 1200));
		expect(await cache.acquireLock(`channel-follow:${channelId}`, 1)).toBeNull();
		await holder;
		expect(await cache.acquireLock(`channel-follow:${channelId}`, 1)).not.toBeNull();
	});
});
