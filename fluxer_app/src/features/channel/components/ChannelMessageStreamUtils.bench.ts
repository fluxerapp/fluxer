// SPDX-License-Identifier: AGPL-3.0-or-later

import {getUnreadDividerBeforeMessageId} from '@app/features/channel/components/ChannelMessageStreamUtils';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import type {ChannelStreamItem} from '@app/features/messaging/utils/MessageGroupingUtils';
import {test} from 'vitest';

const GROUP_ITEMS = Array.from(
	{length: 100},
	(_value, index): ChannelStreamItem => ({
		type: 'MESSAGE',
		content: {
			id: `message-${index}`,
		} as Message,
		groupId: 'group-1',
		showUnreadDividerBefore: index === 74,
	}),
);

const GROUPS = Array.from({length: 1_000}, (_value, groupIndex) =>
	GROUP_ITEMS.map(
		(item, itemIndex): ChannelStreamItem => ({
			...item,
			content: {
				id: `message-${groupIndex}-${itemIndex}`,
			} as Message,
			showUnreadDividerBefore: itemIndex === groupIndex % GROUP_ITEMS.length,
		}),
	),
);

test('ChannelMessageStreamUtils benchmarks', async ({bench}) => {
	await bench('find unread divider id across 1k pending render groups', () => {
		let hits = 0;
		for (const group of GROUPS) {
			if (getUnreadDividerBeforeMessageId(group) !== null) {
				hits += 1;
			}
		}
		(globalThis as {__channelMessageStreamBenchSink?: number}).__channelMessageStreamBenchSink = hits;
	}).run();
});
