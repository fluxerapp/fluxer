// SPDX-License-Identifier: AGPL-3.0-or-later

import type {StateSnapshotEntries} from '@app/features/gateway/snapshot/SnapshotTypes';
import {buildBackgroundSnapshotReadySeed} from '@app/features/gateway/transport/BackgroundGatewaySnapshotHydration';
import {BackgroundReadStateReducer} from '@app/features/gateway/transport/BackgroundReadStateReducer';
import type {BackgroundMessageNotification} from '@app/features/gateway/transport/BackgroundSnapshotSink';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {MessageNotifications} from '@fluxer/constants/src/NotificationConstants';
import {RelationshipTypes} from '@fluxer/constants/src/UserConstants';
import {describe, expect, test} from 'vitest';

const USER_ID = '200';

function rows(values: Record<string, unknown>): Record<string, string> {
	return Object.fromEntries(Object.entries(values).map(([key, value]) => [key, JSON.stringify(value)]));
}

function demotionEntries(): StateSnapshotEntries {
	return {
		user: rows({[USER_ID]: {id: USER_ID, username: 'self'}}),
		guild: rows({
			'guild-1': {
				id: 'guild-1',
				properties: {name: 'Guild One', default_message_notifications: MessageNotifications.ONLY_MENTIONS},
				joined_at: null,
				member_count: 10,
			},
		}),
		channel: rows({
			'channel-1': {id: 'channel-1', guild_id: 'guild-1', type: ChannelTypes.GUILD_TEXT, name: 'general'},
			'dm-1': {id: 'dm-1', type: ChannelTypes.DM},
		}),
		guild_member: rows({[`guild-1:${USER_ID}`]: {user: {id: USER_ID}, roles: []}}),
		relationship: rows({'666': {id: '666', type: RelationshipTypes.BLOCKED}}),
		read_state: rows({
			'channel-1': {mentionCount: 0},
			'dm-1': {mentionCount: 0},
		}),
	};
}

describe('background snapshot ready seed', () => {
	test('mention counts come from the live counter, not the snapshot read state rows', () => {
		const seed = buildBackgroundSnapshotReadySeed(demotionEntries(), USER_ID, new Map([['channel-1', 3]]));

		expect(seed.read_states).toEqual([{id: 'channel-1', mention_count: 3}]);
	});

	test('a seeded reducer keeps guild names, notification defaults and blocked users', () => {
		const counts = new Map<string, number>();
		const notifications: Array<BackgroundMessageNotification> = [];
		const reducer = new BackgroundReadStateReducer({
			userId: USER_ID,
			observeMentionCounts: (mentionCounts) => {
				for (const [channelId, count] of mentionCounts) counts.set(channelId, count);
			},
			observeMessageNotification: (notification) => notifications.push(notification),
		});

		reducer.applyReady(buildBackgroundSnapshotReadySeed(demotionEntries(), USER_ID, new Map([['channel-1', 2]])));
		reducer.applyDispatch('MESSAGE_CREATE', {
			id: 'm-1',
			channel_id: 'channel-1',
			guild_id: 'guild-1',
			author: {id: '999'},
			mentions: [],
		});
		reducer.applyDispatch('MESSAGE_CREATE', {
			id: 'm-2',
			channel_id: 'channel-1',
			guild_id: 'guild-1',
			author: {id: '999'},
			mentions: [{id: USER_ID}],
		});
		reducer.applyDispatch('MESSAGE_CREATE', {id: 'm-3', channel_id: 'dm-1', author: {id: '666'}, mentions: []});

		expect(counts.get('channel-1')).toBe(3);
		expect(notifications.map((notification) => [notification.message.id, notification.guildName])).toEqual([
			['m-2', 'Guild One'],
		]);
	});
});
