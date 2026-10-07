// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {BackgroundReadStateReducer} from '@app/features/gateway/transport/BackgroundReadStateReducer';
import {
	type BackgroundMentionCountMode,
	type BackgroundMessageNotification,
	BackgroundMentionCountMode as MentionCountMode,
} from '@app/features/gateway/transport/BackgroundSnapshotSink';
import {ME} from '@fluxer/constants/src/AppConstants';
import {ChannelTypes, MessageFlags, MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import {MessageNotifications} from '@fluxer/constants/src/NotificationConstants';
import {RelationshipTypes} from '@fluxer/constants/src/UserConstants';
import {beforeEach, describe, expect, test} from 'vitest';

const USER_ID = '200';

class MentionCountRecorder {
	readonly counts = new Map<string, number>();

	observe = (mentionCounts: ReadonlyMap<string, number>, mode: BackgroundMentionCountMode): void => {
		if (mode === MentionCountMode.REPLACE) {
			for (const channelId of [...this.counts.keys()]) {
				if (!mentionCounts.has(channelId)) {
					this.counts.delete(channelId);
				}
			}
		}
		for (const [channelId, count] of mentionCounts) {
			if (count > 0) {
				this.counts.set(channelId, count);
			} else {
				this.counts.delete(channelId);
			}
		}
	};

	get total(): number {
		let total = 0;
		for (const count of this.counts.values()) {
			total += count;
		}
		return total;
	}
}

function readyPayload(overrides: Record<string, unknown> = {}): Record<string, unknown> {
	return {
		user: {id: USER_ID},
		guilds: [
			{
				id: 'guild-1',
				channels: [{id: 'channel-1'}, {id: 'channel-2'}],
				members: [{user: {id: USER_ID}, roles: ['role-1']}],
			},
			{id: 'guild-2', channels: [{id: 'channel-3'}], members: []},
		],
		user_guild_settings: [],
		read_states: [],
		...overrides,
	};
}

function guildMessage(channelId: string, overrides: Record<string, unknown> = {}): Record<string, unknown> {
	return {
		id: 'message-1',
		channel_id: channelId,
		guild_id: 'guild-1',
		author: {id: '999'},
		mentions: [{id: USER_ID}],
		...overrides,
	};
}

let recorder: MentionCountRecorder;
let notifications: Array<BackgroundMessageNotification>;
let reducer: BackgroundReadStateReducer;

beforeEach(() => {
	recorder = new MentionCountRecorder();
	notifications = [];
	reducer = new BackgroundReadStateReducer({
		userId: USER_ID,
		observeMentionCounts: recorder.observe,
		observeMessageNotification: (notification) => notifications.push(notification),
	});
});

describe('background read state reducer', () => {
	test('READY seeds the mention counts and replaces whatever was there', () => {
		reducer.applyReady(readyPayload({read_states: [{id: 'channel-1', mention_count: 3}]}));
		expect(recorder.counts.get('channel-1')).toBe(3);

		reducer.applyReady(readyPayload({read_states: [{id: 'channel-2', mention_count: 1}]}));

		expect(recorder.counts.has('channel-1')).toBe(false);
		expect(recorder.total).toBe(1);
	});

	test('a muted guild still contributes an explicit mention, exactly as the foreground badge does', () => {
		reducer.applyReady(readyPayload({user_guild_settings: [{guild_id: 'guild-1', muted: true}]}));

		reducer.applyDispatch('MESSAGE_CREATE', guildMessage('channel-1'));

		expect(recorder.counts.get('channel-1')).toBe(1);
	});

	test('a muted direct message is not counted', () => {
		reducer.applyReady(
			readyPayload({user_guild_settings: [{guild_id: ME, channel_overrides: [{channel_id: 'dm-1', muted: true}]}]}),
		);

		reducer.applyDispatch('MESSAGE_CREATE', {id: 'm', channel_id: 'dm-1', author: {id: '999'}, mentions: []});

		expect(recorder.total).toBe(0);
	});

	test('a direct message muted by the wire-shaped private-channel row is not counted', () => {
		reducer.applyReady(
			readyPayload({
				user_guild_settings: [
					{guild_id: null, channel_overrides: {'dm-1': {channel_id: 'dm-1', muted: true}}},
					{guild_id: 'guild-1', channel_overrides: null},
				],
			}),
		);

		reducer.applyDispatch('MESSAGE_CREATE', {id: 'm', channel_id: 'dm-1', author: {id: '999'}, mentions: []});

		expect(recorder.total).toBe(0);
	});

	test('an expired direct message mute stops suppressing messages', () => {
		reducer.applyReady(
			readyPayload({
				user_guild_settings: [
					{
						guild_id: ME,
						channel_overrides: [
							{channel_id: 'dm-1', muted: true, mute_config: {end_time: new Date(Date.now() - 1000).toISOString()}},
						],
					},
				],
			}),
		);

		reducer.applyDispatch('MESSAGE_CREATE', {id: 'm', channel_id: 'dm-1', author: {id: '999'}, mentions: []});

		expect(recorder.counts.get('dm-1')).toBe(1);
	});

	test('an everyone mention counts unless the guild suppresses it', () => {
		reducer.applyReady(readyPayload({user_guild_settings: [{guild_id: 'guild-1', suppress_everyone: true}]}));
		reducer.applyDispatch('MESSAGE_CREATE', guildMessage('channel-1', {mentions: [], mention_everyone: true}));
		expect(recorder.total).toBe(0);

		reducer.applyReady(readyPayload());
		reducer.applyDispatch('MESSAGE_CREATE', guildMessage('channel-1', {mentions: [], mention_everyone: true}));
		expect(recorder.counts.get('channel-1')).toBe(1);
	});

	test('a role mention counts only for a role the account actually holds', () => {
		reducer.applyReady(readyPayload());

		reducer.applyDispatch('MESSAGE_CREATE', guildMessage('channel-1', {mentions: [], mention_roles: ['role-9']}));
		expect(recorder.total).toBe(0);

		reducer.applyDispatch('MESSAGE_CREATE', guildMessage('channel-1', {mentions: [], mention_roles: ['role-1']}));
		expect(recorder.counts.get('channel-1')).toBe(1);
	});

	test('a direct message counts without an explicit mention', () => {
		reducer.applyReady(readyPayload());

		reducer.applyDispatch('MESSAGE_CREATE', {
			id: 'message-2',
			channel_id: 'dm-1',
			author: {id: '999'},
			mentions: [],
		});

		expect(recorder.counts.get('dm-1')).toBe(1);
	});

	test('a message the account sent itself never counts', () => {
		reducer.applyReady(readyPayload());

		reducer.applyDispatch('MESSAGE_CREATE', {
			id: 'message-3',
			channel_id: 'dm-1',
			author: {id: USER_ID},
			mentions: [],
		});

		expect(recorder.total).toBe(0);
	});

	test('an acknowledgement clears the channel', () => {
		reducer.applyReady(readyPayload({read_states: [{id: 'channel-1', mention_count: 4}]}));

		reducer.applyDispatch('MESSAGE_ACK', {channel_id: 'channel-1', message_id: 'message-1'});

		expect(recorder.counts.has('channel-1')).toBe(false);
	});

	test('an acknowledgement may carry a remaining mention count', () => {
		reducer.applyReady(readyPayload({read_states: [{id: 'channel-1', mention_count: 4}]}));

		reducer.applyDispatch('MESSAGE_ACK', {channel_id: 'channel-1', message_id: 'message-1', mention_count: 2});

		expect(recorder.counts.get('channel-1')).toBe(2);
	});

	test('CHANNEL_DELETE clears that channel only', () => {
		reducer.applyReady(
			readyPayload({
				read_states: [
					{id: 'channel-1', mention_count: 2},
					{id: 'channel-2', mention_count: 1},
				],
			}),
		);

		reducer.applyDispatch('CHANNEL_DELETE', {id: 'channel-1', guild_id: 'guild-1'});

		expect(recorder.counts.has('channel-1')).toBe(false);
		expect(recorder.counts.get('channel-2')).toBe(1);
	});

	test('GUILD_DELETE removes every channel of that guild and nothing else', () => {
		reducer.applyReady(
			readyPayload({
				read_states: [
					{id: 'channel-1', mention_count: 2},
					{id: 'channel-2', mention_count: 1},
					{id: 'channel-3', mention_count: 5},
				],
			}),
		);

		reducer.applyDispatch('GUILD_DELETE', {id: 'guild-1'});

		expect(recorder.counts.has('channel-1')).toBe(false);
		expect(recorder.counts.has('channel-2')).toBe(false);
		expect(recorder.counts.get('channel-3')).toBe(5);
	});

	test('an unavailable GUILD_DELETE is an outage and keeps the counts', () => {
		reducer.applyReady(readyPayload({read_states: [{id: 'channel-1', mention_count: 2}]}));

		reducer.applyDispatch('GUILD_DELETE', {id: 'guild-1', unavailable: true});

		expect(recorder.counts.get('channel-1')).toBe(2);
	});

	test('USER_GUILD_SETTINGS_UPDATE starts suppressing everyone mentions for later messages', () => {
		reducer.applyReady(readyPayload());

		reducer.applyDispatch('USER_GUILD_SETTINGS_UPDATE', {guild_id: 'guild-1', suppress_everyone: true});
		reducer.applyDispatch('MESSAGE_CREATE', guildMessage('channel-1', {mentions: [], mention_everyone: true}));

		expect(recorder.total).toBe(0);
	});

	test('USER_GUILD_SETTINGS_UPDATE mutes a direct message for later messages', () => {
		reducer.applyReady(readyPayload());

		reducer.applyDispatch('USER_GUILD_SETTINGS_UPDATE', {
			guild_id: ME,
			channel_overrides: [{channel_id: 'dm-1', muted: true}],
		});
		reducer.applyDispatch('MESSAGE_CREATE', {id: 'm', channel_id: 'dm-1', author: {id: '999'}, mentions: []});

		expect(recorder.total).toBe(0);
	});

	test('USER_GUILD_SETTINGS_UPDATE with a null guild id mutes a direct message', () => {
		reducer.applyReady(readyPayload());

		reducer.applyDispatch('USER_GUILD_SETTINGS_UPDATE', {
			guild_id: null,
			channel_overrides: {'dm-1': {channel_id: 'dm-1', muted: true}},
		});
		reducer.applyDispatch('MESSAGE_CREATE', {id: 'm', channel_id: 'dm-1', author: {id: '999'}, mentions: []});

		expect(recorder.total).toBe(0);
	});

	test('reset clears everything it emitted', () => {
		reducer.applyReady(readyPayload({read_states: [{id: 'channel-1', mention_count: 2}]}));

		reducer.reset();

		expect(recorder.total).toBe(0);
	});

	test('a malformed payload is ignored rather than thrown on', () => {
		reducer.applyReady(readyPayload({read_states: [{id: 'channel-1', mention_count: 2}]}));

		reducer.applyDispatch('MESSAGE_CREATE', null);
		reducer.applyDispatch('MESSAGE_ACK', {});
		reducer.applyDispatch('GUILD_DELETE', {});
		reducer.applyDispatch('UNRELATED_EVENT', {id: 'guild-1'});

		expect(recorder.counts.get('channel-1')).toBe(2);
	});

	test('a mention from a blocked author is neither counted nor notified', () => {
		reducer.applyReady(readyPayload({relationships: [{id: '999', type: RelationshipTypes.BLOCKED}]}));

		reducer.applyDispatch('MESSAGE_CREATE', guildMessage('channel-1'));

		expect(recorder.total).toBe(0);
		expect(notifications).toHaveLength(0);
	});
});

function notificationReady(overrides: Record<string, unknown> = {}): Record<string, unknown> {
	return readyPayload({
		guilds: [
			{
				id: 'guild-1',
				properties: {name: 'Guild One', default_message_notifications: MessageNotifications.ONLY_MENTIONS},
				member_count: 10,
				channels: [
					{id: 'category-1', type: ChannelTypes.GUILD_CATEGORY, name: 'Category'},
					{id: 'channel-1', type: ChannelTypes.GUILD_TEXT, name: 'general', parent_id: 'category-1'},
					{id: 'channel-2', type: ChannelTypes.GUILD_TEXT, name: 'random'},
				],
				members: [{user: {id: USER_ID}, roles: ['role-1']}],
			},
		],
		private_channels: [{id: 'group-1', type: ChannelTypes.GROUP_DM, name: 'Friends'}],
		...overrides,
	});
}

describe('background message notifications', () => {
	test('a mention in an only-mentions guild notifies with the guild and channel names', () => {
		reducer.applyReady(notificationReady());

		reducer.applyDispatch('MESSAGE_CREATE', guildMessage('channel-1', {type: MessageTypes.DEFAULT, flags: 0}));

		expect(notifications).toHaveLength(1);
		expect(notifications[0]).toMatchObject({
			guildName: 'Guild One',
			channelName: 'general',
			channelType: ChannelTypes.GUILD_TEXT,
		});
		expect(notifications[0].message.id).toBe('message-1');
	});

	test('a plain message in an only-mentions guild does not notify', () => {
		reducer.applyReady(notificationReady());

		reducer.applyDispatch('MESSAGE_CREATE', guildMessage('channel-1', {mentions: []}));

		expect(notifications).toHaveLength(0);
	});

	test('the guild default applies until the account overrides it, and a category override wins over it', () => {
		reducer.applyReady(
			notificationReady({
				user_guild_settings: [
					{
						guild_id: 'guild-1',
						message_notifications: MessageNotifications.ALL_MESSAGES,
						channel_overrides: [{channel_id: 'category-1', message_notifications: MessageNotifications.NO_MESSAGES}],
					},
				],
			}),
		);

		reducer.applyDispatch('MESSAGE_CREATE', guildMessage('channel-2', {id: 'm-2', mentions: []}));
		reducer.applyDispatch('MESSAGE_CREATE', guildMessage('channel-1', {id: 'm-1'}));

		expect(notifications.map((notification) => notification.message.id)).toEqual(['m-2']);
	});

	test('a large guild falls back to only mentions even when its default is all messages', () => {
		reducer.applyReady(
			notificationReady({
				guilds: [
					{
						id: 'guild-1',
						properties: {name: 'Big', default_message_notifications: MessageNotifications.ALL_MESSAGES},
						member_count: 5000,
						channels: [{id: 'channel-1', type: ChannelTypes.GUILD_TEXT, name: 'general'}],
						members: [],
					},
				],
			}),
		);

		reducer.applyDispatch('MESSAGE_CREATE', guildMessage('channel-1', {mentions: []}));

		expect(notifications).toHaveLength(0);
	});

	test('a muted guild never notifies, even for a mention', () => {
		reducer.applyReady(notificationReady({user_guild_settings: [{guild_id: 'guild-1', muted: true}]}));

		reducer.applyDispatch('MESSAGE_CREATE', guildMessage('channel-1'));

		expect(recorder.counts.get('channel-1')).toBe(1);
		expect(notifications).toHaveLength(0);
	});

	test('suppressed and system messages do not notify', () => {
		reducer.applyReady(notificationReady());

		reducer.applyDispatch(
			'MESSAGE_CREATE',
			guildMessage('channel-1', {id: 'm-1', flags: MessageFlags.SUPPRESS_NOTIFICATIONS}),
		);
		reducer.applyDispatch('MESSAGE_CREATE', guildMessage('channel-1', {id: 'm-2', type: 7}));

		expect(notifications).toHaveLength(0);
	});

	test('a group DM message notifies with the conversation name and a direct message notifies without one', () => {
		reducer.applyReady(notificationReady());

		reducer.applyDispatch('MESSAGE_CREATE', {id: 'm-1', channel_id: 'group-1', author: {id: '999'}, mentions: []});
		reducer.applyDispatch('MESSAGE_CREATE', {id: 'm-2', channel_id: 'dm-1', author: {id: '999'}, mentions: []});

		expect(notifications.map(({channelName, channelType}) => ({channelName, channelType}))).toEqual([
			{channelName: 'Friends', channelType: ChannelTypes.GROUP_DM},
			{channelName: null, channelType: null},
		]);
	});

	test('a direct message set to no messages does not notify', () => {
		reducer.applyReady(
			notificationReady({
				user_guild_settings: [{guild_id: null, message_notifications: MessageNotifications.NO_MESSAGES}],
			}),
		);

		reducer.applyDispatch('MESSAGE_CREATE', {id: 'm-1', channel_id: 'dm-1', author: {id: '999'}, mentions: []});

		expect(notifications).toHaveLength(0);
	});

	test('a guild that arrives after READY is learned from GUILD_CREATE', () => {
		reducer.applyReady(readyPayload({guilds: [{id: 'guild-9', unavailable: true}]}));

		reducer.applyDispatch('GUILD_CREATE', {
			id: 'guild-9',
			properties: {name: 'Late', default_message_notifications: MessageNotifications.ONLY_MENTIONS},
			member_count: 3,
			channels: [{id: 'late-1', type: ChannelTypes.GUILD_TEXT, name: 'late'}],
			members: [{user: {id: USER_ID}, roles: ['late-role']}],
		});
		reducer.applyDispatch(
			'MESSAGE_CREATE',
			guildMessage('late-1', {guild_id: 'guild-9', mentions: [], mention_roles: ['late-role']}),
		);

		expect(recorder.counts.get('late-1')).toBe(1);
		expect(notifications[0]).toMatchObject({guildName: 'Late', channelName: 'late'});
	});

	test('unblocking an author lets their messages notify again', () => {
		reducer.applyReady(notificationReady({relationships: [{id: '999', type: RelationshipTypes.BLOCKED}]}));

		reducer.applyDispatch('RELATIONSHIP_REMOVE', {id: '999'});
		reducer.applyDispatch('MESSAGE_CREATE', guildMessage('channel-1'));

		expect(notifications).toHaveLength(1);
	});

	test('a mention inside a thread counts toward the background badge and names the thread', () => {
		reducer.applyReady(
			readyPayload({
				guilds: [
					{
						id: 'guild-1',
						channels: [{id: 'channel-1', name: 'general', type: ChannelTypes.GUILD_TEXT}],
						threads: [{id: 'thread-1', name: 'plans', parent_id: 'channel-1', type: ChannelTypes.PUBLIC_THREAD}],
						members: [{user: {id: USER_ID}, roles: []}],
					},
				],
			}),
		);

		reducer.applyDispatch('MESSAGE_CREATE', guildMessage('thread-1'));

		expect(recorder.counts.get('thread-1')).toBe(1);
		expect(notifications.at(-1)?.channelName).toBe('plans');
	});

	test('a thread under a muted channel stays quiet for plain messages', () => {
		reducer.applyReady(
			readyPayload({
				user_guild_settings: [
					{guild_id: 'guild-1', channel_overrides: [{channel_id: 'channel-1', muted: true}], message_notifications: 0},
				],
			}),
		);
		reducer.applyDispatch('THREAD_CREATE', {
			id: 'thread-2',
			guild_id: 'guild-1',
			parent_id: 'channel-1',
			type: ChannelTypes.PUBLIC_THREAD,
		});

		reducer.applyDispatch('MESSAGE_CREATE', guildMessage('thread-2', {mentions: []}));

		expect(recorder.total).toBe(0);
		expect(notifications).toHaveLength(0);
	});
});
