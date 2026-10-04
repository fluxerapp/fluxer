/**
 * Test Suite: Gateway WebSocket Payload Serialization & Event Formatting
 */

import {beforeEach, describe, expect, it} from 'vitest';
import {
	AutoArchiveDuration,
	GatewayThreadEventType,
	type ThreadChannel,
	ThreadEventSerializer,
	type ThreadMember,
	ThreadNotificationLevel,
	ThreadState,
	ThreadType,
} from '../src';

describe('ThreadEventSerializer', () => {
	beforeEach(() => {
		ThreadEventSerializer.resetSequence();
	});

	const sampleThread: ThreadChannel = {
		id: 'th_test_100',
		guildId: 'guild_100',
		parentId: 'chan_100',
		ownerId: 'user_dev',
		name: 'General Architecture',
		type: ThreadType.PUBLIC_THREAD,
		state: ThreadState.ACTIVE,
		lastMessageId: 'msg_999',
		messageCount: 15,
		memberCount: 5,
		rateLimitPerUser: 10,
		starterMessageId: 'msg_starter_01',
		createdAt: 1700000000000,
		updatedAt: 1700000500000,
		lastActiveTimestamp: 1700000500000,
		totalMessagesSent: 15,
		threadMetadata: {
			archived: false,
			autoArchiveDuration: AutoArchiveDuration.ONE_DAY,
			archiveTimestamp: 1700086400000,
			locked: false,
			invitable: true,
			createTimestamp: 1700000000000,
			archivedAt: null,
			lockedAt: null,
		},
	};

	const sampleMember: ThreadMember = {
		id: 'th_test_100:user_dev',
		threadId: 'th_test_100',
		userId: 'user_dev',
		joinTimestamp: 1700000000000,
		flags: 0,
		notifications: {
			notificationLevel: ThreadNotificationLevel.ALL_MESSAGES,
			mutedUntil: null,
			suppressEveryoneMentions: false,
			suppressRoleMentions: false,
		},
		lastReadMessageId: 'msg_999',
		lastReadTimestamp: 1700000500000,
		unreadCount: 0,
		muted: false,
	};

	it('should format THREAD_CREATE event with correct envelope and sequence', () => {
		const event = ThreadEventSerializer.formatThreadCreate(sampleThread, true);
		expect(event.op).toBe(0);
		expect(event.t).toBe(GatewayThreadEventType.THREAD_CREATE);
		expect(event.s).toBe(1);
		expect(event.d.id).toBe(sampleThread.id);
		expect(event.d.newly_created).toBe(true);
		expect(event.d.thread_metadata.archived).toBe(false);
		expect(event.d.rate_limit_per_user).toBe(10);
	});

	it('should format THREAD_UPDATE event', () => {
		const updatedThread: ThreadChannel = {
			...sampleThread,
			name: 'Renamed Thread',
			threadMetadata: {
				...sampleThread.threadMetadata,
				archived: true,
				archivedAt: 1700001000000,
			},
		};

		const event = ThreadEventSerializer.formatThreadUpdate(updatedThread);
		expect(event.t).toBe(GatewayThreadEventType.THREAD_UPDATE);
		expect(event.s).toBe(1);
		expect(event.d.name).toBe('Renamed Thread');
		expect(event.d.thread_metadata.archived).toBe(true);
		expect(event.d.thread_metadata.archived_at).not.toBeNull();
	});

	it('should format THREAD_DELETE event', () => {
		const event = ThreadEventSerializer.formatThreadDelete(sampleThread);
		expect(event.t).toBe(GatewayThreadEventType.THREAD_DELETE);
		expect(event.d.id).toBe(sampleThread.id);
		expect(event.d.guild_id).toBe(sampleThread.guildId);
		expect(event.d.parent_id).toBe(sampleThread.parentId);
	});

	it('should format THREAD_LIST_SYNC event', () => {
		const event = ThreadEventSerializer.formatThreadListSync({
			guildId: 'guild_100',
			channelIds: ['chan_100'],
			threads: [sampleThread],
			members: [sampleMember],
		});

		expect(event.t).toBe(GatewayThreadEventType.THREAD_LIST_SYNC);
		expect(event.d.guild_id).toBe('guild_100');
		expect(event.d.threads).toHaveLength(1);
		expect(event.d.members).toHaveLength(1);
	});

	it('should format THREAD_MEMBER_UPDATE event', () => {
		const event = ThreadEventSerializer.formatThreadMemberUpdate(sampleMember, 'guild_100');
		expect(event.t).toBe(GatewayThreadEventType.THREAD_MEMBER_UPDATE);
		expect(event.d.user_id).toBe('user_dev');
		expect(event.d.guild_id).toBe('guild_100');
	});

	it('should format THREAD_MEMBERS_UPDATE event with added and removed lists', () => {
		const event = ThreadEventSerializer.formatThreadMembersUpdate(
			sampleThread.id,
			sampleThread.guildId,
			6,
			[sampleMember],
			['user_removed_1'],
		);

		expect(event.t).toBe(GatewayThreadEventType.THREAD_MEMBERS_UPDATE);
		expect(event.d.id).toBe(sampleThread.id);
		expect(event.d.member_count).toBe(6);
		expect(event.d.added_members).toHaveLength(1);
		expect(event.d.removed_member_ids).toEqual(['user_removed_1']);
	});

	it('should serialize to JSON and parse gateway packet accurately', () => {
		const event = ThreadEventSerializer.formatThreadCreate(sampleThread);
		const jsonStr = ThreadEventSerializer.toJSONString(event);

		expect(typeof jsonStr).toBe('string');
		const parsed = ThreadEventSerializer.parseGatewayEvent(jsonStr);

		expect(parsed.op).toBe(0);
		expect(parsed.t).toBe(GatewayThreadEventType.THREAD_CREATE);
		expect((parsed.d as any).id).toBe(sampleThread.id);
	});

	it('should throw an error when parsing an invalid gateway packet', () => {
		expect(() => ThreadEventSerializer.parseGatewayEvent('invalid json')).toThrow();
		expect(() => ThreadEventSerializer.parseGatewayEvent('{"foo": "bar"}')).toThrow('missing op, t, or d fields');
	});
});
