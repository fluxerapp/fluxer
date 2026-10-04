/**
 * Test Suite: Thread Notification Engine, Mention Routing & Subscription State Machine
 */

import {beforeEach, describe, expect, it} from 'vitest';
import {
	AutoArchiveDuration,
	type ThreadChannel,
	type ThreadMember,
	type ThreadMessage,
	ThreadNotificationEngine,
	ThreadNotificationLevel,
	ThreadState,
	ThreadType,
} from '../src';

describe('ThreadNotificationEngine', () => {
	let engine: ThreadNotificationEngine;

	beforeEach(() => {
		// Role resolver mock: user 'alice' has role 'role_engineers', 'bob' has 'role_designers'
		engine = new ThreadNotificationEngine((userId, roleId) => {
			if (userId === 'alice' && roleId === 'role_engineers') return true;
			if (userId === 'bob' && roleId === 'role_designers') return true;
			return false;
		});
	});

	const dummyThread: ThreadChannel = {
		id: 'th_notif_1',
		guildId: 'guild_1',
		parentId: 'chan_1',
		ownerId: 'alice',
		name: 'Notification Test Thread',
		type: ThreadType.PUBLIC_THREAD,
		state: ThreadState.ACTIVE,
		lastMessageId: null,
		messageCount: 0,
		memberCount: 3,
		rateLimitPerUser: 0,
		starterMessageId: null,
		createdAt: Date.now(),
		updatedAt: Date.now(),
		lastActiveTimestamp: Date.now(),
		totalMessagesSent: 0,
		threadMetadata: {
			archived: false,
			autoArchiveDuration: AutoArchiveDuration.ONE_DAY,
			archiveTimestamp: Date.now() + 86400000,
			locked: false,
			invitable: true,
			createTimestamp: Date.now(),
			archivedAt: null,
			lockedAt: null,
		},
	};

	const createMember = (
		userId: string,
		level = ThreadNotificationLevel.ALL_MESSAGES,
		overrides: Partial<ThreadMember> = {},
	): ThreadMember => ({
		id: `th_notif_1:${userId}`,
		threadId: 'th_notif_1',
		userId,
		joinTimestamp: Date.now(),
		flags: 0,
		notifications: {
			notificationLevel: level,
			mutedUntil: null,
			suppressEveryoneMentions: false,
			suppressRoleMentions: false,
		},
		lastReadMessageId: null,
		lastReadTimestamp: null,
		unreadCount: 0,
		muted: false,
		...overrides,
	});

	describe('Mention Parsing', () => {
		it('should parse user tags, role tags, and broadcast keywords', () => {
			const content = 'Hello <@user1> and <@!user2>, check <@&role_engineers> and @everyone and @bob!';
			const extracted = engine.extractMentions(content);

			expect(extracted.userIds.has('user1')).toBe(true);
			expect(extracted.userIds.has('user2')).toBe(true);
			expect(extracted.userIds.has('bob')).toBe(true);
			expect(extracted.roleIds.has('role_engineers')).toBe(true);
			expect(extracted.everyone).toBe(true);
		});
	});

	describe('Subscription State Machine & Mute Toggles', () => {
		it('should handle subscription updates and clear mutedUntil on unmute', () => {
			const initial = engine.createDefaultSubscription(ThreadNotificationLevel.MUTED);
			initial.mutedUntil = Date.now() + 3600000;

			const updated = engine.updateSubscription(initial, {
				notificationLevel: ThreadNotificationLevel.ALL_MESSAGES,
			});

			expect(updated.notificationLevel).toBe(ThreadNotificationLevel.ALL_MESSAGES);
			expect(updated.mutedUntil).toBeNull();
		});

		it('should accurately detect active mute states (permanent and time-bounded)', () => {
			const now = Date.now();
			const permMuted = createMember('user_perm', ThreadNotificationLevel.MUTED);
			expect(engine.isMemberMuted(permMuted, now)).toBe(true);

			const tempMuted = createMember('user_temp', ThreadNotificationLevel.MUTED);
			tempMuted.notifications.mutedUntil = now + 10000;
			expect(engine.isMemberMuted(tempMuted, now)).toBe(true);

			// After expiration
			expect(engine.isMemberMuted(tempMuted, now + 20000)).toBe(false);
		});
	});

	describe('Notification Routing Decisions', () => {
		it('should route direct mentions even if level is ONLY_MENTIONS', async () => {
			const alice = createMember('alice');
			const bob = createMember('bob', ThreadNotificationLevel.ONLY_MENTIONS);

			const msg: ThreadMessage = {
				id: 'msg_1',
				channelId: dummyThread.id,
				guildId: dummyThread.guildId,
				authorId: 'alice',
				content: 'Hey <@bob>, please review this PR',
				attachments: [],
				mentions: ['bob'],
				mentionRoles: [],
				mentionEveryone: false,
				replyCount: 0,
				createdAt: Date.now(),
				editedAt: null,
				pinned: false,
				reactions: new Map(),
				deleted: false,
			};

			const routing = await engine.evaluateNotifications(dummyThread, msg, [alice, bob]);
			const bobDecision = routing.get('bob');

			expect(bobDecision?.shouldAlert).toBe(true);
			expect(bobDecision?.reason).toBe('DIRECT_MENTION');
			// Alice is author, so no alert
			expect(routing.get('alice')).toBeUndefined();
		});

		it('should route role mentions to members possessing that role', async () => {
			const author = createMember('charlie');
			const alice = createMember('alice', ThreadNotificationLevel.ONLY_MENTIONS);
			const bob = createMember('bob', ThreadNotificationLevel.ONLY_MENTIONS);

			const msg: ThreadMessage = {
				id: 'msg_2',
				channelId: dummyThread.id,
				guildId: dummyThread.guildId,
				authorId: 'charlie',
				content: 'Attention <@&role_engineers>',
				attachments: [],
				mentions: [],
				mentionRoles: ['role_engineers'],
				mentionEveryone: false,
				replyCount: 0,
				createdAt: Date.now(),
				editedAt: null,
				pinned: false,
				reactions: new Map(),
				deleted: false,
			};

			const routing = await engine.evaluateNotifications(dummyThread, msg, [author, alice, bob]);
			expect(routing.get('alice')?.shouldAlert).toBe(true);
			expect(routing.get('alice')?.reason).toBe('ROLE_MENTION');

			expect(routing.get('bob')?.shouldAlert).toBe(false);
		});

		it('should suppress notifications when member is muted', async () => {
			const author = createMember('charlie');
			const bob = createMember('bob', ThreadNotificationLevel.MUTED);

			const msg: ThreadMessage = {
				id: 'msg_3',
				channelId: dummyThread.id,
				guildId: dummyThread.guildId,
				authorId: 'charlie',
				content: 'Hey <@bob> emergency!',
				attachments: [],
				mentions: ['bob'],
				mentionRoles: [],
				mentionEveryone: false,
				replyCount: 0,
				createdAt: Date.now(),
				editedAt: null,
				pinned: false,
				reactions: new Map(),
				deleted: false,
			};

			const routing = await engine.evaluateNotifications(dummyThread, msg, [author, bob]);
			expect(routing.get('bob')?.shouldAlert).toBe(false);
			expect(routing.get('bob')?.suppressed).toBe(true);
		});
	});

	describe('Unread Tracking and Last-Read Message Pointer', () => {
		it('should increment unread count on incoming message and clear upon markAsRead', () => {
			let member = createMember('bob');
			expect(member.unreadCount).toBe(0);

			const msg1: ThreadMessage = {
				id: 'm1',
				channelId: dummyThread.id,
				guildId: dummyThread.guildId,
				authorId: 'alice',
				content: 'First',
				attachments: [],
				mentions: [],
				mentionRoles: [],
				mentionEveryone: false,
				replyCount: 0,
				createdAt: 1000,
				editedAt: null,
				pinned: false,
				reactions: new Map(),
				deleted: false,
			};

			member = engine.processIncomingMessage(member, msg1);
			expect(member.unreadCount).toBe(1);

			const msg2 = {...msg1, id: 'm2', createdAt: 2000, content: 'Second'};
			member = engine.processIncomingMessage(member, msg2);
			expect(member.unreadCount).toBe(2);

			// Bob marks as read
			member = engine.markAsRead(member, 'm2', 2000);
			expect(member.unreadCount).toBe(0);
			expect(member.lastReadMessageId).toBe('m2');
			expect(member.lastReadTimestamp).toBe(2000);
		});
	});
});
