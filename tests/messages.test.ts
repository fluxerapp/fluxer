/**
 * Test Suite: Thread Message Dispatcher, Cursor Pagination, Parent Sync & Slowmode
 */

import {beforeEach, describe, expect, it} from 'vitest';
import {
	AutoArchiveDuration,
	MessageNotFoundError,
	RateLimitExceededError,
	ThreadArchivedError,
	ThreadLockedError,
	ThreadManager,
	type ThreadMessageService,
	ThreadState,
	ThreadType,
} from '../src';

describe('Thread Messages & Cursor Pagination', () => {
	let manager: ThreadManager;
	let messageService: ThreadMessageService;
	const channelId = 'chan_text_1';
	const guildId = 'guild_main';
	const user1 = 'user_alice';
	const user2 = 'user_bob';

	beforeEach(() => {
		manager = new ThreadManager();
		messageService = manager.getMessageService();
	});

	describe('dispatchMessage and Parent Message Sync', () => {
		it('should dispatch messages in thread and update parent starter message reply counters', async () => {
			const parentMsgId = 'msg_starter_100';

			const thread = await manager.createThreadFromMessage(
				channelId,
				parentMsgId,
				'Project Architecture Discussion',
				AutoArchiveDuration.ONE_DAY,
				user1,
				{guildId},
			);

			// Verify initial parent stats
			let parentStats = messageService.getParentMessageStats(channelId, parentMsgId);
			expect(parentStats?.replyCount).toBe(0);
			expect(parentStats?.threadId).toBe(thread.id);

			// Send first message
			const res1 = await manager.sendThreadMessage(thread.id, user1, 'Hello thread world!');
			expect(res1.message.content).toBe('Hello thread world!');
			expect(res1.thread.messageCount).toBe(1);
			expect(res1.thread.lastMessageId).toBe(res1.message.id);

			parentStats = messageService.getParentMessageStats(channelId, parentMsgId);
			expect(parentStats?.replyCount).toBe(1);
			expect(parentStats?.lastReplyId).toBe(res1.message.id);

			// Send second message from another user
			const res2 = await manager.sendThreadMessage(thread.id, user2, 'Excited to be here!');
			expect(res2.thread.messageCount).toBe(2);

			parentStats = messageService.getParentMessageStats(channelId, parentMsgId);
			expect(parentStats?.replyCount).toBe(2);
			expect(parentStats?.lastReplyId).toBe(res2.message.id);
		});

		it('should auto-unarchive an archived thread upon sending a message if autoUnarchive is true', async () => {
			const thread = await manager.createStandaloneThread(
				channelId,
				'Auto unarchive thread',
				ThreadType.PUBLIC_THREAD,
				AutoArchiveDuration.ONE_DAY,
				user1,
				{guildId},
			);

			await manager.archiveThread(thread.id, user1);
			expect(manager.getThread(thread.id)?.state).toBe(ThreadState.ARCHIVED);

			// Sending a message should unarchive it
			const res = await manager.sendThreadMessage(thread.id, user1, 'Waking up the thread!', {
				autoUnarchive: true,
			});

			expect(res.thread.state).toBe(ThreadState.ACTIVE);
			expect(res.thread.threadMetadata.archived).toBe(false);
		});

		it('should reject message in archived thread if autoUnarchive is false', async () => {
			const thread = await manager.createStandaloneThread(
				channelId,
				'Archived thread test',
				ThreadType.PUBLIC_THREAD,
				AutoArchiveDuration.ONE_DAY,
				user1,
				{guildId},
			);

			await manager.archiveThread(thread.id, user1);
			const archivedThread = manager.getThread(thread.id)!;

			await expect(
				messageService.dispatchMessage(archivedThread, user1, 'Should fail', {
					autoUnarchive: false,
				}),
			).rejects.toThrow(ThreadArchivedError);
		});

		it('should reject message in locked thread without bypass permission', async () => {
			const thread = await manager.createStandaloneThread(
				channelId,
				'Locked thread test',
				ThreadType.PUBLIC_THREAD,
				AutoArchiveDuration.ONE_DAY,
				user1,
				{guildId},
			);

			await manager.archiveThread(thread.id, user1, true);
			const lockedThread = manager.getThread(thread.id)!;

			await expect(messageService.dispatchMessage(lockedThread, user2, 'Trying to post in locked')).rejects.toThrow(
				ThreadLockedError,
			);

			// Allowed with bypassLock
			const bypassRes = await messageService.dispatchMessage(lockedThread, user1, 'Moderator announcement', {
				bypassLock: true,
			});
			expect(bypassRes.message.content).toBe('Moderator announcement');
		});
	});

	describe('Slowmode Rate-limiting', () => {
		it('should enforce per-user rate limit within a thread', async () => {
			const thread = await manager.createStandaloneThread(
				channelId,
				'Slowmode 5s Thread',
				ThreadType.PUBLIC_THREAD,
				AutoArchiveDuration.ONE_DAY,
				user1,
				{guildId, rateLimitPerUser: 5},
			);

			// First message succeeds
			await manager.sendThreadMessage(thread.id, user1, 'Message 1');

			// Immediate second message should trigger RateLimitExceededError
			await expect(manager.sendThreadMessage(thread.id, user1, 'Message 2 too fast')).rejects.toThrow(
				RateLimitExceededError,
			);

			// Different user is not rate-limited by user1's bucket
			const user2Msg = await manager.sendThreadMessage(thread.id, user2, 'User2 first message');
			expect(user2Msg.message.content).toBe('User2 first message');
		});
	});

	describe('Cursor Pagination (before, after, around, limit)', () => {
		let threadId: string;
		let messageIds: Array<string> = [];

		beforeEach(async () => {
			messageIds = [];
			const thread = await manager.createStandaloneThread(
				channelId,
				'Pagination Test Bed',
				ThreadType.PUBLIC_THREAD,
				AutoArchiveDuration.ONE_DAY,
				user1,
				{guildId},
			);
			threadId = thread.id;

			// Seed 20 sequential messages
			for (let i = 1; i <= 20; i++) {
				const res = await messageService.dispatchMessage(
					manager.getThread(threadId)!,
					user1,
					`Message #${i.toString().padStart(2, '0')}`,
					{bypassRateLimit: true},
				);
				messageIds.push(res.message.id);
			}
		});

		it('should paginate default tail messages with limit', () => {
			const result = messageService.getMessages(threadId, {limit: 5});
			expect(result.items).toHaveLength(5);
			expect(result.items[4].id).toBe(messageIds[19]);
			expect(result.totalCount).toBe(20);
			expect(result.hasMore).toBe(true);
		});

		it('should paginate using before cursor', () => {
			const targetId = messageIds[10]; // 11th message (index 10)
			const result = messageService.getMessages(threadId, {before: targetId, limit: 5});
			expect(result.items).toHaveLength(5);
			// Items should be indices 5, 6, 7, 8, 9
			expect(result.items[0].id).toBe(messageIds[5]);
			expect(result.items[4].id).toBe(messageIds[9]);
		});

		it('should paginate using after cursor', () => {
			const targetId = messageIds[14]; // 15th message
			const result = messageService.getMessages(threadId, {after: targetId, limit: 5});
			expect(result.items).toHaveLength(5);
			// Items should be indices 15, 16, 17, 18, 19
			expect(result.items[0].id).toBe(messageIds[15]);
			expect(result.items[4].id).toBe(messageIds[19]);
			expect(result.hasMore).toBe(false);
		});

		it('should paginate centered around a target message', () => {
			const targetId = messageIds[10];
			const result = messageService.getMessages(threadId, {around: targetId, limit: 5});
			expect(result.items).toHaveLength(5);
			// Center around index 10: indices 8, 9, 10, 11, 12
			expect(result.items.some((m) => m.id === targetId)).toBe(true);
		});

		it('should throw MessageNotFoundError for invalid cursor', () => {
			expect(() => messageService.getMessages(threadId, {before: 'msg_non_existent'})).toThrow(MessageNotFoundError);
		});
	});

	describe('Reactions and Message Deletion', () => {
		it('should add, aggregate and remove emoji reactions', async () => {
			const thread = await manager.createStandaloneThread(
				channelId,
				'Reactions Thread',
				ThreadType.PUBLIC_THREAD,
				AutoArchiveDuration.ONE_DAY,
				user1,
				{guildId},
			);

			const res = await messageService.dispatchMessage(thread, user1, 'Vote on this!');
			const msgId = res.message.id;

			// Alice reacts 👍
			const r1 = messageService.addReaction(thread.id, msgId, user1, '👍');
			expect(r1.count).toBe(1);
			expect(r1.users.has(user1)).toBe(true);

			// Bob reacts 👍
			const r2 = messageService.addReaction(thread.id, msgId, user2, '👍');
			expect(r2.count).toBe(2);

			// Alice removes reaction
			const r3 = messageService.removeReaction(thread.id, msgId, user1, '👍');
			expect(r3?.count).toBe(1);

			// Bob removes reaction -> reaction removed completely
			const r4 = messageService.removeReaction(thread.id, msgId, user2, '👍');
			expect(r4).toBeNull();
		});

		it('should soft-delete message and adjust thread messageCount', async () => {
			const starterId = 'msg_parent_del';
			const thread = await manager.createThreadFromMessage(
				channelId,
				starterId,
				'Delete Message Thread',
				AutoArchiveDuration.ONE_DAY,
				user1,
				{guildId},
			);

			const res = await messageService.dispatchMessage(thread, user1, 'To be removed');
			expect(messageService.getParentMessageStats(channelId, starterId)?.replyCount).toBe(1);

			const {deletedMessage, updatedThread} = messageService.deleteMessage(
				manager.getThread(thread.id)!,
				res.message.id,
				user1,
			);

			expect(deletedMessage.deleted).toBe(true);
			expect(updatedThread.messageCount).toBe(0);
			expect(messageService.getParentMessageStats(channelId, starterId)?.replyCount).toBe(0);
		});
	});
});
