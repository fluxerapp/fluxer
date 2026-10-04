/**
 * Test Suite: Thread Manager Lifecycle, State Transitions & Permissions
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';
import {
  ThreadManager,
  ThreadType,
  AutoArchiveDuration,
  ThreadState,
  ThreadPermission,
  ThreadNotificationLevel,
  ThreadNotFoundError,
  ThreadPermissionError,
  ThreadAlreadyExistsError,
} from '../src';

describe('ThreadManager', () => {
  let manager: ThreadManager;
  const channelId = 'chan_123';
  const guildId = 'guild_456';
  const ownerId = 'user_owner';
  const moderatorId = 'user_mod';
  const regularUserId = 'user_regular';

  beforeEach(() => {
    manager = new ThreadManager({
      permissionChecker: (userId, gId, cId, perm) => {
        if (userId === moderatorId || userId === ownerId) {
          return true;
        }
        if (perm === ThreadPermission.CREATE_PUBLIC_THREADS || perm === ThreadPermission.SEND_MESSAGES_IN_THREADS) {
          return true;
        }
        return false;
      },
    });
  });

  describe('createThreadFromMessage', () => {
    it('should create a public thread linked to a parent starter message', async () => {
      const messageId = 'msg_parent_1';
      const thread = await manager.createThreadFromMessage(
        channelId,
        messageId,
        'Discussion on Feature X',
        AutoArchiveDuration.ONE_DAY,
        ownerId,
        { guildId }
      );

      expect(thread).toBeDefined();
      expect(thread.id).toMatch(/^th_/);
      expect(thread.name).toBe('Discussion on Feature X');
      expect(thread.parentId).toBe(channelId);
      expect(thread.starterMessageId).toBe(messageId);
      expect(thread.ownerId).toBe(ownerId);
      expect(thread.state).toBe(ThreadState.ACTIVE);
      expect(thread.threadMetadata.archived).toBe(false);
      expect(thread.threadMetadata.autoArchiveDuration).toBe(AutoArchiveDuration.ONE_DAY);

      // Verify creator was auto-added as member
      const participants = manager.getThreadParticipants(thread.id);
      expect(participants).toHaveLength(1);
      expect(participants[0].userId).toBe(ownerId);
      expect(thread.memberCount).toBe(1);

      // Lookup by starter message
      const found = manager.getThreadByStarterMessage(messageId);
      expect(found?.id).toBe(thread.id);
    });

    it('should disallow duplicate threads from the same starter message', async () => {
      const messageId = 'msg_unique_1';
      await manager.createThreadFromMessage(
        channelId,
        messageId,
        'First Thread',
        AutoArchiveDuration.ONE_HOUR,
        ownerId,
        { guildId }
      );

      await expect(
        manager.createThreadFromMessage(
          channelId,
          messageId,
          'Second Thread Attempt',
          AutoArchiveDuration.ONE_HOUR,
          ownerId,
          { guildId }
        )
      ).rejects.toThrow(ThreadAlreadyExistsError);
    });

    it('should enforce CREATE_PUBLIC_THREADS permission', async () => {
      const strictManager = new ThreadManager({
        permissionChecker: () => false,
      });

      await expect(
        strictManager.createThreadFromMessage(
          channelId,
          'msg_fail_perm',
          'Forbidden Thread',
          AutoArchiveDuration.ONE_DAY,
          'unauthorized_user'
        )
      ).rejects.toThrow(ThreadPermissionError);
    });
  });

  describe('createStandaloneThread', () => {
    it('should create a standalone public and private thread', async () => {
      const publicThread = await manager.createStandaloneThread(
        channelId,
        'Standalone General',
        ThreadType.PUBLIC_THREAD,
        AutoArchiveDuration.THREE_DAYS,
        ownerId,
        { guildId }
      );
      expect(publicThread.starterMessageId).toBeNull();
      expect(publicThread.type).toBe(ThreadType.PUBLIC_THREAD);

      const privateThread = await manager.createStandaloneThread(
        channelId,
        'Private Mod Planning',
        ThreadType.PRIVATE_THREAD,
        AutoArchiveDuration.ONE_WEEK,
        moderatorId,
        { guildId }
      );
      expect(privateThread.type).toBe(ThreadType.PRIVATE_THREAD);
      expect(privateThread.ownerId).toBe(moderatorId);
    });

    it('should reject private thread creation if lacking permission', async () => {
      await expect(
        manager.createStandaloneThread(
          channelId,
          'Private Thread',
          ThreadType.PRIVATE_THREAD,
          AutoArchiveDuration.ONE_DAY,
          regularUserId,
          { guildId }
        )
      ).rejects.toThrow(ThreadPermissionError);
    });
  });

  describe('state transitions: archive, unarchive, lock, delete', () => {
    it('should archive and unarchive a thread by owner', async () => {
      const thread = await manager.createStandaloneThread(
        channelId,
        'Lifecycle Test',
        ThreadType.PUBLIC_THREAD,
        AutoArchiveDuration.ONE_DAY,
        ownerId,
        { guildId }
      );

      // Archive
      const archived = await manager.archiveThread(thread.id, ownerId, false);
      expect(archived.state).toBe(ThreadState.ARCHIVED);
      expect(archived.threadMetadata.archived).toBe(true);
      expect(archived.threadMetadata.archivedAt).not.toBeNull();

      // Unarchive
      const unarchived = await manager.unarchiveThread(thread.id, ownerId);
      expect(unarchived.state).toBe(ThreadState.ACTIVE);
      expect(unarchived.threadMetadata.archived).toBe(false);
      expect(unarchived.threadMetadata.archivedAt).toBeNull();
    });

    it('should lock a thread when archived with lock=true by moderator', async () => {
      const thread = await manager.createStandaloneThread(
        channelId,
        'Lock Test',
        ThreadType.PUBLIC_THREAD,
        AutoArchiveDuration.ONE_DAY,
        ownerId,
        { guildId }
      );

      // Lock thread
      const locked = await manager.archiveThread(thread.id, moderatorId, true);
      expect(locked.state).toBe(ThreadState.LOCKED);
      expect(locked.threadMetadata.locked).toBe(true);
      expect(locked.threadMetadata.archived).toBe(true);

      // Non-moderator cannot unarchive locked thread
      await expect(manager.unarchiveThread(thread.id, regularUserId)).rejects.toThrow(
        ThreadPermissionError
      );

      // Moderator can unarchive locked thread
      const unarchived = await manager.unarchiveThread(thread.id, moderatorId);
      expect(unarchived.state).toBe(ThreadState.ACTIVE);
    });

    it('should prevent non-moderators/non-owners from archiving/deleting threads', async () => {
      const thread = await manager.createStandaloneThread(
        channelId,
        'Protected Thread',
        ThreadType.PUBLIC_THREAD,
        AutoArchiveDuration.ONE_DAY,
        ownerId,
        { guildId }
      );

      await expect(manager.archiveThread(thread.id, regularUserId)).rejects.toThrow(
        ThreadPermissionError
      );

      await expect(manager.deleteThread(thread.id, regularUserId)).rejects.toThrow(
        ThreadPermissionError
      );
    });

    it('should delete a thread and update indices', async () => {
      const thread = await manager.createStandaloneThread(
        channelId,
        'To be deleted',
        ThreadType.PUBLIC_THREAD,
        AutoArchiveDuration.ONE_DAY,
        ownerId,
        { guildId }
      );

      const deleted = await manager.deleteThread(thread.id, ownerId);
      expect(deleted.state).toBe(ThreadState.DELETED);
      expect(manager.getThread(thread.id)).toBeNull();
      expect(manager.listActiveThreads(channelId)).toHaveLength(0);
    });
  });

  describe('membership operations', () => {
    it('should add and remove members, maintaining memberCount', async () => {
      const thread = await manager.createStandaloneThread(
        channelId,
        'Member Test',
        ThreadType.PUBLIC_THREAD,
        AutoArchiveDuration.ONE_DAY,
        ownerId,
        { guildId }
      );

      expect(thread.memberCount).toBe(1);

      // Add regular user
      const member = await manager.addThreadMember(thread.id, regularUserId);
      expect(member.userId).toBe(regularUserId);
      expect(member.notifications.notificationLevel).toBe(ThreadNotificationLevel.ALL_MESSAGES);
      expect(thread.memberCount).toBe(2);

      // Check participants list
      const participants = manager.getThreadParticipants(thread.id);
      expect(participants.map((p) => p.userId)).toContain(regularUserId);

      // Remove member
      const removed = await manager.removeThreadMember(thread.id, regularUserId, regularUserId);
      expect(removed).toBe(true);
      expect(thread.memberCount).toBe(1);
      expect(manager.isThreadMember(thread.id, regularUserId)).toBe(false);
    });

    it('should enforce private thread invitation boundary', async () => {
      const privThread = await manager.createStandaloneThread(
        channelId,
        'Secret Squad',
        ThreadType.PRIVATE_THREAD,
        AutoArchiveDuration.ONE_DAY,
        moderatorId,
        { guildId, invitable: false }
      );

      // Regular user cannot invite without being a member and without permission
      await expect(
        manager.addThreadMember(privThread.id, 'other_user', { inviterId: regularUserId })
      ).rejects.toThrow(ThreadPermissionError);
    });
  });

  describe('auto-archiving', () => {
    it('should auto-archive inactive threads when threshold is exceeded', async () => {
      const thread = await manager.createStandaloneThread(
        channelId,
        'Auto Archive Target',
        ThreadType.PUBLIC_THREAD,
        AutoArchiveDuration.ONE_HOUR,
        ownerId,
        { guildId }
      );

      const start = thread.lastActiveTimestamp;

      // 30 minutes later -> should NOT archive
      const after30Min = manager.checkAutoArchive(start + 30 * 60 * 1000);
      expect(after30Min).toHaveLength(0);
      expect(manager.getThread(thread.id)?.state).toBe(ThreadState.ACTIVE);

      // 61 minutes later -> SHOULD archive
      const after61Min = manager.checkAutoArchive(start + 61 * 60 * 1000);
      expect(after61Min).toHaveLength(1);
      expect(after61Min[0].id).toBe(thread.id);
      expect(after61Min[0].state).toBe(ThreadState.ARCHIVED);
      expect(after61Min[0].threadMetadata.archived).toBe(true);
    });
  });

  describe('listActiveThreads and syncThreadList', () => {
    it('should list only active threads and sync thread list for gateway', async () => {
      const t1 = await manager.createStandaloneThread(channelId, 'Thread 1', ThreadType.PUBLIC_THREAD, AutoArchiveDuration.ONE_DAY, ownerId, { guildId });
      const t2 = await manager.createStandaloneThread(channelId, 'Thread 2', ThreadType.PUBLIC_THREAD, AutoArchiveDuration.ONE_DAY, ownerId, { guildId });
      await manager.archiveThread(t2.id, ownerId);

      const active = manager.listActiveThreads(channelId);
      expect(active).toHaveLength(1);
      expect(active[0].id).toBe(t1.id);

      const sync = manager.syncThreadList(guildId, [channelId]);
      expect(sync.guildId).toBe(guildId);
      expect(sync.threads).toHaveLength(2);
      expect(sync.members.length).toBeGreaterThanOrEqual(2);
    });
  });
});
