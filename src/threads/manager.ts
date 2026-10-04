/**
 * Thread Manager - Lifecycle, Membership, State Transitions & Auto-Archiving
 * Fluxer Real-time Architecture
 */

import { EventEmitter } from 'events';
import {
  ThreadChannel,
  ThreadType,
  AutoArchiveDuration,
  ThreadState,
  ThreadMember,
  ThreadListSyncPayload,
  ThreadNotificationLevel,
  ThreadPermission,
  GatewayEvent,
  GatewayThreadEventType,
} from './types';
import { ThreadEventSerializer } from './events';
import { ThreadNotificationEngine } from './notifications';
import { ThreadMessageService } from './messages';

export class ThreadNotFoundError extends Error {
  constructor(threadId: string) {
    super(`Thread ${threadId} was not found.`);
    this.name = 'ThreadNotFoundError';
  }
}

export class ThreadPermissionError extends Error {
  constructor(message = 'Insufficient permissions to perform this thread operation.') {
    super(message);
    this.name = 'ThreadPermissionError';
  }
}

export class ThreadAlreadyExistsError extends Error {
  constructor(message = 'A thread for this message or with this ID already exists.') {
    super(message);
    this.name = 'ThreadAlreadyExistsError';
  }
}

export type PermissionChecker = (
  userId: string,
  guildId: string,
  channelId: string,
  requiredPermission: ThreadPermission
) => boolean | Promise<boolean>;

export interface ThreadManagerOptions {
  notificationEngine?: ThreadNotificationEngine;
  messageService?: ThreadMessageService;
  permissionChecker?: PermissionChecker;
}

/**
 * ThreadManager orchestrates thread creation, member joining/leaving,
 * archiving/unarchiving, locking, deletion, auto-archiving schedules,
 * and gateway event distribution.
 */
export class ThreadManager extends EventEmitter {
  // threadId -> ThreadChannel
  private threads = new Map<string, ThreadChannel>();
  // starterMessageId -> threadId
  private messageThreadIndex = new Map<string, string>();
  // threadId -> Map<userId, ThreadMember>
  private members = new Map<string, Map<string, ThreadMember>>();
  // channelId -> Set<threadId>
  private parentChannelThreads = new Map<string, Set<string>>();

  private notificationEngine: ThreadNotificationEngine;
  private messageService: ThreadMessageService;
  private permissionChecker?: PermissionChecker;

  constructor(options: ThreadManagerOptions = {}) {
    super();
    this.notificationEngine = options.notificationEngine || new ThreadNotificationEngine();
    this.messageService = options.messageService || new ThreadMessageService(this.notificationEngine);
    this.permissionChecker = options.permissionChecker;
  }

  public getNotificationEngine(): ThreadNotificationEngine {
    return this.notificationEngine;
  }

  public getMessageService(): ThreadMessageService {
    return this.messageService;
  }

  public setPermissionChecker(checker: PermissionChecker): void {
    this.permissionChecker = checker;
  }

  private async hasPermission(
    userId: string,
    guildId: string,
    channelId: string,
    perm: ThreadPermission
  ): Promise<boolean> {
    if (!this.permissionChecker) return true;
    return Promise.resolve(this.permissionChecker(userId, guildId, channelId, perm));
  }

  /**
   * Helper to emit typed gateway events.
   */
  private dispatchGatewayEvent(event: GatewayEvent): void {
    this.emit('gateway_event', event);
    this.emit(event.t, event.d);
  }

  /**
   * Creates a thread from an existing message in a text channel.
   */
  public async createThreadFromMessage(
    channelId: string,
    messageId: string,
    name: string,
    autoArchiveDuration: AutoArchiveDuration = AutoArchiveDuration.ONE_DAY,
    creatorId: string,
    options: {
      guildId?: string;
      rateLimitPerUser?: number;
      type?: ThreadType;
    } = {}
  ): Promise<ThreadChannel> {
    if (this.messageThreadIndex.has(messageId)) {
      throw new ThreadAlreadyExistsError(`Thread already spawned for message ${messageId}`);
    }

    const guildId = options.guildId || 'guild_default';

    // Verify creator permission
    const hasCreatePerm = await this.hasPermission(
      creatorId,
      guildId,
      channelId,
      ThreadPermission.CREATE_PUBLIC_THREADS
    );
    if (!hasCreatePerm) {
      throw new ThreadPermissionError('Missing CREATE_PUBLIC_THREADS permission');
    }

    const threadId = `th_${Date.now()}_${Math.random().toString(36).substring(2, 9)}`;
    const now = Date.now();
    const threadType = options.type || ThreadType.PUBLIC_THREAD;

    const thread: ThreadChannel = {
      id: threadId,
      guildId,
      parentId: channelId,
      ownerId: creatorId,
      name: name.trim(),
      type: threadType,
      state: ThreadState.ACTIVE,
      lastMessageId: null,
      messageCount: 0,
      memberCount: 0,
      rateLimitPerUser: options.rateLimitPerUser || 0,
      starterMessageId: messageId,
      createdAt: now,
      updatedAt: now,
      lastActiveTimestamp: now,
      totalMessagesSent: 0,
      threadMetadata: {
        archived: false,
        autoArchiveDuration,
        archiveTimestamp: now + autoArchiveDuration * 60 * 1000,
        locked: false,
        invitable: true,
        createTimestamp: now,
        archivedAt: null,
        lockedAt: null,
      },
    };

    // Store thread
    this.threads.set(threadId, thread);
    this.messageThreadIndex.set(messageId, threadId);

    if (!this.parentChannelThreads.has(channelId)) {
      this.parentChannelThreads.set(channelId, new Set());
    }
    this.parentChannelThreads.get(channelId)!.add(threadId);

    // Link starter message in message service
    this.messageService.linkThreadToParentMessage(channelId, messageId, threadId);

    // Add creator as first participant
    await this.addThreadMember(threadId, creatorId, { isOwner: true });

    // Emit Gateway Event
    const gwEvent = ThreadEventSerializer.formatThreadCreate(thread, true);
    this.dispatchGatewayEvent(gwEvent);

    return thread;
  }

  /**
   * Creates a standalone thread (not attached to a starter message).
   */
  public async createStandaloneThread(
    channelId: string,
    name: string,
    type: ThreadType = ThreadType.PUBLIC_THREAD,
    autoArchiveDuration: AutoArchiveDuration = AutoArchiveDuration.ONE_DAY,
    creatorId: string,
    options: {
      guildId?: string;
      rateLimitPerUser?: number;
      invitable?: boolean;
    } = {}
  ): Promise<ThreadChannel> {
    const guildId = options.guildId || 'guild_default';

    const requiredPerm =
      type === ThreadType.PRIVATE_THREAD
        ? ThreadPermission.CREATE_PRIVATE_THREADS
        : ThreadPermission.CREATE_PUBLIC_THREADS;

    const hasCreatePerm = await this.hasPermission(creatorId, guildId, channelId, requiredPerm);
    if (!hasCreatePerm) {
      throw new ThreadPermissionError(`Missing permission to create ${type}`);
    }

    const threadId = `th_${Date.now()}_${Math.random().toString(36).substring(2, 9)}`;
    const now = Date.now();

    const thread: ThreadChannel = {
      id: threadId,
      guildId,
      parentId: channelId,
      ownerId: creatorId,
      name: name.trim(),
      type,
      state: ThreadState.ACTIVE,
      lastMessageId: null,
      messageCount: 0,
      memberCount: 0,
      rateLimitPerUser: options.rateLimitPerUser || 0,
      starterMessageId: null,
      createdAt: now,
      updatedAt: now,
      lastActiveTimestamp: now,
      totalMessagesSent: 0,
      threadMetadata: {
        archived: false,
        autoArchiveDuration,
        archiveTimestamp: now + autoArchiveDuration * 60 * 1000,
        locked: false,
        invitable: options.invitable !== undefined ? options.invitable : true,
        createTimestamp: now,
        archivedAt: null,
        lockedAt: null,
      },
    };

    this.threads.set(threadId, thread);

    if (!this.parentChannelThreads.has(channelId)) {
      this.parentChannelThreads.set(channelId, new Set());
    }
    this.parentChannelThreads.get(channelId)!.add(threadId);

    // Add creator as first member
    await this.addThreadMember(threadId, creatorId, { isOwner: true });

    // Emit Gateway Event
    const gwEvent = ThreadEventSerializer.formatThreadCreate(thread, true);
    this.dispatchGatewayEvent(gwEvent);

    return thread;
  }

  /**
   * Retrieves a thread by ID.
   */
  public getThread(threadId: string): ThreadChannel | null {
    const thread = this.threads.get(threadId);
    if (!thread || thread.state === ThreadState.DELETED) {
      return null;
    }
    return thread;
  }

  /**
   * Retrieves a thread by its starter message ID.
   */
  public getThreadByStarterMessage(messageId: string): ThreadChannel | null {
    const threadId = this.messageThreadIndex.get(messageId);
    if (!threadId) return null;
    return this.getThread(threadId);
  }

  /**
   * Archives a thread, optionally locking it.
   */
  public async archiveThread(
    threadId: string,
    userId: string,
    lock = false
  ): Promise<ThreadChannel> {
    const thread = this.getThread(threadId);
    if (!thread) throw new ThreadNotFoundError(threadId);

    // Check permissions
    const isOwner = thread.ownerId === userId;
    const hasManage = await this.hasPermission(
      userId,
      thread.guildId,
      thread.parentId,
      ThreadPermission.MANAGE_THREADS
    );

    if (!isOwner && !hasManage) {
      throw new ThreadPermissionError('Only thread owner or moderators with MANAGE_THREADS can archive this thread');
    }

    if (lock && !hasManage) {
      throw new ThreadPermissionError('Only moderators with MANAGE_THREADS can lock this thread');
    }

    const now = Date.now();
    const updated: ThreadChannel = {
      ...thread,
      state: lock ? ThreadState.LOCKED : ThreadState.ARCHIVED,
      updatedAt: now,
      threadMetadata: {
        ...thread.threadMetadata,
        archived: true,
        locked: lock ? true : thread.threadMetadata.locked,
        archivedAt: now,
        lockedAt: lock ? now : thread.threadMetadata.lockedAt,
        archiveTimestamp: now,
      },
    };

    this.threads.set(threadId, updated);

    // Emit THREAD_UPDATE
    const gwEvent = ThreadEventSerializer.formatThreadUpdate(updated);
    this.dispatchGatewayEvent(gwEvent);

    return updated;
  }

  /**
   * Unarchives an archived thread.
   */
  public async unarchiveThread(threadId: string, userId: string): Promise<ThreadChannel> {
    const thread = this.getThread(threadId);
    if (!thread) throw new ThreadNotFoundError(threadId);

    // If locked, only MANAGE_THREADS can unarchive
    if (thread.threadMetadata.locked) {
      const hasManage = await this.hasPermission(
        userId,
        thread.guildId,
        thread.parentId,
        ThreadPermission.MANAGE_THREADS
      );
      if (!hasManage) {
        throw new ThreadPermissionError('Locked threads can only be unarchived by moderators with MANAGE_THREADS');
      }
    }

    const now = Date.now();
    const updated: ThreadChannel = {
      ...thread,
      state: ThreadState.ACTIVE,
      updatedAt: now,
      lastActiveTimestamp: now,
      threadMetadata: {
        ...thread.threadMetadata,
        archived: false,
        archivedAt: null,
        archiveTimestamp: now + thread.threadMetadata.autoArchiveDuration * 60 * 1000,
      },
    };

    this.threads.set(threadId, updated);

    // Emit THREAD_UPDATE
    const gwEvent = ThreadEventSerializer.formatThreadUpdate(updated);
    this.dispatchGatewayEvent(gwEvent);

    return updated;
  }

  /**
   * Deletes a thread permanently.
   */
  public async deleteThread(threadId: string, userId: string): Promise<ThreadChannel> {
    const thread = this.getThread(threadId);
    if (!thread) throw new ThreadNotFoundError(threadId);

    const isOwner = thread.ownerId === userId;
    const hasManage = await this.hasPermission(
      userId,
      thread.guildId,
      thread.parentId,
      ThreadPermission.MANAGE_THREADS
    );

    if (!isOwner && !hasManage) {
      throw new ThreadPermissionError('Only thread owner or moderators can delete this thread');
    }

    const deleted: ThreadChannel = {
      ...thread,
      state: ThreadState.DELETED,
      updatedAt: Date.now(),
    };

    this.threads.set(threadId, deleted);

    // Remove from channel index
    this.parentChannelThreads.get(thread.parentId)?.delete(threadId);
    if (thread.starterMessageId) {
      this.messageThreadIndex.delete(thread.starterMessageId);
    }

    // Emit THREAD_DELETE
    const gwEvent = ThreadEventSerializer.formatThreadDelete(thread);
    this.dispatchGatewayEvent(gwEvent);

    return deleted;
  }

  /**
   * Adds a user as a member of a thread.
   */
  public async addThreadMember(
    threadId: string,
    userId: string,
    options: {
      inviterId?: string;
      isOwner?: boolean;
      notificationLevel?: ThreadNotificationLevel;
    } = {}
  ): Promise<ThreadMember> {
    const thread = this.getThread(threadId);
    if (!thread) throw new ThreadNotFoundError(threadId);

    // If private thread and not owner, check invitable or permission
    if (thread.type === ThreadType.PRIVATE_THREAD && !options.isOwner) {
      if (options.inviterId) {
        const inviterIsMember = this.isThreadMember(threadId, options.inviterId);
        if (!inviterIsMember) {
          throw new ThreadPermissionError('Inviter must be a member of the private thread');
        }
        if (!thread.threadMetadata.invitable) {
          const hasManage = await this.hasPermission(
            options.inviterId,
            thread.guildId,
            thread.parentId,
            ThreadPermission.MANAGE_THREADS
          );
          if (!hasManage) {
            throw new ThreadPermissionError('Private thread is not invitable by regular members');
          }
        }
      }
    }

    if (!this.members.has(threadId)) {
      this.members.set(threadId, new Map());
    }

    const threadMembers = this.members.get(threadId)!;
    let member = threadMembers.get(userId);

    if (!member) {
      const now = Date.now();
      member = {
        id: `${threadId}:${userId}`,
        threadId,
        userId,
        joinTimestamp: now,
        flags: 0,
        notifications: this.notificationEngine.createDefaultSubscription(
          options.notificationLevel || ThreadNotificationLevel.ALL_MESSAGES
        ),
        lastReadMessageId: null,
        lastReadTimestamp: null,
        unreadCount: 0,
        muted: false,
      };

      threadMembers.set(userId, member);

      // Increment thread member count
      thread.memberCount = threadMembers.size;
      thread.updatedAt = Date.now();

      // Emit THREAD_MEMBERS_UPDATE
      const gwMembersEvent = ThreadEventSerializer.formatThreadMembersUpdate(
        threadId,
        thread.guildId,
        thread.memberCount,
        [member],
        []
      );
      this.dispatchGatewayEvent(gwMembersEvent);

      // Emit THREAD_MEMBER_UPDATE
      const gwMemberEvent = ThreadEventSerializer.formatThreadMemberUpdate(member, thread.guildId);
      this.dispatchGatewayEvent(gwMemberEvent);
    }

    return member;
  }

  /**
   * Removes a member from a thread.
   */
  public async removeThreadMember(
    threadId: string,
    userId: string,
    removerId: string = userId
  ): Promise<boolean> {
    const thread = this.getThread(threadId);
    if (!thread) throw new ThreadNotFoundError(threadId);

    const threadMembers = this.members.get(threadId);
    if (!threadMembers || !threadMembers.has(userId)) {
      return false;
    }

    // Check permission if removing someone else
    if (userId !== removerId) {
      const isOwner = thread.ownerId === removerId;
      const hasManage = await this.hasPermission(
        removerId,
        thread.guildId,
        thread.parentId,
        ThreadPermission.MANAGE_THREADS
      );
      if (!isOwner && !hasManage) {
        throw new ThreadPermissionError('Cannot remove other members without MANAGE_THREADS permission');
      }
    }

    threadMembers.delete(userId);
    thread.memberCount = threadMembers.size;
    thread.updatedAt = Date.now();

    // Emit THREAD_MEMBERS_UPDATE
    const gwEvent = ThreadEventSerializer.formatThreadMembersUpdate(
      threadId,
      thread.guildId,
      thread.memberCount,
      [],
      [userId]
    );
    this.dispatchGatewayEvent(gwEvent);

    return true;
  }

  /**
   * Checks if a user is a member of a thread.
   */
  public isThreadMember(threadId: string, userId: string): boolean {
    const threadMembers = this.members.get(threadId);
    return !!threadMembers && threadMembers.has(userId);
  }

  /**
   * Gets a specific member entity in a thread.
   */
  public getThreadMember(threadId: string, userId: string): ThreadMember | null {
    return this.members.get(threadId)?.get(userId) || null;
  }

  /**
   * Gets all participants of a thread.
   */
  public getThreadParticipants(threadId: string): ThreadMember[] {
    const threadMembers = this.members.get(threadId);
    if (!threadMembers) return [];
    return Array.from(threadMembers.values());
  }

  /**
   * Lists all active (non-archived, non-deleted) threads in a channel.
   */
  public listActiveThreads(channelId: string): ThreadChannel[] {
    const threadIds = this.parentChannelThreads.get(channelId);
    if (!threadIds) return [];

    const activeList: ThreadChannel[] = [];
    for (const threadId of threadIds) {
      const thread = this.threads.get(threadId);
      if (
        thread &&
        thread.state === ThreadState.ACTIVE &&
        !thread.threadMetadata.archived
      ) {
        activeList.push(thread);
      }
    }

    return activeList.sort((a, b) => b.lastActiveTimestamp - a.lastActiveTimestamp);
  }

  /**
   * Lists archived threads in a channel.
   */
  public listArchivedThreads(
    channelId: string,
    options: { before?: number; limit?: number } = {}
  ): { threads: ThreadChannel[]; hasMore: boolean } {
    const threadIds = this.parentChannelThreads.get(channelId);
    if (!threadIds) return { threads: [], hasMore: false };

    let archivedList: ThreadChannel[] = [];
    for (const threadId of threadIds) {
      const thread = this.threads.get(threadId);
      if (
        thread &&
        thread.state !== ThreadState.DELETED &&
        (thread.state === ThreadState.ARCHIVED || thread.threadMetadata.archived)
      ) {
        archivedList.push(thread);
      }
    }

    archivedList.sort((a, b) => (b.threadMetadata.archivedAt || 0) - (a.threadMetadata.archivedAt || 0));

    if (options.before) {
      archivedList = archivedList.filter((t) => (t.threadMetadata.archivedAt || 0) < options.before!);
    }

    const limit = options.limit || 50;
    const items = archivedList.slice(0, limit);
    return {
      threads: items,
      hasMore: archivedList.length > limit,
    };
  }

  /**
   * Builds the ThreadListSyncPayload for initial client connection or channel sync.
   */
  public syncThreadList(guildId: string, channelIds?: string[]): ThreadListSyncPayload {
    const allChannels = channelIds || Array.from(this.parentChannelThreads.keys());
    const matchedThreads: ThreadChannel[] = [];
    const matchedMembers: ThreadMember[] = [];

    for (const channelId of allChannels) {
      const threadIds = this.parentChannelThreads.get(channelId);
      if (!threadIds) continue;

      for (const threadId of threadIds) {
        const thread = this.threads.get(threadId);
        if (thread && thread.guildId === guildId && thread.state !== ThreadState.DELETED) {
          matchedThreads.push(thread);
          const threadMems = this.getThreadParticipants(threadId);
          matchedMembers.push(...threadMems);
        }
      }
    }

    const payload: ThreadListSyncPayload = {
      guildId,
      channelIds,
      threads: matchedThreads,
      members: matchedMembers,
    };

    const gwEvent = ThreadEventSerializer.formatThreadListSync(payload);
    this.dispatchGatewayEvent(gwEvent);

    return payload;
  }

  /**
   * Sweeps active threads and auto-archives those whose inactivity duration has expired.
   */
  public checkAutoArchive(currentTime = Date.now()): ThreadChannel[] {
    const archivedThreads: ThreadChannel[] = [];

    for (const thread of this.threads.values()) {
      if (thread.state !== ThreadState.ACTIVE || thread.threadMetadata.archived) {
        continue;
      }

      const durationMs = thread.threadMetadata.autoArchiveDuration * 60 * 1000;
      const timeSinceActive = currentTime - thread.lastActiveTimestamp;

      if (timeSinceActive >= durationMs) {
        thread.state = ThreadState.ARCHIVED;
        thread.threadMetadata.archived = true;
        thread.threadMetadata.archivedAt = currentTime;
        thread.threadMetadata.archiveTimestamp = currentTime;
        thread.updatedAt = currentTime;

        archivedThreads.push(thread);

        // Emit Gateway event
        const gwEvent = ThreadEventSerializer.formatThreadUpdate(thread);
        this.dispatchGatewayEvent(gwEvent);
      }
    }

    return archivedThreads;
  }

  /**
   * High-level send message method that integrates thread manager state, rate-limits,
   * participant auto-join, notification routing, and gateway updates.
   */
  public async sendThreadMessage(
    threadId: string,
    authorId: string,
    content: string,
    options: {
      attachments?: Array<{ id: string; filename: string; size: number; url: string; contentType?: string }>;
      mentions?: string[];
      mentionRoles?: string[];
      mentionEveryone?: boolean;
      autoUnarchive?: boolean;
    } = {}
  ) {
    const thread = this.getThread(threadId);
    if (!thread) throw new ThreadNotFoundError(threadId);

    // Auto-join author to thread if not already joined
    if (!this.isThreadMember(threadId, authorId)) {
      await this.addThreadMember(threadId, authorId);
    }

    const participants = this.getThreadParticipants(threadId);

    // Dispatch message via message service
    const { message, updatedThread, updatedMembers } = await this.messageService.dispatchMessage(
      thread,
      authorId,
      content,
      {
        ...options,
        autoUnarchive: options.autoUnarchive !== undefined ? options.autoUnarchive : true,
      },
      participants
    );

    // Update in-memory thread reference
    this.threads.set(threadId, updatedThread);

    // Update participants memory
    const memberMap = this.members.get(threadId)!;
    for (const m of updatedMembers) {
      memberMap.set(m.userId, m);
    }

    // Evaluate notifications
    const notifications = await this.notificationEngine.evaluateNotifications(
      updatedThread,
      message,
      updatedMembers
    );

    // Emit thread update if state changed (e.g. unarchived)
    if (thread.state !== updatedThread.state || thread.messageCount !== updatedThread.messageCount) {
      const gwUpdate = ThreadEventSerializer.formatThreadUpdate(updatedThread);
      this.dispatchGatewayEvent(gwUpdate);
    }

    return {
      message,
      thread: updatedThread,
      notifications,
    };
  }
}
