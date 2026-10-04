/**
 * Nested Thread Message Dispatcher, Cursor Pagination & Reply Synchronization
 * Fluxer Real-time Architecture
 */

import {
  ThreadChannel,
  ThreadMember,
  ThreadMessage,
  MessagePaginationOptions,
  PaginatedResult,
  ThreadState,
  MessageReaction,
} from './types';
import { ThreadNotificationEngine } from './notifications';

export class ThreadArchivedError extends Error {
  constructor(message = 'Cannot send messages in an archived thread without unarchiving.') {
    super(message);
    this.name = 'ThreadArchivedError';
  }
}

export class ThreadLockedError extends Error {
  constructor(message = 'Thread is locked. Only moderators can send messages.') {
    super(message);
    this.name = 'ThreadLockedError';
  }
}

export class ThreadDeletedError extends Error {
  constructor(message = 'Cannot interact with a deleted thread.') {
    super(message);
    this.name = 'ThreadDeletedError';
  }
}

export class RateLimitExceededError extends Error {
  public retryAfterSeconds: number;
  constructor(retryAfterSeconds: number) {
    super(`Rate limit exceeded in thread slowmode. Retry after ${retryAfterSeconds}s.`);
    this.name = 'RateLimitExceededError';
    this.retryAfterSeconds = retryAfterSeconds;
  }
}

export class MessageNotFoundError extends Error {
  constructor(messageId: string) {
    super(`Message ${messageId} was not found.`);
    this.name = 'MessageNotFoundError';
  }
}

export interface SendMessageOptions {
  attachments?: Array<{ id: string; filename: string; size: number; url: string; contentType?: string }>;
  mentions?: string[];
  mentionRoles?: string[];
  mentionEveryone?: boolean;
  parentId?: string | null;
  bypassRateLimit?: boolean;
  bypassLock?: boolean;
  autoUnarchive?: boolean;
}

/**
 * Message Dispatcher and Storage Service for Threads.
 */
export class ThreadMessageService {
  // threadId -> Map<messageId, ThreadMessage>
  private threadMessages = new Map<string, Map<string, ThreadMessage>>();
  // parentChannelId -> Map<messageId, { replyCount: number; lastReplyId: string | null; threadId: string | null }>
  private parentMessageIndex = new Map<
    string,
    Map<string, { replyCount: number; lastReplyId: string | null; threadId: string | null }>
  >();
  // key: `${threadId}:${userId}` -> timestamp of last message sent
  private rateLimitBuckets = new Map<string, number>();

  private notificationEngine: ThreadNotificationEngine;

  constructor(notificationEngine?: ThreadNotificationEngine) {
    this.notificationEngine = notificationEngine || new ThreadNotificationEngine();
  }

  /**
   * Registers an existing parent message so reply counts and threads can attach.
   */
  public registerParentMessage(channelId: string, messageId: string, initialReplyCount = 0): void {
    if (!this.parentMessageIndex.has(channelId)) {
      this.parentMessageIndex.set(channelId, new Map());
    }
    const channelMap = this.parentMessageIndex.get(channelId)!;
    if (!channelMap.has(messageId)) {
      channelMap.set(messageId, {
        replyCount: initialReplyCount,
        lastReplyId: null,
        threadId: null,
      });
    }
  }

  /**
   * Links a created thread to its starter parent message.
   */
  public linkThreadToParentMessage(channelId: string, messageId: string, threadId: string): void {
    this.registerParentMessage(channelId, messageId);
    const meta = this.parentMessageIndex.get(channelId)!.get(messageId)!;
    meta.threadId = threadId;
  }

  /**
   * Retrieves reply stats for a parent message.
   */
  public getParentMessageStats(channelId: string, messageId: string) {
    return this.parentMessageIndex.get(channelId)?.get(messageId) || null;
  }

  /**
   * Checks slowmode rate limiting for a user within a thread.
   */
  public checkRateLimit(
    thread: ThreadChannel,
    userId: string,
    now = Date.now()
  ): { isLimited: boolean; retryAfterSeconds: number } {
    if (thread.rateLimitPerUser <= 0) {
      return { isLimited: false, retryAfterSeconds: 0 };
    }

    const key = `${thread.id}:${userId}`;
    const lastSent = this.rateLimitBuckets.get(key);
    if (!lastSent) {
      return { isLimited: false, retryAfterSeconds: 0 };
    }

    const elapsedSeconds = (now - lastSent) / 1000;
    if (elapsedSeconds < thread.rateLimitPerUser) {
      const remaining = Math.ceil(thread.rateLimitPerUser - elapsedSeconds);
      return { isLimited: true, retryAfterSeconds: remaining };
    }

    return { isLimited: false, retryAfterSeconds: 0 };
  }

  /**
   * Dispatches a new message inside a thread, enforcing locks, slowmode, and syncing parent replies.
   */
  public async dispatchMessage(
    thread: ThreadChannel,
    authorId: string,
    content: string,
    options: SendMessageOptions = {},
    members: ThreadMember[] = []
  ): Promise<{
    message: ThreadMessage;
    updatedThread: ThreadChannel;
    updatedMembers: ThreadMember[];
  }> {
    const now = Date.now();

    // Verify thread state
    if (thread.state === ThreadState.DELETED) {
      throw new ThreadDeletedError();
    }

    if (thread.state === ThreadState.LOCKED) {
      if (!options.bypassLock) {
        throw new ThreadLockedError();
      }
    } else {
      const isArchived = thread.state === ThreadState.ARCHIVED || thread.threadMetadata.archived;
      if (isArchived && !options.autoUnarchive) {
        throw new ThreadArchivedError();
      }
    }

    const isArchived = thread.state === ThreadState.ARCHIVED || thread.threadMetadata.archived;

    // Check rate limit
    if (!options.bypassRateLimit) {
      const rateCheck = this.checkRateLimit(thread, authorId, now);
      if (rateCheck.isLimited) {
        throw new RateLimitExceededError(rateCheck.retryAfterSeconds);
      }
    }

    // Generate unique message ID
    const messageId = `msg_${Date.now()}_${Math.random().toString(36).substring(2, 9)}`;

    const message: ThreadMessage = {
      id: messageId,
      channelId: thread.id,
      guildId: thread.guildId,
      parentId: thread.starterMessageId || thread.parentId,
      authorId,
      content,
      attachments: options.attachments || [],
      mentions: options.mentions || [],
      mentionRoles: options.mentionRoles || [],
      mentionEveryone: options.mentionEveryone || false,
      replyCount: 0,
      createdAt: now,
      editedAt: null,
      pinned: false,
      reactions: new Map<string, MessageReaction>(),
      deleted: false,
    };

    // Store message
    if (!this.threadMessages.has(thread.id)) {
      this.threadMessages.set(thread.id, new Map());
    }
    this.threadMessages.get(thread.id)!.set(messageId, message);

    // Update rate limit bucket
    this.rateLimitBuckets.set(`${thread.id}:${authorId}`, now);

    // Sync parent message reply stats if thread was spawned from a message
    if (thread.starterMessageId && thread.parentId) {
      this.registerParentMessage(thread.parentId, thread.starterMessageId);
      const parentStats = this.parentMessageIndex.get(thread.parentId)!.get(thread.starterMessageId)!;
      parentStats.replyCount += 1;
      parentStats.lastReplyId = messageId;
    }

    // Update thread object
    const updatedThread: ThreadChannel = {
      ...thread,
      state: isArchived && options.autoUnarchive ? ThreadState.ACTIVE : thread.state,
      lastMessageId: messageId,
      messageCount: thread.messageCount + 1,
      totalMessagesSent: thread.totalMessagesSent + 1,
      lastActiveTimestamp: now,
      updatedAt: now,
      threadMetadata: {
        ...thread.threadMetadata,
        archived: isArchived && options.autoUnarchive ? false : thread.threadMetadata.archived,
        archivedAt: isArchived && options.autoUnarchive ? null : thread.threadMetadata.archivedAt,
      },
    };

    // Update members unread / read pointers
    const updatedMembers = members.map((member) =>
      this.notificationEngine.processIncomingMessage(member, message)
    );

    return {
      message,
      updatedThread,
      updatedMembers,
    };
  }

  /**
   * Retrieves messages with robust cursor-based pagination (before, after, around).
   */
  public getMessages(
    threadId: string,
    options: MessagePaginationOptions = {}
  ): PaginatedResult<ThreadMessage> {
    const threadMap = this.threadMessages.get(threadId);
    if (!threadMap) {
      return {
        items: [],
        hasMore: false,
        nextCursor: null,
        prevCursor: null,
        totalCount: 0,
      };
    }

    // All active non-deleted messages sorted by createdAt ascending
    const allMessages = Array.from(threadMap.values())
      .filter((m) => !m.deleted)
      .sort((a, b) => a.createdAt - b.createdAt);

    const limit = Math.max(1, Math.min(options.limit || 50, 100));

    if (options.around) {
      const targetIndex = allMessages.findIndex((m) => m.id === options.around);
      if (targetIndex === -1) {
        throw new MessageNotFoundError(options.around);
      }
      const half = Math.floor(limit / 2);
      const start = Math.max(0, targetIndex - half);
      const end = Math.min(allMessages.length, start + limit);
      const items = allMessages.slice(start, end);

      return {
        items,
        hasMore: end < allMessages.length,
        nextCursor: items.length > 0 ? items[items.length - 1].id : null,
        prevCursor: items.length > 0 ? items[0].id : null,
        totalCount: allMessages.length,
      };
    }

    if (options.before) {
      const targetIndex = allMessages.findIndex((m) => m.id === options.before);
      if (targetIndex === -1) {
        throw new MessageNotFoundError(options.before);
      }
      const start = Math.max(0, targetIndex - limit);
      const items = allMessages.slice(start, targetIndex);

      return {
        items,
        hasMore: start > 0,
        nextCursor: items.length > 0 ? items[items.length - 1].id : null,
        prevCursor: items.length > 0 ? items[0].id : null,
        totalCount: allMessages.length,
      };
    }

    if (options.after) {
      const targetIndex = allMessages.findIndex((m) => m.id === options.after);
      if (targetIndex === -1) {
        throw new MessageNotFoundError(options.after);
      }
      const start = targetIndex + 1;
      const end = Math.min(allMessages.length, start + limit);
      const items = allMessages.slice(start, end);

      return {
        items,
        hasMore: end < allMessages.length,
        nextCursor: items.length > 0 ? items[items.length - 1].id : null,
        prevCursor: items.length > 0 ? items[0].id : null,
        totalCount: allMessages.length,
      };
    }

    // Default: Return the latest `limit` messages (tail)
    const start = Math.max(0, allMessages.length - limit);
    const items = allMessages.slice(start);

    return {
      items,
      hasMore: start > 0,
      nextCursor: items.length > 0 ? items[items.length - 1].id : null,
      prevCursor: items.length > 0 ? items[0].id : null,
      totalCount: allMessages.length,
    };
  }

  /**
   * Retrieves a single message by ID.
   */
  public getMessage(threadId: string, messageId: string): ThreadMessage | null {
    const threadMap = this.threadMessages.get(threadId);
    if (!threadMap) return null;
    const msg = threadMap.get(messageId);
    if (!msg || msg.deleted) return null;
    return msg;
  }

  /**
   * Edits message content.
   */
  public editMessage(
    threadId: string,
    messageId: string,
    editorUserId: string,
    newContent: string
  ): ThreadMessage {
    const msg = this.getMessage(threadId, messageId);
    if (!msg) throw new MessageNotFoundError(messageId);
    if (msg.authorId !== editorUserId) {
      throw new Error('Unauthorized: only author can edit message');
    }

    msg.content = newContent;
    msg.editedAt = Date.now();
    return msg;
  }

  /**
   * Deletes a message and adjusts parent reply count if relevant.
   */
  public deleteMessage(
    thread: ThreadChannel,
    messageId: string,
    _deleterUserId: string
  ): { deletedMessage: ThreadMessage; updatedThread: ThreadChannel } {
    const threadMap = this.threadMessages.get(thread.id);
    if (!threadMap) throw new MessageNotFoundError(messageId);
    const msg = threadMap.get(messageId);
    if (!msg || msg.deleted) throw new MessageNotFoundError(messageId);

    msg.deleted = true;

    // Decrement reply count on parent message
    if (thread.starterMessageId && thread.parentId) {
      const parentStats = this.parentMessageIndex.get(thread.parentId)?.get(thread.starterMessageId);
      if (parentStats && parentStats.replyCount > 0) {
        parentStats.replyCount -= 1;
      }
    }

    // Recalculate remaining active message count & lastMessageId
    const active = Array.from(threadMap.values()).filter((m) => !m.deleted);
    const lastMsg = active.length > 0 ? active[active.length - 1] : null;

    const updatedThread: ThreadChannel = {
      ...thread,
      messageCount: Math.max(0, thread.messageCount - 1),
      lastMessageId: lastMsg ? lastMsg.id : null,
      updatedAt: Date.now(),
    };

    return {
      deletedMessage: msg,
      updatedThread,
    };
  }

  /**
   * Adds an emoji reaction to a message.
   */
  public addReaction(
    threadId: string,
    messageId: string,
    userId: string,
    emoji: string
  ): MessageReaction {
    const msg = this.getMessage(threadId, messageId);
    if (!msg) throw new MessageNotFoundError(messageId);

    let reaction = msg.reactions.get(emoji);
    if (!reaction) {
      reaction = { emoji, count: 0, users: new Set<string>() };
      msg.reactions.set(emoji, reaction);
    }

    if (!reaction.users.has(userId)) {
      reaction.users.add(userId);
      reaction.count += 1;
    }

    return reaction;
  }

  /**
   * Removes an emoji reaction from a message.
   */
  public removeReaction(
    threadId: string,
    messageId: string,
    userId: string,
    emoji: string
  ): MessageReaction | null {
    const msg = this.getMessage(threadId, messageId);
    if (!msg) throw new MessageNotFoundError(messageId);

    const reaction = msg.reactions.get(emoji);
    if (!reaction) return null;

    if (reaction.users.has(userId)) {
      reaction.users.delete(userId);
      reaction.count = Math.max(0, reaction.count - 1);
    }

    if (reaction.count === 0) {
      msg.reactions.delete(emoji);
      return null;
    }

    return reaction;
  }

  /**
   * Clears internal memory (used in testing).
   */
  public clear(): void {
    this.threadMessages.clear();
    this.parentMessageIndex.clear();
    this.rateLimitBuckets.clear();
  }
}
