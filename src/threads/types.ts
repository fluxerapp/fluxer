/**
 * Message Threads Architecture - Types & Schemas
 * Fluxer Application Architecture
 */

/**
 * Thread channel types supported in Fluxer.
 */
export enum ThreadType {
  PUBLIC_THREAD = 'PUBLIC_THREAD',
  PRIVATE_THREAD = 'PRIVATE_THREAD',
  ANNOUNCEMENT_THREAD = 'ANNOUNCEMENT_THREAD',
}

/**
 * Numeric ThreadType constants for Discord/Fluxer binary/numeric compatibility.
 */
export enum NumericThreadType {
  ANNOUNCEMENT_THREAD = 10,
  PUBLIC_THREAD = 11,
  PRIVATE_THREAD = 12,
}

/**
 * Duration in minutes before an inactive thread is automatically archived.
 */
export enum AutoArchiveDuration {
  ONE_HOUR = 60,
  ONE_DAY = 1440,
  THREE_DAYS = 4320,
  ONE_WEEK = 10080,
}

/**
 * Lifecycle states of a thread.
 */
export enum ThreadState {
  ACTIVE = 'ACTIVE',
  ARCHIVED = 'ARCHIVED',
  LOCKED = 'LOCKED',
  DELETED = 'DELETED',
}

/**
 * Notification subscription levels for thread participants.
 */
export enum ThreadNotificationLevel {
  ALL_MESSAGES = 'ALL_MESSAGES',
  ONLY_MENTIONS = 'ONLY_MENTIONS',
  MUTED = 'MUTED',
}

/**
 * Permission bit flags for Thread actions.
 */
export enum ThreadPermission {
  VIEW_CHANNEL = 1 << 0,
  SEND_MESSAGES = 1 << 1,
  SEND_MESSAGES_IN_THREADS = 1 << 2,
  CREATE_PUBLIC_THREADS = 1 << 3,
  CREATE_PRIVATE_THREADS = 1 << 4,
  MANAGE_THREADS = 1 << 5,
  MANAGE_MESSAGES = 1 << 6,
  READ_MESSAGE_HISTORY = 1 << 7,
  MENTION_EVERYONE = 1 << 8,
}

/**
 * Metadata associated with thread lifecycle and auto-archiving.
 */
export interface ThreadMetadata {
  archived: boolean;
  autoArchiveDuration: AutoArchiveDuration;
  archiveTimestamp: number;
  locked: boolean;
  invitable: boolean;
  createTimestamp: number;
  archivedAt: number | null;
  lockedAt: number | null;
}

/**
 * Subscription and alert preferences for a thread member.
 */
export interface ThreadSubscriptionSettings {
  notificationLevel: ThreadNotificationLevel;
  mutedUntil: number | null;
  suppressEveryoneMentions: boolean;
  suppressRoleMentions: boolean;
}

/**
 * Thread member entity representing a participant in a thread.
 */
export interface ThreadMember {
  id: string; // Composite key: `${threadId}:${userId}` or user ID
  threadId: string;
  userId: string;
  joinTimestamp: number;
  flags: number;
  notifications: ThreadSubscriptionSettings;
  lastReadMessageId: string | null;
  lastReadTimestamp: number | null;
  unreadCount: number;
  muted: boolean;
}

/**
 * Thread channel entity representing the thread container.
 */
export interface ThreadChannel {
  id: string;
  guildId: string;
  parentId: string; // The parent text channel ID
  ownerId: string;
  name: string;
  type: ThreadType;
  state: ThreadState;
  lastMessageId: string | null;
  lastPinTimestamp?: number | null;
  messageCount: number;
  memberCount: number;
  rateLimitPerUser: number; // Slowmode in seconds (0 = disabled)
  threadMetadata: ThreadMetadata;
  starterMessageId: string | null; // Null if standalone thread
  createdAt: number;
  updatedAt: number;
  lastActiveTimestamp: number;
  totalMessagesSent: number;
}

/**
 * Message attachment entity.
 */
export interface MessageAttachment {
  id: string;
  filename: string;
  size: number;
  url: string;
  contentType?: string;
}

/**
 * Reaction aggregate entity.
 */
export interface MessageReaction {
  emoji: string;
  count: number;
  users: Set<string>;
}

/**
 * Message entity within a thread or parent channel.
 */
export interface ThreadMessage {
  id: string;
  channelId: string; // In thread context, this is the threadId
  guildId: string;
  parentId?: string | null; // Parent channel or message reference
  authorId: string;
  content: string;
  attachments: MessageAttachment[];
  mentions: string[]; // List of user IDs mentioned
  mentionRoles: string[]; // List of role IDs mentioned
  mentionEveryone: boolean;
  replyCount: number;
  lastReplyId?: string | null;
  createdAt: number;
  editedAt: number | null;
  pinned: boolean;
  reactions: Map<string, MessageReaction>;
  deleted: boolean;
}

/**
 * Gateway WebSocket payload for bulk thread list synchronization.
 */
export interface ThreadListSyncPayload {
  guildId: string;
  channelIds?: string[];
  threads: ThreadChannel[];
  members: ThreadMember[];
  mostRecentMessages?: ThreadMessage[];
}

/**
 * Cursor-based pagination options for thread messages.
 */
export interface MessagePaginationOptions {
  before?: string;
  after?: string;
  around?: string;
  limit?: number;
}

/**
 * Paginated response structure.
 */
export interface PaginatedResult<T> {
  items: T[];
  hasMore: boolean;
  nextCursor: string | null;
  prevCursor: string | null;
  totalCount: number;
}

/**
 * Options when creating a thread from an existing message.
 */
export interface CreateThreadFromMessageOptions {
  channelId: string;
  messageId: string;
  name: string;
  autoArchiveDuration?: AutoArchiveDuration;
  creatorId: string;
  rateLimitPerUser?: number;
  guildId?: string;
}

/**
 * Options when creating a standalone thread.
 */
export interface CreateStandaloneThreadOptions {
  channelId: string;
  name: string;
  type: ThreadType;
  autoArchiveDuration?: AutoArchiveDuration;
  creatorId: string;
  invitable?: boolean;
  rateLimitPerUser?: number;
  guildId?: string;
}

/**
 * Notification routing decision outcome.
 */
export interface NotificationRoutingResult {
  userId: string;
  reason: 'DIRECT_MENTION' | 'ROLE_MENTION' | 'EVERYONE_MENTION' | 'THREAD_SUBSCRIPTION';
  shouldAlert: boolean;
  incrementBadge: boolean;
  suppressed: boolean;
}

/**
 * Gateway event types dispatched by the thread engine.
 */
export enum GatewayThreadEventType {
  THREAD_CREATE = 'THREAD_CREATE',
  THREAD_UPDATE = 'THREAD_UPDATE',
  THREAD_DELETE = 'THREAD_DELETE',
  THREAD_LIST_SYNC = 'THREAD_LIST_SYNC',
  THREAD_MEMBER_UPDATE = 'THREAD_MEMBER_UPDATE',
  THREAD_MEMBERS_UPDATE = 'THREAD_MEMBERS_UPDATE',
}

/**
 * Standard WebSocket Gateway Event envelope.
 */
export interface GatewayEvent<T = unknown> {
  op: number;
  t: GatewayThreadEventType | string;
  d: T;
  s?: number;
  ts: number;
}
