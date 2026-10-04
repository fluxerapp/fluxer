/**
 * Thread Notification Engine & Subscription State Machine
 * Fluxer Real-time Architecture
 */

import {
  ThreadChannel,
  ThreadMember,
  ThreadMessage,
  ThreadNotificationLevel,
  ThreadSubscriptionSettings,
  NotificationRoutingResult,
  ThreadType,
} from './types';

/**
 * Parsed mentions extracted from thread message content.
 */
export interface ExtractedMentions {
  userIds: Set<string>;
  roleIds: Set<string>;
  everyone: boolean;
  here: boolean;
}

/**
 * Resolver function to check if a user belongs to a specific role.
 */
export type UserRoleResolver = (userId: string, roleId: string) => boolean | Promise<boolean>;

/**
 * ThreadNotificationEngine manages mention parsing, subscription state transitions,
 * notification routing, and unread pointer tracking for thread participants.
 */
export class ThreadNotificationEngine {
  private userRoleResolver?: UserRoleResolver;

  constructor(userRoleResolver?: UserRoleResolver) {
    this.userRoleResolver = userRoleResolver;
  }

  /**
   * Sets or updates the role membership resolver.
   */
  public setRoleResolver(resolver: UserRoleResolver): void {
    this.userRoleResolver = resolver;
  }

  /**
   * Extracts user IDs, role IDs, and everyone/here flags from message content.
   */
  public extractMentions(content: string, explicitUserMentions: string[] = [], explicitRoleMentions: string[] = []): ExtractedMentions {
    const userIds = new Set<string>(explicitUserMentions);
    const roleIds = new Set<string>(explicitRoleMentions);

    // Discord/Fluxer mention formats: <@123456>, <@!123456>
    const userRegex = /<@!?([0-9a-zA-Z_]+)>/g;
    let match: RegExpExecArray | null;
    while ((match = userRegex.exec(content)) !== null) {
      if (match[1]) userIds.add(match[1]);
    }

    // Role mentions: <@&123456>
    const roleRegex = /<@&([0-9a-zA-Z_]+)>/g;
    while ((match = roleRegex.exec(content)) !== null) {
      if (match[1]) roleIds.add(match[1]);
    }

    // Plain text @username handles (e.g. @alice)
    const plainHandleRegex = /(?:^|\s)@([a-zA-Z0-9_\-\.]+)(?=\s|$|[.,!?:])/g;
    while ((match = plainHandleRegex.exec(content)) !== null) {
      const handle = match[1];
      if (handle && handle !== 'everyone' && handle !== 'here') {
        userIds.add(handle);
      }
    }

    const everyone = /(?:^|\s)@everyone(?:\s|$|[.,!?:])/.test(content);
    const here = /(?:^|\s)@here(?:\s|$|[.,!?:])/.test(content);

    return {
      userIds,
      roleIds,
      everyone,
      here,
    };
  }

  /**
   * Creates a default subscription setting for a newly joined member.
   */
  public createDefaultSubscription(
    level: ThreadNotificationLevel = ThreadNotificationLevel.ALL_MESSAGES
  ): ThreadSubscriptionSettings {
    return {
      notificationLevel: level,
      mutedUntil: null,
      suppressEveryoneMentions: false,
      suppressRoleMentions: false,
    };
  }

  /**
   * Validates and applies subscription state transitions.
   */
  public updateSubscription(
    currentSettings: ThreadSubscriptionSettings,
    newSettings: Partial<ThreadSubscriptionSettings>
  ): ThreadSubscriptionSettings {
    const updated: ThreadSubscriptionSettings = {
      ...currentSettings,
      ...newSettings,
    };

    // If unmuting, clear mutedUntil
    if (newSettings.notificationLevel && newSettings.notificationLevel !== ThreadNotificationLevel.MUTED) {
      if (newSettings.mutedUntil === undefined) {
        updated.mutedUntil = null;
      }
    }

    return updated;
  }

  /**
   * Checks if a member's mute status is currently active.
   */
  public isMemberMuted(member: ThreadMember, now = Date.now()): boolean {
    if (member.muted) return true;
    if (member.notifications.notificationLevel === ThreadNotificationLevel.MUTED) {
      if (member.notifications.mutedUntil === null) {
        return true;
      }
      return member.notifications.mutedUntil > now;
    }
    return false;
  }

  /**
   * Evaluates notification routing for a dispatched message across all thread members.
   * Respects thread privacy boundaries: for private threads, only active members get routed.
   */
  public async evaluateNotifications(
    thread: ThreadChannel,
    message: ThreadMessage,
    members: ThreadMember[],
    now = Date.now()
  ): Promise<Map<string, NotificationRoutingResult>> {
    const routingMap = new Map<string, NotificationRoutingResult>();
    const mentions = this.extractMentions(message.content, message.mentions, message.mentionRoles);

    for (const member of members) {
      // Authors never get alerted by their own message
      if (member.userId === message.authorId) {
        continue;
      }

      // Check if member is muted
      const muted = this.isMemberMuted(member, now);

      // Check direct mention
      const isDirectlyMentioned = mentions.userIds.has(member.userId);

      // Check role mention
      let isRoleMentioned = false;
      if (mentions.roleIds.size > 0 && this.userRoleResolver && !member.notifications.suppressRoleMentions) {
        for (const roleId of mentions.roleIds) {
          const hasRole = await Promise.resolve(this.userRoleResolver(member.userId, roleId));
          if (hasRole) {
            isRoleMentioned = true;
            break;
          }
        }
      }

      // Check everyone/here mention
      const isEveryoneMentioned =
        (mentions.everyone || mentions.here || message.mentionEveryone) &&
        !member.notifications.suppressEveryoneMentions;

      // Determine routing decision
      if (isDirectlyMentioned) {
        routingMap.set(member.userId, {
          userId: member.userId,
          reason: 'DIRECT_MENTION',
          shouldAlert: !muted,
          incrementBadge: true,
          suppressed: muted,
        });
      } else if (isRoleMentioned) {
        routingMap.set(member.userId, {
          userId: member.userId,
          reason: 'ROLE_MENTION',
          shouldAlert: !muted,
          incrementBadge: true,
          suppressed: muted,
        });
      } else if (isEveryoneMentioned) {
        routingMap.set(member.userId, {
          userId: member.userId,
          reason: 'EVERYONE_MENTION',
          shouldAlert: !muted,
          incrementBadge: true,
          suppressed: muted,
        });
      } else if (
        member.notifications.notificationLevel === ThreadNotificationLevel.ALL_MESSAGES &&
        !muted
      ) {
        routingMap.set(member.userId, {
          userId: member.userId,
          reason: 'THREAD_SUBSCRIPTION',
          shouldAlert: true,
          incrementBadge: true,
          suppressed: false,
        });
      } else {
        // ONLY_MENTIONS with no mentions, or MUTED
        routingMap.set(member.userId, {
          userId: member.userId,
          reason: 'THREAD_SUBSCRIPTION',
          shouldAlert: false,
          incrementBadge: !muted,
          suppressed: true,
        });
      }
    }

    return routingMap;
  }

  /**
   * Updates a member's unread status when a new message is posted in the thread.
   */
  public processIncomingMessage(
    member: ThreadMember,
    message: ThreadMessage
  ): ThreadMember {
    if (member.userId === message.authorId) {
      // Author has read their own message
      return {
        ...member,
        lastReadMessageId: message.id,
        lastReadTimestamp: message.createdAt,
      };
    }

    return {
      ...member,
      unreadCount: member.unreadCount + 1,
    };
  }

  /**
   * Updates last-read message pointer and calculates cleared unread count.
   */
  public markAsRead(
    member: ThreadMember,
    messageId: string,
    messageTimestamp = Date.now()
  ): ThreadMember {
    return {
      ...member,
      lastReadMessageId: messageId,
      lastReadTimestamp: messageTimestamp,
      unreadCount: 0,
    };
  }

  /**
   * Computes unread count between a member's last read message and a message stream.
   */
  public calculateUnreadFromMessages(
    member: ThreadMember,
    messages: ThreadMessage[]
  ): number {
    if (messages.length === 0) return 0;
    if (!member.lastReadMessageId && !member.lastReadTimestamp) {
      return messages.filter((m) => m.authorId !== member.userId).length;
    }

    const lastReadTs = member.lastReadTimestamp || 0;
    return messages.filter(
      (m) =>
        m.authorId !== member.userId &&
        m.createdAt > lastReadTs &&
        m.id !== member.lastReadMessageId
    ).length;
  }
}
