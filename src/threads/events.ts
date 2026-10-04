/**
 * Gateway WebSocket Events Serializer & Formatter
 * Fluxer Real-time Architecture
 */

import {
	type GatewayEvent,
	GatewayThreadEventType,
	type ThreadChannel,
	type ThreadListSyncPayload,
	type ThreadMember,
	type ThreadMetadata,
} from './types';

/**
 * Serialized representation of a Thread Channel in Gateway payloads.
 */
export interface SerializedThreadChannel {
	id: string;
	type: string;
	guild_id: string;
	parent_id: string;
	owner_id: string;
	name: string;
	last_message_id: string | null;
	message_count: number;
	member_count: number;
	rate_limit_per_user: number;
	starter_message_id: string | null;
	thread_metadata: {
		archived: boolean;
		auto_archive_duration: number;
		archive_timestamp: string;
		locked: boolean;
		invitable?: boolean;
		create_timestamp: string;
		archived_at: string | null;
		locked_at: string | null;
	};
	total_messages_sent: number;
	created_at: string;
	updated_at: string;
}

/**
 * Serialized representation of a Thread Member in Gateway payloads.
 */
export interface SerializedThreadMember {
	id: string;
	user_id: string;
	join_timestamp: string;
	flags: number;
	muted: boolean;
	unread_count: number;
	last_read_message_id: string | null;
	notifications: {
		notification_level: string;
		muted_until: string | null;
		suppress_everyone_mentions: boolean;
		suppress_role_mentions: boolean;
	};
}

/**
 * Serialized THREAD_DELETE payload.
 */
export interface SerializedThreadDeletePayload {
	id: string;
	guild_id: string;
	parent_id: string;
	type: string;
}

/**
 * Serialized THREAD_MEMBERS_UPDATE payload.
 */
export interface SerializedThreadMembersUpdatePayload {
	id: string;
	guild_id: string;
	member_count: number;
	added_members?: Array<SerializedThreadMember>;
	removed_member_ids?: Array<string>;
}

/**
 * Gateway Event Serializer responsible for converting internal entities
 * to standardized Gateway WebSocket wire payloads.
 */
// biome-ignore lint/complexity/noStaticOnlyClass: utility serializer class with static state
export class ThreadEventSerializer {
	private static sequenceCounter = 0;

	/**
	 * Resets sequence counter (useful in tests).
	 */
	public static resetSequence(): void {
		ThreadEventSerializer.sequenceCounter = 0;
	}

	/**
	 * Generates the next monotonic sequence number.
	 */
	public static getNextSequence(): number {
		return ++ThreadEventSerializer.sequenceCounter;
	}

	/**
	 * Serializes a ThreadMetadata object.
	 */
	public static serializeMetadata(meta: ThreadMetadata) {
		return {
			archived: meta.archived,
			auto_archive_duration: Number(meta.autoArchiveDuration),
			archive_timestamp: new Date(meta.archiveTimestamp).toISOString(),
			locked: meta.locked,
			invitable: meta.invitable,
			create_timestamp: new Date(meta.createTimestamp).toISOString(),
			archived_at: meta.archivedAt ? new Date(meta.archivedAt).toISOString() : null,
			locked_at: meta.lockedAt ? new Date(meta.lockedAt).toISOString() : null,
		};
	}

	/**
	 * Serializes a ThreadChannel domain entity to wire format.
	 */
	public static serializeThread(thread: ThreadChannel): SerializedThreadChannel {
		return {
			id: thread.id,
			type: thread.type,
			guild_id: thread.guildId,
			parent_id: thread.parentId,
			owner_id: thread.ownerId,
			name: thread.name,
			last_message_id: thread.lastMessageId,
			message_count: thread.messageCount,
			member_count: thread.memberCount,
			rate_limit_per_user: thread.rateLimitPerUser,
			starter_message_id: thread.starterMessageId,
			thread_metadata: ThreadEventSerializer.serializeMetadata(thread.threadMetadata),
			total_messages_sent: thread.totalMessagesSent,
			created_at: new Date(thread.createdAt).toISOString(),
			updated_at: new Date(thread.updatedAt).toISOString(),
		};
	}

	/**
	 * Serializes a ThreadMember domain entity to wire format.
	 */
	public static serializeMember(member: ThreadMember): SerializedThreadMember {
		return {
			id: member.id,
			user_id: member.userId,
			join_timestamp: new Date(member.joinTimestamp).toISOString(),
			flags: member.flags,
			muted: member.muted,
			unread_count: member.unreadCount,
			last_read_message_id: member.lastReadMessageId,
			notifications: {
				notification_level: member.notifications.notificationLevel,
				muted_until: member.notifications.mutedUntil ? new Date(member.notifications.mutedUntil).toISOString() : null,
				suppress_everyone_mentions: member.notifications.suppressEveryoneMentions,
				suppress_role_mentions: member.notifications.suppressRoleMentions,
			},
		};
	}

	/**
	 * Formats a THREAD_CREATE Gateway event.
	 */
	public static formatThreadCreate(
		thread: ThreadChannel,
		newlyCreated = true,
	): GatewayEvent<SerializedThreadChannel & {newly_created: boolean}> {
		return {
			op: 0,
			t: GatewayThreadEventType.THREAD_CREATE,
			s: ThreadEventSerializer.getNextSequence(),
			ts: Date.now(),
			d: {
				...ThreadEventSerializer.serializeThread(thread),
				newly_created: newlyCreated,
			},
		};
	}

	/**
	 * Formats a THREAD_UPDATE Gateway event.
	 */
	public static formatThreadUpdate(thread: ThreadChannel): GatewayEvent<SerializedThreadChannel> {
		return {
			op: 0,
			t: GatewayThreadEventType.THREAD_UPDATE,
			s: ThreadEventSerializer.getNextSequence(),
			ts: Date.now(),
			d: ThreadEventSerializer.serializeThread(thread),
		};
	}

	/**
	 * Formats a THREAD_DELETE Gateway event.
	 */
	public static formatThreadDelete(
		thread: ThreadChannel | {id: string; guildId: string; parentId: string; type: string},
	): GatewayEvent<SerializedThreadDeletePayload> {
		return {
			op: 0,
			t: GatewayThreadEventType.THREAD_DELETE,
			s: ThreadEventSerializer.getNextSequence(),
			ts: Date.now(),
			d: {
				id: thread.id,
				guild_id: 'guildId' in thread ? thread.guildId : (thread as any).guild_id,
				parent_id: 'parentId' in thread ? thread.parentId : (thread as any).parent_id,
				type: thread.type,
			},
		};
	}

	/**
	 * Formats a THREAD_LIST_SYNC Gateway event.
	 */
	public static formatThreadListSync(payload: ThreadListSyncPayload): GatewayEvent<{
		guild_id: string;
		channel_ids?: Array<string>;
		threads: Array<SerializedThreadChannel>;
		members: Array<SerializedThreadMember>;
	}> {
		return {
			op: 0,
			t: GatewayThreadEventType.THREAD_LIST_SYNC,
			s: ThreadEventSerializer.getNextSequence(),
			ts: Date.now(),
			d: {
				guild_id: payload.guildId,
				channel_ids: payload.channelIds,
				threads: payload.threads.map((t) => ThreadEventSerializer.serializeThread(t)),
				members: payload.members.map((m) => ThreadEventSerializer.serializeMember(m)),
			},
		};
	}

	/**
	 * Formats a THREAD_MEMBER_UPDATE Gateway event.
	 */
	public static formatThreadMemberUpdate(
		member: ThreadMember,
		guildId?: string,
	): GatewayEvent<SerializedThreadMember & {guild_id?: string}> {
		return {
			op: 0,
			t: GatewayThreadEventType.THREAD_MEMBER_UPDATE,
			s: ThreadEventSerializer.getNextSequence(),
			ts: Date.now(),
			d: {
				...ThreadEventSerializer.serializeMember(member),
				...(guildId ? {guild_id: guildId} : {}),
			},
		};
	}

	/**
	 * Formats a THREAD_MEMBERS_UPDATE Gateway event.
	 */
	public static formatThreadMembersUpdate(
		threadId: string,
		guildId: string,
		memberCount: number,
		addedMembers: Array<ThreadMember> = [],
		removedMemberIds: Array<string> = [],
	): GatewayEvent<SerializedThreadMembersUpdatePayload> {
		return {
			op: 0,
			t: GatewayThreadEventType.THREAD_MEMBERS_UPDATE,
			s: ThreadEventSerializer.getNextSequence(),
			ts: Date.now(),
			d: {
				id: threadId,
				guild_id: guildId,
				member_count: memberCount,
				added_members: addedMembers.map((m) => ThreadEventSerializer.serializeMember(m)),
				removed_member_ids: removedMemberIds,
			},
		};
	}

	/**
	 * Converts a gateway event to a JSON string ready for transmission.
	 */
	public static toJSONString<T>(event: GatewayEvent<T>): string {
		return JSON.stringify(event);
	}

	/**
	 * Parses and validates a raw gateway event string.
	 */
	public static parseGatewayEvent<T = unknown>(rawJson: string): GatewayEvent<T> {
		const parsed = JSON.parse(rawJson);
		if (typeof parsed !== 'object' || parsed === null) {
			throw new Error('Invalid gateway packet: not an object');
		}
		if (typeof parsed.op !== 'number' || typeof parsed.t !== 'string' || !parsed.d) {
			throw new Error('Invalid gateway packet: missing op, t, or d fields');
		}
		return parsed as GatewayEvent<T>;
	}
}
