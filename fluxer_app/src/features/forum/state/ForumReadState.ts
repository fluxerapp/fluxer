// SPDX-License-Identifier: AGPL-3.0-or-later

import Authentication from '@app/features/auth/state/Authentication';
import type {Channel} from '@app/features/channel/models/Channel';
import GatewayConnection from '@app/features/gateway/transport/GatewayConnection';
import ReadStates from '@app/features/read_state/state/ReadStates';
import ThreadGuilds from '@app/features/threads/state/ThreadGuilds';
import ThreadMemberships from '@app/features/threads/state/ThreadMemberships';
import * as UserGuildSettingsCommands from '@app/features/user/commands/UserGuildSettingsCommands';
import UserGuildSettings from '@app/features/user/state/UserGuildSettings';
import {
	CHANNEL_OVERRIDE_FLAG_MASK,
	ChannelOverrideFlags,
	FORUM_UNREADS_MAX_THREADS,
} from '@fluxer/constants/src/ThreadConstants';
import * as SnowflakeUtils from '@fluxer/snowflake/src/SnowflakeUtils';
import {makeAutoObservable, observable} from 'mobx';

export function isNewPostsUnreadEnabled(forum: Channel): boolean {
	const flags = UserGuildSettings.getChannelOverride(forum.guildId ?? null, forum.id)?.flags ?? 0;
	return (flags & ChannelOverrideFlags.NEW_FORUM_THREADS_OFF) === 0;
}

export function setNewPostsUnreadEnabled(forum: Channel, enabled: boolean): void {
	if (!forum.guildId || !ThreadGuilds.isActive(forum.guildId)) return;
	const flags = UserGuildSettings.getChannelOverride(forum.guildId, forum.id)?.flags ?? 0;
	const next =
		(flags & ~CHANNEL_OVERRIDE_FLAG_MASK) |
		(enabled ? ChannelOverrideFlags.NEW_FORUM_THREADS_ON : ChannelOverrideFlags.NEW_FORUM_THREADS_OFF);
	UserGuildSettingsCommands.updateChannelOverride(forum.guildId, forum.id, {flags: next});
}

export function hasForumUnread(forum: Channel): boolean {
	return isNewPostsUnreadEnabled(forum) && ReadStates.hasUnread(forum.id);
}

export function isPostNewerThan(
	post: {id: string; ownerId: string | null},
	snapshotId: string,
	currentUserId: string | null,
): boolean {
	if (post.ownerId != null && post.ownerId === currentUserId) return false;
	return SnowflakeUtils.compare(post.id, snapshotId) > 0;
}

export interface ForumUnreadsPayload {
	guild_id: string;
	channel_id: string;
	threads: Array<{thread_id: string; count?: number; missing?: boolean}>;
}

const FORUM_UNREADS_SENDS_PER_WINDOW = 4;
const FORUM_UNREADS_WINDOW_MS = 5000;
const FORUM_UNREADS_REPLY_TIMEOUT_MS = 15000;

interface QueuedAck {
	guildId: string;
	forumId: string;
	ackMessageId: string;
}

interface PostUnread {
	count: number;
	ackMessageId: string;
}

function postAckMessageId(postId: string): string | null {
	return ReadStates.getIfExists(postId)?.ackMessageId ?? null;
}

class ForumReadState {
	private readonly snapshots = observable.map<string, string>();
	private readonly postUnreads = observable.map<string, PostUnread>();
	private readonly sentAcks = new Map<string, string>();
	private readonly awaiting = new Map<string, number>();
	private readonly queue = new Map<string, QueuedAck>();
	private sendTimes: Array<number> = [];
	private pump: ReturnType<typeof setTimeout> | null = null;

	constructor() {
		makeAutoObservable<this, 'snapshots' | 'postUnreads' | 'sentAcks' | 'awaiting' | 'queue' | 'sendTimes' | 'pump'>(
			this,
			{
				snapshots: false,
				postUnreads: false,
				sentAcks: false,
				awaiting: false,
				queue: false,
				sendTimes: false,
				pump: false,
			},
			{autoBind: true},
		);
	}

	requestPostUnreads(guildId: string, forumId: string, postIds: ReadonlyArray<string>): void {
		this.expireAwaiting();
		for (const postId of postIds) {
			if (ThreadMemberships.isMember(postId)) continue;
			const ackMessageId = postAckMessageId(postId);
			if (!ackMessageId || this.sentAcks.get(postId) === ackMessageId) continue;
			if (this.queue.get(postId)?.ackMessageId === ackMessageId) continue;
			this.queue.set(postId, {guildId, forumId, ackMessageId});
		}
		this.drain();
	}

	private expireAwaiting(): void {
		const now = Date.now();
		for (const [postId, sentAt] of this.awaiting) {
			if (now - sentAt < FORUM_UNREADS_REPLY_TIMEOUT_MS) continue;
			this.awaiting.delete(postId);
			this.sentAcks.delete(postId);
		}
	}

	private drain(): void {
		if (this.pump != null) clearTimeout(this.pump);
		this.pump = null;
		while (this.queue.size > 0) {
			const socket = GatewayConnection.socket;
			if (!socket?.isConnected()) {
				this.queue.clear();
				return;
			}
			const now = Date.now();
			this.sendTimes = this.sendTimes.filter((sentAt) => now - sentAt < FORUM_UNREADS_WINDOW_MS);
			if (this.sendTimes.length >= FORUM_UNREADS_SENDS_PER_WINDOW) {
				this.pump = setTimeout(this.drain, FORUM_UNREADS_WINDOW_MS - (now - this.sendTimes[0]));
				return;
			}
			const head = this.queue.values().next().value!;
			const threads: Array<{thread_id: string; ack_message_id: string}> = [];
			for (const [postId, entry] of this.queue) {
				if (threads.length >= FORUM_UNREADS_MAX_THREADS) break;
				if (entry.guildId === head.guildId && entry.forumId === head.forumId) {
					threads.push({thread_id: postId, ack_message_id: entry.ackMessageId});
				}
			}
			for (const entry of threads) {
				this.queue.delete(entry.thread_id);
				this.awaiting.set(entry.thread_id, now);
				this.sentAcks.set(entry.thread_id, entry.ack_message_id);
			}
			this.sendTimes.push(now);
			socket.requestForumUnreads({guild_id: head.guildId, channel_id: head.forumId, threads});
		}
	}

	private resetRequests(): void {
		if (this.pump != null) clearTimeout(this.pump);
		this.pump = null;
		this.queue.clear();
		this.awaiting.clear();
		this.sentAcks.clear();
	}

	handleGatewayReady(): void {
		this.resetRequests();
	}

	purgeGuild(guildId: string): void {
		for (const [postId, entry] of this.queue) {
			if (entry.guildId === guildId) this.queue.delete(postId);
		}
	}

	handleForumUnreads(data: ForumUnreadsPayload): void {
		for (const entry of data.threads) {
			this.awaiting.delete(entry.thread_id);
			const ackMessageId = this.sentAcks.get(entry.thread_id);
			if (entry.count && ackMessageId && ackMessageId === postAckMessageId(entry.thread_id)) {
				this.postUnreads.set(entry.thread_id, {count: entry.count, ackMessageId});
			} else {
				this.postUnreads.delete(entry.thread_id);
			}
		}
	}

	getPostUnreadCount(postId: string): number {
		const entry = this.postUnreads.get(postId);
		if (entry == null || ThreadMemberships.isMember(postId)) return 0;
		void ReadStates.version;
		return postAckMessageId(postId) === entry.ackMessageId ? entry.count : 0;
	}

	beginViewing(forumId: string): void {
		if (this.snapshots.has(forumId)) return;
		this.resetRequests();
		const state = ReadStates.get(forumId);
		this.snapshots.set(forumId, state.ackMessageId ?? SnowflakeUtils.fromTimestamp(state.ackTimestamp));
	}

	endViewing(forumId: string): void {
		this.snapshots.delete(forumId);
	}

	getSnapshot(forumId: string): string | undefined {
		return this.snapshots.get(forumId);
	}

	isNewPost(post: Channel): boolean {
		const snapshot = post.parentId ? this.snapshots.get(post.parentId) : undefined;
		if (snapshot == null) return false;
		return isPostNewerThan(post, snapshot, Authentication.currentUserId);
	}
}

export default new ForumReadState();
