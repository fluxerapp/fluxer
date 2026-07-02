// SPDX-License-Identifier: AGPL-3.0-or-later

import {Channel} from '@app/features/channel/models/Channel';
import Channels from '@app/features/channel/state/Channels';
import Users from '@app/features/user/state/Users';
import type {ThreadPreviewCard, ThreadResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import {action, makeAutoObservable, observable} from 'mobx';
import Permission from '@app/features/permissions/state/Permission';

export interface ThreadPreview {
	lastMessagePreview: string | null;
	lastMessageAt: Date | null;
	lastMessageAuthorId: string | null;
	lastMessageAuthorUsername: string | null;
	lastMessageAuthorAvatar: string | null;
}

function mapPreview(card: Partial<ThreadPreviewCard>): ThreadPreview {
	return {
		lastMessagePreview: card.last_message_preview ?? null,
		lastMessageAt: card.last_message_at ? new Date(card.last_message_at) : null,
		lastMessageAuthorId: card.last_message_author_id ?? null,
		lastMessageAuthorUsername: card.last_message_author_username ?? null,
		lastMessageAuthorAvatar: card.last_message_author_avatar ?? null,
	};
}

export class Thread {
	readonly id: string;
	readonly guildId?: string;
	readonly name?: string;
	readonly type: number;
	readonly lastMessageId: string | null;
	readonly threadState: number;
	readonly threadParentChannelId: string;
	readonly threadCreatorId: string | null;
	readonly threadCreatorUsername: string | null;
	readonly threadExpiresAt: Date | null;
	readonly threadSourceMessageId: string | null;
	preview: ThreadPreview;
	messageCount: number;
	private readonly _channel: Channel;

	constructor(data: ThreadResponse) {
		this._channel = new Channel(data as ConstructorParameters<typeof Channel>[0]);
		this.id = data.id;
		this.guildId = data.guild_id;
		this.name = data.name;
		this.type = data.type ?? 11;
		this.lastMessageId = data.last_message_id ?? null;
		this.threadState = data.thread_state;
		this.threadParentChannelId = data.thread_parent_channel_id;
		this.threadCreatorId = data.thread_creator_id ?? null;
		this.threadCreatorUsername = data.thread_creator_username ?? null;
		this.threadExpiresAt = data.thread_expires_at ? new Date(data.thread_expires_at) : null;
		this.threadSourceMessageId = data.thread_source_message_id ?? null;
		this.preview = mapPreview(data);
		this.messageCount = data.thread_member_count ?? 0;
		makeAutoObservable(this, {id: false, guildId: false, type: false, threadParentChannelId: false}, {autoBind: true});
	}

	get createdAt(): Date {
		return this._channel.createdAt;
	}

	isOpen(): boolean {
		return this.threadState === 0;
	}

	isClosed(): boolean {
		return this.threadState === 1;
	}

	isArchived(): boolean {
		return this.threadState === 2;
	}

	isLocked(): boolean {
		return false;
	}

	toChannel(): Channel {
		return this._channel;
	}
}

class ThreadStore {
	private readonly threadsById = new Map<string, Thread>();
	private readonly threadsByChannel = new Map<string, Set<string>>();
	readonly joinedThreadIds = observable.set<string>();

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	getThread(threadId: string): Thread | undefined {
		return this.threadsById.get(threadId);
	}

	getThreadsForChannel(channelId: string): ReadonlyArray<Thread> {
		const ids = this.threadsByChannel.get(channelId);
		if (!ids || ids.size === 0) return [];
		const result: Thread[] = [];
		for (const id of ids) {
			const thread = this.threadsById.get(id);
			if (thread) result.push(thread);
		}
		return result.sort((a, b) => a.createdAt.getTime() - b.createdAt.getTime());
	}

	getJoinedThreadsForChannel(channelId: string): ReadonlyArray<Thread> {
		return this.getThreadsForChannel(channelId).filter(
			(t) => this.joinedThreadIds.has(t.id) && t.isOpen(),
		);
	}

	isJoined(threadId: string): boolean {
		return this.joinedThreadIds.has(threadId);
	}

	@action
	private setThread(data: ThreadResponse): void {
		const existing = this.threadsById.get(data.id);
		const thread = new Thread(data);
		if (existing) {
			const prevParent = existing.threadParentChannelId;
			if (prevParent !== thread.threadParentChannelId) {
				this.threadsByChannel.get(prevParent)?.delete(data.id);
			}
			if (data.thread_member_count == null) {
				thread.messageCount = existing.messageCount;
			}
			if (!data.last_message_preview && existing.preview.lastMessagePreview) {
				Object.assign(thread.preview, existing.preview);
			}
		}
		this.threadsById.set(data.id, thread);
		const parentId = thread.threadParentChannelId;
		if (!this.threadsByChannel.has(parentId)) {
			this.threadsByChannel.set(parentId, new Set());
		}
		this.threadsByChannel.get(parentId)!.add(data.id);
		Channels.handleChannelCreate({channel: thread.toChannel().toJSON()});
		Permission.handleChannelUpdate(data.id);
	}

	@action
	private removeThread(threadId: string): void {
		const thread = this.threadsById.get(threadId);
		if (thread) {
			this.threadsByChannel.get(thread.threadParentChannelId)?.delete(threadId);
			this.threadsById.delete(threadId);
			this.joinedThreadIds.delete(threadId);
		}
	}

	@action
	handleThreadCreate(data: ThreadResponse): void {
		this.setThread(data);
	}

	@action
	handleThreadUpdate(data: ThreadResponse): void {
		this.setThread(data);
	}

	@action
	handleThreadDelete({threadId}: {threadId: string}): void {
		this.removeThread(threadId);
	}

	@action
	handleThreadMemberAdd({threadId, userId}: {threadId: string; userId?: string}): void {
		const currentUser = Users.getCurrentUser();
		if (!userId || !currentUser || userId === currentUser.id) {
			this.joinedThreadIds.add(threadId);
		}
	}

	@action
	handleThreadMemberRemove({threadId, userId}: {threadId: string; userId?: string}): void {
		const currentUser = Users.getCurrentUser();
		if (!userId || !currentUser || userId === currentUser.id) {
			this.joinedThreadIds.delete(threadId);
		}
	}

	@action
	handleThreadListSync({threads, joinedThreadIds}: {threads: ThreadResponse[]; joinedThreadIds: string[]}): void {
		for (const thread of threads) {
			this.setThread(thread);
		}
		this.joinedThreadIds.replace(joinedThreadIds);
	}

	@action
	updateThreadPreview(threadId: string, preview: Partial<ThreadPreview>): void {
		const thread = this.threadsById.get(threadId);
		if (!thread) return;
		Object.assign(thread.preview, preview);
		thread.messageCount += 1;
	}

	@action
	handleGuildDelete(guildId: string): void {
		for (const [id, thread] of this.threadsById) {
			if (thread.guildId === guildId) {
				this.removeThread(id);
			}
		}
	}

	@action
	clear(): void {
		this.threadsById.clear();
		this.threadsByChannel.clear();
		this.joinedThreadIds.clear();
	}
}

export default new ThreadStore();
