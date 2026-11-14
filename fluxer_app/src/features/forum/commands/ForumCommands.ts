// SPDX-License-Identifier: AGPL-3.0-or-later

import {Endpoints} from '@app/features/app/constants/Endpoints';
import type {Channel, ChannelWire} from '@app/features/channel/models/Channel';
import Channels from '@app/features/channel/state/Channels';
import ForumPosts from '@app/features/forum/state/ForumPosts';
import MessageQueue from '@app/features/messaging/state/MessageQueue';
import {CloudUpload} from '@app/features/messaging/upload/CloudUpload';
import {normalizeMessageContent} from '@app/features/messaging/utils/MessageRequestUtils';
import {http} from '@app/features/platform/transport/RestTransport';
import ChannelThreads from '@app/features/threads/state/ChannelThreads';
import ThreadGuilds from '@app/features/threads/state/ThreadGuilds';
import {ChannelFlags} from '@fluxer/constants/src/ThreadConstants';
import type {Message as WireMessage} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';

export interface ForumTagInput {
	id?: string;
	name: string;
	moderated: boolean;
	emoji_id: string | null;
	emoji_name: string | null;
}

export interface ForumChannelPatch {
	topic?: string | null;
	rate_limit_per_user?: number;
	default_auto_archive_duration?: number | null;
	default_thread_rate_limit_per_user?: number;
	default_reaction_emoji?: {emoji_id: string | null; emoji_name: string | null} | null;
	default_sort_order?: number | null;
	default_forum_layout?: number;
	default_tag_setting?: string | null;
	available_tags?: ReadonlyArray<ForumTagInput>;
	flags?: number;
}

export interface ForumPostInput {
	name: string;
	appliedTags: ReadonlyArray<string>;
	content: string;
	nonce: string;
	hasAttachments: boolean;
	stickerIds: ReadonlyArray<string>;
}

function assertActive(channel: Channel): asserts channel is Channel & {guildId: string} {
	if (!channel.guildId || !ThreadGuilds.isActive(channel.guildId)) {
		throw new Error('Forums are not available in this community');
	}
}

function ingestChannel(wire: ChannelWire | undefined): void {
	if (!wire) return;
	const existing = Channels.getChannel(wire.id);
	Channels.handleChannelCreate({channel: existing ? existing.withUpdates(wire).toJSON() : wire});
}

export async function createForumPost(forum: Channel, input: ForumPostInput): Promise<Channel | undefined> {
	assertActive(forum);
	const prepared = input.hasAttachments
		? await MessageQueue.prepareAttachmentsForSend({channelId: forum.id, nonce: input.nonce})
		: {};
	if (prepared == null) return undefined;
	assertActive(forum);
	const normalized = normalizeMessageContent(input.content);
	const message: Record<string, unknown> = {content: normalized.content};
	if (normalized.flags !== 0) message.flags = normalized.flags;
	if (prepared.attachments?.length) message.attachments = prepared.attachments;
	if (input.stickerIds.length > 0) message.sticker_ids = [...input.stickerIds];
	const payload = {
		name: input.name,
		auto_archive_duration: forum.defaultAutoArchiveDuration ?? undefined,
		applied_tags: input.appliedTags.length > 0 ? [...input.appliedTags] : undefined,
		message,
	};
	let body: FormData | typeof payload = payload;
	if (prepared.files?.length) {
		const formData = new FormData();
		formData.append('payload_json', JSON.stringify(payload));
		prepared.files.forEach((file, index) => {
			formData.append(`files[${index}]`, file);
		});
		body = formData;
	}
	try {
		const response = await http.post<ChannelWire & {message?: WireMessage}>(Endpoints.CHANNEL_THREADS(forum.id), {
			body,
			onProgress: prepared.files?.length
				? (event) => {
						if (event.lengthComputable && event.total > 0) {
							CloudUpload.updateSendingProgress(input.nonce, (event.loaded / event.total) * 100);
						}
					}
				: undefined,
		});
		const {message: firstMessage, ...wire} = response.body;
		if (input.hasAttachments) CloudUpload.removeMessageUpload(input.nonce);
		if (firstMessage) ForumPosts.setFirstMessage(firstMessage);
		return ChannelThreads.upsert(wire, forum.guildId);
	} catch (error) {
		if (input.hasAttachments) CloudUpload.restoreAttachmentsToTextarea(input.nonce);
		throw error;
	}
}

export async function updateForumChannel(forum: Channel, patch: ForumChannelPatch): Promise<void> {
	assertActive(forum);
	const response = await http.patch<ChannelWire>(Endpoints.CHANNEL(forum.id), {body: patch});
	ingestChannel(response.body);
}

export async function createForumTag(forum: Channel, tag: ForumTagInput): Promise<void> {
	assertActive(forum);
	const {id: _id, ...body} = tag;
	const response = await http.post<ChannelWire>(Endpoints.CHANNEL_TAGS(forum.id), {body});
	ingestChannel(response.body);
}

export async function updateForumTag(forum: Channel, tagId: string, tag: ForumTagInput): Promise<void> {
	assertActive(forum);
	const {id: _id, ...body} = tag;
	const response = await http.put<ChannelWire>(Endpoints.CHANNEL_TAG(forum.id, tagId), {body});
	ingestChannel(response.body);
}

export async function deleteForumTag(forum: Channel, tagId: string): Promise<void> {
	assertActive(forum);
	const response = await http.delete<ChannelWire>(Endpoints.CHANNEL_TAG(forum.id, tagId));
	ingestChannel(response.body);
}

async function patchPost(post: Channel, body: Record<string, unknown>): Promise<void> {
	assertActive(post);
	const response = await http.patch<ChannelWire>(Endpoints.CHANNEL(post.id), {body});
	if (response.body) ChannelThreads.upsert(response.body, post.guildId);
}

export function setPostPinned(post: Channel, pinned: boolean): Promise<void> {
	const flags = pinned ? post.flags | ChannelFlags.PINNED : post.flags & ~ChannelFlags.PINNED;
	return patchPost(post, {flags});
}

export function setPostTags(post: Channel, appliedTags: ReadonlyArray<string>): Promise<void> {
	return patchPost(post, {applied_tags: [...appliedTags]});
}
