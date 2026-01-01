/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type {ChannelID, MessageID, UserID} from '~/BrandedTypes';
import {makeAttachmentCdnUrl} from '~/channel/services/message/MessageHelpers';
import type {IMediaService} from '~/infrastructure/IMediaService';
import type {UserCacheService} from '~/infrastructure/UserCacheService';
import type {Attachment, Embed, EmbedField, Message, MessageReaction, MessageRef, StickerItem, User} from '~/Models';
import type {RequestCache} from '~/middleware/RequestCacheMiddleware';
import type {AttachmentDecayRow} from '~/types/AttachmentDecayTypes';
import {getCachedUserPartialResponse, getCachedUserPartialResponses} from '~/user/UserCacheHelpers';
import type {UserPartialResponse} from '~/user/UserModel';
import * as SnowflakeUtils from '~/utils/SnowflakeUtils';
import type {EmbedFieldResponse, MessageEmbedResponse} from './EmbedTypes';
import type {
	MessageAttachmentResponse,
	MessageReactionResponse,
	MessageReferenceResponse,
	MessageResponse,
	MessageStickerResponse,
} from './MessageTypes';

interface MapMessageToResponseParams {
	message: Message;
	currentUserId?: UserID;
	nonce?: string;
	tts?: boolean;
	withMessageReference?: boolean;
	getAuthor?: (userId: UserID) => Promise<User | null>;
	getReactions?: (channelId: ChannelID, messageId: MessageID) => Promise<Array<MessageReaction>>;
	getReferencedMessage?: (channelId: ChannelID, messageId: MessageID) => Promise<Message | null>;
	setHasReaction?: (channelId: ChannelID, messageId: MessageID, hasReaction: boolean) => Promise<void>;
	userCacheService: UserCacheService;
	requestCache: RequestCache;
	mediaService: IMediaService;
	attachmentDecayMap?: Map<string, AttachmentDecayRow>;
}

interface MapEmbedFieldToResponseParams {
	field: EmbedField;
}

interface MapMessageEmbedToResponseParams {
	embed: Embed;
	mediaService: IMediaService;
}

interface MapMessageAttachmentToResponseParams {
	channelId: ChannelID;
	attachment: Attachment;
	mediaService: IMediaService;
}

interface MapReactionsParams {
	reactions: Array<MessageReaction>;
	currentUserId: UserID;
}

interface MapMessageReferenceParams {
	reference: MessageRef;
}

const mapEmbedFieldToResponse = ({field}: MapEmbedFieldToResponseParams): EmbedFieldResponse => ({
	name: field.name,
	value: field.value,
	inline: field.inline ?? false,
});

const mapMessageEmbedToResponse = ({embed, mediaService}: MapMessageEmbedToResponseParams): MessageEmbedResponse => ({
	type: embed.type ?? 'rich',
	url: embed.url ?? undefined,
	title: embed.title ?? undefined,
	color: embed.color ?? undefined,
	timestamp: embed.timestamp?.toISOString() ?? undefined,
	description: embed.description ?? undefined,
	author: embed.author
		? {
				name: embed.author.name!,
				url: embed.author.url ?? undefined,
				icon_url: embed.author.iconUrl ?? undefined,
				proxy_icon_url: embed.author.iconUrl ? mediaService.getExternalMediaProxyURL(embed.author.iconUrl) : undefined,
			}
		: undefined,
	image: embed.image
		? {
				url: embed.image.url!,
				proxy_url: mediaService.getExternalMediaProxyURL(embed.image.url!),
				content_type: embed.image.contentType ?? undefined,
				content_hash: embed.image.contentHash ?? undefined,
				width: embed.image.width ?? undefined,
				height: embed.image.height ?? undefined,
				description: embed.image.description ?? undefined,
				placeholder: embed.image.placeholder ?? undefined,
				flags: embed.image.flags,
			}
		: undefined,
	thumbnail: embed.thumbnail
		? {
				url: embed.thumbnail.url!,
				proxy_url: mediaService.getExternalMediaProxyURL(embed.thumbnail.url!),
				content_type: embed.thumbnail.contentType ?? undefined,
				content_hash: embed.thumbnail.contentHash ?? undefined,
				width: embed.thumbnail.width ?? undefined,
				height: embed.thumbnail.height ?? undefined,
				description: embed.thumbnail.description ?? undefined,
				placeholder: embed.thumbnail.placeholder ?? undefined,
				flags: embed.thumbnail.flags,
			}
		: undefined,
	footer: embed.footer
		? {
				text: embed.footer.text!,
				icon_url: embed.footer.iconUrl ?? undefined,
				proxy_icon_url: embed.footer.iconUrl ? mediaService.getExternalMediaProxyURL(embed.footer.iconUrl) : undefined,
			}
		: undefined,
	fields:
		embed.fields && embed.fields.length > 0 ? embed.fields.map((field) => mapEmbedFieldToResponse({field})) : undefined,
	provider: embed.provider
		? {
				name: embed.provider.name!,
				url: embed.provider.url ?? undefined,
				icon_url: undefined,
				proxy_icon_url: undefined,
			}
		: undefined,
	video: embed.video
		? {
				url: embed.video.url!,
				proxy_url: mediaService.getExternalMediaProxyURL(embed.video.url!),
				content_type: embed.video.contentType ?? undefined,
				content_hash: embed.video.contentHash ?? undefined,
				width: embed.video.width ?? undefined,
				height: embed.video.height ?? undefined,
				description: embed.video.description ?? undefined,
				placeholder: embed.video.placeholder ?? undefined,
				flags: embed.video.flags,
				duration: embed.video.duration,
			}
		: undefined,
	audio: undefined,
	nsfw: embed.nsfw ?? undefined,
});

const mapMessageAttachmentToResponse = ({
	channelId,
	attachment,
	mediaService,
	attachmentDecayMap,
}: MapMessageAttachmentToResponseParams & {
	attachmentDecayMap?: Map<string, AttachmentDecayRow>;
}): MessageAttachmentResponse => {
	const url = makeAttachmentCdnUrl(channelId, attachment.id, attachment.filename);
	const decay = attachmentDecayMap?.get(attachment.id.toString());
	const expired = decay ? decay.expires_at.getTime() <= Date.now() : false;
	return {
		id: attachment.id.toString(),
		filename: attachment.filename,
		title: attachment.title ?? undefined,
		description: attachment.description ?? undefined,
		content_type: attachment.contentType ?? undefined,
		content_hash: attachment.contentHash ?? undefined,
		size: Number(attachment.size),
		url: expired ? null : url,
		proxy_url: expired ? null : mediaService.getExternalMediaProxyURL(url),
		width: attachment.width ?? undefined,
		height: attachment.height ?? undefined,
		placeholder: attachment.placeholder ?? undefined,
		flags: attachment.flags,
		nsfw: attachment.nsfw ?? undefined,
		duration: attachment.duration,
		expires_at: decay?.expires_at?.toISOString?.() ?? null,
		expired: expired || undefined,
	};
};

const mapStickerItemToResponse = (sticker: StickerItem): MessageStickerResponse => ({
	id: sticker.id.toString(),
	name: sticker.name,
	format_type: sticker.formatType,
});

const mapReactions = ({reactions, currentUserId}: MapReactionsParams): Array<MessageReactionResponse> => {
	if (!reactions?.length) return [];

	const reactionMap = new Map<
		string,
		{emoji: {id?: string; name: string; animated?: boolean}; count: number; me: boolean}
	>();

	for (const reaction of reactions) {
		const {emojiId, emojiName, isEmojiAnimated, userId} = reaction;
		const isCustomEmoji = emojiId !== 0n;
		const emojiKey = isCustomEmoji ? `custom_${emojiId}` : `unicode_${emojiName}`;
		const existing = reactionMap.get(emojiKey);

		if (existing) {
			existing.count++;
			if (userId === currentUserId) {
				existing.me = true;
			}
		} else {
			reactionMap.set(emojiKey, {
				emoji: {
					id: isCustomEmoji ? emojiId.toString() : undefined,
					name: emojiName,
					animated: isEmojiAnimated || undefined,
				},
				count: 1,
				me: userId === currentUserId,
			});
		}
	}

	return Array.from(reactionMap.values()).map(({emoji, count, me}) => ({
		emoji,
		count,
		me: me || undefined,
	}));
};

const mapMessageReference = ({reference}: MapMessageReferenceParams): MessageReferenceResponse => ({
	channel_id: reference.channelId.toString(),
	message_id: reference.messageId.toString(),
	guild_id: reference.guildId?.toString(),
	type: reference.type,
});

export async function mapMessageToResponse({
	message,
	currentUserId,
	nonce,
	tts,
	withMessageReference = true,
	getAuthor,
	getReactions,
	getReferencedMessage,
	setHasReaction,
	userCacheService,
	requestCache,
	mediaService,
	attachmentDecayMap,
}: MapMessageToResponseParams): Promise<MessageResponse> {
	let author: UserPartialResponse;

	if (message.authorId) {
		author = await getCachedUserPartialResponse({userId: message.authorId, userCacheService, requestCache});
	} else if (message.webhookId && message.webhookName) {
		author = {
			id: message.webhookId.toString(),
			username: message.webhookName,
			discriminator: '0000',
			avatar: message.webhookAvatarHash,
			bot: true,
			flags: 0,
		};
	} else {
		throw new Error(`Message ${message.id} has neither authorId nor webhookId`);
	}

	const response: Partial<MessageResponse> = {
		id: message.id.toString(),
		type: message.type,
		content: message.content ?? '',
		channel_id: message.channelId.toString(),
		author,
		attachments:
			message.attachments?.map((att) =>
				mapMessageAttachmentToResponse({
					channelId: message.channelId,
					attachment: att,
					mediaService,
					attachmentDecayMap,
				}),
			) ?? [],
		stickers: message.stickers?.map((sticker) => mapStickerItemToResponse(sticker)) ?? [],
		embeds: message.embeds?.map((embed) => mapMessageEmbedToResponse({embed, mediaService})) ?? [],
		timestamp: new Date(SnowflakeUtils.extractTimestamp(message.id)).toISOString(),
		edited_timestamp: message.editedTimestamp?.toISOString() ?? null,
		flags: message.flags ?? 0,
		mention_everyone: message.mentionEveryone,
		pinned: message.pinnedTimestamp != null,
	};

	if (message.webhookId) {
		response.webhook_id = message.webhookId.toString();
	}

	if (tts) {
		response.tts = true;
	}

	if (message.mentionedUserIds.size > 0) {
		const mentionedUserIds = Array.from(message.mentionedUserIds);
		const mentionedUserPartials = await getCachedUserPartialResponses({
			userIds: mentionedUserIds,
			userCacheService,
			requestCache,
		});
		response.mentions = mentionedUserIds.map((userId) => mentionedUserPartials.get(userId)!);
	}

	if (message.mentionedRoleIds.size > 0) {
		response.mention_roles = Array.from(message.mentionedRoleIds).map(String);
	}

	if (currentUserId != null && getReactions && message.hasReaction !== false) {
		const reactions = await getReactions(message.channelId, message.id);
		const hasReactions = reactions && reactions.length > 0;

		if (message.hasReaction == null && setHasReaction) {
			void setHasReaction(message.channelId, message.id, hasReactions);
		}

		if (hasReactions) {
			const mappedReactions = mapReactions({reactions, currentUserId});
			if (mappedReactions.length > 0) {
				response.reactions = mappedReactions;
			}
		}
	}

	if (message.reference) {
		response.message_reference = mapMessageReference({reference: message.reference});
		if (withMessageReference && getReferencedMessage) {
			const referencedMessage = await getReferencedMessage(message.reference.channelId, message.reference.messageId);
			if (referencedMessage) {
				response.referenced_message = await mapMessageToResponse({
					message: referencedMessage,
					withMessageReference: false,
					getAuthor,
					getReactions,
					getReferencedMessage,
					setHasReaction,
					userCacheService,
					requestCache,
					mediaService,
					attachmentDecayMap,
				});
			}
		}
	}

	if (message.messageSnapshots && message.messageSnapshots.length > 0) {
		response.message_snapshots = message.messageSnapshots.map((snapshot) => ({
			content: snapshot.content ?? undefined,
			timestamp: snapshot.timestamp.toISOString(),
			edited_timestamp: snapshot.editedTimestamp?.toISOString() ?? undefined,
			mentions: snapshot.mentionedUserIds.size > 0 ? Array.from(snapshot.mentionedUserIds).map(String) : undefined,
			mention_roles: snapshot.mentionedRoleIds.size > 0 ? Array.from(snapshot.mentionedRoleIds).map(String) : undefined,
			embeds: snapshot.embeds?.map((embed) => mapMessageEmbedToResponse({embed, mediaService})) ?? undefined,
			attachments:
				snapshot.attachments?.map((att) =>
					mapMessageAttachmentToResponse({
						channelId: message.channelId,
						attachment: att,
						mediaService,
						attachmentDecayMap,
					}),
				) ?? undefined,
			type: snapshot.type,
			flags: snapshot.flags,
		}));
	}

	if (nonce) {
		response.nonce = nonce;
	}

	if (message.call) {
		response.call = {
			participants: Array.from(message.call.participantIds).map(String),
			ended_timestamp: message.call.endedTimestamp?.toISOString() ?? null,
		};
	}

	return response as MessageResponse;
}
