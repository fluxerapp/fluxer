// SPDX-License-Identifier: AGPL-3.0-or-later

import {MessageFlags} from '@fluxer/constants/src/ChannelConstants';
import {
	DELETED_USER_DISCRIMINATOR,
	DELETED_USER_GLOBAL_NAME,
	DELETED_USER_USERNAME,
} from '@fluxer/constants/src/UserConstants';
import type {MessageEmbedResponse} from '@fluxer/schema/src/domains/message/EmbedSchemas';
import type {
	MessageAttachmentResponse,
	MessageChannelMentionResponse,
	MessageReactionResponse,
	MessageResponse,
	MessageSnapshotResponse,
	MessageStickerResponse,
} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import type {UserPartialResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {snowflakeToDate} from '@fluxer/snowflake/src/Snowflake';
import type {INatsConnectionManager} from '@pkgs/nats/src/INatsConnectionManager';
import type {NatsConnection} from 'nats';
import {AttachmentDecayRepository} from '../../attachment/AttachmentDecayRepository';
import {AttachmentDecayService} from '../../attachment/AttachmentDecayService';
import type {ChannelID, GuildID, MessageID, UserID} from '../../BrandedTypes';
import {createUserID} from '../../BrandedTypes';
import {Config} from '../../Config';
import {
	type MessageResponseAccessContext,
	MessageResponseDataService,
	messageResponseAccessForChannel,
} from '../../channel/services/message/MessageResponseDataService';
import {getChannelRepository, getUserRepository} from '../../middleware/ServiceSingletons';
import type {Attachment} from '../../models/Attachment';
import type {CallInfo} from '../../models/CallInfo';
import type {Embed} from '../../models/Embed';
import type {EmbedAuthor} from '../../models/EmbedAuthor';
import type {EmbedField} from '../../models/EmbedField';
import type {EmbedFooter} from '../../models/EmbedFooter';
import type {EmbedMedia} from '../../models/EmbedMedia';
import type {EmbedProvider} from '../../models/EmbedProvider';
import type {Message} from '../../models/Message';
import type {MessageSnapshot} from '../../models/MessageSnapshot';
import type {StickerItem} from '../../models/StickerItem';
import {mapUserToPartialResponse} from '../../user/UserMappers';
import {assertSafeByteSize} from '../../utils/ByteSizeUtils';

class NoopNatsConnectionManager implements INatsConnectionManager {
	async connect(): Promise<void> {}

	async drain(): Promise<void> {}

	isClosed(): boolean {
		return false;
	}

	getConnection(): NatsConnection {
		throw new Error('RepositoryBackedMessageResponseDataService does not use NATS');
	}
}

export class RepositoryBackedMessageResponseDataService extends MessageResponseDataService {
	private readonly attachmentDecayRepository = new AttachmentDecayRepository();
	private readonly attachmentDecayService = new AttachmentDecayService(this.attachmentDecayRepository);

	constructor() {
		super(new NoopNatsConnectionManager());
	}

	override async listMessages(params: {
		userId: UserID;
		channelId: ChannelID;
		limit: number;
		before?: MessageID;
		after?: MessageID;
		around?: MessageID;
		access: MessageResponseAccessContext;
	}): Promise<Array<MessageResponse>> {
		const messages = params.around
			? await this.listMessagesAround(params.channelId, params.around, params.limit)
			: await getChannelRepository().messages.listMessages(params.channelId, params.before, params.limit, params.after);
		return this.buildMessages({
			userId: params.userId,
			messages: this.filterByAccess(messages, params.access),
			access: params.access,
		});
	}

	override async extractMentions(contents: Array<string>) {
		return contents.map((content) => ({
			users: [...content.matchAll(/<@!?(\d+)>/g)].map((match) => match[1]!),
			roles: [...content.matchAll(/<@&(\d+)>/g)].map((match) => match[1]!),
			channels: [...content.matchAll(/<#(\d+)>/g)].map((match) => match[1]!),
			everyone: /(^|\s)@everyone(\s|$)/.test(content),
			here: /(^|\s)@here(\s|$)/.test(content),
		}));
	}

	override async getMessage(params: {
		userId: UserID;
		channelId: ChannelID;
		messageId: MessageID;
		access: MessageResponseAccessContext;
		nonce?: string;
		tts?: boolean;
	}): Promise<MessageResponse | null> {
		const message = await getChannelRepository().messages.getMessage(params.channelId, params.messageId);
		if (!message || this.filterByAccess([message], params.access).length === 0) {
			return null;
		}
		return this.buildMessage({
			userId: params.userId,
			message,
			access: params.access,
			nonce: params.nonce,
			tts: params.tts,
		});
	}

	override async buildMessage(params: {
		userId: UserID;
		message: Message;
		access: MessageResponseAccessContext;
		nonce?: string;
		tts?: boolean;
		includeReactions?: boolean;
	}): Promise<MessageResponse> {
		return this.mapMessage(params.message, {
			currentUserId: params.userId,
			nonce: params.nonce,
			tts: params.tts,
			includeReactions: params.includeReactions ?? true,
			depth: 0,
		});
	}

	override async buildMessageForChannel(params: {
		channel: {guildId: GuildID | null};
		message: Message;
		userId?: UserID;
		nonce?: string;
		tts?: boolean;
	}): Promise<MessageResponse> {
		return this.buildMessage({
			userId: params.userId ?? params.message.authorId ?? createUserID(0n),
			message: params.message,
			access: messageResponseAccessForChannel(params.channel),
			nonce: params.nonce,
			tts: params.tts,
		});
	}

	override async buildBroadcastMessage(params: {
		channel: {guildId: GuildID | null};
		message: Message;
		userId?: UserID;
		nonce?: string;
		tts?: boolean;
		sourceGuildId?: GuildID | null;
	}): Promise<MessageResponse> {
		return this.mapMessage(params.message, {
			currentUserId: params.userId ?? params.message.authorId ?? undefined,
			nonce: params.nonce,
			tts: params.tts,
			includeReactions: false,
			depth: 0,
		});
	}

	override async buildMessages(params: {
		userId: UserID;
		messages: Array<Message>;
		access: MessageResponseAccessContext;
		includeReactions?: boolean;
	}): Promise<Array<MessageResponse>> {
		return Promise.all(
			this.filterByAccess(params.messages, params.access).map((message) =>
				this.mapMessage(message, {
					currentUserId: params.userId,
					includeReactions: params.includeReactions ?? true,
					depth: 0,
				}),
			),
		);
	}

	override async buildMessagesForChannels(params: {
		userId: UserID;
		messages: Array<Message>;
		channelById: ReadonlyMap<string, {guildId: GuildID | null}>;
		includeReactions?: boolean;
	}): Promise<Array<MessageResponse>> {
		return Promise.all(
			params.messages.map((message) =>
				this.mapMessage(message, {
					currentUserId: params.userId,
					includeReactions: params.includeReactions ?? true,
					depth: 0,
				}),
			),
		);
	}

	private async listMessagesAround(channelId: ChannelID, around: MessageID, limit: number): Promise<Array<Message>> {
		const beforeLimit = Math.floor((limit - 1) / 2);
		const afterLimit = Math.max(0, limit - beforeLimit - 1);
		const [before, current, after] = await Promise.all([
			getChannelRepository().messages.listMessages(channelId, around, beforeLimit),
			getChannelRepository().messages.getMessage(channelId, around),
			getChannelRepository().messages.listMessages(channelId, undefined, afterLimit, around, {immediateAfter: true}),
		]);
		return [...before, ...(current ? [current] : []), ...after];
	}

	private filterByAccess(messages: Array<Message>, access: MessageResponseAccessContext): Array<Message> {
		if (access.canReadMessageHistory || !access.messageHistoryCutoff) {
			return messages;
		}
		const cutoff = new Date(access.messageHistoryCutoff).getTime();
		return messages.filter((message) => snowflakeToDate(message.id).getTime() >= cutoff);
	}

	private async mapMessage(
		message: Message,
		options: {
			currentUserId?: UserID;
			nonce?: string;
			tts?: boolean;
			includeReactions: boolean;
			depth: number;
		},
	): Promise<MessageResponse> {
		const [author, mentions, mentionChannels, reactions, referencedMessage] = await Promise.all([
			this.resolveAuthor(message),
			this.resolveUserPartials([...message.mentionedUserIds]),
			this.resolveMentionChannels([...message.mentionedChannelIds]),
			options.includeReactions ? this.mapReactions(message, options.currentUserId) : Promise.resolve(null),
			this.resolveReferencedMessage(message, options),
		]);
		return {
			id: message.id.toString(),
			channel_id: message.channelId.toString(),
			author,
			webhook_id: message.webhookId?.toString() ?? null,
			type: message.type as MessageResponse['type'],
			flags: message.flags,
			content: message.content ?? '',
			timestamp: snowflakeToDate(message.id).toISOString(),
			edited_timestamp: message.editedTimestamp?.toISOString() ?? null,
			pinned: message.pinnedTimestamp != null,
			mention_everyone: message.mentionEveryone,
			tts: options.tts ?? false,
			mentions,
			mention_roles: [...message.mentionedRoleIds].map((id) => id.toString()),
			mention_channels: mentionChannels.length > 0 ? mentionChannels : null,
			embeds: this.mapEmbeds(message.embeds, message),
			attachments: await this.mapAttachments(message.attachments, message),
			stickers: this.mapStickers(message.stickers),
			nsfw_emojis: message.nsfwEmojis.size > 0 ? [...message.nsfwEmojis].map((id) => id.toString()) : undefined,
			reactions,
			message_reference: message.reference
				? {
						channel_id: message.reference.channelId.toString(),
						message_id: message.reference.messageId.toString(),
						guild_id: message.reference.guildId?.toString() ?? null,
						type: message.reference.type,
					}
				: null,
			message_snapshots: await this.mapSnapshots(message.messageSnapshots, message),
			nonce: options.nonce ?? null,
			call: this.mapCall(message.call),
			referenced_message: referencedMessage,
		};
	}

	private async resolveAuthor(message: Message): Promise<UserPartialResponse> {
		if (message.authorId) {
			return this.resolveUserPartial(message.authorId);
		}
		return {
			id: message.webhookId?.toString() ?? '0',
			username: message.webhookName ?? DELETED_USER_USERNAME,
			discriminator: DELETED_USER_DISCRIMINATOR.toString().padStart(4, '0'),
			global_name: message.webhookName ?? DELETED_USER_GLOBAL_NAME,
			avatar: message.webhookAvatarHash,
			avatar_color: null,
			flags: 0,
		};
	}

	private async resolveUserPartials(userIds: Array<UserID>): Promise<Array<UserPartialResponse>> {
		return Promise.all(userIds.map((userId) => this.resolveUserPartial(userId)));
	}

	private async resolveUserPartial(userId: UserID): Promise<UserPartialResponse> {
		const user = await getUserRepository().findUnique(userId);
		if (user) {
			return mapUserToPartialResponse(user);
		}
		return {
			id: userId.toString(),
			username: DELETED_USER_USERNAME,
			discriminator: DELETED_USER_DISCRIMINATOR.toString().padStart(4, '0'),
			global_name: DELETED_USER_GLOBAL_NAME,
			avatar: null,
			avatar_color: null,
			flags: 0,
		};
	}

	private async resolveMentionChannels(channelIds: Array<ChannelID>): Promise<Array<MessageChannelMentionResponse>> {
		const channels = await Promise.all(channelIds.map((channelId) => getChannelRepository().findUnique(channelId)));
		return channels.flatMap((channel) =>
			channel?.name
				? [
						{
							id: channel.id.toString(),
							name: channel.name,
							type: channel.type,
						},
					]
				: [],
		);
	}

	private async resolveReferencedMessage(
		message: Message,
		options: {currentUserId?: UserID; includeReactions: boolean; depth: number},
	): Promise<MessageResponse | null | undefined> {
		if (!message.reference || options.depth > 0) {
			return undefined;
		}
		const referenced = await getChannelRepository().messages.getMessage(
			message.reference.channelId,
			message.reference.messageId,
		);
		if (!referenced) {
			return null;
		}
		return this.mapMessage(referenced, {
			currentUserId: options.currentUserId,
			includeReactions: options.includeReactions,
			depth: options.depth + 1,
		});
	}

	private mapAttachmentUrl(message: Message, attachment: Attachment): string {
		const filename = encodeURIComponent(attachment.filename);
		return `${Config.endpoints.media}/attachments/${message.channelId.toString()}/${message.id.toString()}/${attachment.id.toString()}/${filename}`;
	}

	private async mapAttachments(
		attachments: Array<Attachment>,
		message: Message,
	): Promise<Array<MessageAttachmentResponse> | null> {
		if (attachments.length === 0) return null;
		await this.attachmentDecayService.extendForAttachments(
			attachments.map((attachment) => ({
				attachmentId: attachment.id,
				channelId: message.channelId,
				messageId: message.id,
				filename: attachment.filename,
				sizeBytes: attachment.size,
				uploadedAt: snowflakeToDate(message.id),
			})),
		);
		return Promise.all(
			attachments.map(async (attachment) => {
				const decay = await this.attachmentDecayRepository.fetchById(attachment.id);
				const url = this.mapAttachmentUrl(message, attachment);
				return {
					id: attachment.id.toString(),
					filename: attachment.filename,
					title: attachment.title,
					description: attachment.description,
					content_type: attachment.contentType,
					content_hash: attachment.contentHash,
					size: assertSafeByteSize(attachment.size, 'message attachment size'),
					url,
					proxy_url: url,
					width: attachment.width,
					height: attachment.height,
					placeholder: attachment.placeholder,
					flags: attachment.flags,
					nsfw: attachment.nsfw,
					duration: attachment.duration,
					waveform: attachment.waveform,
					expires_at: decay?.expires_at.toISOString() ?? null,
					expired: decay ? decay.expires_at.getTime() <= Date.now() : undefined,
				};
			}),
		);
	}

	private mapEmbeds(embeds: Array<Embed>, message: Message): Array<MessageEmbedResponse> | null {
		if ((message.flags & MessageFlags.SUPPRESS_EMBEDS) !== 0) return [];
		if (embeds.length === 0) return null;
		return embeds.map((embed) => this.mapEmbed(embed, message));
	}

	private mapEmbed(embed: Embed, message: Message): MessageEmbedResponse {
		return {
			type: embed.type ?? 'rich',
			url: embed.url,
			title: embed.title,
			color: embed.color,
			timestamp: embed.timestamp?.toISOString() ?? null,
			description: embed.description,
			author: this.mapEmbedAuthor(embed.author),
			image: this.mapEmbedMedia(embed.image, message),
			thumbnail: this.mapEmbedMedia(embed.thumbnail, message),
			footer: this.mapEmbedFooter(embed.footer),
			fields: embed.fields.length > 0 ? embed.fields.map((field) => this.mapEmbedField(field)) : null,
			provider: this.mapEmbedAuthor(embed.provider),
			video: this.mapEmbedMedia(embed.video, message),
			audio: this.mapEmbedMedia(embed.audio, message),
			html: embed.html,
			html_width: embed.htmlWidth,
			html_height: embed.htmlHeight,
			nsfw: embed.nsfw,
			children: embed.children.length > 0 ? embed.children.map((child) => this.mapEmbed(child, message)) : null,
		};
	}

	private mapEmbedAuthor(author: EmbedAuthor | EmbedProvider | null) {
		if (!author?.name) return null;
		const iconUrl = 'iconUrl' in author ? author.iconUrl : null;
		return {
			name: author.name,
			url: author.url,
			icon_url: iconUrl,
			proxy_icon_url: iconUrl,
		};
	}

	private mapEmbedFooter(footer: EmbedFooter | null) {
		if (!footer?.text) return null;
		return {
			text: footer.text,
			icon_url: footer.iconUrl,
			proxy_icon_url: footer.iconUrl,
		};
	}

	private mapEmbedField(field: EmbedField) {
		return {
			name: field.name,
			value: field.value,
			inline: field.inline,
		};
	}

	private mapEmbedMedia(media: EmbedMedia | null, message: Message) {
		if (!media?.url) return null;
		const url = this.resolveAttachmentUrl(media.url, message);
		return {
			url,
			proxy_url: url.startsWith('http') ? url : null,
			content_type: media.contentType,
			content_hash: media.contentHash,
			width: media.width,
			height: media.height,
			description: media.description,
			placeholder: media.placeholder,
			duration: media.duration,
			flags: media.flags,
		};
	}

	private resolveAttachmentUrl(url: string, message: Message): string {
		if (!url.startsWith('attachment://')) {
			return url;
		}
		const filename = decodeURIComponent(url.slice('attachment://'.length));
		const attachment = message.attachments.find((entry) => entry.filename === filename);
		return attachment ? this.mapAttachmentUrl(message, attachment) : url;
	}

	private mapStickers(stickers: Array<StickerItem>): Array<MessageStickerResponse> | null {
		if (stickers.length === 0) return null;
		return stickers.map((sticker) => ({
			id: sticker.id.toString(),
			name: sticker.name,
			animated: sticker.animated,
			nsfw: sticker.nsfw,
		}));
	}

	private async mapSnapshots(
		snapshots: Array<MessageSnapshot>,
		sourceMessage: Message,
	): Promise<Array<MessageSnapshotResponse> | null> {
		if (snapshots.length === 0) return null;
		return Promise.all(
			snapshots.map(async (snapshot) => ({
				content: snapshot.content,
				timestamp: snapshot.timestamp.toISOString(),
				edited_timestamp: snapshot.editedTimestamp?.toISOString() ?? null,
				mentions: [...snapshot.mentionedUserIds].map((id) => id.toString()),
				mention_roles: [...snapshot.mentionedRoleIds].map((id) => id.toString()),
				mention_channels: await this.resolveMentionChannels([...snapshot.mentionedChannelIds]),
				embeds:
					(snapshot.flags & MessageFlags.SUPPRESS_EMBEDS) === 0 && snapshot.embeds.length > 0
						? snapshot.embeds.map((embed) => this.mapEmbed(embed, sourceMessage))
						: null,
				attachments: await this.mapAttachments(snapshot.attachments, sourceMessage),
				stickers: this.mapStickers(snapshot.stickers),
				type: snapshot.type as MessageSnapshotResponse['type'],
				flags: snapshot.flags,
			})),
		);
	}

	private mapCall(call: CallInfo | null) {
		if (!call) return null;
		return {
			participants: [...call.participantIds].map((id) => id.toString()),
			ended_timestamp: call.endedTimestamp?.toISOString() ?? null,
		};
	}

	private async mapReactions(message: Message, currentUserId?: UserID): Promise<Array<MessageReactionResponse> | null> {
		const reactions = await getChannelRepository().messageInteractions.listMessageReactions(
			message.channelId,
			message.id,
		);
		if (reactions.length === 0) return null;
		const byEmoji = new Map<
			string,
			{
				id: string | null;
				name: string;
				animated: boolean;
				count: number;
				me: boolean;
			}
		>();
		for (const reaction of reactions) {
			const emojiId = reaction.emojiId.toString() === '0' ? null : reaction.emojiId.toString();
			const key = `${emojiId ?? ''}:${reaction.emojiName}`;
			const entry = byEmoji.get(key) ?? {
				id: emojiId,
				name: reaction.emojiName,
				animated: reaction.isEmojiAnimated,
				count: 0,
				me: false,
			};
			entry.count += 1;
			entry.me = entry.me || reaction.userId === currentUserId;
			byEmoji.set(key, entry);
		}
		return [...byEmoji.values()].map((entry) => ({
			emoji: {
				id: entry.id,
				name: entry.name,
				animated: entry.animated || undefined,
			},
			count: entry.count,
			me: entry.me ? true : undefined,
		}));
	}
}
