// SPDX-License-Identifier: AGPL-3.0-or-later

import {STICKER_DESCRIPTOR, STICKERS_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {MarkdownContext} from '@app/features/messaging/components/markdown/renderers/RendererTypes';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import {getParserFlagsForContext} from '@app/features/messaging/utils/markdown/MarkdownParserFlags';
import {parseAndRenderToPlaintext} from '@app/features/messaging/utils/markdown/Plaintext';
import {SystemMessageUtils} from '@app/features/messaging/utils/SystemMessageUtils';
import {MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import type {MessageEmbed} from '@fluxer/schema/src/domains/message/EmbedSchemas';
import type {
	ChannelMention,
	MessageAttachment,
	MessageMention,
	MessageSnapshot,
	MessageStickerItem,
	Message as WireMessage,
} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {formatListWithConfig} from '@pkgs/list_utils/src/ListFormatting';

const MESSAGE_NOTIFICATION_PARSER_FLAGS = getParserFlagsForContext(MarkdownContext.STANDARD_WITHOUT_JUMBO);
const ATTACHMENT_DESCRIPTOR = msg({
	message: 'Attachment: {filename}',
	comment: 'Notification body shown when a message has no text but includes an attachment. filename is the file name.',
});
const FORWARDED_MESSAGE_DESCRIPTOR = msg({
	message: 'Forwarded a message',
	comment: 'Notification body shown when a forwarded message has no other previewable content.',
});
const STICKER_NOTIFICATION_BODY_DESCRIPTOR = msg({
	message: 'Sticker: {stickerName}',
	comment:
		'Notification body shown when a message has no text but includes one sticker. stickerName is the sticker name.',
});
const STICKERS_NOTIFICATION_BODY_DESCRIPTOR = msg({
	message: 'Stickers: {stickerNames}',
	comment:
		'Notification body shown when a message has no text but includes multiple stickers. stickerNames is a localized list of sticker names.',
});

interface MarkdownPreviewOptions {
	channelId: string;
	mentionChannels?: ReadonlyArray<ChannelMention>;
	i18n: I18n;
}

interface MessagePreviewContent {
	attachments?: ReadonlyArray<MessageAttachment> | null;
	embeds?: ReadonlyArray<MessageEmbed> | null;
	stickers?: ReadonlyArray<MessageStickerItem> | null;
}

function isUserMessageType(type: number): boolean {
	return type === MessageTypes.DEFAULT || type === MessageTypes.REPLY || type === MessageTypes.CLIENT_SYSTEM;
}

function renderMarkdownPreview(content: string | null | undefined, options: MarkdownPreviewOptions): string {
	if (!content) return '';
	return parseAndRenderToPlaintext(content, MESSAGE_NOTIFICATION_PARSER_FLAGS, {
		channelId: options.channelId,
		preserveMarkdown: true,
		includeEmojiNames: true,
		mentionChannels: options.mentionChannels,
		i18n: options.i18n,
	});
}

function buildAttachmentNotificationPreview(
	attachments: ReadonlyArray<MessageAttachment> | null | undefined,
	i18n: I18n,
): string {
	if (!attachments?.length) return '';
	return i18n._(ATTACHMENT_DESCRIPTOR, {filename: attachments[0].filename});
}

function buildEmbedNotificationPreview(embeds: ReadonlyArray<MessageEmbed> | null | undefined): string {
	if (!embeds?.length) return '';
	const embed = embeds[0];
	if (embed.description) {
		return embed.title ? `${embed.title}: ${embed.description}` : embed.description;
	}
	if (embed.title) {
		return embed.title;
	}
	if (embed.fields?.length) {
		const field = embed.fields[0];
		return `${field.name}: ${field.value}`;
	}
	return '';
}

export function buildStickerNotificationPreview(
	stickers: ReadonlyArray<MessageStickerItem> | null | undefined,
	i18n: I18n,
): string {
	if (!stickers?.length) return '';
	const stickerNames = stickers.map((sticker) => sticker.name.trim()).filter(Boolean);
	if (stickerNames.length === 0) {
		return i18n._(stickers.length === 1 ? STICKER_DESCRIPTOR : STICKERS_DESCRIPTOR);
	}
	if (stickers.length === 1) {
		return i18n._(STICKER_NOTIFICATION_BODY_DESCRIPTOR, {stickerName: stickerNames[0]});
	}
	const formattedStickerNames = formatListWithConfig(stickerNames, {
		locale: i18n.locale,
		style: 'long',
		type: 'conjunction',
	});
	return i18n._(STICKERS_NOTIFICATION_BODY_DESCRIPTOR, {stickerNames: formattedStickerNames});
}

function buildMessageContentFallbackPreview(content: MessagePreviewContent, i18n: I18n): string {
	return (
		buildStickerNotificationPreview(content.stickers, i18n) ||
		buildAttachmentNotificationPreview(content.attachments, i18n) ||
		buildEmbedNotificationPreview(content.embeds)
	);
}

function buildSnapshotNotificationPreview(snapshot: MessageSnapshot, options: MarkdownPreviewOptions): string {
	return (
		renderMarkdownPreview(snapshot.content, {
			...options,
			mentionChannels: snapshot.mention_channels ?? options.mentionChannels,
		}) ||
		buildMessageContentFallbackPreview(
			{
				attachments: snapshot.attachments,
				embeds: snapshot.embeds,
				stickers: snapshot.stickers,
			},
			options.i18n,
		) ||
		options.i18n._(FORWARDED_MESSAGE_DESCRIPTOR)
	);
}

export function buildMessageNotificationBody(message: Message, i18n: I18n): string {
	const markdownOptions: MarkdownPreviewOptions = {
		channelId: message.channelId,
		mentionChannels: message.mentionChannels,
		i18n,
	};
	if (!isUserMessageType(message.type)) {
		const systemBody = SystemMessageUtils.stringify(message, i18n);
		if (systemBody) return systemBody;
	} else {
		const userBody = renderMarkdownPreview(message.content, markdownOptions);
		if (userBody) return userBody;
	}
	const contentFallback = buildMessageContentFallbackPreview(
		{
			attachments: message.attachments,
			embeds: message.embeds,
			stickers: message.stickerItems,
		},
		i18n,
	);
	if (contentFallback) return contentFallback;
	const snapshot = message.messageSnapshots?.[0];
	if (snapshot) {
		return buildSnapshotNotificationPreview(snapshot, markdownOptions);
	}
	return '';
}

const USER_MENTION_PATTERN = /<@!?(\d+)>/g;

function inlineUserMentions(content: string, mentions: ReadonlyArray<MessageMention> | undefined): string {
	if (!content || !mentions?.length) return content;
	return content.replace(USER_MENTION_PATTERN, (token, userId: string) => {
		const user = mentions.find((mention) => mention.id === userId);
		if (!user) return token;
		return `@${user.member?.nick || user.global_name || user.username}`;
	});
}

export function buildWireMessageNotificationBody(message: WireMessage, i18n: I18n): string {
	const markdownOptions: MarkdownPreviewOptions = {
		channelId: message.channel_id,
		mentionChannels: message.mention_channels,
		i18n,
	};
	const userBody = renderMarkdownPreview(inlineUserMentions(message.content, message.mentions), markdownOptions);
	if (userBody) return userBody;
	const contentFallback = buildMessageContentFallbackPreview(
		{
			attachments: message.attachments,
			embeds: message.embeds,
			stickers: message.stickers,
		},
		i18n,
	);
	if (contentFallback) return contentFallback;
	const snapshot = message.message_snapshots?.[0];
	if (snapshot) {
		return buildSnapshotNotificationPreview(snapshot, markdownOptions);
	}
	return '';
}
