// SPDX-License-Identifier: AGPL-3.0-or-later

import {MarkdownContext} from '@app/features/messaging/components/markdown/renderers/RendererTypes';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import {getParserFlagsForContext} from '@app/features/messaging/utils/markdown/MarkdownParserFlags';
import {
	type PlaintextRenderOptions,
	parseAndRenderToPlaintext,
	renderAstToPlaintext,
} from '@app/features/messaging/utils/markdown/Plaintext';
import type {Node} from '@app/features/messaging/utils/markdown/parser/Nodes';
import * as DateUtils from '@app/features/user/utils/DateFormatting';
import {MessageEmbedTypes} from '@fluxer/constants/src/ChannelConstants';
import type {MessageEmbed} from '@fluxer/schema/src/domains/message/EmbedSchemas';
import type {
	ChannelMention,
	MessageAttachment,
	MessageSnapshot,
	MessageStickerItem,
} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import type {I18n} from '@lingui/core';

const MESSAGE_COPY_PARSER_FLAGS = getParserFlagsForContext(MarkdownContext.STANDARD_WITHOUT_JUMBO);
const EMBED_INLINE_COPY_PARSER_FLAGS = getParserFlagsForContext(MarkdownContext.RESTRICTED_INLINE_REPLY);
const EMBED_DESCRIPTION_COPY_PARSER_FLAGS = getParserFlagsForContext(MarkdownContext.RESTRICTED_EMBED_DESCRIPTION);

interface MarkdownCopyContext {
	channelId: string;
	messageId?: string;
	mentionChannels?: ReadonlyArray<ChannelMention>;
	i18n: I18n;
}

interface MessageCopyTextOptions extends MarkdownCopyContext {
	includeEmbeds?: boolean;
}

interface EmbedCopyTextOptions extends MarkdownCopyContext {
	includeFields?: boolean;
	omittedUrls?: ReadonlySet<string>;
}

function appendCopyBlock(blocks: Array<string>, value?: string | null): void {
	const block = normaliseCopyBlock(value);
	if (!block) {
		return;
	}
	if (blocks.includes(block)) {
		return;
	}
	blocks.push(block);
}

function appendUrlCopyBlock(blocks: Array<string>, value?: string | null, omittedUrls?: ReadonlySet<string>): void {
	const normalisedUrl = normaliseUrlForComparison(value);
	if (normalisedUrl && omittedUrls?.has(normalisedUrl)) {
		return;
	}
	appendCopyBlock(blocks, value);
}

function joinCopyBlocks(blocks: Array<string>): string {
	return blocks.filter(Boolean).join('\n\n').trim();
}

function normaliseCopyBlock(value?: string | null): string {
	return (value ?? '')
		.replace(/\r\n/gu, '\n')
		.replace(/\u00a0/gu, ' ')
		.replace(/[ \t]+\n/gu, '\n')
		.replace(/\n{3,}/gu, '\n\n')
		.trim();
}

function normaliseUrlForComparison(value?: string | null): string | null {
	if (!value) {
		return null;
	}
	try {
		return new URL(value).href.replace(/\/$/u, '');
	} catch {
		return null;
	}
}

function trimUrlCandidate(value: string): string {
	return value.replace(/[),.;:!?]+$/u, '');
}

function extractNormalisedUrls(value?: string | null): Set<string> {
	const urls = new Set<string>();
	const matches = value?.matchAll(/\bhttps?:\/\/[^\s<>"'`]+/giu) ?? [];
	for (const match of matches) {
		const normalisedUrl = normaliseUrlForComparison(trimUrlCandidate(match[0]));
		if (normalisedUrl) {
			urls.add(normalisedUrl);
		}
	}
	return urls;
}

function buildOmittedEmbedUrls(content?: string | null, renderedContent?: string | null): Set<string> {
	const urls = extractNormalisedUrls(content);
	for (const url of extractNormalisedUrls(renderedContent)) {
		urls.add(url);
	}
	return urls;
}

function createPlaintextCopyOptions(context: MarkdownCopyContext): PlaintextRenderOptions {
	return {
		channelId: context.channelId,
		preserveMarkdown: false,
		includeEmojiNames: true,
		includeLinkUrls: true,
		mentionChannels: context.mentionChannels,
		i18n: context.i18n,
	};
}

function renderMarkdownCopyText(
	content: string | undefined | null,
	parserFlags: number,
	context: MarkdownCopyContext,
): string {
	if (!content) {
		return '';
	}
	return parseAndRenderToPlaintext(content, parserFlags, createPlaintextCopyOptions(context));
}

export function buildMessageContentCopyText(nodes: Array<Node>, context: MarkdownCopyContext): string {
	return normaliseCopyBlock(renderAstToPlaintext(nodes, createPlaintextCopyOptions(context)));
}

function buildAttachmentCopyText(attachment: MessageAttachment): string {
	const blocks: Array<string> = [];
	appendCopyBlock(blocks, attachment.title || attachment.filename);
	appendCopyBlock(blocks, attachment.description);
	appendCopyBlock(blocks, attachment.url);
	return joinCopyBlocks(blocks);
}

function buildStickerCopyText(sticker: MessageStickerItem): string {
	return normaliseCopyBlock(sticker.name);
}

function getFormattedEmbedTimestamp(timestamp?: string): string {
	if (!timestamp) {
		return '';
	}
	const parsed = new Date(timestamp);
	if (Number.isNaN(parsed.getTime())) {
		return '';
	}
	return DateUtils.getFormattedDateTime(parsed);
}

function isMediaOnlyEmbed(embed: MessageEmbed): boolean {
	const hasProviderText = Boolean(embed.provider && embed.type !== MessageEmbedTypes.GIFV);
	return Boolean(
		!embed.title &&
			!embed.description &&
			!embed.author &&
			!embed.footer &&
			!embed.fields?.length &&
			!hasProviderText &&
			(embed.image || embed.thumbnail || embed.video || embed.audio),
	);
}

function buildMediaOnlyEmbedCopyText(embed: MessageEmbed, options: EmbedCopyTextOptions): string {
	const blocks: Array<string> = [];
	appendUrlCopyBlock(blocks, embed.url, options.omittedUrls);
	if (blocks.length === 0 && !embed.url) {
		appendUrlCopyBlock(blocks, embed.image?.url, options.omittedUrls);
		appendUrlCopyBlock(blocks, embed.thumbnail?.url, options.omittedUrls);
		appendUrlCopyBlock(blocks, embed.video?.url, options.omittedUrls);
		appendUrlCopyBlock(blocks, embed.audio?.url, options.omittedUrls);
	}
	return joinCopyBlocks(blocks);
}

function shouldOmitEmbedForMessageContent(embed: MessageEmbed, omittedUrls?: ReadonlySet<string>): boolean {
	const normalisedEmbedUrl = normaliseUrlForComparison(embed.url);
	return Boolean(normalisedEmbedUrl && omittedUrls?.has(normalisedEmbedUrl));
}

export function buildMessageEmbedCopyText(embed: MessageEmbed, options: EmbedCopyTextOptions): string {
	if (isMediaOnlyEmbed(embed)) {
		return buildMediaOnlyEmbedCopyText(embed, options);
	}
	const blocks: Array<string> = [];
	appendCopyBlock(blocks, embed.provider?.name);
	appendCopyBlock(blocks, embed.author?.name);
	appendCopyBlock(blocks, renderMarkdownCopyText(embed.title, EMBED_INLINE_COPY_PARSER_FLAGS, options));
	appendCopyBlock(blocks, renderMarkdownCopyText(embed.description, EMBED_DESCRIPTION_COPY_PARSER_FLAGS, options));
	if (options.includeFields !== false && embed.type !== MessageEmbedTypes.BLUESKY) {
		for (const field of embed.fields ?? []) {
			const fieldName = renderMarkdownCopyText(field.name, EMBED_INLINE_COPY_PARSER_FLAGS, options);
			const fieldValue = renderMarkdownCopyText(field.value, EMBED_DESCRIPTION_COPY_PARSER_FLAGS, options);
			appendCopyBlock(blocks, joinCopyBlocks([fieldName, fieldValue]));
		}
	}
	appendCopyBlock(blocks, renderMarkdownCopyText(embed.footer?.text, EMBED_INLINE_COPY_PARSER_FLAGS, options));
	appendCopyBlock(blocks, getFormattedEmbedTimestamp(embed.timestamp));
	appendCopyBlock(blocks, embed.image?.description);
	appendCopyBlock(blocks, embed.thumbnail?.description);
	appendCopyBlock(blocks, embed.video?.description);
	appendCopyBlock(blocks, embed.audio?.description);
	appendUrlCopyBlock(blocks, embed.url, options.omittedUrls);
	appendUrlCopyBlock(blocks, embed.image?.url, options.omittedUrls);
	appendUrlCopyBlock(blocks, embed.thumbnail?.url, options.omittedUrls);
	appendUrlCopyBlock(blocks, embed.video?.url, options.omittedUrls);
	appendUrlCopyBlock(blocks, embed.audio?.url, options.omittedUrls);
	for (const child of embed.children ?? []) {
		appendCopyBlock(blocks, buildMessageEmbedCopyText(child, options));
	}
	return joinCopyBlocks(blocks);
}

export function buildMessageSnapshotCopyText(snapshot: MessageSnapshot, options: MessageCopyTextOptions): string {
	const blocks: Array<string> = [];
	const snapshotOptions: MessageCopyTextOptions = {
		...options,
		mentionChannels: snapshot.mention_channels ?? options.mentionChannels,
	};
	const renderedContent = renderMarkdownCopyText(snapshot.content, MESSAGE_COPY_PARSER_FLAGS, snapshotOptions);
	appendCopyBlock(blocks, renderedContent);
	const embedOptions: EmbedCopyTextOptions = {
		...snapshotOptions,
		omittedUrls: buildOmittedEmbedUrls(snapshot.content, renderedContent),
	};
	for (const attachment of snapshot.attachments ?? []) {
		appendCopyBlock(blocks, buildAttachmentCopyText(attachment));
	}
	if (options.includeEmbeds !== false) {
		for (const embed of snapshot.embeds ?? []) {
			if (shouldOmitEmbedForMessageContent(embed, embedOptions.omittedUrls)) {
				continue;
			}
			appendCopyBlock(blocks, buildMessageEmbedCopyText(embed, embedOptions));
		}
	}
	for (const sticker of snapshot.stickers ?? []) {
		appendCopyBlock(blocks, buildStickerCopyText(sticker));
	}
	return joinCopyBlocks(blocks);
}

export function buildUserMessageCopyText(message: Message, options: MessageCopyTextOptions): string {
	const blocks: Array<string> = [];
	const messageOptions: MessageCopyTextOptions = {
		...options,
		mentionChannels: message.mentionChannels,
	};
	const renderedContent = renderMarkdownCopyText(message.content, MESSAGE_COPY_PARSER_FLAGS, messageOptions);
	appendCopyBlock(blocks, renderedContent);
	const embedOptions: EmbedCopyTextOptions = {
		...messageOptions,
		omittedUrls: buildOmittedEmbedUrls(message.content, renderedContent),
	};
	for (const snapshot of message.messageSnapshots ?? []) {
		appendCopyBlock(blocks, buildMessageSnapshotCopyText(snapshot, messageOptions));
	}
	for (const sticker of message.stickerItems) {
		appendCopyBlock(blocks, buildStickerCopyText(sticker));
	}
	for (const attachment of message.attachments) {
		appendCopyBlock(blocks, buildAttachmentCopyText(attachment));
	}
	if (options.includeEmbeds !== false) {
		for (const embed of message.embeds) {
			if (shouldOmitEmbedForMessageContent(embed, embedOptions.omittedUrls)) {
				continue;
			}
			appendCopyBlock(blocks, buildMessageEmbedCopyText(embed, embedOptions));
		}
	}
	return joinCopyBlocks(blocks);
}
