// SPDX-License-Identifier: AGPL-3.0-or-later

import type {SearchableMessage} from '@fluxer/schema/src/contracts/search/SearchDocumentTypes';
import {snowflakeToDate} from '@fluxer/snowflake/src/Snowflake';
import type {UserID} from '../../BrandedTypes';
import type {Attachment} from '../../models/Attachment';
import type {Embed} from '../../models/Embed';
import type {Message} from '../../models/Message';
import type {MessageSnapshot} from '../../models/MessageSnapshot';
import type { Poll } from '@app/api/models/Poll';

const LINK_URL_MATCHER = /https?:\/\/[^\s<>"']+/gi;
const HAS_LINK_REGEX = /https?:\/\/[^\s/]+/i;
const TRAILING_URL_PUNCTUATION = /[.,;:!?)\]}\x22'\u00bb\u201c\u201d]+$/;

function getAuthorType(message: Message, authorIsBot?: boolean): 'user' | 'bot' | 'webhook' {
	if (message.webhookId) {
		return 'webhook';
	}
	if (authorIsBot) {
		return 'bot';
	}
	return 'user';
}

function attachmentHasPrefix(attachments: Array<Attachment>, prefix: string): boolean {
	return attachments.some((att) => att.contentType.trim().toLowerCase().startsWith(prefix));
}

function collectEmbedText(embed: Embed, sink: Array<string>): void {
	if (embed.title) sink.push(embed.title);
	if (embed.description) sink.push(embed.description);
	if (embed.url) sink.push(embed.url);
	if (embed.author?.name) sink.push(embed.author.name);
	if (embed.provider?.name) sink.push(embed.provider.name);
	if (embed.footer?.text) sink.push(embed.footer.text);
	for (const field of embed.fields) {
		if (field.name) sink.push(field.name);
		if (field.value) sink.push(field.value);
	}
	for (const child of embed.children) {
		collectEmbedText(child, sink);
	}
}

function extractTextLinkHostnames(content: string | null, sink: Array<string>): void {
	if (!content) {
		return;
	}
	const matches = content.matchAll(LINK_URL_MATCHER);
	for (const match of matches) {
		const rawUrl = match[0]?.replace(TRAILING_URL_PUNCTUATION, '');
		if (!rawUrl) continue;
		try {
			const {hostname} = new URL(rawUrl);
			if (hostname && !sink.includes(hostname)) {
				sink.push(hostname);
			}
		} catch {}
	}
}

function extractEmbedLinkHostnames(embeds: Array<Embed>, sink: Array<string>): void {
	for (const embed of embeds) {
		if (!embed.url) {
			continue;
		}
		try {
			const url = new URL(embed.url);
			if (!sink.includes(url.hostname)) {
				sink.push(url.hostname);
			}
		} catch {}
	}
}

function pushUnique(sink: Array<string>, value: string): void {
	if (value && !sink.includes(value)) {
		sink.push(value);
	}
}

function pushAttachmentExtension(sink: Array<string>, filename: string): void {
	const parts = filename.split('.');
	if (parts.length <= 1) {
		return;
	}
	const ext = parts[parts.length - 1]!.toLowerCase();
	if (ext.length > 0 && ext.length <= 10) {
		pushUnique(sink, ext);
	}
}

interface IndexAccumulator {
	contentChunks: Array<string>;
	embedTypes: Array<string>;
	embedProviders: Array<string>;
	embedContent: Array<string>;
	linkHostnames: Array<string>;
	attachmentFilenames: Array<string>;
	attachmentExtensions: Array<string>;
	mentionedUserIds: Array<string>;
	hasLink: boolean;
	hasEmbed: boolean;
	hasPoll: boolean;
	hasFile: boolean;
	hasImage: boolean;
	hasVideo: boolean;
	hasSound: boolean;
	hasSticker: boolean;
}

function emptyAccumulator(): IndexAccumulator {
	return {
		contentChunks: [],
		embedTypes: [],
		embedProviders: [],
		embedContent: [],
		linkHostnames: [],
		attachmentFilenames: [],
		attachmentExtensions: [],
		mentionedUserIds: [],
		hasLink: false,
		hasEmbed: false,
		hasPoll: false,
		hasFile: false,
		hasImage: false,
		hasVideo: false,
		hasSound: false,
		hasSticker: false,
	};
}

function accumulateContent(
	acc: IndexAccumulator,
	content: string | null,
	embeds: Array<Embed>,
	poll: Poll | null,
	attachments: Array<Attachment>,
	mentionedUserIds: Iterable<UserID>,
	stickerCount: number,
): void {
	if (content) {
		acc.contentChunks.push(content);
		if (HAS_LINK_REGEX.test(content)) {
			acc.hasLink = true;
		}
	}
	if (embeds.length > 0) {
		acc.hasEmbed = true;
		for (const embed of embeds) {
			if (embed.type) {
				pushUnique(acc.embedTypes, embed.type);
			}
			if (embed.provider?.name) {
				pushUnique(acc.embedProviders, embed.provider.name);
			}
			collectEmbedText(embed, acc.embedContent);
		}
	}
	if (poll) {
		acc.hasPoll = true;
	}
	if (attachments.length > 0) {
		acc.hasFile = true;
		if (attachmentHasPrefix(attachments, 'image/')) acc.hasImage = true;
		if (attachmentHasPrefix(attachments, 'video/')) acc.hasVideo = true;
		if (attachmentHasPrefix(attachments, 'audio/')) acc.hasSound = true;
		for (const attachment of attachments) {
			pushUnique(acc.attachmentFilenames, attachment.filename);
			pushAttachmentExtension(acc.attachmentExtensions, attachment.filename);
		}
	}
	if (stickerCount > 0) {
		acc.hasSticker = true;
	}
	extractTextLinkHostnames(content, acc.linkHostnames);
	extractEmbedLinkHostnames(embeds, acc.linkHostnames);
	for (const userId of mentionedUserIds) {
		pushUnique(acc.mentionedUserIds, userId.toString());
	}
}

function mergeSnapshotsIntoAccumulator(acc: IndexAccumulator, snapshots: Array<MessageSnapshot>): void {
	for (const snapshot of snapshots) {
		accumulateContent(
			acc,
			snapshot.content,
			snapshot.embeds,
			snapshot.poll,
			snapshot.attachments,
			snapshot.mentionedUserIds,
			snapshot.stickers.length,
		);
	}
}

export function convertToSearchableMessage(message: Message, authorIsBot?: boolean): SearchableMessage {
	const createdAt = Math.floor(snowflakeToDate(BigInt(message.id)).getTime() / 1000);
	const editedAt = message.editedTimestamp ? Math.floor(message.editedTimestamp.getTime() / 1000) : null;
	const authorType = getAuthorType(message, authorIsBot);
	const acc = emptyAccumulator();
	accumulateContent(
		acc,
		message.content,
		message.embeds,
		message.poll,
		message.attachments,
		message.mentionedUserIds,
		message.stickers.length,
	);
	const isForward = message.reference?.type === 1;
	if (isForward) {
		mergeSnapshotsIntoAccumulator(acc, message.messageSnapshots);
	}
	const content = acc.contentChunks.length > 0 ? acc.contentChunks.join('\n') : null;
	return {
		id: message.id.toString(),
		channelId: message.channelId.toString(),
		guildId: null,
		authorId: message.authorId?.toString() ?? null,
		authorType,
		content,
		createdAt,
		editedAt,
		isPinned: message.pinnedTimestamp !== null,
		mentionedUserIds: acc.mentionedUserIds,
		mentionEveryone: message.mentionEveryone,
		hasLink: acc.hasLink,
		hasEmbed: acc.hasEmbed,
		hasPoll: acc.hasPoll,
		hasFile: acc.hasFile,
		hasVideo: acc.hasVideo,
		hasImage: acc.hasImage,
		hasSound: acc.hasSound,
		hasSticker: acc.hasSticker,
		hasForward: isForward,
		embedTypes: acc.embedTypes,
		embedProviders: acc.embedProviders,
		embedContent: acc.embedContent,
		linkHostnames: acc.linkHostnames,
		attachmentFilenames: acc.attachmentFilenames,
		attachmentExtensions: acc.attachmentExtensions,
	};
}

export function convertMessagesToSearchableMessages(
	messages: Array<Message>,
	authorBotMap?: Map<UserID, boolean>,
): Array<SearchableMessage> {
	return messages.map((message) =>
		convertToSearchableMessage(message, message.authorId ? (authorBotMap?.get(message.authorId) ?? false) : false),
	);
}
