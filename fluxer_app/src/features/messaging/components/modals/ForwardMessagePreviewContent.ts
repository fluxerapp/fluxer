// SPDX-License-Identifier: AGPL-3.0-or-later

import {isMediaAttachment} from '@app/features/channel/components/MessageAttachmentUtils';
import type {ForwardMediaSelection} from '@app/features/messaging/commands/MessageCommands';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import {
	buildMediaProxyURL,
	snapMediaProxyImageSize,
	stripMediaProxyParams,
} from '@app/features/messaging/utils/MediaProxyUtils';
import {EmbedMediaFlags, MessageAttachmentFlags} from '@fluxer/constants/src/ChannelConstants';
import type {MessageEmbed} from '@fluxer/schema/src/domains/message/EmbedSchemas';
import type {
	ChannelMention,
	MessageAttachment,
	MessageSnapshot,
} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';

const FORWARD_MESSAGE_PREVIEW_THUMBNAIL_SIZE = 56;

export type ForwardMessagePreviewMessage = Pick<
	Message,
	'attachments' | 'content' | 'editedTimestamp' | 'embeds' | 'mentionChannels' | 'messageSnapshots'
>;

export type ForwardMessagePreviewAttachmentSummary =
	| {readonly kind: 'images_and_videos'; readonly imageCount: number; readonly videoCount: number}
	| {readonly kind: 'videos'; readonly count: number}
	| {readonly kind: 'images'; readonly count: number}
	| {readonly kind: 'files'; readonly count: number};

export interface ForwardMessagePreviewThumbnail {
	readonly proxyUrl: string;
	readonly isVideo: boolean;
	readonly showsPlayIcon: boolean;
	readonly showsFilePlaceholder: boolean;
	readonly isSensitive: boolean;
	readonly width: number | null;
	readonly height: number | null;
}

export interface ForwardMessagePreviewContent {
	readonly text: string | null;
	readonly isEdited: boolean;
	readonly mentionChannels: ReadonlyArray<ChannelMention> | undefined;
	readonly attachmentSummary: ForwardMessagePreviewAttachmentSummary | null;
	readonly thumbnail: ForwardMessagePreviewThumbnail | null;
	readonly overflowCount: number;
}

interface ResolveForwardMessagePreviewContentRequest {
	readonly message: ForwardMessagePreviewMessage;
	readonly mediaSelection: ForwardMediaSelection | undefined;
	readonly canEmbedLinks: boolean;
}

interface ForwardMessagePreviewSource {
	readonly content: string;
	readonly attachments: ReadonlyArray<MessageAttachment>;
	readonly embeds: ReadonlyArray<MessageEmbed>;
	readonly mentionChannels: ReadonlyArray<ChannelMention> | undefined;
}

interface VisualAttachmentCounts {
	readonly imageCount: number;
	readonly videoCount: number;
}

type PreviewAttachmentKind = 'image' | 'video' | 'file';

const EMPTY_ATTACHMENTS: ReadonlyArray<MessageAttachment> = Object.freeze([]);
const EMPTY_EMBEDS: ReadonlyArray<MessageEmbed> = Object.freeze([]);

function readSnapshotContent(snapshot: MessageSnapshot): string {
	if (snapshot.content == null) return '';
	return snapshot.content;
}

function readSnapshotAttachments(snapshot: MessageSnapshot): ReadonlyArray<MessageAttachment> {
	if (snapshot.attachments == null) return EMPTY_ATTACHMENTS;
	return snapshot.attachments;
}

function resolvePreviewSource(message: ForwardMessagePreviewMessage): ForwardMessagePreviewSource {
	const snapshot = message.messageSnapshots?.[0];
	if (snapshot == null) {
		return {
			content: message.content,
			attachments: message.attachments,
			embeds: message.embeds,
			mentionChannels: message.mentionChannels,
		};
	}
	let embeds = EMPTY_EMBEDS;
	if (snapshot.embeds != null) embeds = snapshot.embeds;
	return {
		content: readSnapshotContent(snapshot),
		attachments: readSnapshotAttachments(snapshot),
		embeds,
		mentionChannels: snapshot.mention_channels,
	};
}

function messageHasForwardableContent(message: ForwardMessagePreviewMessage): boolean {
	if (message.content !== '') return true;
	const snapshots = message.messageSnapshots;
	if (snapshots == null) return false;
	return snapshots.some(
		(snapshot) => readSnapshotContent(snapshot) !== '' || readSnapshotAttachments(snapshot).length > 0,
	);
}

function selectPreviewAttachments(
	attachments: ReadonlyArray<MessageAttachment>,
	mediaSelection: ForwardMediaSelection | undefined,
): ReadonlyArray<MessageAttachment> {
	if (mediaSelection === undefined) return attachments;
	const {attachmentIds, embedIndices} = mediaSelection;
	if (attachmentIds !== undefined) return attachments.filter((attachment) => attachmentIds.includes(attachment.id));
	if (embedIndices !== undefined) return EMPTY_ATTACHMENTS;
	return attachments;
}

function selectPreviewEmbeds(
	embeds: ReadonlyArray<MessageEmbed>,
	mediaSelection: ForwardMediaSelection | undefined,
): ReadonlyArray<MessageEmbed> {
	if (mediaSelection === undefined) return embeds;
	const {attachmentIds, embedIndices} = mediaSelection;
	if (embedIndices !== undefined) return embeds.filter((_embed, index) => embedIndices.includes(index));
	if (attachmentIds !== undefined) return EMPTY_EMBEDS;
	return embeds;
}

function resolvePreviewText(
	source: ForwardMessagePreviewSource,
	embeds: ReadonlyArray<MessageEmbed>,
	mediaSelection: ForwardMediaSelection | undefined,
): string {
	let text = source.content;
	const hasEmbedSelection = mediaSelection !== undefined && mediaSelection.embedIndices !== undefined;
	if (hasEmbedSelection || (text === '' && embeds.length > 0)) {
		text = embeds.map((embed) => embed.url ?? '').join('\n');
	}
	if (text !== '') return text;
	const firstEmbed = source.embeds[0];
	if (firstEmbed == null || firstEmbed.description == null) return text;
	return firstEmbed.description;
}

function classifyPreviewAttachment(attachment: MessageAttachment): PreviewAttachmentKind {
	if (!isMediaAttachment(attachment)) return 'file';
	if (attachment.content_type?.startsWith('video/') === true) return 'video';
	return 'image';
}

function countVisualAttachments(attachments: ReadonlyArray<MessageAttachment>): VisualAttachmentCounts {
	let imageCount = 0;
	let videoCount = 0;
	for (const attachment of attachments) {
		const kind = classifyPreviewAttachment(attachment);
		if (kind === 'image') imageCount += 1;
		if (kind === 'video') videoCount += 1;
	}
	return {imageCount, videoCount};
}

function summarizePreviewAttachments(
	attachments: ReadonlyArray<MessageAttachment>,
	{imageCount, videoCount}: VisualAttachmentCounts,
): ForwardMessagePreviewAttachmentSummary | null {
	if (attachments.length === 0) return null;
	if (imageCount > 0 && videoCount > 0) return Object.freeze({kind: 'images_and_videos', imageCount, videoCount});
	if (videoCount > 0) return Object.freeze({kind: 'videos', count: videoCount});
	if (imageCount > 0) return Object.freeze({kind: 'images', count: imageCount});
	return Object.freeze({kind: 'files', count: attachments.length});
}

function isSensitiveAttachment(attachment: MessageAttachment): boolean {
	if (attachment.nsfw === true) return true;
	const sensitiveFlags = MessageAttachmentFlags.IS_SPOILER | MessageAttachmentFlags.CONTAINS_EXPLICIT_MEDIA;
	return (attachment.flags & sensitiveFlags) !== 0;
}

function resolveAttachmentThumbnail(
	attachment: MessageAttachment,
	showsPlayIcon: boolean,
): ForwardMessagePreviewThumbnail | null {
	const kind = classifyPreviewAttachment(attachment);
	const proxyUrl = attachment.proxy_url ?? attachment.url;
	if (proxyUrl == null) return null;
	return Object.freeze({
		proxyUrl,
		isVideo: kind === 'video',
		showsPlayIcon,
		showsFilePlaceholder: kind === 'file',
		isSensitive: isSensitiveAttachment(attachment),
		width: attachment.width ?? null,
		height: attachment.height ?? null,
	});
}

function resolveEmbedThumbnail(embed: MessageEmbed | undefined): ForwardMessagePreviewThumbnail | null {
	if (embed == null || embed.thumbnail == null) return null;
	const {thumbnail} = embed;
	if (thumbnail.proxy_url == null) return null;
	const isSensitive =
		embed.nsfw === true || thumbnail.nsfw === true || (thumbnail.flags & EmbedMediaFlags.CONTAINS_EXPLICIT_MEDIA) !== 0;
	return Object.freeze({
		proxyUrl: thumbnail.proxy_url,
		isVideo: false,
		showsPlayIcon: false,
		showsFilePlaceholder: false,
		isSensitive,
		width: thumbnail.width ?? null,
		height: thumbnail.height ?? null,
	});
}

function resolvePreviewThumbnail(
	attachments: ReadonlyArray<MessageAttachment>,
	embeds: ReadonlyArray<MessageEmbed>,
	{videoCount}: VisualAttachmentCounts,
): ForwardMessagePreviewThumbnail | null {
	const firstAttachment = attachments[0];
	if (firstAttachment == null) return resolveEmbedThumbnail(embeds[0]);
	const everyAttachmentIsVideo = videoCount > 0 && videoCount === attachments.length;
	return resolveAttachmentThumbnail(firstAttachment, everyAttachmentIsVideo);
}

export function resolveForwardMessagePreviewContent({
	message,
	mediaSelection,
	canEmbedLinks,
}: ResolveForwardMessagePreviewContentRequest): ForwardMessagePreviewContent {
	const source = resolvePreviewSource(message);
	const attachments = selectPreviewAttachments(source.attachments, mediaSelection);
	let embeds = EMPTY_EMBEDS;
	if (canEmbedLinks || !messageHasForwardableContent(message)) {
		embeds = selectPreviewEmbeds(source.embeds, mediaSelection);
	}
	const text = resolvePreviewText(source, embeds, mediaSelection);
	const hasAttachmentSelection = mediaSelection !== undefined && mediaSelection.attachmentIds !== undefined;
	const counts = countVisualAttachments(attachments);
	const thumbnail = resolvePreviewThumbnail(attachments, embeds, counts);
	let overflowCount = 0;
	if (thumbnail != null && attachments.length > 1) overflowCount = attachments.length - 1;
	return Object.freeze({
		text: text !== '' && !hasAttachmentSelection ? text : null,
		isEdited: message.content !== '' && message.editedTimestamp != null,
		mentionChannels: source.mentionChannels,
		attachmentSummary: summarizePreviewAttachments(attachments, counts),
		thumbnail,
		overflowCount,
	});
}

function resolveCoverWidthScale({width, height}: ForwardMessagePreviewThumbnail): number {
	if (width == null || height == null || width <= 0 || height <= 0) return 1;
	return Math.max(1, width / height);
}

export function buildForwardMessagePreviewThumbnailURL(thumbnail: ForwardMessagePreviewThumbnail): string {
	const sourceUrl = stripMediaProxyParams(thumbnail.proxyUrl);
	if (!thumbnail.isVideo) {
		const size = snapMediaProxyImageSize(FORWARD_MESSAGE_PREVIEW_THUMBNAIL_SIZE);
		return buildMediaProxyURL(sourceUrl, {format: 'webp', width: size, height: size});
	}
	const width = snapMediaProxyImageSize(FORWARD_MESSAGE_PREVIEW_THUMBNAIL_SIZE * resolveCoverWidthScale(thumbnail));
	return buildMediaProxyURL(sourceUrl, {format: 'webp', width});
}
