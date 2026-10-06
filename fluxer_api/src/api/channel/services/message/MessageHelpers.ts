// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AttachmentID, ChannelID, UserID} from '@app/api/BrandedTypes';
import {createAttachmentID, userIdToChannelId} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {forEachEmbedMedia} from '@app/api/channel/services/message/CrosspostEmbedObjects';
import type {
	MessageSnapshot as CassandraMessageSnapshot,
	MessageAttachment,
	MessageEmbed,
	MessageEmbedMedia,
} from '@app/api/database/types/MessageTypes';
import type {IPurgeQueue} from '@app/api/infrastructure/CachePurgeQueue';
import type {ISnowflakeService} from '@app/api/infrastructure/ISnowflakeService';
import type {IStorageService} from '@app/api/infrastructure/IStorageService';
import {Logger} from '@app/api/Logger';
import type {LimitConfigService} from '@app/api/limits/LimitConfigService';
import {resolveLimitSafe} from '@app/api/limits/LimitConfigUtils';
import {createLimitMatchContext} from '@app/api/limits/LimitMatchContextBuilder';
import {Attachment} from '@app/api/models/Attachment';
import type {Message} from '@app/api/models/Message';
import {MessageSnapshot as MessageSnapshotModel} from '@app/api/models/MessageSnapshot';
import type {User} from '@app/api/models/User';
import {S3ServiceException} from '@aws-sdk/client-s3';
import {MessageFlags, SENDABLE_MESSAGE_FLAGS} from '@fluxer/constants/src/ChannelConstants';
import {ATTACHMENT_MAX_SIZE_NON_PREMIUM} from '@fluxer/constants/src/LimitConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {UnknownMessageError} from '@fluxer/errors/src/domains/channel/UnknownMessageError';
import {FileSizeTooLargeError} from '@fluxer/errors/src/domains/core/FileSizeTooLargeError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {snowflakeToDate} from '@fluxer/snowflake/src/Snowflake';
import {getContentTypeFromFilename, isSupportedMediaContentType} from '@pkgs/mime_utils/src/ContentTypeUtils';
import {seconds} from 'itty-time';

export const MESSAGE_NONCE_TTL = seconds('5 minutes');

export interface ForwardMediaSelection {
	attachmentIds?: ReadonlySet<AttachmentID>;
	embedIndices?: ReadonlySet<number>;
}

function hasForwardMediaSelection(selection?: ForwardMediaSelection): boolean {
	return Boolean(selection?.attachmentIds?.size || selection?.embedIndices?.size);
}

function selectForwardAttachments(
	attachments: ReadonlyArray<Attachment>,
	selection?: ForwardMediaSelection,
): Array<Attachment> {
	if (!hasForwardMediaSelection(selection)) {
		return [...attachments];
	}
	if (!selection?.attachmentIds?.size) {
		return [];
	}
	const selected = attachments.filter((attachment) => selection.attachmentIds!.has(attachment.id));
	if (selected.length !== selection.attachmentIds.size) {
		throw InputValidationError.fromCode(
			'message_reference.attachment_ids',
			ValidationErrorCodes.REFERENCED_ATTACHMENT_NOT_FOUND,
		);
	}
	return selected;
}

function selectForwardEmbeds<T>(embeds: ReadonlyArray<T>, selection?: ForwardMediaSelection): Array<T> {
	if (!hasForwardMediaSelection(selection)) {
		return [...embeds];
	}
	if (!selection?.embedIndices?.size) {
		return [];
	}
	const selected: Array<T> = [];
	for (const embedIndex of selection.embedIndices) {
		if (embedIndex < 0 || embedIndex >= embeds.length) {
			throw InputValidationError.fromCode(
				'message_reference.embed_indices',
				ValidationErrorCodes.EMBED_INDEX_OUT_OF_BOUNDS,
			);
		}
		selected.push(embeds[embedIndex]);
	}
	return selected;
}

export function isMediaFile(contentType: string): boolean {
	return isSupportedMediaContentType(contentType);
}

export function isPersonalNotesChannel({userId, channelId}: {userId: UserID; channelId: ChannelID}): boolean {
	return userIdToChannelId(userId) === channelId;
}

export function getContentType(filename: string): string {
	return getContentTypeFromFilename(filename);
}

export function validateAttachmentIds(
	attachments: Array<{
		id: bigint;
	}>,
): void {
	const ids = new Set(attachments.map((a) => a.id));
	if (ids.size !== attachments.length) {
		throw InputValidationError.fromCode('attachments', ValidationErrorCodes.DUPLICATE_ATTACHMENT_IDS_NOT_ALLOWED);
	}
}

function validateTotalAttachmentSize(
	attachments: Array<{
		size: number | bigint;
	}>,
	user: User | null,
	limitConfigService: LimitConfigService,
): void {
	const fallbackMaxSize = ATTACHMENT_MAX_SIZE_NON_PREMIUM;
	const ctx = createLimitMatchContext({user});
	const maxFileSize = Math.floor(
		resolveLimitSafe(limitConfigService.getConfigSnapshot(), ctx, 'max_attachment_file_size', fallbackMaxSize, 'user'),
	);
	assertAttachmentFileSizesWithinLimit(
		attachments.map(({size}) => size),
		maxFileSize,
	);
}

export function assertAttachmentFileSizesWithinLimit(fileSizes: Iterable<number | bigint>, maxFileSize: number): void {
	for (const fileSize of fileSizes) {
		if (Number(fileSize) > maxFileSize) {
			throw new FileSizeTooLargeError(maxFileSize);
		}
	}
}

export function makeAttachmentCdnKey(
	channelId: ChannelID,
	attachmentId: AttachmentID | bigint,
	filename: string,
): string {
	return `attachments/${channelId}/${attachmentId}/${filename}`;
}

export function makeAttachmentCdnUrl(
	channelId: ChannelID,
	attachmentId: AttachmentID | bigint,
	filename: string,
): string {
	return `${Config.endpoints.media}/${makeAttachmentCdnKey(channelId, attachmentId, filename)}`;
}

export function isCrosspostCopy(message: Pick<Message, 'flags'>): boolean {
	return (message.flags & MessageFlags.IS_CROSSPOST) !== 0;
}

export function attachmentStorageChannelId(message: Pick<Message, 'flags' | 'channelId' | 'reference'>): ChannelID {
	if (isCrosspostCopy(message) && message.reference) {
		return message.reference.channelId;
	}
	return message.channelId;
}

function isMissingStorageObjectError(error: unknown): boolean {
	return (
		(error instanceof S3ServiceException && (error.name === 'NoSuchKey' || error.name === 'NotFound')) ||
		(error instanceof Error && (error.name === 'NoSuchKey' || error.name === 'NotFound'))
	);
}

async function copyCdnObject(
	storageService: IStorageService,
	sourceKey: string,
	destinationKey: string,
	contentType: string | null | undefined,
): Promise<boolean> {
	try {
		await storageService.copyObject({
			sourceBucket: Config.s3.buckets.cdn,
			sourceKey,
			destinationBucket: Config.s3.buckets.cdn,
			destinationKey,
			newContentType: contentType ?? undefined,
		});
		return true;
	} catch (error) {
		if (isMissingStorageObjectError(error)) {
			Logger.warn({error, sourceKey, destinationKey}, 'Skipping missing attachment while cloning message');
			return false;
		}
		throw error;
	}
}

async function cloneAttachments(
	attachments: Array<Attachment>,
	sourceChannelId: ChannelID,
	destinationChannelId: ChannelID,
	storageService: IStorageService,
	snowflakeService: ISnowflakeService,
): Promise<Array<MessageAttachment>> {
	const clonedAttachments: Array<MessageAttachment> = [];
	for (const attachment of attachments) {
		const newAttachmentId = createAttachmentID(await snowflakeService.generate());
		const copied = await copyCdnObject(
			storageService,
			makeAttachmentCdnKey(sourceChannelId, attachment.id, attachment.filename),
			makeAttachmentCdnKey(destinationChannelId, newAttachmentId, attachment.filename),
			attachment.contentType,
		);
		if (!copied) {
			continue;
		}
		clonedAttachments.push({
			attachment_id: newAttachmentId,
			filename: attachment.filename,
			size: BigInt(attachment.size),
			title: attachment.title,
			description: attachment.description,
			width: attachment.width,
			height: attachment.height,
			content_type: attachment.contentType,
			content_hash: attachment.contentHash,
			placeholder: attachment.placeholder,
			flags: attachment.flags ?? 0,
			duration: attachment.duration,
			nsfw: attachment.nsfw,
			waveform: attachment.waveform ?? null,
		});
	}
	return clonedAttachments;
}

export async function createMessageSnapshotsForForward(
	referencedMessage: Message,
	user: User | null,
	destinationChannelId: ChannelID,
	storageService: IStorageService,
	snowflakeService: ISnowflakeService,
	limitConfigService: LimitConfigService,
	selection?: ForwardMediaSelection,
): Promise<Array<MessageSnapshotModel>> {
	if ((referencedMessage.flags & MessageFlags.SOURCE_MESSAGE_DELETED) !== 0) {
		throw new UnknownMessageError();
	}
	const isMediaOnlyForward = hasForwardMediaSelection(selection);
	if (referencedMessage.messageSnapshots && referencedMessage.messageSnapshots.length > 0) {
		const snapshot = referencedMessage.messageSnapshots[0];
		const snapshotAttachments = selectForwardAttachments(snapshot.attachments ?? [], selection);
		const snapshotEmbeds = selectForwardEmbeds(
			(snapshot.flags & MessageFlags.SUPPRESS_EMBEDS) === 0
				? snapshot.embeds.map((embed) => embed.toMessageEmbed())
				: [],
			selection,
		);
		if (isMediaOnlyForward && snapshotAttachments.length === 0 && snapshotEmbeds.length === 0) {
			throw InputValidationError.fromCode('message_reference', ValidationErrorCodes.NO_VALID_MEDIA_IN_MESSAGE);
		}
		validateTotalAttachmentSize(snapshotAttachments, user, limitConfigService);
		const attachmentsForClone = snapshotAttachments.map((att) =>
			att instanceof Attachment ? att : new Attachment(att),
		);
		const clonedAttachments = await cloneAttachments(
			attachmentsForClone,
			attachmentStorageChannelId(referencedMessage),
			destinationChannelId,
			storageService,
			snowflakeService,
		);
		await cloneOwnedEmbedAttachments(
			snapshotEmbeds,
			attachmentStorageChannelId(referencedMessage),
			destinationChannelId,
			storageService,
			snowflakeService,
		);
		const snapshotData: CassandraMessageSnapshot = {
			content: isMediaOnlyForward ? null : snapshot.content,
			timestamp: snapshot.timestamp,
			edited_timestamp: isMediaOnlyForward ? null : snapshot.editedTimestamp,
			mention_users: isMediaOnlyForward ? null : snapshot.mentionedUserIds,
			mention_roles: isMediaOnlyForward ? null : snapshot.mentionedRoleIds,
			mention_channels: isMediaOnlyForward ? null : snapshot.mentionedChannelIds,
			attachments: clonedAttachments.length > 0 ? clonedAttachments : null,
			embeds: snapshotEmbeds.length > 0 ? snapshotEmbeds : null,
			sticker_items: isMediaOnlyForward ? null : snapshot.stickers.map((sticker) => sticker.toMessageStickerItem()),
			type: snapshot.type,
			flags: snapshot.flags & SENDABLE_MESSAGE_FLAGS,
		};
		return [new MessageSnapshotModel(snapshotData)];
	}
	const selectedAttachments = selectForwardAttachments(referencedMessage.attachments, selection);
	validateTotalAttachmentSize(selectedAttachments, user, limitConfigService);
	const clonedAttachments = await cloneAttachments(
		selectedAttachments,
		attachmentStorageChannelId(referencedMessage),
		destinationChannelId,
		storageService,
		snowflakeService,
	);
	const referencedMessageEmbeds = selectForwardEmbeds(
		(referencedMessage.flags & MessageFlags.SUPPRESS_EMBEDS) === 0
			? referencedMessage.embeds.map((embed) => embed.toMessageEmbed())
			: [],
		selection,
	);
	if (isMediaOnlyForward && selectedAttachments.length === 0 && referencedMessageEmbeds.length === 0) {
		throw InputValidationError.fromCode('message_reference', ValidationErrorCodes.NO_VALID_MEDIA_IN_MESSAGE);
	}
	await cloneOwnedEmbedAttachments(
		referencedMessageEmbeds,
		attachmentStorageChannelId(referencedMessage),
		destinationChannelId,
		storageService,
		snowflakeService,
	);
	const snapshotData: CassandraMessageSnapshot = {
		content: isMediaOnlyForward ? null : referencedMessage.content,
		timestamp: snowflakeToDate(referencedMessage.id),
		edited_timestamp: isMediaOnlyForward ? null : referencedMessage.editedTimestamp,
		mention_users: isMediaOnlyForward
			? null
			: referencedMessage.mentionedUserIds.size > 0
				? referencedMessage.mentionedUserIds
				: null,
		mention_roles: isMediaOnlyForward
			? null
			: referencedMessage.mentionedRoleIds.size > 0
				? referencedMessage.mentionedRoleIds
				: null,
		mention_channels: isMediaOnlyForward
			? null
			: referencedMessage.mentionedChannelIds.size > 0
				? referencedMessage.mentionedChannelIds
				: null,
		attachments: clonedAttachments.length > 0 ? clonedAttachments : null,
		embeds: referencedMessageEmbeds.length > 0 ? referencedMessageEmbeds : null,
		sticker_items:
			!isMediaOnlyForward && referencedMessage.stickers.length > 0
				? referencedMessage.stickers.map((s) => s.toMessageStickerItem())
				: null,
		type: referencedMessage.type,
		flags: referencedMessage.flags & SENDABLE_MESSAGE_FLAGS,
	};
	return [new MessageSnapshotModel(snapshotData)];
}

export const EMBED_MEDIA_OWNED_ATTACHMENT_FLAG = 1 << 30;

function isOwnedEmbedAttachment(media: Pick<MessageEmbedMedia, 'flags'>): boolean {
	return (media.flags & EMBED_MEDIA_OWNED_ATTACHMENT_FLAG) !== 0;
}

export function keepOwnedEmbedAttachments(previous: Message, embeds: Array<MessageEmbed> | null): void {
	const ownedUrls = new Set<string>();
	forEachEmbedMedia(
		previous.embeds.map((embed) => embed.toMessageEmbed()),
		(media) => {
			if (media.url && isOwnedEmbedAttachment(media)) ownedUrls.add(media.url);
		},
	);
	if (ownedUrls.size === 0 || !embeds) return;
	forEachEmbedMedia(embeds, (media) => {
		if (media.url && ownedUrls.has(media.url)) {
			media.flags |= EMBED_MEDIA_OWNED_ATTACHMENT_FLAG;
		}
	});
}

async function cloneOwnedEmbedAttachments(
	embeds: Array<MessageEmbed>,
	sourceChannelId: ChannelID,
	destinationChannelId: ChannelID,
	storageService: IStorageService,
	snowflakeService: ISnowflakeService,
): Promise<void> {
	const mediaPrefix = `${Config.endpoints.media}/`;
	const sourcePrefix = `${mediaPrefix}attachments/${sourceChannelId}/`;
	const owned: Array<MessageEmbedMedia> = [];
	forEachEmbedMedia(embeds, (media) => {
		if (isOwnedEmbedAttachment(media)) owned.push(media);
	});
	const clonedUrls = new Map<string, string | null>();
	for (const media of owned) {
		media.flags &= ~EMBED_MEDIA_OWNED_ATTACHMENT_FLAG;
		const url = media.url;
		if (!url?.startsWith(sourcePrefix)) continue;
		if (!clonedUrls.has(url)) {
			const filename = url.slice(sourcePrefix.length).split('/').slice(1).join('/');
			const clonedId = createAttachmentID(await snowflakeService.generate());
			const copied = await copyCdnObject(
				storageService,
				url.slice(mediaPrefix.length),
				makeAttachmentCdnKey(destinationChannelId, clonedId, filename),
				media.content_type,
			);
			clonedUrls.set(url, copied ? makeAttachmentCdnUrl(destinationChannelId, clonedId, filename) : null);
		}
		const clonedUrl = clonedUrls.get(url);
		if (clonedUrl) {
			media.url = clonedUrl;
			media.flags |= EMBED_MEDIA_OWNED_ATTACHMENT_FLAG;
		}
	}
}

export function collectOwnedEmbedAttachments(message: Message): Array<{key: string; media: MessageEmbedMedia}> {
	const mediaPrefix = `${Config.endpoints.media}/`;
	const ownPrefix = `${mediaPrefix}attachments/${attachmentStorageChannelId(message)}/`;
	const seen = new Set<string>();
	const owned: Array<{key: string; media: MessageEmbedMedia}> = [];
	const embeds = [...message.embeds, ...message.messageSnapshots.flatMap((snapshot) => snapshot.embeds)];
	forEachEmbedMedia(
		embeds.map((embed) => embed.toMessageEmbed()),
		(media) => {
			if (!media.url?.startsWith(ownPrefix) || !isOwnedEmbedAttachment(media)) return;
			const key = media.url.slice(mediaPrefix.length);
			if (seen.has(key)) return;
			seen.add(key);
			owned.push({key, media});
		},
	);
	return owned;
}

export async function purgeMessageAttachments(
	message: Message,
	storageService: IStorageService,
	purgeQueue: IPurgeQueue,
): Promise<void> {
	if (isCrosspostCopy(message)) {
		return;
	}
	const cdnKeys = new Set<string>();
	const cdnUrls: Array<string> = [];
	for (const attachment of collectMessageAttachments(message)) {
		const cdnKey = makeAttachmentCdnKey(message.channelId, attachment.id, attachment.filename);
		if (cdnKeys.has(cdnKey)) {
			continue;
		}
		cdnKeys.add(cdnKey);
		cdnUrls.push(makeAttachmentCdnUrl(message.channelId, attachment.id, attachment.filename));
	}
	for (const {key: embedKey} of collectOwnedEmbedAttachments(message)) {
		if (cdnKeys.has(embedKey)) {
			continue;
		}
		cdnKeys.add(embedKey);
		cdnUrls.push(`${Config.endpoints.media}/${embedKey}`);
	}
	await Promise.all([...cdnKeys].map((cdnKey) => storageService.deleteObject(Config.s3.buckets.cdn, cdnKey)));
	if (cdnUrls.length > 0) {
		await purgeQueue.addUrls(cdnUrls);
	}
}

export function isOperationDisabled(guild: GuildResponse | null, operation: number): boolean {
	if (!guild) return false;
	return (guild.disabled_operations & operation) !== 0;
}

export function isMessageEmpty(message: Message, excludingAttachments = false): boolean {
	const hasContent = !!message.content;
	const hasEmbeds = message.embeds.length > 0;
	const hasStickers = message.stickers.length > 0;
	const hasAttachments = !excludingAttachments && message.attachments.length > 0;
	return !hasContent && !hasEmbeds && !hasStickers && !hasAttachments;
}

export function collectMessageAttachments(message: Message): Array<Attachment> {
	return [...message.attachments, ...message.messageSnapshots.flatMap((snapshot) => snapshot.attachments)];
}
