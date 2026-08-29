// SPDX-License-Identifier: AGPL-3.0-or-later

import {S3ServiceException} from '@aws-sdk/client-s3';
import {MessageFlags} from '@fluxer/constants/src/ChannelConstants';
import {ATTACHMENT_MAX_SIZE_NON_PREMIUM} from '@fluxer/constants/src/LimitConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {FileSizeTooLargeError} from '@fluxer/errors/src/domains/core/FileSizeTooLargeError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {snowflakeToDate} from '@fluxer/snowflake/src/Snowflake';
import {getContentTypeFromFilename, isSupportedMediaContentType} from '@pkgs/mime_utils/src/ContentTypeUtils';
import {seconds} from 'itty-time';
import type {AttachmentID, ChannelID, UserID} from '../../../BrandedTypes';
import {createAttachmentID, userIdToChannelId} from '../../../BrandedTypes';
import {Config} from '../../../Config';
import type {
	MessageSnapshot as CassandraMessageSnapshot,
	MessageAttachment,
} from '../../../database/types/MessageTypes';
import type {IPurgeQueue} from '../../../infrastructure/BunnyPurgeQueue';
import type {ISnowflakeService} from '../../../infrastructure/ISnowflakeService';
import type {IStorageService} from '../../../infrastructure/IStorageService';
import {Logger} from '../../../Logger';
import type {LimitConfigService} from '../../../limits/LimitConfigService';
import {resolveLimitSafe} from '../../../limits/LimitConfigUtils';
import {createLimitMatchContext} from '../../../limits/LimitMatchContextBuilder';
import {Attachment} from '../../../models/Attachment';
import type {Embed} from '../../../models/Embed';
import type {Message} from '../../../models/Message';
import {MessageSnapshot as MessageSnapshotModel} from '../../../models/MessageSnapshot';
import type {User} from '../../../models/User';

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

function isMissingStorageObjectError(error: unknown): boolean {
	return (
		(error instanceof S3ServiceException && (error.name === 'NoSuchKey' || error.name === 'NotFound')) ||
		(error instanceof Error && (error.name === 'NoSuchKey' || error.name === 'NotFound'))
	);
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
		const sourceKey = makeAttachmentCdnKey(sourceChannelId, attachment.id, attachment.filename);
		const destinationKey = makeAttachmentCdnKey(destinationChannelId, newAttachmentId, attachment.filename);
		try {
			await storageService.copyObject({
				sourceBucket: Config.s3.buckets.cdn,
				sourceKey,
				destinationBucket: Config.s3.buckets.cdn,
				destinationKey,
				newContentType: attachment.contentType,
			});
		} catch (error) {
			if (isMissingStorageObjectError(error)) {
				Logger.warn(
					{
						error,
						sourceChannelId,
						destinationChannelId,
						sourceKey,
						destinationKey,
						attachmentId: attachment.id,
						filename: attachment.filename,
					},
					'Skipping missing attachment while cloning forwarded message',
				);
				continue;
			}
			throw error;
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
			referencedMessage.channelId,
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
			flags: snapshot.flags,
		};
		return [new MessageSnapshotModel(snapshotData)];
	}
	const selectedAttachments = selectForwardAttachments(referencedMessage.attachments, selection);
	validateTotalAttachmentSize(selectedAttachments, user, limitConfigService);
	const clonedAttachments = await cloneAttachments(
		selectedAttachments,
		referencedMessage.channelId,
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
		flags: referencedMessage.flags,
	};
	return [new MessageSnapshotModel(snapshotData)];
}

function collectEmbedReferencedAttachmentCdnKeys(message: Message): Array<string> {
	const mediaPrefix = `${Config.endpoints.media}/`;
	const keys = new Set<string>();
	const consider = (url: string | null | undefined): void => {
		if (!url || !url.startsWith(mediaPrefix)) {
			return;
		}
		const key = url.slice(mediaPrefix.length);
		if (key.startsWith('attachments/')) {
			keys.add(key);
		}
	};
	const scanEmbeds = (embeds: Array<Embed>): void => {
		for (const embed of embeds) {
			consider(embed.image?.url);
			consider(embed.thumbnail?.url);
			consider(embed.video?.url);
			consider(embed.audio?.url);
		}
	};
	scanEmbeds(message.embeds);
	for (const snapshot of message.messageSnapshots) {
		scanEmbeds(snapshot.embeds);
	}
	return [...keys];
}

export async function purgeMessageAttachments(
	message: Message,
	storageService: IStorageService,
	purgeQueue: IPurgeQueue,
): Promise<void> {
	const cdnKeys = new Set<string>();
	const cdnUrls: Array<string> = [];
	for (const attachment of collectMessageAttachments(message)) {
		const cdnKey = makeAttachmentCdnKey(message.channelId, attachment.id, attachment.filename);
		if (cdnKeys.has(cdnKey)) {
			continue;
		}
		cdnKeys.add(cdnKey);
		if (Config.bunny.purgeEnabled) {
			cdnUrls.push(makeAttachmentCdnUrl(message.channelId, attachment.id, attachment.filename));
		}
	}
	for (const embedKey of collectEmbedReferencedAttachmentCdnKeys(message)) {
		if (cdnKeys.has(embedKey)) {
			continue;
		}
		cdnKeys.add(embedKey);
		if (Config.bunny.purgeEnabled) {
			cdnUrls.push(`${Config.endpoints.media}/${embedKey}`);
		}
	}
	await Promise.all([...cdnKeys].map((cdnKey) => storageService.deleteObject(Config.s3.buckets.cdn, cdnKey)));
	if (Config.bunny.purgeEnabled && cdnUrls.length > 0) {
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
