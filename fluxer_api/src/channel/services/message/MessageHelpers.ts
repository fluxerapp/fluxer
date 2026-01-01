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

import mime from 'mime';
import {type AttachmentID, type ChannelID, createAttachmentID, type UserID, userIdToChannelId} from '~/BrandedTypes';
import {Config} from '~/Config';
import type {MessageAttachment, MessageSnapshot} from '~/database/CassandraTypes';
import {FileSizeTooLargeError, InputValidationError} from '~/Errors';
import type {GuildResponse} from '~/guild/GuildModel';
import type {ICloudflarePurgeQueue} from '~/infrastructure/CloudflarePurgeQueue';
import type {IStorageService} from '~/infrastructure/IStorageService';
import type {SnowflakeService} from '~/infrastructure/SnowflakeService';
import {Attachment, type Message, type User} from '~/Models';
import {getAttachmentMaxSize} from '~/utils/AttachmentUtils';
import {extractTimestamp} from '~/utils/SnowflakeUtils';

export const MESSAGE_NONCE_TTL = 60 * 5;

export const VIRUS_MESSAGE_PREFIX =
	"Hm, it looks like that file might've been a virus. Instead of cooking up trouble, try cooking up a ";

export const VIRUS_RECIPE_SUGGESTIONS = [
	'Chocolate Avocado Protein Smoothie: <https://spinach4breakfast.com/chocolate-avocado-protein-smoothie>',
	'Spicy Italian Meatball: <https://www.foodnetwork.com/recipes/ree-drummond/spicy-italian-meatballs.html>',
	'Hawaiian Banana Nut Bread: <https://www.food.com/recipe/hawaiian-banana-nut-bread-113608>',
	'Panang Red Seafood Curry: <https://www.deliaonline.com/recipes/international/asian/chinese/panang-red-seafood-curry>',
	'Veggie Tofu Stir Fry: <https://minimalistbaker.com/tofu-that-tastes-good-stir-fry>',
	'Chicken Tikka Masala: <https://www.epicurious.com/recipes/food/views/chicken-tikka-masala-51171400>',
	'Slow-Cooked Pulled Pork Sliders: <https://www.foodnetwork.com/recipes/food-network-kitchens/slow-cooker-pulled-pork-sandwiches-recipe.html>',
	'Beet-Pickled Deviled Eggs: <https://www.thekitchn.com/recipe-beet-pickled-deviled-eggs-151550>',
];

export function isMediaFile(contentType: string): boolean {
	return contentType.startsWith('image/') || contentType.startsWith('video/') || contentType.startsWith('audio/');
}

export function cleanTextForMentions(content: string): string {
	return content
		.replace(/```[\s\S]*?```/g, '')
		.replace(/`[^`]*`/g, '')
		.replace(/\[([^\]]+)\]\((https?:\/\/[^\s)]+)\)/g, '$1')
		.replace(/https?:\/\/[^\s]+/g, '');
}

export function isPersonalNotesChannel({userId, channelId}: {userId: UserID; channelId: ChannelID}): boolean {
	return userIdToChannelId(userId) === channelId;
}

export function getContentType(filename: string): string {
	return mime.getType(filename) || 'application/octet-stream';
}

export function validateAttachmentIds(attachments: Array<{id: bigint}>): void {
	const ids = new Set(attachments.map((a) => a.id));
	if (ids.size !== attachments.length) {
		throw InputValidationError.create('attachments', 'Duplicate attachment ids are not allowed');
	}
}

export function validateAttachmentSizes(attachments: Array<{size: number | bigint}>, user: User): void {
	const maxAttachmentSize = getAttachmentMaxSize(user.isPremium());

	for (const {size} of attachments) {
		if (Number(size) > maxAttachmentSize) {
			throw new FileSizeTooLargeError();
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

async function cloneAttachments(
	attachments: Array<Attachment>,
	sourceChannelId: ChannelID,
	destinationChannelId: ChannelID,
	storageService: IStorageService,
	snowflakeService: SnowflakeService,
): Promise<Array<MessageAttachment>> {
	const clonedAttachments: Array<MessageAttachment> = [];

	for (const attachment of attachments) {
		const newAttachmentId = createAttachmentID(snowflakeService.generate());

		const sourceKey = makeAttachmentCdnKey(sourceChannelId, attachment.id, attachment.filename);
		const destinationKey = makeAttachmentCdnKey(destinationChannelId, newAttachmentId, attachment.filename);

		await storageService.copyObject({
			sourceBucket: Config.s3.buckets.cdn,
			sourceKey,
			destinationBucket: Config.s3.buckets.cdn,
			destinationKey,
			newContentType: attachment.contentType,
		});

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
		});
	}

	return clonedAttachments;
}

export async function createMessageSnapshotsForForward(
	referencedMessage: Message,
	user: User,
	destinationChannelId: ChannelID,
	storageService: IStorageService,
	snowflakeService: SnowflakeService,
	validateAttachmentSizesOverride?: (attachments: Array<{size: number | bigint}>, user: User) => void,
): Promise<Array<MessageSnapshot>> {
	const validate = validateAttachmentSizesOverride ?? validateAttachmentSizes;

	if (referencedMessage.messageSnapshots && referencedMessage.messageSnapshots.length > 0) {
		const snapshot = referencedMessage.messageSnapshots[0];
		const snapshotAttachments = snapshot.attachments ?? [];

		validate(snapshotAttachments, user);

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

		return [
			{
				content: snapshot.content,
				timestamp: snapshot.timestamp,
				edited_timestamp: snapshot.editedTimestamp,
				mention_users: snapshot.mentionedUserIds,
				mention_roles: snapshot.mentionedRoleIds,
				mention_channels: snapshot.mentionedChannelIds,
				attachments: clonedAttachments.length > 0 ? clonedAttachments : null,
				embeds: snapshot.embeds.map((embed) => embed.toMessageEmbed()),
				sticker_items: snapshot.stickers.map((sticker) => sticker.toMessageStickerItem()),
				type: snapshot.type,
				flags: snapshot.flags,
			},
		];
	}

	validate(referencedMessage.attachments, user);

	const clonedAttachments = await cloneAttachments(
		referencedMessage.attachments,
		referencedMessage.channelId,
		destinationChannelId,
		storageService,
		snowflakeService,
	);

	return [
		{
			content: referencedMessage.content,
			timestamp: new Date(extractTimestamp(referencedMessage.id)),
			edited_timestamp: referencedMessage.editedTimestamp,
			mention_users: referencedMessage.mentionedUserIds.size > 0 ? referencedMessage.mentionedUserIds : null,
			mention_roles: referencedMessage.mentionedRoleIds.size > 0 ? referencedMessage.mentionedRoleIds : null,
			mention_channels: referencedMessage.mentionedChannelIds.size > 0 ? referencedMessage.mentionedChannelIds : null,
			attachments: clonedAttachments.length > 0 ? clonedAttachments : null,
			embeds: referencedMessage.embeds.length > 0 ? referencedMessage.embeds.map((e) => e.toMessageEmbed()) : null,
			sticker_items:
				referencedMessage.stickers.length > 0 ? referencedMessage.stickers.map((s) => s.toMessageStickerItem()) : null,
			type: referencedMessage.type,
			flags: referencedMessage.flags,
		},
	];
}

export async function purgeMessageAttachments(
	message: Message,
	storageService: IStorageService,
	cloudflarePurgeQueue: ICloudflarePurgeQueue,
): Promise<void> {
	const cdnUrls: Array<string> = [];

	await Promise.all(
		message.attachments.map(async (attachment) => {
			const cdnKey = makeAttachmentCdnKey(message.channelId, attachment.id, attachment.filename);
			await storageService.deleteObject(Config.s3.buckets.cdn, cdnKey);

			if (Config.cloudflare.purgeEnabled) {
				const cdnUrl = makeAttachmentCdnUrl(message.channelId, attachment.id, attachment.filename);
				cdnUrls.push(cdnUrl);
			}
		}),
	);

	if (Config.cloudflare.purgeEnabled && cdnUrls.length > 0) {
		await cloudflarePurgeQueue.addUrls(cdnUrls);
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
