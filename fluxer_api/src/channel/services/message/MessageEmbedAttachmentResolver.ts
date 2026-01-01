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

import type {AttachmentID, ChannelID} from '~/BrandedTypes';
import type {AttachmentRequestData} from '~/channel/AttachmentDTOs';
import type {RichEmbedMediaWithMetadata, RichEmbedRequest} from '~/channel/ChannelModel';
import {InputValidationError} from '~/Errors';
import {makeAttachmentCdnUrl} from './MessageHelpers';

interface ProcessedAttachment {
	attachment_id: AttachmentID;
	filename: string;
	width: number | null;
	height: number | null;
	content_type: string;
	content_hash: string | null;
	placeholder: string | null;
	flags: number;
	duration: number | null;
	nsfw: boolean | null;
}

interface RichEmbedRequestWithMetadata extends Omit<RichEmbedRequest, 'image' | 'thumbnail'> {
	image?: RichEmbedMediaWithMetadata | null;
	thumbnail?: RichEmbedMediaWithMetadata | null;
}

const SUPPORTED_IMAGE_EXTENSIONS = new Set(['png', 'jpg', 'jpeg', 'webp', 'gif']);

export class MessageEmbedAttachmentResolver {
	validateAttachmentReferences(params: {
		embeds: Array<RichEmbedRequest> | undefined;
		attachments: Array<AttachmentRequestData> | undefined;
		existingAttachments?: Array<{filename: string}>;
	}): void {
		if (!params.embeds || params.embeds.length === 0) {
			return;
		}

		let availableFilenames: Set<string> | undefined;

		if (params.attachments !== undefined) {
			const filenames = params.attachments
				.map((att) => ('filename' in att ? att.filename : undefined))
				.filter((filename): filename is string => typeof filename === 'string' && filename.length > 0);
			if (filenames.length > 0) {
				availableFilenames = new Set(filenames);
			}
		} else if (params.existingAttachments) {
			availableFilenames = new Set(params.existingAttachments.map((att) => att.filename));
		}

		if (!availableFilenames || availableFilenames.size === 0) {
			for (const embed of params.embeds) {
				if (embed.image?.url?.startsWith('attachment://') || embed.thumbnail?.url?.startsWith('attachment://')) {
					throw InputValidationError.create('embeds', 'Cannot reference attachments when no attachments are provided');
				}
			}
			return;
		}

		const validateAttachmentReference = (filename: string, field: string, embedIndex: number) => {
			if (!availableFilenames.has(filename)) {
				throw InputValidationError.create(
					`embeds[${embedIndex}].${field}`,
					`Referenced attachment "${filename}" not found in message attachments`,
				);
			}

			const extension = filename.split('.').pop()?.toLowerCase();
			if (!extension || !SUPPORTED_IMAGE_EXTENSIONS.has(extension)) {
				throw InputValidationError.create(
					`embeds[${embedIndex}].${field}`,
					`Attachment "${filename}" must be an image file (png, jpg, jpeg, webp, or gif)`,
				);
			}
		};

		for (let embedIndex = 0; embedIndex < params.embeds.length; embedIndex++) {
			const embed = params.embeds[embedIndex];

			if (embed.image?.url?.startsWith('attachment://')) {
				const filename = embed.image.url.slice(13);
				validateAttachmentReference(filename, 'image.url', embedIndex);
			}

			if (embed.thumbnail?.url?.startsWith('attachment://')) {
				const filename = embed.thumbnail.url.slice(13);
				validateAttachmentReference(filename, 'thumbnail.url', embedIndex);
			}
		}
	}

	resolveEmbedAttachmentUrls(params: {
		embeds: Array<RichEmbedRequest> | undefined;
		attachments: Array<ProcessedAttachment>;
		channelId: ChannelID;
	}): Array<RichEmbedRequestWithMetadata> | undefined {
		if (!params.embeds || params.embeds.length === 0) {
			return params.embeds as Array<RichEmbedRequestWithMetadata> | undefined;
		}

		const attachmentMap = new Map<string, {cdnUrl: string; metadata: ProcessedAttachment}>();
		for (const attachment of params.attachments) {
			const cdnUrl = makeAttachmentCdnUrl(params.channelId, attachment.attachment_id, attachment.filename);
			attachmentMap.set(attachment.filename, {cdnUrl, metadata: attachment});
		}

		const resolveAttachmentUrl = (filename: string, field: string): {cdnUrl: string; metadata: ProcessedAttachment} => {
			const attachmentData = attachmentMap.get(filename);
			if (!attachmentData) {
				throw InputValidationError.create(
					field,
					`Referenced attachment "${filename}" not found in message attachments`,
				);
			}

			const extension = filename.split('.').pop()?.toLowerCase();
			if (!extension || !SUPPORTED_IMAGE_EXTENSIONS.has(extension)) {
				throw InputValidationError.create(
					field,
					`Attachment "${filename}" must be an image file (png, jpg, jpeg, webp, or gif)`,
				);
			}

			return attachmentData;
		};

		return params.embeds.map((embed) => {
			const resolvedEmbed: RichEmbedRequestWithMetadata = {...embed};

			if (embed.image?.url?.startsWith('attachment://')) {
				const filename = embed.image.url.slice(13);
				const {cdnUrl, metadata} = resolveAttachmentUrl(filename, 'embeds.image.url');
				resolvedEmbed.image = {
					...embed.image,
					url: cdnUrl,
					_attachmentMetadata: {
						width: metadata.width,
						height: metadata.height,
						content_type: metadata.content_type,
						content_hash: metadata.content_hash,
						placeholder: metadata.placeholder,
						flags: metadata.flags,
						duration: metadata.duration,
						nsfw: metadata.nsfw,
					},
				};
			}

			if (embed.thumbnail?.url?.startsWith('attachment://')) {
				const filename = embed.thumbnail.url.slice(13);
				const {cdnUrl, metadata} = resolveAttachmentUrl(filename, 'embeds.thumbnail.url');
				resolvedEmbed.thumbnail = {
					...embed.thumbnail,
					url: cdnUrl,
					_attachmentMetadata: {
						width: metadata.width,
						height: metadata.height,
						content_type: metadata.content_type,
						content_hash: metadata.content_hash,
						placeholder: metadata.placeholder,
						flags: metadata.flags,
						duration: metadata.duration,
						nsfw: metadata.nsfw,
					},
				};
			}

			return resolvedEmbed;
		});
	}
}
