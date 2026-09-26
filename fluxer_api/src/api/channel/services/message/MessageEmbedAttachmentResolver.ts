// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AttachmentID, ChannelID} from '@app/api/BrandedTypes';
import type {AttachmentRequestData} from '@app/api/channel/AttachmentDTOs';
import type {RichEmbedMediaWithMetadata} from '@app/api/channel/EmbedTypes';
import {getContentType, makeAttachmentCdnUrl} from '@app/api/channel/services/message/MessageHelpers';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import type {RichEmbedRequest} from '@fluxer/schema/src/domains/message/MessageRequestSchemas';

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

function isEmbeddableMediaType(contentType: string): boolean {
	const normalized = contentType.toLowerCase();
	return normalized.startsWith('image/') || normalized.startsWith('video/');
}

export class MessageEmbedAttachmentResolver {
	validateAttachmentReferences(params: {
		embeds: Array<RichEmbedRequest> | undefined;
		attachments: Array<AttachmentRequestData> | undefined;
		existingAttachments?: Array<{
			filename: string;
		}>;
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
					throw InputValidationError.fromCode(
						'embeds',
						ValidationErrorCodes.CANNOT_REFERENCE_ATTACHMENTS_WITHOUT_ATTACHMENTS,
					);
				}
			}
			return;
		}
		const validateAttachmentReference = (filename: string, field: string, embedIndex: number) => {
			if (!availableFilenames.has(filename)) {
				throw InputValidationError.fromCode(
					`embeds[${embedIndex}].${field}`,
					ValidationErrorCodes.REFERENCED_ATTACHMENT_NOT_FOUND,
					{filename},
				);
			}
			if (!isEmbeddableMediaType(getContentType(filename))) {
				throw InputValidationError.fromCode(
					`embeds[${embedIndex}].${field}`,
					ValidationErrorCodes.ATTACHMENT_MUST_BE_IMAGE,
					{filename},
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

	collectReferencedAttachmentFilenames(embeds: Array<RichEmbedRequest> | undefined): Set<string> {
		const filenames = new Set<string>();
		if (!embeds) {
			return filenames;
		}
		for (const embed of embeds) {
			if (embed.image?.url?.startsWith('attachment://')) {
				filenames.add(embed.image.url.slice(13));
			}
			if (embed.thumbnail?.url?.startsWith('attachment://')) {
				filenames.add(embed.thumbnail.url.slice(13));
			}
		}
		return filenames;
	}

	resolveEmbedAttachmentUrls(params: {
		embeds: Array<RichEmbedRequest> | undefined;
		attachments: Array<ProcessedAttachment>;
		channelId: ChannelID;
	}): Array<RichEmbedRequestWithMetadata> | undefined {
		if (!params.embeds || params.embeds.length === 0) {
			return params.embeds as Array<RichEmbedRequestWithMetadata> | undefined;
		}
		const attachmentMap = new Map<
			string,
			{
				cdnUrl: string;
				metadata: ProcessedAttachment;
			}
		>();
		for (const attachment of params.attachments) {
			const cdnUrl = makeAttachmentCdnUrl(params.channelId, attachment.attachment_id, attachment.filename);
			attachmentMap.set(attachment.filename, {cdnUrl, metadata: attachment});
		}
		const resolveAttachmentUrl = (
			filename: string,
			field: string,
		): {
			cdnUrl: string;
			metadata: ProcessedAttachment;
		} => {
			const attachmentData = attachmentMap.get(filename);
			if (!attachmentData) {
				throw InputValidationError.fromCode(field, ValidationErrorCodes.REFERENCED_ATTACHMENT_NOT_FOUND, {filename});
			}
			if (!isEmbeddableMediaType(attachmentData.metadata.content_type)) {
				throw InputValidationError.fromCode(field, ValidationErrorCodes.ATTACHMENT_MUST_BE_IMAGE, {filename});
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
