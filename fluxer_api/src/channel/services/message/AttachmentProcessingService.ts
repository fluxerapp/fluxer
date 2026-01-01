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

import {createAttachmentID} from '~/BrandedTypes';
import {Config} from '~/Config';
import {MessageAttachmentFlags} from '~/Constants';
import type {AttachmentToProcess} from '~/channel/AttachmentDTOs';
import type {MessageAttachment} from '~/database/CassandraTypes';
import {InputValidationError} from '~/Errors';
import type {GuildMemberResponse, GuildResponse} from '~/guild/GuildModel';
import type {IMediaService} from '~/infrastructure/IMediaService';
import type {IStorageService} from '~/infrastructure/IStorageService';
import type {IVirusScanService} from '~/infrastructure/IVirusScanService';
import {getMetricsService} from '~/infrastructure/MetricsService';
import type {SnowflakeService} from '~/infrastructure/SnowflakeService';
import {Logger} from '~/Logger';
import type {Channel, Message} from '~/Models';
import {getContentType, isMediaFile, makeAttachmentCdnKey, validateAttachmentIds} from './MessageHelpers';

interface ProcessAttachmentParams {
	message: Message;
	attachment: AttachmentToProcess;
	index: number;
	channel?: Channel;
	guild?: GuildResponse | null;
	member?: GuildMemberResponse | null;
	isNSFWAllowed: boolean;
}

interface AttachmentCopyOperation {
	sourceBucket: string;
	sourceKey: string;
	destinationBucket: string;
	destinationKey: string;
	newContentType: string;
}

interface ProcessedAttachment {
	attachment: MessageAttachment;
	copyOperation: AttachmentCopyOperation;
	hasVirusDetected: boolean;
}

export class AttachmentProcessingService {
	constructor(
		private storageService: IStorageService,
		private mediaService: IMediaService,
		private virusScanService: IVirusScanService,
		private snowflakeService: SnowflakeService,
	) {}

	async computeAttachments(params: {
		message: Message;
		attachments: Array<AttachmentToProcess>;
		channel?: Channel;
		guild?: GuildResponse | null;
		member?: GuildMemberResponse | null;
		isNSFWAllowed: boolean;
	}): Promise<{attachments: Array<MessageAttachment>; hasVirusDetected: boolean}> {
		validateAttachmentIds(params.attachments.map((a) => ({id: BigInt(a.id)})));

		const results = await Promise.all(
			params.attachments.map((attachment, index) =>
				this.processAttachment({
					message: params.message,
					attachment,
					index,
					channel: params.channel,
					guild: params.guild,
					member: params.member,
					isNSFWAllowed: params.isNSFWAllowed,
				}),
			),
		);

		const hasVirusDetected = results.some((result) => result.hasVirusDetected);
		if (hasVirusDetected) {
			return {attachments: [], hasVirusDetected: true};
		}

		const copyResults = await Promise.all(
			results.map((result) =>
				this.storageService.copyObjectWithJpegProcessing({
					sourceBucket: result.copyOperation.sourceBucket,
					sourceKey: result.copyOperation.sourceKey,
					destinationBucket: result.copyOperation.destinationBucket,
					destinationKey: result.copyOperation.destinationKey,
					contentType: result.copyOperation.newContentType,
				}),
			),
		);

		for (const result of results) {
			void this.deleteUploadObject(result.copyOperation.sourceBucket, result.copyOperation.sourceKey);
		}

		const processedAttachments: Array<MessageAttachment> = results.map((result, index) => {
			const maybeDimensions = copyResults[index];
			if (maybeDimensions) {
				return {
					...result.attachment,
					width: maybeDimensions.width,
					height: maybeDimensions.height,
				};
			}
			return result.attachment;
		});

		const metrics = getMetricsService();
		for (let index = 0; index < processedAttachments.length; index++) {
			const result = results[index];
			if (result.hasVirusDetected) continue;

			const attachment = processedAttachments[index];
			const contentType = attachment.content_type ?? 'unknown';
			const filename = attachment.filename;
			const extension =
				(filename?.includes('.') ?? false) ? (filename.split('.').pop()?.toLowerCase() ?? 'unknown') : 'unknown';

			const channelType = params.channel?.type ?? 'unknown';

			metrics.counter({
				name: 'attachment.created',
				dimensions: {
					content_type: contentType,
					attachment_extension: extension,
					channel_type: channelType.toString(),
				},
			});
			metrics.counter({
				name: 'attachment.storage.bytes',
				dimensions: {
					content_type: contentType,
					channel_type: channelType.toString(),
					action: 'create',
				},
				value: Number(attachment.size),
			});
		}

		return {attachments: processedAttachments, hasVirusDetected: false};
	}

	private async processAttachment(params: ProcessAttachmentParams): Promise<ProcessedAttachment> {
		const {message, attachment, index, isNSFWAllowed} = params;

		const uploadedFile = await this.storageService.getObjectMetadata(
			Config.s3.buckets.uploads,
			attachment.upload_filename,
		);

		if (!uploadedFile) {
			throw InputValidationError.create(`attachments.${index}.upload_filename`, 'File not found');
		}

		const attachmentId = createAttachmentID(this.snowflakeService.generate());
		const cdnKey = makeAttachmentCdnKey(message.channelId, attachmentId, attachment.filename);

		let contentType = getContentType(attachment.filename);
		let size = BigInt(uploadedFile.contentLength);
		const clientFlags =
			(attachment.flags ?? 0) & (MessageAttachmentFlags.IS_SPOILER | MessageAttachmentFlags.CONTAINS_EXPLICIT_MEDIA);

		let flags = clientFlags;
		let width: number | null = null;
		let height: number | null = null;
		let placeholder: string | null = null;
		let duration: number | null = null;
		let hasVirusDetected = false;
		let nsfw: boolean | null = null;
		let contentHash: string | null = null;

		const isMedia = isMediaFile(contentType);

		const scanResult = await this.scanMalware(attachment);
		if (scanResult.isVirusDetected) {
			hasVirusDetected = true;
			await this.storageService.deleteObject(Config.s3.buckets.uploads, attachment.upload_filename);

			return {
				attachment: {
					attachment_id: attachmentId,
					filename: attachment.filename,
					size,
					title: attachment.title ?? null,
					description: attachment.description ?? null,
					height,
					width,
					content_type: contentType,
					content_hash: contentHash,
					placeholder,
					flags,
					duration,
					nsfw,
				},
				copyOperation: {
					sourceBucket: Config.s3.buckets.uploads,
					sourceKey: attachment.upload_filename,
					destinationBucket: Config.s3.buckets.cdn,
					destinationKey: cdnKey,
					newContentType: contentType,
				},
				hasVirusDetected,
			};
		}

		if (isMedia) {
			const metadata = await this.mediaService.getMetadata({
				type: 'upload',
				upload_filename: attachment.upload_filename,
				isNSFWAllowed,
			});

			if (metadata) {
				contentType = metadata.content_type;
				contentHash = metadata.content_hash;
				size = BigInt(metadata.size);
				placeholder = metadata.placeholder ?? null;
				duration = metadata.duration && metadata.duration > 0 ? metadata.duration : null;
				width = metadata.width ?? null;
				height = metadata.height ?? null;

				if (metadata.animated) {
					flags |= MessageAttachmentFlags.IS_ANIMATED;
				}
				if (metadata.nsfw) {
					flags |= MessageAttachmentFlags.CONTAINS_EXPLICIT_MEDIA;
				}
				nsfw = metadata.nsfw;
			}
		}

		return {
			attachment: {
				attachment_id: attachmentId,
				filename: attachment.filename,
				size,
				title: attachment.title ?? null,
				description: attachment.description ?? null,
				height,
				width,
				content_type: contentType,
				content_hash: contentHash,
				placeholder,
				flags,
				duration,
				nsfw,
			},
			copyOperation: {
				sourceBucket: Config.s3.buckets.uploads,
				sourceKey: attachment.upload_filename,
				destinationBucket: Config.s3.buckets.cdn,
				destinationKey: cdnKey,
				newContentType: contentType,
			},
			hasVirusDetected,
		};
	}

	private async scanMalware(attachment: AttachmentToProcess): Promise<{isVirusDetected: boolean}> {
		const fileData = await this.storageService.readObject(Config.s3.buckets.uploads, attachment.upload_filename);

		if (!fileData) {
			throw InputValidationError.create('attachment', 'File not found for scanning');
		}

		const fileBuffer = Buffer.from(fileData);
		const scanResult = await this.virusScanService.scanBuffer(fileBuffer, attachment.filename);

		return {isVirusDetected: !scanResult.isClean};
	}

	private deleteUploadObject(bucket: string, key: string): void {
		void this.storageService.deleteObject(bucket, key).catch((error) => {
			Logger.warn({bucket, key, error}, 'Failed to delete temporary upload object');
		});
	}
}
