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

import fs from 'node:fs/promises';
import {createAttachmentID} from '@fluxer/api/src/BrandedTypes';
import {Config} from '@fluxer/api/src/Config';
import type {AttachmentToProcess} from '@fluxer/api/src/channel/AttachmentDTOs';
import {
	getContentType,
	isMediaFile,
	makeAttachmentCdnKey,
	validateAttachmentIds,
} from '@fluxer/api/src/channel/services/message/MessageHelpers';
import type {ICsamReportSnapshotService} from '@fluxer/api/src/csam/ICsamReportSnapshotService';
import type {ISynchronousCsamScanner} from '@fluxer/api/src/csam/ISynchronousCsamScanner';
import type {SynchronousCsamScanResult} from '@fluxer/api/src/csam/SynchronousCsamScanner';
import type {MessageAttachment} from '@fluxer/api/src/database/types/MessageTypes';
import type {IMediaService} from '@fluxer/api/src/infrastructure/IMediaService';
import type {ISnowflakeService} from '@fluxer/api/src/infrastructure/ISnowflakeService';
import type {IStorageService} from '@fluxer/api/src/infrastructure/IStorageService';
import {Logger} from '@fluxer/api/src/Logger';
import type {Channel} from '@fluxer/api/src/models/Channel';
import type {Message} from '@fluxer/api/src/models/Message';
import {recordAttachmentOperation} from '@fluxer/api/src/telemetry/MessageTelemetry';
import {MessageAttachmentFlags} from '@fluxer/constants/src/ChannelConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {ContentBlockedError} from '@fluxer/errors/src/domains/content/ContentBlockedError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import type {GuildMemberResponse} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {recordCounter} from '@fluxer/telemetry/src/Metrics';
import type {IVirusScanService} from '@fluxer/virus_scan/src/IVirusScanService';
import {temporaryFile} from 'tempy';

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

interface HandleCsamMatchParams {
	scanResult: SynchronousCsamScanResult;
	result: ProcessedAttachment;
	results: Array<ProcessedAttachment>;
	message: Message;
	guild?: GuildResponse | null;
	csamReportSnapshotService: ICsamReportSnapshotService;
}

export class AttachmentProcessingService {
	constructor(
		private storageService: IStorageService,
		private mediaService: IMediaService,
		private virusScanService: IVirusScanService,
		private snowflakeService: ISnowflakeService,
		private readonly synchronousCsamScanner?: ISynchronousCsamScanner,
		private readonly csamReportSnapshotService?: ICsamReportSnapshotService,
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

		if (this.synchronousCsamScanner && this.csamReportSnapshotService) {
			for (const result of results) {
				const scanResult = await this.synchronousCsamScanner.scanMedia({
					bucket: result.copyOperation.sourceBucket,
					key: result.copyOperation.sourceKey,
					contentType: result.attachment.content_type,
				});

				if (scanResult.isMatch) {
					await this.handleCsamMatch({
						scanResult,
						result,
						results,
						message: params.message,
						guild: params.guild,
						csamReportSnapshotService: this.csamReportSnapshotService,
					});
				}
			}
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

		for (let index = 0; index < processedAttachments.length; index++) {
			const result = results[index];
			if (result.hasVirusDetected) continue;

			const attachment = processedAttachments[index];
			const contentType = attachment.content_type ?? 'unknown';
			const filename = attachment.filename;
			const extension =
				(filename?.includes('.') ?? false) ? (filename.split('.').pop()?.toLowerCase() ?? 'unknown') : 'unknown';

			const channelType = params.channel?.type ?? 'unknown';

			recordCounter({
				name: 'attachment.created',
				dimensions: {
					content_type: contentType,
					attachment_extension: extension,
					channel_type: channelType.toString(),
				},
			});
			recordCounter({
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

	private async handleCsamMatch(params: HandleCsamMatchParams): Promise<never> {
		const messageId = params.message.id.toString();
		const bucket = params.result.copyOperation.sourceBucket;
		const key = params.result.copyOperation.sourceKey;

		if (!params.scanResult.matchResult) {
			Logger.error({bucket, key, messageId}, 'CSAM match detected without match details');
			await this.deleteCsamUploads(params.results);
			throw new ContentBlockedError();
		}

		const mediaData = await this.storageService.readObject(bucket, key);

		if (!mediaData) {
			Logger.error({bucket, key, messageId}, 'CSAM match detected but media data could not be read');
			await this.deleteCsamUploads(params.results);
			throw new ContentBlockedError();
		}

		await params.csamReportSnapshotService.createSnapshot({
			scanResult: params.scanResult.matchResult,
			resourceType: 'attachment',
			userId: params.message.authorId?.toString() ?? null,
			guildId: params.guild?.id ?? null,
			channelId: params.message.channelId.toString(),
			messageId,
			mediaData: Buffer.from(mediaData),
			filename: params.result.attachment.filename,
			contentType: params.result.attachment.content_type,
		});

		await this.deleteCsamUploads(params.results);

		throw new ContentBlockedError();
	}

	private async deleteCsamUploads(results: Array<ProcessedAttachment>): Promise<void> {
		for (const result of results) {
			await this.storageService.deleteObject(result.copyOperation.sourceBucket, result.copyOperation.sourceKey);
		}
	}

	private async processAttachment(params: ProcessAttachmentParams): Promise<ProcessedAttachment> {
		try {
			const {message, attachment, index, isNSFWAllowed} = params;

			const uploadedFile = await this.storageService.getObjectMetadata(
				Config.s3.buckets.uploads,
				attachment.upload_filename,
			);

			if (!uploadedFile) {
				throw InputValidationError.fromCode(
					`attachments.${index}.upload_filename`,
					ValidationErrorCodes.FILE_NOT_FOUND,
				);
			}

			const attachmentId = createAttachmentID(await this.snowflakeService.generate());
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
			const clientDuration: number | null = attachment.duration ?? null;
			const waveform: string | null = attachment.waveform ?? null;

			const isMedia = isMediaFile(contentType);

			const scanResult = await this.scanMalware(attachment);
			if (scanResult.isVirusDetected) {
				hasVirusDetected = true;
				await this.storageService.deleteObject(Config.s3.buckets.uploads, attachment.upload_filename);

				recordAttachmentOperation({
					operation: 'process',
					contentType: attachment.content_type || 'unknown',
					status: 'success',
				});

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
						duration: duration ?? clientDuration,
						nsfw,
						waveform,
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
					filename: attachment.filename,
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

			const isAudio = contentType.startsWith('audio/');
			if (waveform && !isAudio) {
				throw InputValidationError.fromCode(
					`attachments.${index}.upload_filename`,
					ValidationErrorCodes.VOICE_MESSAGES_ATTACHMENT_MUST_BE_AUDIO,
				);
			}

			recordAttachmentOperation({
				operation: 'process',
				contentType: attachment.content_type || 'unknown',
				status: 'success',
			});

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
					duration: duration ?? clientDuration,
					nsfw,
					waveform,
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
		} catch (error) {
			recordAttachmentOperation({
				operation: 'process',
				contentType: params.attachment.content_type || 'unknown',
				status: 'error',
			});
			throw error;
		}
	}

	private async scanMalware(attachment: AttachmentToProcess): Promise<{isVirusDetected: boolean}> {
		const tempPath = temporaryFile();
		try {
			await this.storageService.writeObjectToDisk(Config.s3.buckets.uploads, attachment.upload_filename, tempPath);
			const scanResult = await this.virusScanService.scanFile(tempPath);
			return {isVirusDetected: !scanResult.isClean};
		} finally {
			await fs.unlink(tempPath).catch(() => {});
		}
	}

	private deleteUploadObject(bucket: string, key: string): void {
		void this.storageService.deleteObject(bucket, key).catch((error) => {
			Logger.warn({bucket, key, error}, 'Failed to delete temporary upload object');
		});
	}
}
