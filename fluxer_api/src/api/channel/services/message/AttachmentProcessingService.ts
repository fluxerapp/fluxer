// SPDX-License-Identifier: AGPL-3.0-or-later

import fs from 'node:fs';
import {MessageAttachmentFlags} from '@fluxer/constants/src/ChannelConstants';
import {MAX_MEDIA_DURATION_SECONDS} from '@fluxer/constants/src/LimitConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {ContentBlockedError} from '@fluxer/errors/src/domains/content/ContentBlockedError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {ExplicitContentCannotBeSentError} from '@fluxer/errors/src/domains/moderation/ExplicitContentCannotBeSentError';
import type {GuildMemberResponse} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {isSupportedMediaContentType} from '@pkgs/mime_utils/src/ContentTypeUtils';
import type {IVirusScanService} from '@pkgs/virus_scan/src/IVirusScanService';
import {temporaryFile} from 'tempy';
import {createAttachmentID, type UserID} from '../../../BrandedTypes';
import {Config} from '../../../Config';
import type {MessageAttachment} from '../../../database/types/MessageTypes';
import {contentModerationService, type ModerationContext} from '../../../infrastructure/ContentModerationService';
import type {IMediaService, MediaProxyNsfwMode} from '../../../infrastructure/IMediaService';
import type {ISnowflakeService} from '../../../infrastructure/ISnowflakeService';
import type {IStorageService} from '../../../infrastructure/IStorageService';
import {hashFileSha256} from '../../../infrastructure/StorageObjectHelpers';
import {Logger} from '../../../Logger';
import type {Channel} from '../../../models/Channel';
import type {Message} from '../../../models/Message';
import type {AttachmentToProcess} from '../../AttachmentDTOs';
import type {AttachmentUploadTraceRepository} from '../../repositories/message/AttachmentUploadTraceRepository';
import {getContentType, isMediaFile, makeAttachmentCdnKey, validateAttachmentIds} from './MessageHelpers';

const ATTACHMENT_PROCESSING_CONCURRENCY = 2;
const METADATA_PROBE_DEGRADED_CONTEXT = 'message_attachment';

async function mapWithConcurrency<T, R>(
	items: ReadonlyArray<T>,
	limit: number,
	fn: (item: T, index: number) => Promise<R>,
): Promise<Array<R>> {
	if (items.length === 0) return [];
	const results = new Array<R>(items.length);
	let nextIndex = 0;
	const worker = async (): Promise<void> => {
		while (true) {
			const index = nextIndex++;
			if (index >= items.length) return;
			results[index] = await fn(items[index], index);
		}
	};
	const workerCount = Math.min(limit, items.length);
	await Promise.all(Array.from({length: workerCount}, () => worker()));
	return results;
}

interface ProcessAttachmentParams {
	message: Message;
	attachment: AttachmentToProcess;
	index: number;
	uploadUserId: UserID;
	channel?: Channel;
	guild?: GuildResponse | null;
	member?: GuildMemberResponse | null;
	nsfwMode: MediaProxyNsfwMode;
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
	applyFinalObjectMetadata: boolean;
	sourceLocalPath: string | null;
}

export class AttachmentProcessingService {
	constructor(
		private storageService: IStorageService,
		private attachmentUploadTraceRepository: AttachmentUploadTraceRepository,
		private mediaService: IMediaService,
		private virusScanService: IVirusScanService,
		private snowflakeService: ISnowflakeService,
	) {}

	async computeAttachments(params: {
		message: Message;
		attachments: Array<AttachmentToProcess>;
		uploadUserId: UserID;
		channel?: Channel;
		guild?: GuildResponse | null;
		member?: GuildMemberResponse | null;
		nsfwMode: MediaProxyNsfwMode;
	}): Promise<{
		attachments: Array<MessageAttachment>;
		hasVirusDetected: boolean;
	}> {
		validateAttachmentIds(params.attachments.map((a) => ({id: BigInt(a.id)})));
		const results = await mapWithConcurrency(
			params.attachments,
			ATTACHMENT_PROCESSING_CONCURRENCY,
			(attachment, index) =>
				this.processAttachment({
					message: params.message,
					attachment,
					index,
					uploadUserId: params.uploadUserId,
					channel: params.channel,
					guild: params.guild,
					member: params.member,
					nsfwMode: params.nsfwMode,
				}),
		);
		const hasVirusDetected = results.some((result) => result.hasVirusDetected);
		if (hasVirusDetected) {
			return {attachments: [], hasVirusDetected: true};
		}
		const copyResults = await mapWithConcurrency(results, ATTACHMENT_PROCESSING_CONCURRENCY, (result) =>
			this.storageService.copyObjectWithMetadataStripping({
				sourceBucket: result.copyOperation.sourceBucket,
				sourceKey: result.copyOperation.sourceKey,
				destinationBucket: result.copyOperation.destinationBucket,
				destinationKey: result.copyOperation.destinationKey,
				contentType: result.copyOperation.newContentType,
				...(result.sourceLocalPath ? {sourceLocalPath: result.sourceLocalPath} : {}),
			}),
		);
		await Promise.all(
			results.map(async (result) => {
				if (result.sourceLocalPath) {
					await fs.promises.unlink(result.sourceLocalPath).catch(() => undefined);
				}
			}),
		);
		const bindingResults = await Promise.all(
			results.map(async (result, index) => ({
				index,
				result,
				bound: await this.attachmentUploadTraceRepository.bindAttachment(
					result.copyOperation.sourceKey,
					result.attachment.attachment_id,
				),
			})),
		);
		for (const result of results) {
			void this.deleteUploadObject(result.copyOperation.sourceBucket, result.copyOperation.sourceKey);
		}
		const unboundResult = bindingResults.find(({bound}) => bound === null);
		if (unboundResult) {
			for (const result of results) {
				this.deleteUploadObject(result.copyOperation.destinationBucket, result.copyOperation.destinationKey);
			}
			throw InputValidationError.fromCode(
				`attachments.${unboundResult.index}.upload_filename`,
				ValidationErrorCodes.UPLOADED_ATTACHMENT_NOT_FOUND,
				{filename: unboundResult.result.attachment.filename},
			);
		}
		const processedAttachments: Array<MessageAttachment> = results.map((result, index) => {
			const finalObject = copyResults[index];
			if (result.applyFinalObjectMetadata && finalObject) {
				return {
					...result.attachment,
					content_type: finalObject.contentType,
					content_hash: finalObject.contentHash,
					size: BigInt(finalObject.contentLength),
					width: finalObject.width ?? result.attachment.width,
					height: finalObject.height ?? result.attachment.height,
				};
			}
			return result.attachment;
		});
		return {attachments: processedAttachments, hasVirusDetected: false};
	}

	private async processAttachment(params: ProcessAttachmentParams): Promise<ProcessedAttachment> {
		const {message, attachment, index, nsfwMode} = params;
		const pendingUpload = await this.attachmentUploadTraceRepository.getPendingUpload({
			uploadKey: attachment.upload_filename,
			userId: params.uploadUserId,
			channelId: message.channelId,
		});
		if (!pendingUpload) {
			throw InputValidationError.fromCode(
				`attachments.${index}.upload_filename`,
				ValidationErrorCodes.UPLOADED_ATTACHMENT_NOT_FOUND,
				{filename: attachment.filename},
			);
		}
		const uploadedFile = await this.storageService.getObjectMetadata(
			Config.s3.buckets.uploads,
			attachment.upload_filename,
		);
		if (!uploadedFile) {
			throw InputValidationError.fromCode(`attachments.${index}.upload_filename`, ValidationErrorCodes.FILE_NOT_FOUND);
		}
		const attachmentId = createAttachmentID(await this.snowflakeService.generate());
		const cdnKey = makeAttachmentCdnKey(message.channelId, attachmentId, attachment.filename);
		let contentType = attachment.content_type ?? getContentType(attachment.filename);
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
		let applyFinalObjectMetadata = false;
		const clientDuration: number | null = attachment.duration ?? null;
		const waveform: string | null = attachment.waveform ?? null;
		const isMedia = isMediaFile(contentType);
		let metadata: Awaited<ReturnType<AttachmentProcessingService['getAttachmentMediaMetadata']>> = null;
		if (isMedia) {
			metadata = await this.getAttachmentMediaMetadata({
				index,
				uploadFilename: attachment.upload_filename,
				filename: attachment.filename,
				nsfwMode,
			});
			if (metadata) {
				applyFinalObjectMetadata = true;
				contentType = metadata.content_type;
				contentHash = metadata.content_hash;
				size = BigInt(metadata.size);
				placeholder = metadata.placeholder ?? null;
				duration =
					metadata.duration && metadata.duration > 0 ? Math.min(metadata.duration, MAX_MEDIA_DURATION_SECONDS) : null;
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
		const needsLocalDownload =
			this.virusScanService.enabled || metadata == null || isSupportedMediaContentType(contentType);
		let sourceLocalPath: string | null = null;
		try {
			if (needsLocalDownload) {
				sourceLocalPath = temporaryFile();
				await this.storageService.writeObjectToDisk(
					Config.s3.buckets.uploads,
					attachment.upload_filename,
					sourceLocalPath,
				);
			}
			if (metadata) {
				await this.scanContentModerationHash({
					contentHash: metadata.content_hash,
					bucket: Config.s3.buckets.uploads,
					key: attachment.upload_filename,
					message,
				});
			} else {
				await this.scanContentModerationWithLocalFile({
					localPath: sourceLocalPath,
					bucket: Config.s3.buckets.uploads,
					key: attachment.upload_filename,
					message,
				});
			}
			const scanResult = await this.scanMalware(sourceLocalPath);
			if (scanResult.isVirusDetected) {
				hasVirusDetected = true;
				await this.storageService.deleteObject(Config.s3.buckets.uploads, attachment.upload_filename);
				if (sourceLocalPath) {
					await fs.promises.unlink(sourceLocalPath).catch(() => undefined);
					sourceLocalPath = null;
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
					applyFinalObjectMetadata,
					sourceLocalPath: null,
				};
			}
			const isAudio = contentType.startsWith('audio/');
			if (waveform && !isAudio) {
				throw InputValidationError.fromCode(
					`attachments.${index}.upload_filename`,
					ValidationErrorCodes.VOICE_MESSAGES_ATTACHMENT_MUST_BE_AUDIO,
				);
			}
			const retainedLocalPath = sourceLocalPath;
			sourceLocalPath = null;
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
				applyFinalObjectMetadata,
				sourceLocalPath: retainedLocalPath,
			};
		} catch (error) {
			if (sourceLocalPath) {
				await fs.promises.unlink(sourceLocalPath).catch(() => undefined);
			}
			throw error;
		}
	}

	private async getAttachmentMediaMetadata(params: {
		index: number;
		uploadFilename: string;
		filename: string;
		nsfwMode: MediaProxyNsfwMode;
	}) {
		try {
			const metadata = await this.mediaService.getMetadata({
				type: 'upload',
				upload_filename: params.uploadFilename,
				filename: params.filename,
				nsfw: params.nsfwMode,
			});
			if (metadata) {
				return metadata;
			}
			Logger.warn(
				{
					context: METADATA_PROBE_DEGRADED_CONTEXT,
					attachmentIndex: params.index,
					uploadFilename: params.uploadFilename,
					filename: params.filename,
				},
				'Attachment media metadata unavailable; storing attachment as a plain file',
			);
			return null;
		} catch (error) {
			if (error instanceof ExplicitContentCannotBeSentError) {
				throw error;
			}
			Logger.warn(
				{
					error,
					context: METADATA_PROBE_DEGRADED_CONTEXT,
					attachmentIndex: params.index,
					uploadFilename: params.uploadFilename,
					filename: params.filename,
				},
				'Attachment media metadata probe failed; storing attachment as a plain file',
			);
			return null;
		}
	}

	private async scanContentModerationWithLocalFile(params: {
		localPath: string | null;
		bucket: string;
		key: string;
		message: Message;
	}): Promise<void> {
		const sha = params.localPath
			? await hashFileSha256(params.localPath)
			: await this.storageService.computeObjectSha256(params.bucket, params.key);
		await this.scanContentModerationHash({
			contentHash: sha,
			bucket: params.bucket,
			key: params.key,
			message: params.message,
		});
	}

	private async scanContentModerationHash(params: {
		contentHash: string;
		bucket: string;
		key: string;
		message: Message;
	}): Promise<void> {
		const {bucket, key, message} = params;
		const modCtx: ModerationContext = {
			userId: message.authorId ?? null,
			guildId: null,
			channelId: message.channelId,
			messageId: message.id,
			surface: 'message_attachment',
		};
		try {
			contentModerationService.scanSha256(params.contentHash, modCtx);
		} catch (error) {
			if (error instanceof ContentBlockedError) {
				await this.storageService.deleteObject(bucket, key);
			}
			throw error;
		}
	}

	private async scanMalware(localPath: string | null): Promise<{
		isVirusDetected: boolean;
	}> {
		if (!this.virusScanService.enabled || !localPath) {
			return {isVirusDetected: false};
		}
		const scanResult = await this.virusScanService.scanFile(localPath);
		return {isVirusDetected: !scanResult.isClean};
	}

	private deleteUploadObject(bucket: string, key: string): void {
		void this.storageService.deleteObject(bucket, key).catch((error) => {
			Logger.warn({bucket, key, error}, 'Failed to delete temporary upload object');
		});
	}
}
