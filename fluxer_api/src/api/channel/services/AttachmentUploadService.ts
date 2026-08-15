// SPDX-License-Identifier: AGPL-3.0-or-later

import {Permissions, TEXT_BASED_CHANNEL_TYPES} from '@fluxer/constants/src/ChannelConstants';
import {GuildOperations} from '@fluxer/constants/src/GuildConstants';
import {
	ATTACHMENT_MAX_SIZE_BOT,
	ATTACHMENT_MAX_SIZE_NON_PREMIUM,
	ATTACHMENT_UPLOAD_CHUNK_THRESHOLD,
	ATTACHMENT_UPLOAD_MAX_CHUNKS,
	resolveAttachmentUploadPartSize,
} from '@fluxer/constants/src/LimitConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {CannotSendMessageToNonTextChannelError} from '@fluxer/errors/src/domains/channel/CannotSendMessageToNonTextChannelError';
import {UnknownMessageError} from '@fluxer/errors/src/domains/channel/UnknownMessageError';
import {FeatureTemporarilyDisabledError} from '@fluxer/errors/src/domains/core/FeatureTemporarilyDisabledError';
import {FileSizeTooLargeError} from '@fluxer/errors/src/domains/core/FileSizeTooLargeError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {MissingPermissionsError} from '@fluxer/errors/src/domains/core/MissingPermissionsError';
import {UnknownUserError} from '@fluxer/errors/src/domains/user/UnknownUserError';
import {ServiceUnavailableError} from '@fluxer/errors/src/HttpErrors';
import type {
	CompleteMultipartAttachmentUploadItem,
	CompleteMultipartAttachmentUploadResult,
	PresignedAttachmentUploadRequestItem,
	PresignedAttachmentUploadResponseItem,
} from '@fluxer/schema/src/domains/message/AttachmentUploadSchemas';
import type {AttachmentID, ChannelID, MessageID, UserID} from '../../BrandedTypes';
import {Config} from '../../Config';
import type {IPurgeQueue} from '../../infrastructure/BunnyPurgeQueue';
import type {IStorageService} from '../../infrastructure/IStorageService';
import type {LimitConfigService} from '../../limits/LimitConfigService';
import {resolveLimitSafe} from '../../limits/LimitConfigUtils';
import {createLimitMatchContext} from '../../limits/LimitMatchContextBuilder';
import type {RequestCache} from '../../middleware/RequestCacheMiddleware';
import type {Attachment} from '../../models/Attachment';
import type {Channel} from '../../models/Channel';
import type {Message} from '../../models/Message';
import type {IUserRepository} from '../../user/IUserRepository';
import {assertGuildMemberCanCommunicate} from '../../utils/GuildCommunicationUtils';
import type {UploadedAttachment} from '../AttachmentDTOs';
import type {IChannelRepositoryAggregate} from '../repositories/IChannelRepositoryAggregate';
import type {
	AttachmentUploadMode,
	AttachmentUploadTraceRepository,
} from '../repositories/message/AttachmentUploadTraceRepository';
import type {MessageInteractionService} from './MessageInteractionService';
import type {MessageService} from './MessageService';
import {
	assertAttachmentFileSizesWithinLimit,
	getContentType,
	isMessageEmpty,
	isOperationDisabled,
	makeAttachmentCdnKey,
	makeAttachmentCdnUrl,
	purgeMessageAttachments as purgeMessageAttachmentsHelper,
} from './message/MessageHelpers';
import {applyUploadRelayDecision, resolveUploadRelayDecision} from './UploadRelay';

interface DeleteAttachmentParams {
	userId: UserID;
	channelId: ChannelID;
	messageId: MessageID;
	attachmentId: AttachmentID;
	requestCache: RequestCache;
}

interface UploadFormDataAttachmentsParams {
	userId: UserID;
	channelId: ChannelID;
	clientIp: string;
	files: Array<{
		file: File;
		index: number;
	}>;
	attachmentMetadata: Array<{
		id: number;
		filename: string;
	}>;
	expiresAt?: Date;
}

interface RequestPresignedAttachmentUploadUrlsParams {
	userId: UserID;
	channelId: ChannelID;
	clientIp: string;
	attachments: Array<PresignedAttachmentUploadRequestItem>;
}

interface CompleteMultipartAttachmentUploadsParams {
	userId: UserID;
	channelId: ChannelID;
	clientIp: string;
	uploads: Array<CompleteMultipartAttachmentUploadItem>;
}

const FORM_DATA_UPLOAD_CONCURRENCY = 2;

export class AttachmentUploadService {
	constructor(
		private channelRepository: IChannelRepositoryAggregate,
		private userRepository: IUserRepository,
		private storageService: IStorageService,
		private attachmentUploadTraceRepository: AttachmentUploadTraceRepository,
		private purgeQueue: IPurgeQueue,
		private messageInteractionService: MessageInteractionService,
		private messageService: MessageService,
		private limitConfigService: LimitConfigService,
	) {}

	async uploadFormDataAttachments({
		userId,
		channelId,
		clientIp,
		files,
		attachmentMetadata,
		expiresAt,
	}: UploadFormDataAttachmentsParams): Promise<Array<UploadedAttachment>> {
		const {maxFileSize} = await this.getUploadPermissionAndLimit({userId, channelId});
		assertAttachmentFileSizesWithinLimit(
			files.map(({file}) => file.size),
			maxFileSize,
		);
		const metadataMap = new Map(attachmentMetadata.map((m) => [m.id, m]));
		const uploadedAttachments = await mapWithConcurrency(files, FORM_DATA_UPLOAD_CONCURRENCY, async (fileWithIndex) => {
			const {file, index} = fileWithIndex;
			const metadata = metadataMap.get(index);
			if (!metadata) {
				throw new Error(`Internal error: metadata not found for file index ${index}`);
			}
			const filename = metadata.filename;
			const uploadKey = crypto.randomUUID();
			const arrayBuffer = await file.arrayBuffer();
			const body = new Uint8Array(arrayBuffer);
			const contentType = getContentType(filename);
			await runAttachmentStorageOperation(() =>
				this.storageService.uploadObject({
					bucket: Config.s3.buckets.uploads,
					key: uploadKey,
					body,
					contentType,
					expiresAt: expiresAt ?? undefined,
				}),
			);
			await this.attachmentUploadTraceRepository.recordRequestedUpload({
				uploadKey,
				userId,
				channelId,
				filename,
				contentType,
				uploadMode: 'form_data',
				requestIp: clientIp,
			});
			const uploaded: UploadedAttachment = {
				id: index,
				upload_filename: uploadKey,
				filename: filename,
				file_size: file.size,
				content_type: contentType,
			};
			return uploaded;
		});
		return uploadedAttachments;
	}

	async requestPresignedAttachmentUploadUrls({
		userId,
		channelId,
		clientIp,
		attachments,
	}: RequestPresignedAttachmentUploadUrlsParams): Promise<Array<PresignedAttachmentUploadResponseItem>> {
		if (!Config.presignedAttachmentUploadsEnabled) {
			throw new FeatureTemporarilyDisabledError();
		}
		const {maxFileSize} = await this.getUploadPermissionAndLimit({userId, channelId});
		assertAttachmentFileSizesWithinLimit(
			attachments.map(({file_size}) => file_size),
			maxFileSize,
		);
		const uploadRelayDecision = await resolveUploadRelayDecision(clientIp);
		return Promise.all(
			attachments.map(async (attachment) => {
				const uploadKey = crypto.randomUUID();
				const derivedContentType = getContentType(attachment.filename);
				const bucket = Config.s3.buckets.uploads;
				const uploadMode: AttachmentUploadMode =
					attachment.file_size <= ATTACHMENT_UPLOAD_CHUNK_THRESHOLD ? 'presigned_singlepart' : 'presigned_multipart';
				await this.attachmentUploadTraceRepository.recordRequestedUpload({
					uploadKey,
					userId,
					channelId,
					filename: attachment.filename,
					contentType: derivedContentType,
					uploadMode,
					requestIp: clientIp,
				});
				if (attachment.file_size <= ATTACHMENT_UPLOAD_CHUNK_THRESHOLD) {
					const presigned_upload_url = await this.storageService.getPresignedUploadURL({
						bucket,
						key: uploadKey,
						contentType: derivedContentType,
						contentLength: attachment.file_size,
					});
					const upload_url = applyUploadRelayDecision({
						presignedUrl: presigned_upload_url,
						bucket,
						key: uploadKey,
						relayDecision: uploadRelayDecision,
						contentType: derivedContentType,
						maxBytes: attachment.file_size,
					});
					return {
						upload_mode: 'singlepart',
						id: attachment.id,
						filename: attachment.filename,
						upload_filename: uploadKey,
						upload_url,
						file_size: attachment.file_size,
						content_type: derivedContentType,
					};
				}
				const partSize = resolveAttachmentUploadPartSize(attachment.file_size);
				const partCount = Math.ceil(attachment.file_size / partSize);
				if (partCount > ATTACHMENT_UPLOAD_MAX_CHUNKS) {
					throw new FileSizeTooLargeError(maxFileSize);
				}
				const {uploadId} = await runAttachmentStorageOperation(() =>
					this.storageService.createMultipartUpload({
						bucket,
						key: uploadKey,
						contentType: derivedContentType,
					}),
				);
				const parts = await Promise.all(
					Array.from({length: partCount}, async (_, index) => {
						const partNumber = index + 1;
						const presigned_upload_url = await this.storageService.getPresignedUploadPartURL({
							bucket,
							key: uploadKey,
							uploadId,
							partNumber,
						});
						const upload_url = applyUploadRelayDecision({
							presignedUrl: presigned_upload_url,
							bucket,
							key: uploadKey,
							relayDecision: uploadRelayDecision,
							uploadId,
							partNumber,
							maxBytes: partSize,
						});
						return {part_number: partNumber, upload_url};
					}),
				);
				return {
					upload_mode: 'multipart',
					id: attachment.id,
					filename: attachment.filename,
					upload_filename: uploadKey,
					file_size: attachment.file_size,
					content_type: derivedContentType,
					upload_id: uploadId,
					part_size: partSize,
					parts,
				};
			}),
		);
	}

	async completeMultipartAttachmentUploads({
		userId,
		channelId,
		clientIp,
		uploads,
	}: CompleteMultipartAttachmentUploadsParams): Promise<Array<CompleteMultipartAttachmentUploadResult>> {
		if (!Config.presignedAttachmentUploadsEnabled) {
			throw new FeatureTemporarilyDisabledError();
		}
		await this.getUploadPermissionAndLimit({userId, channelId});
		const bucket = Config.s3.buckets.uploads;
		return Promise.all(
			uploads.map(async ({upload_filename, upload_id}, index) => {
				const pendingUpload = await this.attachmentUploadTraceRepository.getPendingUpload({
					uploadKey: upload_filename,
					userId,
					channelId,
					uploadMode: 'presigned_multipart',
				});
				if (!pendingUpload) {
					throw InputValidationError.fromCode(
						`uploads.${index}.upload_filename`,
						ValidationErrorCodes.UPLOADED_ATTACHMENT_NOT_FOUND,
						{filename: upload_filename},
					);
				}
				const parts = await runAttachmentStorageOperation(() =>
					this.storageService.listParts({
						bucket,
						key: upload_filename,
						uploadId: upload_id,
					}),
				);
				if (parts.length === 0) {
					await this.storageService
						.abortMultipartUpload({bucket, key: upload_filename, uploadId: upload_id})
						.catch(() => undefined);
					throw InputValidationError.fromCode('parts', ValidationErrorCodes.NO_UPLOADED_PARTS_TO_FINALIZE);
				}
				try {
					await runAttachmentStorageOperation(() =>
						this.storageService.completeMultipartUpload({
							bucket,
							key: upload_filename,
							uploadId: upload_id,
							parts,
						}),
					);
					await this.attachmentUploadTraceRepository.markUploadCompleted({
						uploadKey: upload_filename,
						completionIp: clientIp,
					});
				} catch (error) {
					await this.storageService
						.abortMultipartUpload({bucket, key: upload_filename, uploadId: upload_id})
						.catch(() => undefined);
					throw error;
				}
				return {upload_filename};
			}),
		);
	}

	async deleteAttachment({
		userId,
		channelId,
		messageId,
		attachmentId,
		requestCache,
	}: DeleteAttachmentParams): Promise<void> {
		const {channel, guild} = await this.messageInteractionService.authService.getChannelAuthenticated({
			userId,
			channelId,
		});
		if (isOperationDisabled(guild, GuildOperations.SEND_MESSAGE)) {
			throw new FeatureTemporarilyDisabledError();
		}
		const message = await this.channelRepository.messages.getMessage(channelId, messageId);
		if (!message) {
			throw new UnknownMessageError();
		}
		if (message.authorId !== userId) {
			throw new MissingPermissionsError();
		}
		if (!message.attachments || message.attachments.length === 0) {
			throw new UnknownMessageError();
		}
		const attachment = message.attachments.find((a: Attachment) => a.id === attachmentId);
		if (!attachment) {
			throw new UnknownMessageError();
		}
		const isLastAttachment = message.attachments.length === 1;
		const willBeEmpty = isLastAttachment && isMessageEmpty(message, true);
		if (willBeEmpty) {
			await this.messageService.deletion.deleteMessage({
				userId,
				channelId,
				messageId,
				requestCache,
			});
			return;
		}
		const cdnKey = makeAttachmentCdnKey(message.channelId, attachment.id, attachment.filename);
		await this.storageService.deleteObject(Config.s3.buckets.cdn, cdnKey);
		if (Config.bunny.purgeEnabled) {
			const cdnUrl = makeAttachmentCdnUrl(message.channelId, attachment.id, attachment.filename);
			await this.purgeQueue.addUrls([cdnUrl]);
		}
		const updatedAttachments = message.attachments.filter((a: Attachment) => a.id !== attachmentId);
		const updatedRowData = {
			...message.toRow(),
			edited_timestamp: new Date(),
			attachments:
				updatedAttachments.length > 0 ? updatedAttachments.map((a: Attachment) => a.toMessageAttachment()) : null,
		};
		const updatedMessage = await this.channelRepository.messages.upsertMessage(updatedRowData, message.toRow());
		await this.messageInteractionService.dispatchMessageUpdate({channel, message: updatedMessage, requestCache});
	}

	async purgeChannelAttachments(channel: Channel): Promise<void> {
		const batchSize = 100;
		let beforeMessageId: MessageID | undefined;
		while (true) {
			const messages = await this.channelRepository.messages.listMessages(channel.id, beforeMessageId, batchSize);
			if (messages.length === 0) {
				return;
			}
			await Promise.all(
				messages.map((message: Message) =>
					purgeMessageAttachmentsHelper(message, this.storageService, this.purgeQueue),
				),
			);
			if (messages.length < batchSize) {
				return;
			}
			beforeMessageId = messages[messages.length - 1].id;
		}
	}

	private async getUploadPermissionAndLimit({userId, channelId}: {userId: UserID; channelId: ChannelID}): Promise<{
		maxFileSize: number;
	}> {
		const {channel, guild, checkPermission, member} =
			await this.messageInteractionService.authService.getChannelAuthenticated({
				userId,
				channelId,
			});
		if (!TEXT_BASED_CHANNEL_TYPES.has(channel.type)) {
			throw new CannotSendMessageToNonTextChannelError();
		}
		if (guild) {
			await checkPermission(Permissions.SEND_MESSAGES | Permissions.ATTACH_FILES);
			assertGuildMemberCanCommunicate(member);
		}
		const user = await this.userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}
		const fallbackMaxSize = ATTACHMENT_MAX_SIZE_NON_PREMIUM;
		const ctx = createLimitMatchContext({user, guildFeatures: guild?.features ?? null});
		const resolvedMaxFileSize = Math.floor(
			resolveLimitSafe(this.limitConfigService.getConfigSnapshot(), ctx, 'max_attachment_file_size', fallbackMaxSize),
		);
		const maxFileSize = user.isBot ? Math.min(resolvedMaxFileSize, ATTACHMENT_MAX_SIZE_BOT) : resolvedMaxFileSize;
		return {maxFileSize};
	}
}

async function mapWithConcurrency<T, TResult>(
	items: ReadonlyArray<T>,
	concurrency: number,
	mapper: (item: T, index: number) => Promise<TResult>,
): Promise<Array<TResult>> {
	const results = new Array<TResult>(items.length);
	let nextIndex = 0;
	async function worker(): Promise<void> {
		for (;;) {
			const index = nextIndex++;
			if (index >= items.length) return;
			results[index] = await mapper(items[index]!, index);
		}
	}
	await Promise.all(Array.from({length: Math.min(concurrency, items.length)}, () => worker()));
	return results;
}

async function runAttachmentStorageOperation<T>(operation: () => Promise<T>): Promise<T> {
	try {
		return await operation();
	} catch (error) {
		throw new ServiceUnavailableError({
			message: 'Attachment storage is temporarily unavailable',
			cause: error instanceof Error ? error : undefined,
		});
	}
}
