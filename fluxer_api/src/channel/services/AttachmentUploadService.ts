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

import type {AttachmentID, ChannelID, MessageID, UserID} from '~/BrandedTypes';
import {Config} from '~/Config';
import {GuildOperations, Permissions} from '~/Constants';
import type {UploadedAttachment} from '~/channel/AttachmentDTOs';
import {
	FeatureTemporarilyDisabledError,
	MissingPermissionsError,
	UnknownMessageError,
	UnknownUserError,
} from '~/Errors';
import type {ICloudflarePurgeQueue} from '~/infrastructure/CloudflarePurgeQueue';
import type {IStorageService} from '~/infrastructure/IStorageService';
import type {Attachment, Channel, Message} from '~/Models';
import type {RequestCache} from '~/middleware/RequestCacheMiddleware';
import type {IUserRepository} from '~/user/IUserRepository';
import type {IChannelRepositoryAggregate} from '../repositories/IChannelRepositoryAggregate';
import type {AuthenticatedChannel} from './AuthenticatedChannel';
import {
	getContentType,
	isMessageEmpty,
	isOperationDisabled,
	makeAttachmentCdnKey,
	makeAttachmentCdnUrl,
	purgeMessageAttachments as purgeMessageAttachmentsHelper,
	validateAttachmentSizes,
} from './message/MessageHelpers';

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
	files: Array<{file: File; index: number}>;
	attachmentMetadata: Array<{id: number; filename: string}>;
	expiresAt?: Date;
}

export class AttachmentUploadService {
	constructor(
		private channelRepository: IChannelRepositoryAggregate,
		private userRepository: IUserRepository,
		private storageService: IStorageService,
		private cloudflarePurgeQueue: ICloudflarePurgeQueue,
		private getChannelAuthenticated: (params: {userId: UserID; channelId: ChannelID}) => Promise<AuthenticatedChannel>,
		private ensureTextChannel: (channel: Channel) => void,
		private dispatchMessageUpdate: (params: {
			channel: Channel;
			message: Message;
			requestCache: RequestCache;
			currentUserId?: UserID;
		}) => Promise<void>,
		private deleteMessage: (params: {
			userId: UserID;
			channelId: ChannelID;
			messageId: MessageID;
			requestCache: RequestCache;
		}) => Promise<void>,
	) {}

	async uploadFormDataAttachments({
		userId,
		channelId,
		files,
		attachmentMetadata,
		expiresAt,
	}: UploadFormDataAttachmentsParams): Promise<Array<UploadedAttachment>> {
		const {channel, guild, checkPermission} = await this.getChannelAuthenticated({userId, channelId});
		this.ensureTextChannel(channel);

		if (guild) {
			await checkPermission(Permissions.SEND_MESSAGES | Permissions.ATTACH_FILES);
		}

		const user = await this.userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}

		validateAttachmentSizes(
			files.map((fileWithIndex) => ({size: fileWithIndex.file.size})),
			user,
		);

		const metadataMap = new Map(attachmentMetadata.map((m) => [m.id, m]));

		const uploadedAttachments = await Promise.all(
			files.map(async (fileWithIndex) => {
				const {file, index} = fileWithIndex;

				const metadata = metadataMap.get(index);
				if (!metadata) {
					throw new Error(`Internal error: metadata not found for file index ${index}`);
				}

				const filename = metadata.filename;
				const uploadKey = crypto.randomUUID();

				const arrayBuffer = await file.arrayBuffer();
				const body = new Uint8Array(arrayBuffer);

				const contentType = this.resolveContentType(file, filename);

				await this.storageService.uploadObject({
					bucket: Config.s3.buckets.uploads,
					key: uploadKey,
					body,
					contentType,
					expiresAt: expiresAt ?? undefined,
				});

				const uploaded: UploadedAttachment = {
					id: index,
					upload_filename: uploadKey,
					filename: filename,
					file_size: file.size,
					content_type: contentType,
				};

				return uploaded;
			}),
		);

		return uploadedAttachments;
	}

	async deleteAttachment({
		userId,
		channelId,
		messageId,
		attachmentId,
		requestCache,
	}: DeleteAttachmentParams): Promise<void> {
		const {channel, guild} = await this.getChannelAuthenticated({userId, channelId});

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
			await this.deleteMessage({
				userId,
				channelId,
				messageId,
				requestCache,
			});
			return;
		}

		const cdnKey = makeAttachmentCdnKey(message.channelId, attachment.id, attachment.filename);
		await this.storageService.deleteObject(Config.s3.buckets.cdn, cdnKey);

		if (Config.cloudflare.purgeEnabled) {
			const cdnUrl = makeAttachmentCdnUrl(message.channelId, attachment.id, attachment.filename);
			await this.cloudflarePurgeQueue.addUrls([cdnUrl]);
		}

		const updatedAttachments = message.attachments.filter((a: Attachment) => a.id !== attachmentId);
		const updatedRowData = {
			...message.toRow(),
			attachments:
				updatedAttachments.length > 0 ? updatedAttachments.map((a: Attachment) => a.toMessageAttachment()) : null,
		};

		const updatedMessage = await this.channelRepository.messages.upsertMessage(updatedRowData, message.toRow());
		await this.dispatchMessageUpdate({channel, message: updatedMessage, requestCache});
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
					purgeMessageAttachmentsHelper(message, this.storageService, this.cloudflarePurgeQueue),
				),
			);

			if (messages.length < batchSize) {
				return;
			}

			beforeMessageId = messages[messages.length - 1].id;
		}
	}

	private resolveContentType(_file: File, normalizedFilename: string): string {
		return getContentType(normalizedFilename);
	}
}
