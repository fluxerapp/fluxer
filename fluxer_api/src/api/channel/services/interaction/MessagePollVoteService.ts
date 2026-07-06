// SPDX-License-Identifier: AGPL-3.0-or-later

import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {GuildOperations} from '@fluxer/constants/src/GuildConstants';
import {MAX_ATTACHMENTS_PER_MESSAGE, MAX_POLL_OPTIONS} from '@fluxer/constants/src/LimitConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {UnknownMessageError} from '@fluxer/errors/src/domains/channel/UnknownMessageError';
import {FeatureTemporarilyDisabledError} from '@fluxer/errors/src/domains/core/FeatureTemporarilyDisabledError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {MissingPermissionsError} from '@fluxer/errors/src/domains/core/MissingPermissionsError';
import type {PollCustomOptionRequest, PollVoteRequest} from '@fluxer/schema/src/domains/message/PollSchemas';
import {snowflakeToDate} from '@fluxer/snowflake/src/Snowflake';
import {createMessageID, type MessageID, type UserID} from '../../../BrandedTypes';
import type {MessagePoll, MessagePollOption, MessagePollVoteRow} from '../../../database/types/MessageTypes';
import type {GuildAuditLogService} from '../../../guild/GuildAuditLogService';
import {contentModerationService} from '../../../infrastructure/ContentModerationService';
import type {IGatewayService} from '../../../infrastructure/IGatewayService';
import type {ISnowflakeService} from '../../../infrastructure/ISnowflakeService';
import type {Message} from '../../../models/Message';
import {assertGuildMemberCanCommunicate} from '../../../utils/GuildCommunicationUtils';
import type {AttachmentToProcess} from '../../AttachmentDTOs';
import type {IChannelRepositoryAggregate} from '../../repositories/IChannelRepositoryAggregate';
import type {AuthenticatedChannel} from '../AuthenticatedChannel';
import {dispatchMessageUpdateBroadcast} from '../message/MessageGatewayDispatch';
import {getContentType} from '../message/MessageHelpers';
import type {MessagePersistenceService} from '../message/MessagePersistenceService';
import type {MessageSearchService} from '../message/MessageSearchService';
import {MessageInteractionBase} from './MessageInteractionBase';

export class MessagePollVoteService extends MessageInteractionBase {
	constructor(
		gatewayService: IGatewayService,
		private channelRepository: IChannelRepositoryAggregate,
		private snowflakeService: ISnowflakeService,
		private messagePersistenceService: MessagePersistenceService,
		private searchService: MessageSearchService,
		private guildAuditLogService: GuildAuditLogService,
	) {
		super(gatewayService);
	}

	private async assertMessageHistoryAccess({
		authChannel,
		messageId,
	}: {
		authChannel: AuthenticatedChannel;
		messageId: MessageID;
	}): Promise<void> {
		if (!authChannel.guild) {
			return;
		}
		if (await authChannel.hasPermission(Permissions.READ_MESSAGE_HISTORY)) {
			return;
		}
		const cutoff = authChannel.guild.message_history_cutoff;
		if (!cutoff || snowflakeToDate(messageId).getTime() < new Date(cutoff).getTime()) {
			throw new UnknownMessageError();
		}
	}

	async vote({
		authChannel,
		messageId,
		userId,
		data,
	}: {
		authChannel: AuthenticatedChannel;
		messageId: MessageID;
		userId: UserID;
		data: PollVoteRequest;
	}): Promise<void> {
		const {channel} = authChannel;
		this.ensureTextChannel(channel);
		assertGuildMemberCanCommunicate(authChannel.member);
		await this.assertMessageHistoryAccess({authChannel, messageId});
		const message = await this.channelRepository.messages.getMessage(channel.id, messageId);
		if (!message) throw new UnknownMessageError();
		const poll = this.getOpenPoll(message);
		const optionIds = data.option_ids.map((optionId) => createMessageID(optionId));
		this.validateVoteOptions(poll, optionIds);
		await this.channelRepository.messageInteractions.upsertPollVote(channel.id, messageId, userId, optionIds);
		const updatedMessage = await this.updatePollVoteCounts({message, excludedVote: null});
		await dispatchMessageUpdateBroadcast({
			gatewayService: this.gatewayService,
			channel,
			message: updatedMessage,
		});
	}

	async addCustomOption({
		authChannel,
		messageId,
		userId,
		data,
	}: {
		authChannel: AuthenticatedChannel;
		messageId: MessageID;
		userId: UserID;
		data: PollCustomOptionRequest;
	}): Promise<void> {
		const {channel, guild} = authChannel;
		const attachmentToProcess = this.getCustomOptionAttachment(data);
		this.ensureTextChannel(channel);
		await authChannel.checkPermission(
			attachmentToProcess ? Permissions.SEND_MESSAGES | Permissions.ATTACH_FILES : Permissions.SEND_MESSAGES,
		);
		assertGuildMemberCanCommunicate(authChannel.member);
		await this.assertMessageHistoryAccess({authChannel, messageId});
		if (this.isOperationDisabled(guild, GuildOperations.SEND_MESSAGE)) {
			throw new FeatureTemporarilyDisabledError();
		}
		const message = await this.channelRepository.messages.getMessage(channel.id, messageId);
		if (!message) throw new UnknownMessageError();
		const poll = this.getOpenPoll(message);
		if (!poll.allow_custom_answers || poll.options.length >= MAX_POLL_OPTIONS) {
			throw this.invalidPoll();
		}
		if (attachmentToProcess && message.attachments.length >= MAX_ATTACHMENTS_PER_MESSAGE) {
			throw InputValidationError.fromCode('attachments', ValidationErrorCodes.TOO_MANY_FILES, {
				maxFiles: MAX_ATTACHMENTS_PER_MESSAGE,
			});
		}
		this.validateNewOptionText(poll, data.text);
		contentModerationService.scanText(data.text, {
			userId,
			guildId: channel.guildId,
			channelId: channel.id,
			messageId,
			surface: 'message_content',
		});
		const processedAttachments = attachmentToProcess
			? await this.messagePersistenceService.processAdditionalAttachments({
					message,
					attachments: [attachmentToProcess],
					channel,
					guild,
					member: authChannel.member,
				})
			: [];
		const optionAttachment = processedAttachments[0] ?? null;
		const option: MessagePollOption = {
			option_id: createMessageID(await this.snowflakeService.generateForChannel(channel.id)),
			text: data.text,
			attachment_id: optionAttachment?.attachment_id ?? null,
			vote_count: 0,
		};
		const updatedPoll: MessagePoll = {
			...poll,
			options: [...poll.options, option],
		};
		const updatedAttachments = [
			...message.attachments.map((attachment) => attachment.toMessageAttachment()),
			...processedAttachments,
		];
		const updatedMessage = await this.channelRepository.messages.upsertMessage(
			{
				...message.toRow(),
				attachments: updatedAttachments.length > 0 ? updatedAttachments : null,
				poll: updatedPoll,
			},
			message.toRow(),
		);
		if (channel.guildId) {
			await this.guildAuditLogService
				.createBuilder(channel.guildId, userId)
				.withAction(AuditLogActionType.POLL_OPTION_CREATE, option.option_id.toString())
				.withMetadata({
					channel_id: channel.id.toString(),
					message_id: messageId.toString(),
					poll_id: poll.poll_id.toString(),
					option_id: option.option_id.toString(),
					...(optionAttachment ? {attachment_id: optionAttachment.attachment_id.toString()} : {}),
				})
				.withReason(null)
				.commit();
		}
		void this.searchService.updateMessageIndex(updatedMessage);
		await dispatchMessageUpdateBroadcast({
			gatewayService: this.gatewayService,
			channel,
			message: updatedMessage,
		});
	}

	async removeOwnVote({
		authChannel,
		messageId,
		userId,
	}: {
		authChannel: AuthenticatedChannel;
		messageId: MessageID;
		userId: UserID;
	}): Promise<void> {
		const {channel} = authChannel;
		this.ensureTextChannel(channel);
		await this.assertMessageHistoryAccess({authChannel, messageId});
		const message = await this.channelRepository.messages.getMessage(channel.id, messageId);
		if (!message) throw new UnknownMessageError();
		this.getOpenPoll(message);
		const existingVote = await this.channelRepository.messageInteractions.getPollVote(channel.id, messageId, userId);
		if (!existingVote) {
			return;
		}
		await this.channelRepository.messageInteractions.removePollVote(channel.id, messageId, userId);
		const updatedMessage = await this.updatePollVoteCounts({
			message,
			excludedVote: existingVote,
		});
		await dispatchMessageUpdateBroadcast({
			gatewayService: this.gatewayService,
			channel,
			message: updatedMessage,
		});
	}

	async closePoll({
		authChannel,
		messageId,
		userId,
	}: {
		authChannel: AuthenticatedChannel;
		messageId: MessageID;
		userId: UserID;
	}): Promise<void> {
		const {channel, hasPermission} = authChannel;
		this.ensureTextChannel(channel);
		await this.assertMessageHistoryAccess({authChannel, messageId});
		const message = await this.channelRepository.messages.getMessage(channel.id, messageId);
		if (!message) throw new UnknownMessageError();
		if (!message.poll) {
			throw this.invalidPoll();
		}
		const canClose =
			message.authorId === userId || (channel.guildId && (await hasPermission(Permissions.MANAGE_MESSAGES)));
		if (!canClose) {
			throw new MissingPermissionsError();
		}
		if (message.poll.closed_at) {
			return;
		}
		const updatedPoll: MessagePoll = {
			...message.poll,
			closed_at: new Date(),
		};
		const updatedMessageData = {
			...message.toRow(),
			poll: updatedPoll,
		};
		const updatedMessage = await this.channelRepository.messages.upsertMessage(updatedMessageData, message.toRow());
		if (channel.guildId) {
			await this.guildAuditLogService
				.createBuilder(channel.guildId, userId)
				.withAction(AuditLogActionType.POLL_CLOSE, message.poll.poll_id.toString())
				.withMetadata({
					channel_id: channel.id.toString(),
					message_id: messageId.toString(),
					poll_id: message.poll.poll_id.toString(),
				})
				.withReason(null)
				.commit();
		}
		await dispatchMessageUpdateBroadcast({
			gatewayService: this.gatewayService,
			channel,
			message: updatedMessage,
		});
	}

	private getOpenPoll(message: Message): MessagePoll {
		if (!message.poll) {
			throw this.invalidPoll();
		}
		if (message.poll.closed_at || message.poll.expires_at.getTime() <= Date.now()) {
			throw this.invalidPoll();
		}
		return message.poll;
	}

	private validateNewOptionText(poll: MessagePoll, text: string): void {
		const normalized = text.toLowerCase();
		if (poll.options.some((option) => option.text.toLowerCase() === normalized)) {
			throw this.invalidPoll();
		}
	}

	private getCustomOptionAttachment(data: PollCustomOptionRequest): AttachmentToProcess | null {
		const attachment = data.attachments?.[0];
		if (!attachment) {
			return null;
		}
		if (!('upload_filename' in attachment)) {
			throw InputValidationError.fromCode('attachments', ValidationErrorCodes.NO_FILE_FOR_ATTACHMENT, {
				attachmentId: attachment.id,
			});
		}
		const contentType = attachment.content_type ?? getContentType(attachment.filename);
		if (!contentType.toLowerCase().startsWith('image/')) {
			throw InputValidationError.fromCode('attachments', ValidationErrorCodes.ATTACHMENT_MUST_BE_IMAGE, {
				filename: attachment.filename,
			});
		}
		return {...attachment, content_type: contentType} as AttachmentToProcess;
	}

	private validateVoteOptions(poll: MessagePoll, optionIds: Array<MessageID>): void {
		if (!poll.allow_ranked_choice && optionIds.length !== 1) {
			throw this.invalidPoll();
		}
		const optionIdSet = new Set(poll.options.map((option) => option.option_id.toString()));
		for (const optionId of optionIds) {
			if (!optionIdSet.has(optionId.toString())) {
				throw this.invalidPoll();
			}
		}
	}

	private async updatePollVoteCounts({
		message,
		excludedVote,
	}: {
		message: Message;
		excludedVote: MessagePollVoteRow | null;
	}): Promise<Message> {
		if (!message.poll) {
			throw this.invalidPoll();
		}
		const votes = await this.channelRepository.messageInteractions.listPollVotes(message.channelId, message.id);
		const votesForCount = excludedVote ? votes.filter((vote) => !this.isExcludedVote(vote, excludedVote)) : votes;
		const voteCountByOption = new Map<string, number>();
		for (const vote of votesForCount) {
			const countedOptionId = vote.option_ids[0];
			if (!countedOptionId) {
				continue;
			}
			const optionKey = countedOptionId.toString();
			voteCountByOption.set(optionKey, (voteCountByOption.get(optionKey) ?? 0) + 1);
		}
		const updatedPoll: MessagePoll = {
			...message.poll,
			options: message.poll.options.map((option) => ({
				...option,
				vote_count: voteCountByOption.get(option.option_id.toString()) ?? 0,
			})),
		};
		const updatedMessageData = {
			...message.toRow(),
			poll: updatedPoll,
		};
		return this.channelRepository.messages.upsertMessage(updatedMessageData, message.toRow());
	}

	private isExcludedVote(vote: MessagePollVoteRow, excludedVote: MessagePollVoteRow): boolean {
		return vote.user_id === excludedVote.user_id;
	}

	private invalidPoll(): InputValidationError {
		return InputValidationError.fromCode('poll', ValidationErrorCodes.INVALID_MESSAGE_DATA);
	}
}
