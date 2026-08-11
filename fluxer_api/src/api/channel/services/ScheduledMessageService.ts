// SPDX-License-Identifier: AGPL-3.0-or-later

import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {FeatureTemporarilyDisabledError} from '@fluxer/errors/src/domains/core/FeatureTemporarilyDisabledError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import type {IWorkerService} from '@pkgs/worker/src/contracts/IWorkerService';
import type {WorkerJobPayload} from '@pkgs/worker/src/contracts/WorkerTypes';

import {DateTime, IANAZone} from 'luxon';
import type {ChannelID, MessageID, UserID} from '../../BrandedTypes';
import {createMessageID} from '../../BrandedTypes';
import type {ISnowflakeService} from '../../infrastructure/ISnowflakeService';
import {MAX_JOB_SCHEDULE_DELAY_MS, WORKER_QUEUE_MAX_AGE_MS} from '../../jobs/JobSchedulingPolicy';
import type {ScheduledMessagePayload} from '../../models/ScheduledMessage';
import {ScheduledMessage} from '../../models/ScheduledMessage';
import type {User} from '../../models/User';
import type {ScheduledMessageRepository} from '../../user/repositories/ScheduledMessageRepository';
import type {WorkerTaskName} from '../../worker/WorkerLaneConfig';
import type {MessageRequest} from '../MessageTypes';
import type {ChannelService} from './ChannelService';

export const SCHEDULED_MESSAGE_TTL_SECONDS = WORKER_QUEUE_MAX_AGE_MS / 1000;
const DEFAULT_TIMEZONE = 'UTC';
const WORKER_TASK_NAME = 'sendScheduledMessage';

interface ScheduleParams {
	user: User;
	channelId: ChannelID;
	data: MessageRequest;
	scheduledLocalAt: string;
	timezone?: string;
}

interface UpdateScheduleParams extends ScheduleParams {
	scheduledMessageId: MessageID;
	existing?: ScheduledMessage;
}

interface SendScheduledMessageWorkerPayload extends WorkerJobPayload {
	userId: string;
	scheduledMessageId: string;
	expectedScheduledAt: string;
}

export class ScheduledMessageService {
	constructor(
		private readonly channelService: ChannelService,
		private readonly scheduledMessageRepository: ScheduledMessageRepository,
		private readonly workerService: IWorkerService<WorkerTaskName>,
		private readonly snowflakeService: ISnowflakeService,
	) {}

	async listScheduledMessages(userId: UserID): Promise<Array<ScheduledMessage>> {
		return await this.scheduledMessageRepository.listScheduledMessages(userId);
	}

	async getScheduledMessage(userId: UserID, scheduledMessageId: MessageID): Promise<ScheduledMessage | null> {
		return await this.scheduledMessageRepository.getScheduledMessage(userId, scheduledMessageId);
	}

	async createScheduledMessage(params: ScheduleParams): Promise<ScheduledMessage> {
		return await this.upsertScheduledMessage({
			...params,
			scheduledMessageId: await createSnowflake(this.snowflakeService),
		});
	}

	async updateScheduledMessage(params: UpdateScheduleParams): Promise<ScheduledMessage> {
		return await this.upsertScheduledMessage({
			...params,
			existing:
				params.existing ?? (await this.getScheduledMessage(params.user.id, params.scheduledMessageId)) ?? undefined,
		});
	}

	async cancelScheduledMessage(userId: UserID, scheduledMessageId: MessageID): Promise<void> {
		await this.scheduledMessageRepository.deleteScheduledMessage(userId, scheduledMessageId);
	}

	async markInvalid(userId: UserID, scheduledMessageId: MessageID, reason: string): Promise<void> {
		await this.scheduledMessageRepository.markInvalid(
			userId,
			scheduledMessageId,
			reason,
			SCHEDULED_MESSAGE_TTL_SECONDS,
		);
	}

	private async upsertScheduledMessage(params: UpdateScheduleParams): Promise<ScheduledMessage> {
		const {user, channelId, data, scheduledLocalAt, timezone} = params;
		if ((user.flags & UserFlags.STAFF) === 0n) {
			throw new FeatureTemporarilyDisabledError();
		}
		await this.channelService.messages.send.validateMessageCanBeSent({
			user,
			channelId,
			data,
		});
		const scheduledAt = this.resolveScheduledAt(scheduledLocalAt, timezone);
		const payload = toScheduledPayload(data);
		const existing = params.existing;
		const message = new ScheduledMessage({
			userId: user.id,
			id: params.scheduledMessageId,
			channelId,
			scheduledAt,
			scheduledLocalAt,
			timezone: timezone ?? DEFAULT_TIMEZONE,
			payload,
			status: 'pending',
			statusReason: null,
			createdAt: existing?.createdAt,
			invalidatedAt: null,
		});
		await this.scheduledMessageRepository.upsertScheduledMessage(message, SCHEDULED_MESSAGE_TTL_SECONDS);
		await this.scheduleWorker(message);
		return message;
	}

	private resolveScheduledAt(local: string, timezone?: string): Date {
		const zone = timezone?.trim() || DEFAULT_TIMEZONE;
		if (!IANAZone.isValidZone(zone)) {
			throw InputValidationError.fromCode('timezone', ValidationErrorCodes.INVALID_TIMEZONE_IDENTIFIER);
		}
		const dt = DateTime.fromISO(local, {zone});
		if (!dt.isValid) {
			throw InputValidationError.fromCode(
				'scheduled_local_at',
				ValidationErrorCodes.INVALID_DATETIME_FOR_SCHEDULED_SEND,
			);
		}
		const scheduledAt = dt.toJSDate();
		const now = Date.now();
		const diffMs = scheduledAt.getTime() - now;
		if (diffMs <= 0) {
			throw InputValidationError.fromCode('scheduled_local_at', ValidationErrorCodes.SCHEDULED_TIME_MUST_BE_FUTURE);
		}
		if (diffMs > MAX_JOB_SCHEDULE_DELAY_MS) {
			throw InputValidationError.fromCode('scheduled_local_at', ValidationErrorCodes.SCHEDULED_MESSAGES_MAX_30_DAYS);
		}
		return scheduledAt;
	}

	private async scheduleWorker(message: ScheduledMessage): Promise<void> {
		const payload: SendScheduledMessageWorkerPayload = {
			userId: message.userId.toString(),
			scheduledMessageId: message.id.toString(),
			expectedScheduledAt: message.scheduledAt.toISOString(),
		};
		await this.workerService.addJob(WORKER_TASK_NAME, payload, {runAt: message.scheduledAt});
	}
}

async function createSnowflake(snowflakeService: ISnowflakeService): Promise<MessageID> {
	return createMessageID(await snowflakeService.generate());
}

function toScheduledPayload(data: MessageRequest): ScheduledMessagePayload {
	return {
		content: data.content ?? null,
		embeds: data.embeds,
		attachments: data.attachments,
		message_reference: data.message_reference
			? {
					message_id: data.message_reference.message_id.toString(),
					channel_id: data.message_reference.channel_id?.toString(),
					guild_id: data.message_reference.guild_id?.toString(),
					type: data.message_reference.type,
				}
			: undefined,
		allowed_mentions: data.allowed_mentions
			? {
					parse: data.allowed_mentions.parse,
					users: data.allowed_mentions.users?.map((id) => id.toString()),
					roles: data.allowed_mentions.roles?.map((id) => id.toString()),
					replied_user: data.allowed_mentions.replied_user,
				}
			: undefined,
		flags: data.flags,
		nonce: data.nonce,
		favorite_meme_id: data.favorite_meme_id?.toString(),
		sticker_ids: data.sticker_ids?.map((id) => id.toString()),
		tts: data.tts,
	};
}
