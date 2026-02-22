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

import type {ChannelID, MessageID, UserID} from '@fluxer/api/src/BrandedTypes';
import {createMessageID} from '@fluxer/api/src/BrandedTypes';
import type {MessageRequest} from '@fluxer/api/src/channel/MessageTypes';
import type {ChannelService} from '@fluxer/api/src/channel/services/ChannelService';
import type {SnowflakeService} from '@fluxer/api/src/infrastructure/SnowflakeService';
import type {ScheduledMessagePayload} from '@fluxer/api/src/models/ScheduledMessage';
import {ScheduledMessage} from '@fluxer/api/src/models/ScheduledMessage';
import type {User} from '@fluxer/api/src/models/User';
import {withBusinessSpan} from '@fluxer/api/src/telemetry/BusinessSpans';
import type {ScheduledMessageRepository} from '@fluxer/api/src/user/repositories/ScheduledMessageRepository';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {FeatureTemporarilyDisabledError} from '@fluxer/errors/src/domains/core/FeatureTemporarilyDisabledError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {recordCounter} from '@fluxer/telemetry/src/Metrics';
import type {IWorkerService} from '@fluxer/worker/src/contracts/IWorkerService';
import type {WorkerJobPayload} from '@fluxer/worker/src/contracts/WorkerTypes';
import {ms, seconds} from 'itty-time';
import {DateTime, IANAZone} from 'luxon';

export const SCHEDULED_MESSAGE_TTL_SECONDS = seconds('31 days');

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
		private readonly workerService: IWorkerService,
		private readonly snowflakeService: SnowflakeService,
	) {}

	async listScheduledMessages(userId: UserID): Promise<Array<ScheduledMessage>> {
		return await this.scheduledMessageRepository.listScheduledMessages(userId);
	}

	async getScheduledMessage(userId: UserID, scheduledMessageId: MessageID): Promise<ScheduledMessage | null> {
		return await this.scheduledMessageRepository.getScheduledMessage(userId, scheduledMessageId);
	}

	async createScheduledMessage(params: ScheduleParams): Promise<ScheduledMessage> {
		return await withBusinessSpan(
			'fluxer.message.schedule',
			'fluxer.messages.scheduled',
			{
				channel_id: params.channelId.toString(),
				scheduled_for: params.scheduledLocalAt,
				has_attachments: String(!!params.data.attachments && params.data.attachments.length > 0),
			},
			async () =>
				this.upsertScheduledMessage({
					...params,
					scheduledMessageId: await createSnowflake(this.snowflakeService),
				}),
		);
	}

	async updateScheduledMessage(params: UpdateScheduleParams): Promise<ScheduledMessage> {
		return await withBusinessSpan(
			'fluxer.message.schedule.update',
			'fluxer.messages.scheduled_updated',
			{
				channel_id: params.channelId.toString(),
				scheduled_message_id: params.scheduledMessageId.toString(),
			},
			async () =>
				this.upsertScheduledMessage({
					...params,
					existing:
						params.existing ??
						(await this.getScheduledMessage(params.user.id as UserID, params.scheduledMessageId)) ??
						undefined,
				}),
		);
	}

	async cancelScheduledMessage(userId: UserID, scheduledMessageId: MessageID): Promise<void> {
		await this.scheduledMessageRepository.deleteScheduledMessage(userId, scheduledMessageId);
		recordCounter({
			name: 'fluxer.messages.scheduled_deleted',
			dimensions: {scheduled_message_id: scheduledMessageId.toString()},
			value: 1,
		});
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

		await this.channelService.messages.validateMessageCanBeSent({
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

		if (diffMs > ms('30 days')) {
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

async function createSnowflake(snowflakeService: SnowflakeService): Promise<MessageID> {
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
