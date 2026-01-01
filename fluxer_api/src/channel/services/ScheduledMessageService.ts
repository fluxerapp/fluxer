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

import {DateTime, IANAZone} from 'luxon';
import type {ChannelID, MessageID, UserID} from '~/BrandedTypes';
import {createMessageID} from '~/BrandedTypes';
import type {MessageRequest} from '~/channel/ChannelModel';
import type {IChannelRepository} from '~/channel/IChannelRepository';
import {FeatureFlags} from '~/constants/FeatureFlags';
import {InputValidationError} from '~/Errors';
import {FeatureTemporarilyDisabledError} from '~/errors/FeatureTemporarilyDisabledError';
import type {FeatureFlagService} from '~/feature_flag/FeatureFlagService';
import type {SnowflakeService} from '~/infrastructure/SnowflakeService';
import type {User} from '~/Models';
import type {ScheduledMessagePayload} from '~/models/ScheduledMessage';
import {ScheduledMessage} from '~/models/ScheduledMessage';
import type {ScheduledMessageRepository} from '~/user/repositories/ScheduledMessageRepository';
import type {WorkerService} from '~/worker/WorkerService';
import type {ChannelService} from './ChannelService';

const MAX_SCHEDULE_DELAY_MS = 30 * 24 * 60 * 60 * 1000;
export const SCHEDULED_MESSAGE_TTL_SECONDS = 31 * 24 * 60 * 60;
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

interface SendScheduledMessageWorkerPayload {
	userId: string;
	scheduledMessageId: string;
	expectedScheduledAt: string;
}

export class ScheduledMessageService {
	constructor(
		private readonly channelService: ChannelService,
		private readonly scheduledMessageRepository: ScheduledMessageRepository,
		private readonly workerService: WorkerService,
		private readonly snowflakeService: SnowflakeService,
		private readonly channelRepository?: IChannelRepository,
		private readonly featureFlagService?: FeatureFlagService,
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
			scheduledMessageId: createSnowflake(this.snowflakeService),
		});
	}

	async updateScheduledMessage(params: UpdateScheduleParams): Promise<ScheduledMessage> {
		return await this.upsertScheduledMessage({
			...params,
			existing:
				params.existing ??
				(await this.getScheduledMessage(params.user.id as UserID, params.scheduledMessageId)) ??
				undefined,
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

		if (this.featureFlagService && this.channelRepository) {
			const channel = await this.channelRepository.findUnique(channelId);
			if (channel?.guildId) {
				const guildId = channel.guildId.toString();
				if (!this.featureFlagService.isFeatureEnabled(FeatureFlags.MESSAGE_SCHEDULING, guildId)) {
					throw new FeatureTemporarilyDisabledError();
				}
			}
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
			throw InputValidationError.create('timezone', 'Invalid timezone identifier');
		}

		const dt = DateTime.fromISO(local, {zone});
		if (!dt.isValid) {
			throw InputValidationError.create('scheduled_local_at', 'Invalid date/time for scheduled send');
		}

		const scheduledAt = dt.toJSDate();
		const now = Date.now();

		const diffMs = scheduledAt.getTime() - now;
		if (diffMs <= 0) {
			throw InputValidationError.create('scheduled_local_at', 'Scheduled time must be in the future');
		}

		if (diffMs > MAX_SCHEDULE_DELAY_MS) {
			throw InputValidationError.create(
				'scheduled_local_at',
				'Scheduled messages can be at most 30 days in the future',
			);
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

function createSnowflake(snowflakeService: SnowflakeService): MessageID {
	return createMessageID(snowflakeService.generate());
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
