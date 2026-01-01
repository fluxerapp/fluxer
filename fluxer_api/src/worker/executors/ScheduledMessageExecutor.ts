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

import type {MessageID, UserID} from '~/BrandedTypes';
import {createMessageID, createUserID} from '~/BrandedTypes';
import {SCHEDULED_MESSAGE_TTL_SECONDS} from '~/channel/services/ScheduledMessageService';
import {createRequestCache} from '~/middleware/RequestCacheMiddleware';
import {ScheduledMessageRepository} from '~/user/repositories/ScheduledMessageRepository';
import type {WorkerDependencies} from '../WorkerDependencies';

export interface WorkerLogger {
	debug(message: string, extra?: object): void;
	info(message: string, extra?: object): void;
	warn(message: string, extra?: object): void;
	error(message: string, extra?: object): void;
}

export interface SendScheduledMessageParams {
	userId: string;
	scheduledMessageId: string;
	expectedScheduledAt: string;
}

export class ScheduledMessageExecutor {
	constructor(
		private readonly deps: WorkerDependencies,
		private readonly logger: WorkerLogger,
		private readonly scheduledMessageRepository: ScheduledMessageRepository = new ScheduledMessageRepository(),
	) {}

	async execute(params: SendScheduledMessageParams): Promise<void> {
		const userId = this.parseUserID(params.userId);
		const scheduledMessageId = this.parseMessageID(params.scheduledMessageId);

		if (!userId || !scheduledMessageId) {
			this.logger.warn('Malformed scheduled message job payload', {payload: params});
			return;
		}

		const expectedScheduledAt = this.parseScheduledAt(params.expectedScheduledAt);
		if (!expectedScheduledAt) {
			this.logger.warn('Invalid expectedScheduledAt for scheduled message job', {payload: params});
			return;
		}

		const scheduledMessage = await this.scheduledMessageRepository.getScheduledMessage(userId, scheduledMessageId);
		if (!scheduledMessage) {
			this.logger.info('Scheduled message not found, skipping', {userId, scheduledMessageId});
			return;
		}

		if (scheduledMessage.status !== 'pending') {
			this.logger.info('Scheduled message already processed', {
				scheduledMessageId,
				status: scheduledMessage.status,
			});
			return;
		}

		if (scheduledMessage.scheduledAt.toISOString() !== expectedScheduledAt.toISOString()) {
			this.logger.info('Scheduled message time mismatch, skipping stale job', {
				scheduledMessageId,
				expected: expectedScheduledAt.toISOString(),
				actual: scheduledMessage.scheduledAt.toISOString(),
			});
			return;
		}

		const user = await this.deps.userRepository.findUnique(userId);
		if (!user) {
			await this.markInvalid(userId, scheduledMessageId, 'User not found');
			return;
		}

		const messageRequest = scheduledMessage.parseToMessageRequest();
		const requestCache = createRequestCache();

		try {
			await this.deps.channelService.messages.validateMessageCanBeSent({
				user,
				channelId: scheduledMessage.channelId,
				data: messageRequest,
			});

			await this.deps.channelService.messages.sendMessage({
				user,
				channelId: scheduledMessage.channelId,
				data: messageRequest,
				requestCache,
			});

			await this.scheduledMessageRepository.deleteScheduledMessage(userId, scheduledMessageId);
			this.logger.info('Scheduled message sent successfully', {scheduledMessageId, userId});
		} catch (error) {
			const reason = error instanceof Error ? error.message : 'Failed to send scheduled message';
			this.logger.warn('Marking scheduled message invalid', {scheduledMessageId, userId, reason});
			await this.markInvalid(userId, scheduledMessageId, reason);
		} finally {
			requestCache.clear();
		}
	}

	private async markInvalid(userId: UserID, scheduledMessageId: MessageID, reason: string): Promise<void> {
		try {
			await this.scheduledMessageRepository.markInvalid(
				userId,
				scheduledMessageId,
				reason,
				SCHEDULED_MESSAGE_TTL_SECONDS,
			);
		} catch (error) {
			this.logger.error('Failed to mark scheduled message invalid', {error, scheduledMessageId});
		}
	}

	private parseUserID(value: string): UserID | null {
		try {
			return createUserID(BigInt(value));
		} catch {
			return null;
		}
	}

	private parseMessageID(value: string): MessageID | null {
		try {
			return createMessageID(BigInt(value));
		} catch {
			return null;
		}
	}

	private parseScheduledAt(value: string): Date | null {
		const date = new Date(value);
		return Number.isNaN(date.getTime()) ? null : date;
	}
}
