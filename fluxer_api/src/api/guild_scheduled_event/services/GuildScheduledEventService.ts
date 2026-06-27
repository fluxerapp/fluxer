// SPDX-License-Identifier: AGPL-3.0-or-later

import {GuildScheduledEventStatus} from '@fluxer/constants/src/GuildScheduledEventConstants';
import type {ChannelID, GuildID, GuildScheduledEventID, UserID} from '../../BrandedTypes';
import {createGuildScheduledEventID} from '../../BrandedTypes';
import type {ISnowflakeService} from '../../infrastructure/ISnowflakeService';
import type {IGatewayService} from '../../infrastructure/IGatewayService';
import {GuildScheduledEvent} from '../../models/GuildScheduledEvent';
import type {User} from '../../models/User';
import {GuildScheduledEventRepository} from '../repositories/GuildScheduledEventRepository';

interface CreateEventParams {
	user: User;
	guildId: GuildID;
	name: string;
	description?: string;
	scheduledStartTime: Date;
	scheduledEndTime?: Date | null;
	privacyLevel?: number;
	entityType: number;
	entityId?: ChannelID | null;
	channelId?: ChannelID | null;
	location?: string | null;
}

interface UpdateEventParams {
	user: User;
	eventId: GuildScheduledEventID;
	name?: string;
	description?: string | null;
	scheduledStartTime?: Date;
	scheduledEndTime?: Date | null;
	privacyLevel?: number;
	entityType?: number;
	entityId?: ChannelID | null;
	channelId?: ChannelID | null;
	location?: string | null;
	status?: number;
}

export class GuildScheduledEventService {
	constructor(
		private readonly repository: GuildScheduledEventRepository,
		private readonly snowflakeService: ISnowflakeService,
		private readonly gatewayService: IGatewayService,
	) {}

	async getEvent(eventId: GuildScheduledEventID): Promise<GuildScheduledEvent | null> {
		return this.repository.getEvent(eventId);
	}

	async listEventsByGuild(guildId: GuildID): Promise<Array<GuildScheduledEvent>> {
		return this.repository.listEventsByGuild(guildId);
	}

	async listEventsByUser(userId: UserID): Promise<Array<GuildScheduledEvent>> {
		return this.repository.listEventsByUser(userId);
	}

	async createEvent(params: CreateEventParams): Promise<GuildScheduledEvent> {
		const eventId = createGuildScheduledEventID(await this.snowflakeService.generate());
		const now = new Date();

		const event = new GuildScheduledEvent({
			guild_scheduled_event_id: eventId,
			guild_id: params.guildId,
			channel_id: params.channelId ?? null,
			creator_id: params.user.id,
			name: params.name,
			description: params.description ?? null,
			scheduled_start_time: params.scheduledStartTime,
			scheduled_end_time: params.scheduledEndTime ?? null,
			privacy_level: params.privacyLevel ?? 2,
			status: GuildScheduledEventStatus.SCHEDULED,
			entity_type: params.entityType,
			entity_id: params.entityId ?? null,
			location: params.location ?? null,
			image: null,
			created_at: now,
			updated_at: now,
			version: 0,
		});

		await this.repository.upsertEvent(event);

		// Auto-subscribe the creator
		await this.repository.subscribeUser(params.user.id, eventId, params.guildId);

		return event;
	}

	async updateEvent(params: UpdateEventParams): Promise<GuildScheduledEvent> {
		const existing = await this.repository.getEvent(params.eventId);
		if (!existing) {
			throw new Error('Event not found');
		}

		const now = new Date();
		const updated = new GuildScheduledEvent({
			...existing.toRow(),
			name: params.name ?? existing.name,
			description: params.description !== undefined ? params.description : existing.description,
			scheduled_start_time: params.scheduledStartTime ?? existing.scheduledStartTime,
			scheduled_end_time: params.scheduledEndTime !== undefined ? params.scheduledEndTime : existing.scheduledEndTime,
			privacy_level: params.privacyLevel ?? existing.privacyLevel,
			entity_type: params.entityType ?? existing.entityType,
			entity_id: params.entityId !== undefined ? params.entityId : existing.entityId,
			channel_id: params.channelId !== undefined ? params.channelId : existing.channelId,
			location: params.location !== undefined ? params.location : existing.location,
			status: params.status ?? existing.status,
			updated_at: now,
			version: existing.version + 1,
		});

		await this.repository.upsertEvent(updated);
		return updated;
	}

	async deleteEvent(user: User, eventId: GuildScheduledEventID): Promise<void> {
		const event = await this.repository.getEvent(eventId);
		if (!event) {
			throw new Error('Event not found');
		}
		await this.repository.deleteEvent(event);
	}

	async subscribeUser(userId: UserID, eventId: GuildScheduledEventID, guildId: GuildID): Promise<void> {
		await this.repository.subscribeUser(userId, eventId, guildId);
	}

	async unsubscribeUser(userId: UserID, eventId: GuildScheduledEventID, guildId: GuildID): Promise<void> {
		await this.repository.unsubscribeUser(userId, eventId, guildId);
	}

	async isUserSubscribed(userId: UserID, eventId: GuildScheduledEventID): Promise<boolean> {
		return this.repository.isUserSubscribed(userId, eventId);
	}

	async getSubscriberCount(eventId: GuildScheduledEventID): Promise<number> {
		return this.repository.getSubscriberCount(eventId);
	}

	async listSubscribers(eventId: GuildScheduledEventID): Promise<Array<UserID>> {
		return this.repository.listSubscribers(eventId);
	}
}
