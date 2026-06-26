// SPDX-License-Identifier: AGPL-3.0-or-later

import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {MissingPermissionsError} from '@fluxer/errors/src/domains/core/MissingPermissionsError';
import {UnknownGuildError} from '@fluxer/errors/src/domains/guild/UnknownGuildError';
import type {
	GuildScheduledEventCreateRequest,
	GuildScheduledEventUpdateRequest,
} from '@fluxer/schema/src/domains/guild/GuildRequestSchemas';
import type {
	GuildScheduledEventResponse,
	GuildScheduledEventRsvpResponse,
} from '@fluxer/schema/src/domains/guild/GuildScheduledEventSchemas';
import type {GuildID, ScheduledEventID, UserID} from '../../BrandedTypes';
import {createScheduledEventID} from '../../BrandedTypes';
import type {IGatewayService} from '../../infrastructure/IGatewayService';
import type {ISnowflakeService} from '../../infrastructure/ISnowflakeService';
import {GuildScheduledEventRepository} from '../repositories/GuildScheduledEventRepository';

// ponytail: no separate model class, Row reused directly

export class GuildScheduledEventService {
	constructor(
		private readonly eventRepository: GuildScheduledEventRepository,
		private readonly snowflakeService: ISnowflakeService,
		private readonly gatewayService: IGatewayService,
	) {}

	async createEvent(
		userId: UserID,
		guildId: GuildID,
		data: GuildScheduledEventCreateRequest,
	): Promise<GuildScheduledEventResponse> {
		await this.requireManageGuild(userId, guildId);

		const eventId = createScheduledEventID(await this.snowflakeService.generate());
		const row: import('../../database/types/GuildTypes').GuildScheduledEventRow = {
			guild_id: guildId,
			scheduled_event_id: eventId,
			channel_id: data.channel_id ? (BigInt(data.channel_id) as any) : null,
			creator_id: userId,
			name: data.name,
			description: data.description ?? null,
			scheduled_start_time: data.scheduled_start_time,
			scheduled_end_time: data.scheduled_end_time ?? null,
			privacy_level: data.privacy_level ?? 2,
			status: 1, // SCHEDULED
			entity_type: data.entity_type ?? null,
			entity_id: data.entity_id ?? null,
			location: data.location ?? null,
			image: data.image ?? null,
			version: 1,
		};

		await this.eventRepository.upsertEvent(row);
		return this.toResponse(row);
	}

	async listEvents(userId: UserID, guildId: GuildID): Promise<Array<GuildScheduledEventResponse>> {
		await this.requireGuildAccess(userId, guildId);
		const events = await this.eventRepository.listEvents(guildId);
		return events.map((e) => this.toResponse(e));
	}

	async getEvent(
		userId: UserID,
		guildId: GuildID,
		eventId: ScheduledEventID,
	): Promise<GuildScheduledEventResponse | null> {
		await this.requireGuildAccess(userId, guildId);
		const event = await this.eventRepository.getEvent(eventId, guildId);
		return event ? this.toResponse(event) : null;
	}

	async updateEvent(
		userId: UserID,
		guildId: GuildID,
		eventId: ScheduledEventID,
		data: GuildScheduledEventUpdateRequest,
	): Promise<GuildScheduledEventResponse | null> {
		await this.requireManageGuild(userId, guildId);

		const existing = await this.eventRepository.getEvent(eventId, guildId);
		if (!existing) return null;

		const updated = {
			...existing,
			...data,
			channel_id: data.channel_id !== undefined ? (BigInt(data.channel_id) as any) : existing.channel_id,
			version: existing.version + 1,
		};

		await this.eventRepository.upsertEvent(updated);
		return this.toResponse(updated);
	}

	async deleteEvent(userId: UserID, guildId: GuildID, eventId: ScheduledEventID): Promise<void> {
		await this.requireManageGuild(userId, guildId);
		await this.eventRepository.deleteEvent(guildId, eventId);
	}

	async setRsvp(
		userId: UserID,
		guildId: GuildID,
		eventId: ScheduledEventID,
		status: number,
	): Promise<GuildScheduledEventRsvpResponse> {
		await this.requireGuildAccess(userId, guildId);

		const row: import('../../database/types/GuildTypes').GuildScheduledEventRsvpRow = {
			guild_id: guildId,
			scheduled_event_id: eventId,
			user_id: userId,
			status,
		};

		await this.eventRepository.upsertRsvp(row);
		return {
			guild_id: guildId.toString(),
			scheduled_event_id: eventId.toString(),
			user_id: userId.toString(),
			status,
		};
	}

	async listRsvps(
		userId: UserID,
		guildId: GuildID,
		eventId: ScheduledEventID,
	): Promise<Array<GuildScheduledEventRsvpResponse>> {
		await this.requireGuildAccess(userId, guildId);
		const rsvps = await this.eventRepository.listRsvps(guildId, eventId);
		return rsvps.map((r) => ({
			guild_id: r.guild_id.toString(),
			scheduled_event_id: r.scheduled_event_id.toString(),
			user_id: r.user_id.toString(),
			status: r.status,
		}));
	}

	private toResponse(
		row: import('../../database/types/GuildTypes').GuildScheduledEventRow,
	): GuildScheduledEventResponse {
		return {
			id: row.scheduled_event_id.toString(),
			guild_id: row.guild_id.toString(),
			channel_id: row.channel_id?.toString(),
			creator_id: row.creator_id.toString(),
			name: row.name,
			description: row.description ?? undefined,
			scheduled_start_time: row.scheduled_start_time,
			scheduled_end_time: row.scheduled_end_time ?? undefined,
			privacy_level: row.privacy_level,
			status: row.status,
			entity_type: row.entity_type ?? undefined,
			entity_id: row.entity_id ?? undefined,
			location: row.location ?? undefined,
			image: row.image ?? undefined,
		};
	}

	private async requireManageGuild(userId: UserID, guildId: GuildID): Promise<void> {
		const ok = await this.gatewayService.checkPermission({
			guildId,
			userId,
			permission: Permissions.MANAGE_GUILD,
		});
		if (!ok) throw new MissingPermissionsError();
	}

	private async requireGuildAccess(userId: UserID, guildId: GuildID): Promise<void> {
		const ok = await this.gatewayService.checkPermission({
			guildId,
			userId,
			permission: Permissions.VIEW_CHANNEL,
		});
		if (!ok) throw new UnknownGuildError();
	}
}
