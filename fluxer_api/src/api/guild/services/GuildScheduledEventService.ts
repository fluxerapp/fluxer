// SPDX-License-Identifier: AGPL-3.0-or-later

import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {MissingPermissionsError} from '@fluxer/errors/src/domains/core/MissingPermissionsError';
import {UnknownGuildError} from '@fluxer/errors/src/domains/guild/UnknownGuildError';
import {UnknownGuildScheduledEventError} from '@fluxer/errors/src/domains/guild/UnknownGuildScheduledEventError';
import type {
	GuildScheduledEventResponse,
} from '@fluxer/schema/src/domains/guild/GuildScheduledEventSchemas';
import type {GuildScheduledEventCreateRequest, GuildScheduledEventUpdateRequest} from '@fluxer/schema/src/domains/guild/GuildRequestSchemas';
import type {AvatarService} from '../../infrastructure/AvatarService';
import type {IGatewayService} from '../../infrastructure/IGatewayService';
import type {ISnowflakeService} from '../../infrastructure/ISnowflakeService';
import type {GuildAuditLogService} from '../GuildAuditLogService';
import type {IGuildScheduledEventRepository} from '../repositories/IGuildScheduledEventRepository';
import type {GuildID, ScheduledEventID, UserID} from '../../BrandedTypes';
import {createScheduledEventID} from '../../BrandedTypes';
import type {User} from '../../models/User';
import {mapScheduledEventToResponse} from '../GuildModel';

export class GuildScheduledEventService {
	constructor(
		private readonly eventRepository: IGuildScheduledEventRepository,
		private readonly gatewayService: IGatewayService,
		private readonly snowflakeService: ISnowflakeService,
		private readonly avatarService: AvatarService,
		private readonly guildAuditLogService: GuildAuditLogService,
	) {}

	async listEvents(params: {userId: UserID; guildId: GuildID}): Promise<Array<GuildScheduledEventResponse>> {
		const {userId, guildId} = params;
		const guildData = await this.gatewayService.getGuildData({guildId, userId});
		if (!guildData) throw new UnknownGuildError();
		const events = await this.eventRepository.listEvents(guildId);
		return Promise.all(
			events.map(async (event) => {
				const userCount = await this.eventRepository.countUserEvents(guildId, event.id);
				return mapScheduledEventToResponse(event, userCount);
			}),
		);
	}

	async getEvent(params: {
		userId: UserID;
		guildId: GuildID;
		eventId: ScheduledEventID;
	}): Promise<GuildScheduledEventResponse> {
		const {userId, guildId, eventId} = params;
		const guildData = await this.gatewayService.getGuildData({guildId, userId});
		if (!guildData) throw new UnknownGuildError();
		const event = await this.eventRepository.getEvent(guildId, eventId);
		if (!event) throw new UnknownGuildScheduledEventError();
		const userCount = await this.eventRepository.countUserEvents(guildId, eventId);
		return mapScheduledEventToResponse(event, userCount);
	}

	async createEvent(
		params: {user: User; guildId: GuildID; data: GuildScheduledEventCreateRequest},
		auditLogReason?: string | null,
	): Promise<GuildScheduledEventResponse> {
		const {user, guildId, data} = params;
		const hasPermission = await this.gatewayService.checkPermission({
			guildId,
			userId: user.id,
			permission: Permissions.MANAGE_EVENTS,
		});
		if (!hasPermission) throw new MissingPermissionsError();

		let imageHash: string | null = null;
		if (data.image) {
			const processed = await this.avatarService.processImage({
				errorPath: 'image',
				base64Image: data.image,
			});
			imageHash = processed.imageHash;
		}

		const eventId = createScheduledEventID(this.snowflakeService.nextId());
		const event = await this.eventRepository.createEvent({
			event_id: eventId,
			guild_id: guildId,
			channel_id: data.channel_id ? (BigInt(data.channel_id) as any) : null,
			creator_id: user.id,
			name: data.name,
			description: data.description ?? null,
			image_hash: imageHash,
			scheduled_start_time: new Date(data.scheduled_start_time),
			scheduled_end_time: data.scheduled_end_time ? new Date(data.scheduled_end_time) : null,
			status: 'SCHEDULED',
			entity_type: data.entity_type,
			entity_id: null,
			entity_metadata: data.entity_metadata ?? null,
		});

		await this.guildAuditLogService.log({
			guildId,
			userId: user.id,
			actionType: AuditLogActionType.GUILD_SCHEDULED_EVENT_CREATE,
			targetId: eventId.toString(),
			reason: auditLogReason ?? null,
		});

		await this.gatewayService.dispatchGuildScheduledEventCreate({guildId, event});

		return mapScheduledEventToResponse(event, 0);
	}

	async updateEvent(
		params: {
			user: User;
			guildId: GuildID;
			eventId: ScheduledEventID;
			data: GuildScheduledEventUpdateRequest;
		},
		auditLogReason?: string | null,
	): Promise<GuildScheduledEventResponse> {
		const {user, guildId, eventId, data} = params;
		const hasPermission = await this.gatewayService.checkPermission({
			guildId,
			userId: user.id,
			permission: Permissions.MANAGE_EVENTS,
		});
		if (!hasPermission) throw new MissingPermissionsError();

		const existing = await this.eventRepository.getEvent(guildId, eventId);
		if (!existing) throw new UnknownGuildScheduledEventError();

		let imageHash = existing.imageHash;
		if (data.image !== undefined) {
			if (data.image === null) {
				imageHash = null;
			} else {
				const processed = await this.avatarService.processImage({
					errorPath: 'image',
					base64Image: data.image,
				});
				imageHash = processed.imageHash;
			}
		}

		const updated = await this.eventRepository.updateEvent(guildId, eventId, {
			...(data.name !== undefined && {name: data.name}),
			...(data.description !== undefined && {description: data.description ?? null}),
			...(imageHash !== existing.imageHash && {image_hash: imageHash}),
			...(data.channel_id !== undefined && {channel_id: data.channel_id ? (BigInt(data.channel_id) as any) : null}),
			...(data.entity_type !== undefined && {entity_type: data.entity_type}),
			...(data.entity_metadata !== undefined && {entity_metadata: data.entity_metadata ?? null}),
			...(data.scheduled_start_time !== undefined && {scheduled_start_time: new Date(data.scheduled_start_time)}),
			...(data.scheduled_end_time !== undefined && {
				scheduled_end_time: data.scheduled_end_time ? new Date(data.scheduled_end_time) : null,
			}),
			...(data.status !== undefined && {status: data.status}),
		});
		if (!updated) throw new UnknownGuildScheduledEventError();

		await this.guildAuditLogService.log({
			guildId,
			userId: user.id,
			actionType: AuditLogActionType.GUILD_SCHEDULED_EVENT_UPDATE,
			targetId: eventId.toString(),
			reason: auditLogReason ?? null,
		});

		await this.gatewayService.dispatchGuildScheduledEventUpdate({guildId, event: updated});

		const userCount = await this.eventRepository.countUserEvents(guildId, eventId);
		return mapScheduledEventToResponse(updated, userCount);
	}

	async deleteEvent(
		params: {user: User; guildId: GuildID; eventId: ScheduledEventID},
		auditLogReason?: string | null,
	): Promise<void> {
		const {user, guildId, eventId} = params;
		const hasPermission = await this.gatewayService.checkPermission({
			guildId,
			userId: user.id,
			permission: Permissions.MANAGE_EVENTS,
		});
		if (!hasPermission) throw new MissingPermissionsError();

		const existing = await this.eventRepository.getEvent(guildId, eventId);
		if (!existing) throw new UnknownGuildScheduledEventError();

		await this.eventRepository.deleteEvent(guildId, eventId);

		await this.guildAuditLogService.log({
			guildId,
			userId: user.id,
			actionType: AuditLogActionType.GUILD_SCHEDULED_EVENT_DELETE,
			targetId: eventId.toString(),
			reason: auditLogReason ?? null,
		});

		await this.gatewayService.dispatchGuildScheduledEventDelete({guildId, eventId});
	}

	async rsvpEvent(params: {userId: UserID; guildId: GuildID; eventId: ScheduledEventID}): Promise<void> {
		const {userId, guildId, eventId} = params;
		const guildData = await this.gatewayService.getGuildData({guildId, userId});
		if (!guildData) throw new UnknownGuildError();
		const event = await this.eventRepository.getEvent(guildId, eventId);
		if (!event) throw new UnknownGuildScheduledEventError();
		await this.eventRepository.rsvpEvent(guildId, eventId, userId);
	}

	async unrsvpEvent(params: {userId: UserID; guildId: GuildID; eventId: ScheduledEventID}): Promise<void> {
		const {userId, guildId, eventId} = params;
		const guildData = await this.gatewayService.getGuildData({guildId, userId});
		if (!guildData) throw new UnknownGuildError();
		const event = await this.eventRepository.getEvent(guildId, eventId);
		if (!event) throw new UnknownGuildScheduledEventError();
		await this.eventRepository.unrsvpEvent(guildId, eventId, userId);
	}
}
