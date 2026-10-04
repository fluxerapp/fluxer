// SPDX-License-Identifier: AGPL-3.0-or-later

import {createGuildEventID, type GuildEventID, type GuildID, type UserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import type {GuildAuditLogService} from '@app/api/guild/GuildAuditLogService';
import {GuildEventRepository} from '@app/api/guild/repositories/GuildEventRepository';
import type {AvatarService} from '@app/api/infrastructure/AvatarService';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {ISnowflakeService} from '@app/api/infrastructure/ISnowflakeService';
import {Logger} from '@app/api/Logger';
import type {GuildEvent as StoredGuildEvent} from '@app/api/models/GuildEvent';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {MissingAccessError} from '@fluxer/errors/src/domains/core/MissingAccessError';
import {MissingPermissionsError} from '@fluxer/errors/src/domains/core/MissingPermissionsError';
import {NotFoundError} from '@fluxer/errors/src/domains/core/NotFoundError';
import type {
	GuildEventCreate,
	GuildEvent as GuildEventResponse,
	GuildEventUpdate,
} from '@fluxer/schema/src/domains/guild/GuildEventSchemas';

export class GuildEventService {
	private readonly repository = new GuildEventRepository();

	constructor(
		private readonly gatewayService: IGatewayService,
		private readonly avatarService: AvatarService,
		private readonly snowflakeService: ISnowflakeService,
		private readonly guildAuditLogService: GuildAuditLogService,
	) {}

	private auditSnapshot(event: StoredGuildEvent | null): Record<string, unknown> | null {
		if (!event) return null;
		return {
			creator_id: event.creatorId.toString(),
			name: event.name,
			description: event.description,
			location: event.location,
			starts_at: event.startsAt.toISOString(),
			ends_at: event.endsAt?.toISOString() ?? null,
			image_hash: event.imageHash,
		};
	}

	private async recordAuditLog(params: {
		userId: UserID;
		event: StoredGuildEvent;
		action: AuditLogActionType;
		previous: StoredGuildEvent | null;
		next: StoredGuildEvent | null;
		auditLogReason?: string | null;
	}): Promise<void> {
		try {
			await this.guildAuditLogService
				.createBuilder(params.event.guildId, params.userId)
				.withAction(params.action, params.event.id.toString())
				.withReason(params.auditLogReason)
				.withComputedChanges(this.auditSnapshot(params.previous), this.auditSnapshot(params.next))
				.commit();
		} catch (error) {
			Logger.error(
				{
					error,
					guildId: params.event.guildId.toString(),
					eventId: params.event.id.toString(),
					userId: params.userId.toString(),
					action: params.action,
				},
				'Failed to record guild event audit log',
			);
		}
	}

	private async requireMembership(userId: UserID, guildId: GuildID): Promise<void> {
		const guild = await this.gatewayService.getGuildData({guildId, userId});
		if (!guild) throw new MissingAccessError();
	}

	private async permissions(userId: UserID, guildId: GuildID): Promise<bigint> {
		await this.requireMembership(userId, guildId);
		return await this.gatewayService.getUserPermissions({guildId, userId});
	}

	private canCreateEvents(permissions: bigint): boolean {
		return (
			(permissions & Permissions.CREATE_EVENTS) === Permissions.CREATE_EVENTS ||
			(permissions & Permissions.MANAGE_EVENTS) === Permissions.MANAGE_EVENTS
		);
	}

	private canManageEvents(permissions: bigint): boolean {
		return (permissions & Permissions.MANAGE_EVENTS) === Permissions.MANAGE_EVENTS;
	}

	private map(event: StoredGuildEvent): GuildEventResponse {
		return {
			id: event.id.toString(),
			guild_id: event.guildId.toString(),
			creator_id: event.creatorId.toString(),
			name: event.name,
			description: event.description,
			location: event.location,
			starts_at: event.startsAt.toISOString(),
			ends_at: event.endsAt?.toISOString() ?? null,
			image_url: event.imageHash
				? `${Config.endpoints.media}/guild-events/${event.guildId}/${event.id}/${event.imageHash}`
				: null,
			created_at: event.createdAt.toISOString(),
		};
	}

	async list(params: {userId: UserID; guildId: GuildID}): Promise<Array<GuildEventResponse>> {
		await this.requireMembership(params.userId, params.guildId);
		return (await this.repository.list(params.guildId)).map((event) => this.map(event));
	}

	async create(
		params: {userId: UserID; guildId: GuildID; data: GuildEventCreate},
		auditLogReason?: string | null,
	): Promise<GuildEventResponse> {
		const permissions = await this.permissions(params.userId, params.guildId);
		if (!this.canCreateEvents(permissions)) throw new MissingPermissionsError();
		const eventId = createGuildEventID(await this.snowflakeService.generate());
		const imageHash = params.data.image
			? await this.avatarService.uploadGuildEventImage({
					guildId: params.guildId,
					eventId,
					errorPath: 'image',
					base64Image: params.data.image,
				})
			: null;
		const event = await this.repository.upsert({
			guild_id: params.guildId,
			event_id: eventId,
			creator_id: params.userId,
			name: params.data.name,
			description: params.data.description ?? null,
			location: params.data.location ?? null,
			starts_at: new Date(params.data.starts_at),
			ends_at: params.data.ends_at ? new Date(params.data.ends_at) : null,
			image_hash: imageHash,
			created_at: new Date(),
			version: 1,
		});
		await this.recordAuditLog({
			userId: params.userId,
			event,
			action: AuditLogActionType.GUILD_EVENT_CREATE,
			previous: null,
			next: event,
			auditLogReason,
		});
		return this.map(event);
	}

	private async ownedEvent(params: {
		userId: UserID;
		guildId: GuildID;
		eventId: GuildEventID;
	}): Promise<{event: StoredGuildEvent; canManage: boolean}> {
		const [event, permissions] = await Promise.all([
			this.repository.get(params.guildId, params.eventId),
			this.permissions(params.userId, params.guildId),
		]);
		if (!event) throw new NotFoundError({code: APIErrorCodes.NOT_FOUND});
		const canManage = this.canManageEvents(permissions);
		if (!canManage && event.creatorId !== params.userId) throw new MissingPermissionsError();
		if (!canManage && !this.canCreateEvents(permissions)) throw new MissingPermissionsError();
		return {event, canManage};
	}

	async update(
		params: {
			userId: UserID;
			guildId: GuildID;
			eventId: GuildEventID;
			data: GuildEventUpdate;
		},
		auditLogReason?: string | null,
	): Promise<GuildEventResponse> {
		const {event} = await this.ownedEvent(params);
		const startsAt = params.data.starts_at ? new Date(params.data.starts_at) : event.startsAt;
		const endsAt =
			params.data.ends_at === undefined
				? event.endsAt
				: params.data.ends_at === null
					? null
					: new Date(params.data.ends_at);
		if (endsAt && endsAt.getTime() <= startsAt.getTime()) {
			throw InputValidationError.fromCode('ends_at', ValidationErrorCodes.INVALID_FORMAT);
		}
		let imageHash = event.imageHash;
		if (params.data.image !== undefined) {
			if (params.data.image === null) {
				await this.avatarService.deleteGuildEventImage({
					guildId: params.guildId,
					eventId: params.eventId,
					imageHash: event.imageHash,
				});
				imageHash = null;
			} else {
				imageHash = await this.avatarService.uploadGuildEventImage({
					guildId: params.guildId,
					eventId: params.eventId,
					errorPath: 'image',
					base64Image: params.data.image,
					previousHash: event.imageHash,
				});
			}
		}
		const updated = await this.repository.upsert(
			{
				...event.toRow(),
				name: params.data.name ?? event.name,
				description: params.data.description === undefined ? event.description : params.data.description,
				location: params.data.location === undefined ? event.location : params.data.location,
				starts_at: startsAt,
				ends_at: endsAt,
				image_hash: imageHash,
			},
			event.toRow(),
		);
		await this.recordAuditLog({
			userId: params.userId,
			event: updated,
			action: AuditLogActionType.GUILD_EVENT_UPDATE,
			previous: event,
			next: updated,
			auditLogReason,
		});
		return this.map(updated);
	}

	async delete(
		params: {userId: UserID; guildId: GuildID; eventId: GuildEventID},
		auditLogReason?: string | null,
	): Promise<void> {
		const {event} = await this.ownedEvent(params);
		await this.repository.delete(params.guildId, params.eventId);
		await this.recordAuditLog({
			userId: params.userId,
			event,
			action: AuditLogActionType.GUILD_EVENT_DELETE,
			previous: event,
			next: null,
			auditLogReason,
		});
		await this.avatarService.deleteGuildEventImage({
			guildId: params.guildId,
			eventId: params.eventId,
			imageHash: event.imageHash,
		});
	}
}
