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

import type {ChannelID, EmojiID, GuildID, RoleID, StickerID, UserID} from '@fluxer/api/src/BrandedTypes';
import {createChannelID, createRoleID, createUserID} from '@fluxer/api/src/BrandedTypes';
import {mapChannelToResponse} from '@fluxer/api/src/channel/ChannelMappers';
import type {IChannelRepository} from '@fluxer/api/src/channel/IChannelRepository';
import type {PermissionOverwrite} from '@fluxer/api/src/database/types/ChannelTypes';
import type {GuildAuditLogService} from '@fluxer/api/src/guild/GuildAuditLogService';
import type {AuditLogChange, GuildAuditLogChange} from '@fluxer/api/src/guild/GuildAuditLogTypes';
import type {IGuildRepositoryAggregate} from '@fluxer/api/src/guild/repositories/IGuildRepositoryAggregate';
import {ChannelHelpers, type ChannelReorderOperation} from '@fluxer/api/src/guild/services/channel/ChannelHelpers';
import type {IGatewayService} from '@fluxer/api/src/infrastructure/IGatewayService';
import {getMetricsService} from '@fluxer/api/src/infrastructure/MetricsService';
import type {SnowflakeService} from '@fluxer/api/src/infrastructure/SnowflakeService';
import type {UserCacheService} from '@fluxer/api/src/infrastructure/UserCacheService';
import {Logger} from '@fluxer/api/src/Logger';
import type {LimitConfigService} from '@fluxer/api/src/limits/LimitConfigService';
import {resolveLimitSafe} from '@fluxer/api/src/limits/LimitConfigUtils';
import {createLimitMatchContext} from '@fluxer/api/src/limits/LimitMatchContextBuilder';
import type {RequestCache} from '@fluxer/api/src/middleware/RequestCacheMiddleware';
import type {Channel} from '@fluxer/api/src/models/Channel';
import {ChannelPermissionOverwrite} from '@fluxer/api/src/models/ChannelPermissionOverwrite';
import type {ICacheService} from '@fluxer/cache/src/ICacheService';
import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import {ChannelTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import {MAX_CHANNELS_PER_CATEGORY, MAX_GUILD_CHANNELS} from '@fluxer/constants/src/LimitConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {MaxCategoryChannelsError} from '@fluxer/errors/src/domains/channel/MaxCategoryChannelsError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {MissingPermissionsError} from '@fluxer/errors/src/domains/core/MissingPermissionsError';
import {ResourceLockedError} from '@fluxer/errors/src/domains/core/ResourceLockedError';
import {MaxGuildChannelsError} from '@fluxer/errors/src/domains/guild/MaxGuildChannelsError';
import type {ChannelCreateRequest} from '@fluxer/schema/src/domains/channel/ChannelRequestSchemas';
import type {ChannelResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import {
	computeChannelMoveBlockIds,
	computeGuildChannelReorderPlan,
	type GuildChannelReorderErrorCode,
	sortChannelsForOrdering,
} from '@fluxer/schema/src/domains/channel/GuildChannelOrdering';
import {ChannelNameType} from '@fluxer/schema/src/primitives/ChannelValidators';

export class ChannelOperationsService {
	constructor(
		private readonly channelRepository: IChannelRepository,
		private readonly guildRepository: IGuildRepositoryAggregate,
		private readonly userCacheService: UserCacheService,
		private readonly gatewayService: IGatewayService,
		private readonly cacheService: ICacheService,
		private readonly snowflakeService: SnowflakeService,
		private readonly guildAuditLogService: GuildAuditLogService,
		private readonly limitConfigService: LimitConfigService,
	) {}

	async createChannel(
		params: {userId: UserID; guildId: GuildID; data: ChannelCreateRequest; requestCache: RequestCache},
		auditLogReason?: string | null,
	): Promise<ChannelResponse> {
		await this.ensureGuildHasCapacity(params.guildId);
		const channels = await this.channelRepository.listGuildChannels(params.guildId);

		const parentId = params.data.parent_id ? createChannelID(params.data.parent_id) : null;

		if (parentId) {
			await this.ensureCategoryHasCapacity({guildId: params.guildId, categoryId: parentId});
		}

		const newPosition = ChannelHelpers.getNextGlobalChannelPosition(params.data.type, parentId, channels);
		let permissionOverwrites: Map<RoleID | UserID, PermissionOverwrite> | null = null;

		const requestedOverwrites = params.data.permission_overwrites ?? null;
		if (requestedOverwrites) {
			const basePermissions = await this.gatewayService.getUserPermissions({
				guildId: params.guildId,
				userId: params.userId,
				channelId: parentId ?? undefined,
			});

			for (const overwrite of requestedOverwrites) {
				const allowPerms = overwrite.allow ? BigInt(overwrite.allow) : 0n;
				const denyPerms = overwrite.deny ? BigInt(overwrite.deny) : 0n;
				const combined = allowPerms | denyPerms;
				if ((combined & ~basePermissions) !== 0n) {
					throw new MissingPermissionsError();
				}
			}

			permissionOverwrites = new Map(
				requestedOverwrites.map((overwrite) => {
					const targetId = overwrite.type === 0 ? createRoleID(overwrite.id) : createUserID(overwrite.id);
					return [
						targetId,
						new ChannelPermissionOverwrite({
							type: overwrite.type,
							allow_: overwrite.allow ? BigInt(overwrite.allow) : 0n,
							deny_: overwrite.deny ? BigInt(overwrite.deny) : 0n,
						}).toPermissionOverwrite(),
					];
				}),
			);
		} else if (parentId) {
			const parentChannel = await this.channelRepository.findUnique(parentId);
			if (parentChannel?.permissionOverwrites) {
				permissionOverwrites = new Map(
					Array.from(parentChannel.permissionOverwrites.entries()).map(([targetId, overwrite]) => [
						targetId,
						overwrite.toPermissionOverwrite(),
					]),
				);
			}
		}

		let channelName = params.data.name;
		if (params.data.type === ChannelTypes.GUILD_TEXT) {
			const guildData = await this.gatewayService.getGuildData({
				guildId: params.guildId,
				userId: params.userId,
				skipMembershipCheck: true,
			});
			const hasFlexibleNamesEnabled = guildData.features.includes(GuildFeatures.TEXT_CHANNEL_FLEXIBLE_NAMES);
			if (!hasFlexibleNamesEnabled) {
				channelName = ChannelNameType.parse(channelName);
			}
		}

		const channelId = createChannelID(await this.snowflakeService.generate());
		const channel = await this.channelRepository.upsert({
			channel_id: channelId,
			guild_id: params.guildId,
			type: params.data.type,
			name: channelName,
			topic: params.data.topic ?? null,
			icon_hash: null,
			url: params.data.url ?? null,
			parent_id: parentId,
			position: newPosition,
			owner_id: null,
			recipient_ids: null,
			nsfw: false,
			rate_limit_per_user: 0,
			bitrate: params.data.type === ChannelTypes.GUILD_VOICE ? (params.data.bitrate ?? 64000) : null,
			user_limit: params.data.type === ChannelTypes.GUILD_VOICE ? (params.data.user_limit ?? 0) : null,
			rtc_region: null,
			last_message_id: null,
			last_pin_timestamp: null,
			permission_overwrites: permissionOverwrites,
			nicks: null,
			soft_deleted: false,
			indexed_at: null,
			version: 1,
		});

		await this.dispatchChannelCreate({guildId: params.guildId, channel, requestCache: params.requestCache});

		await this.recordAuditLog({
			guildId: params.guildId,
			userId: params.userId,
			action: AuditLogActionType.CHANNEL_CREATE,
			targetId: channel.id,
			auditLogReason: auditLogReason ?? null,
			metadata: {name: channel.name ?? '', type: channel.type.toString()},
			changes: this.guildAuditLogService.computeChanges(null, ChannelHelpers.serializeChannelForAudit(channel)),
		});

		getMetricsService().counter({
			name: 'fluxer.channels.created',
			dimensions: {
				guild_id: params.guildId.toString(),
				channel_type: channel.type.toString(),
			},
		});

		return await mapChannelToResponse({
			channel,
			currentUserId: null,
			userCacheService: this.userCacheService,
			requestCache: params.requestCache,
		});
	}

	async updateChannelPositionsLocked(params: {
		userId: UserID;
		guildId: GuildID;
		operation: ChannelReorderOperation;
		requestCache: RequestCache;
	}): Promise<void> {
		const lockKey = `guild:${params.guildId}:channel-positions`;
		const lockToken = await this.cacheService.acquireLock(lockKey, 30);
		if (!lockToken) {
			throw new ResourceLockedError();
		}

		try {
			await this.executeChannelReorder(params);
		} finally {
			await this.cacheService.releaseLock(lockKey, lockToken);
		}
	}

	async sanitizeTextChannelNames(params: {guildId: GuildID; requestCache: RequestCache}): Promise<void> {
		const {guildId, requestCache} = params;
		const channels = await this.channelRepository.listGuildChannels(guildId);

		let hasChanges = false;
		const updatedChannels: Array<Channel> = [];

		for (const channel of channels) {
			if (channel.type !== ChannelTypes.GUILD_TEXT || channel.name == null) {
				updatedChannels.push(channel);
				continue;
			}

			const normalized = ChannelNameType.parse(channel.name);
			if (normalized === channel.name) {
				updatedChannels.push(channel);
				continue;
			}

			const updated = await this.channelRepository.upsert({
				...channel.toRow(),
				name: normalized,
			});
			updatedChannels.push(updated);
			hasChanges = true;
		}

		if (hasChanges) {
			await this.dispatchChannelUpdateBulk({guildId, channels: updatedChannels, requestCache});
		}
	}

	async setChannelPermissionOverwrite(params: {
		userId: UserID;
		channelId: ChannelID;
		overwriteId: bigint;
		overwrite: {type: number; allow_: bigint; deny_: bigint};
		requestCache: RequestCache;
	}): Promise<void> {
		const channel = await this.channelRepository.findUnique(params.channelId);
		if (!channel || !channel.guildId) {
			throw InputValidationError.fromCode('channel_id', ValidationErrorCodes.INVALID_CHANNEL);
		}
		const guildId = channel.guildId;
		const hasManageRoles = await this.gatewayService.checkPermission({
			guildId,
			userId: params.userId,
			permission: Permissions.MANAGE_ROLES,
		});
		if (!hasManageRoles) {
			throw new MissingPermissionsError();
		}

		const userPermissions = await this.gatewayService.getUserPermissions({
			guildId,
			userId: params.userId,
			channelId: channel.id,
		});
		const combined = params.overwrite.allow_ | params.overwrite.deny_;
		if ((combined & ~userPermissions) !== 0n) {
			throw new MissingPermissionsError();
		}

		const overwrites = new Map(channel.permissionOverwrites ?? []);
		const targetId = params.overwrite.type === 0 ? createRoleID(params.overwriteId) : createUserID(params.overwriteId);
		const previousOverwrite = overwrites.get(targetId);
		overwrites.set(targetId, new ChannelPermissionOverwrite(params.overwrite));

		const updated = await this.channelRepository.upsert({
			...channel.toRow(),
			permission_overwrites: new Map(
				Array.from(overwrites.entries()).map(([id, ow]) => [id, ow.toPermissionOverwrite()]),
			),
		});
		await this.dispatchChannelUpdateBulk({guildId, channels: [updated], requestCache: params.requestCache});

		const changeSet = this.buildOverwriteChanges(previousOverwrite, new ChannelPermissionOverwrite(params.overwrite));
		if (changeSet.length > 0) {
			await this.recordAuditLog({
				guildId,
				userId: params.userId,
				action: previousOverwrite
					? AuditLogActionType.CHANNEL_OVERWRITE_UPDATE
					: AuditLogActionType.CHANNEL_OVERWRITE_CREATE,
				targetId,
				auditLogReason: null,
				metadata: {
					type: params.overwrite.type.toString(),
					channel_id: channel.id.toString(),
				},
				changes: changeSet,
			});

			getMetricsService().counter({
				name: 'fluxer.channels.permissions_updated',
				dimensions: {
					guild_id: guildId.toString(),
					channel_id: channel.id.toString(),
					action: previousOverwrite ? 'update' : 'create',
				},
			});
		}
	}

	async deleteChannelPermissionOverwrite(params: {
		userId: UserID;
		channelId: ChannelID;
		overwriteId: bigint;
		requestCache: RequestCache;
	}): Promise<void> {
		const channel = await this.channelRepository.findUnique(params.channelId);
		if (!channel || !channel.guildId) {
			throw InputValidationError.fromCode('channel_id', ValidationErrorCodes.INVALID_CHANNEL);
		}
		const guildId = channel.guildId;
		const hasManageRoles = await this.gatewayService.checkPermission({
			guildId,
			userId: params.userId,
			permission: Permissions.MANAGE_ROLES,
		});
		if (!hasManageRoles) {
			throw new MissingPermissionsError();
		}

		const overwrites = new Map(channel.permissionOverwrites ?? []);
		const roleKey = createRoleID(params.overwriteId);
		const userKey = createUserID(params.overwriteId);
		let targetId: (RoleID | UserID) | null = null;
		let targetType: number | null = null;

		if (overwrites.has(roleKey)) {
			targetId = roleKey;
			targetType = 0;
		} else if (overwrites.has(userKey)) {
			targetId = userKey;
			targetType = 1;
		}

		const previousOverwrite = targetId ? overwrites.get(targetId) : undefined;

		overwrites.delete(roleKey);
		overwrites.delete(userKey);

		const updated = await this.channelRepository.upsert({
			...channel.toRow(),
			permission_overwrites: new Map(
				Array.from(overwrites.entries()).map(([id, ow]) => [id, ow.toPermissionOverwrite()]),
			),
		});
		await this.dispatchChannelUpdateBulk({guildId, channels: [updated], requestCache: params.requestCache});

		if (targetId && previousOverwrite) {
			const changeSet = this.buildOverwriteChanges(previousOverwrite, undefined);
			if (changeSet.length > 0) {
				await this.recordAuditLog({
					guildId,
					userId: params.userId,
					action: AuditLogActionType.CHANNEL_OVERWRITE_DELETE,
					targetId,
					auditLogReason: null,
					metadata: {
						type: targetType !== null ? targetType.toString() : '0',
						channel_id: channel.id.toString(),
					},
					changes: changeSet,
				});

				getMetricsService().counter({
					name: 'fluxer.channels.permissions_updated',
					dimensions: {
						guild_id: guildId.toString(),
						channel_id: channel.id.toString(),
						action: 'delete',
					},
				});
			}
		}
	}

	async updateChannelPositionsByList(params: {
		userId: UserID;
		guildId: GuildID;
		updates: Array<{
			channelId: ChannelID;
			position?: number;
			parentId: ChannelID | null | undefined;
			precedingSiblingId: ChannelID | null | undefined;
			lockPermissions: boolean;
		}>;
		requestCache: RequestCache;
		auditLogReason: string | null;
	}): Promise<void> {
		const {guildId, userId, updates, requestCache} = params;
		const lockKey = `guild:${guildId}:channel-positions`;
		const lockToken = await this.cacheService.acquireLock(lockKey, 30);
		if (!lockToken) {
			throw new ResourceLockedError();
		}

		try {
			const allChannels = await this.channelRepository.listGuildChannels(guildId);
			const channelMap = new Map(allChannels.map((ch) => [ch.id, ch]));

			for (const update of updates) {
				if (!channelMap.has(update.channelId)) {
					throw InputValidationError.fromCode('id', ValidationErrorCodes.CHANNEL_NOT_FOUND);
				}
				if (update.parentId && !channelMap.has(update.parentId)) {
					throw InputValidationError.fromCode('parent_id', ValidationErrorCodes.INVALID_PARENT_CHANNEL);
				}
				if (update.precedingSiblingId && !channelMap.has(update.precedingSiblingId)) {
					throw InputValidationError.fromCode('preceding_sibling_id', ValidationErrorCodes.INVALID_CHANNEL_ID, {
						channelId: update.precedingSiblingId.toString(),
					});
				}
			}

			const viewable = new Set(await this.gatewayService.getViewableChannels({guildId, userId}));

			for (const update of updates) {
				if (!viewable.has(update.channelId)) {
					throw new MissingPermissionsError();
				}
				if (update.parentId && !viewable.has(update.parentId)) {
					throw new MissingPermissionsError();
				}
				if (update.precedingSiblingId && !viewable.has(update.precedingSiblingId)) {
					throw new MissingPermissionsError();
				}
			}

			for (const update of updates) {
				await this.applySinglePositionUpdate({
					guildId,
					userId,
					update,
					requestCache,
				});
			}
		} finally {
			await this.cacheService.releaseLock(lockKey, lockToken);
		}
	}

	private async applySinglePositionUpdate(params: {
		guildId: GuildID;
		userId: UserID;
		update: {
			channelId: ChannelID;
			position?: number;
			parentId: ChannelID | null | undefined;
			precedingSiblingId: ChannelID | null | undefined;
			lockPermissions: boolean;
		};
		requestCache: RequestCache;
	}): Promise<void> {
		const {guildId, update, requestCache} = params;
		const allChannels = await this.channelRepository.listGuildChannels(guildId);
		const channelMap = new Map(allChannels.map((ch) => [ch.id, ch]));
		const target = channelMap.get(update.channelId);
		if (!target) {
			throw InputValidationError.fromCode('id', ValidationErrorCodes.CHANNEL_NOT_FOUND);
		}

		const desiredParent = update.parentId === undefined ? (target.parentId ?? null) : update.parentId;
		if (desiredParent && !channelMap.has(desiredParent)) {
			throw InputValidationError.fromCode('parent_id', ValidationErrorCodes.INVALID_PARENT_CHANNEL);
		}
		if (desiredParent) {
			const parentChannel = channelMap.get(desiredParent)!;
			if (parentChannel.type !== ChannelTypes.GUILD_CATEGORY) {
				throw InputValidationError.fromCode('parent_id', ValidationErrorCodes.PARENT_MUST_BE_CATEGORY);
			}
		}
		if (target.type === ChannelTypes.GUILD_CATEGORY && desiredParent) {
			throw InputValidationError.fromCode('parent_id', ValidationErrorCodes.CATEGORIES_CANNOT_HAVE_PARENTS);
		}

		let precedingSibling = update.precedingSiblingId ?? null;
		if (update.precedingSiblingId === undefined) {
			const orderedChannels = sortChannelsForOrdering(allChannels);
			const siblings = orderedChannels.filter((ch) => (ch.parentId ?? null) === desiredParent);
			const blockIds = computeChannelMoveBlockIds({channels: orderedChannels, targetId: target.id});
			const siblingsWithoutBlock = siblings.filter((ch) => !blockIds.has(ch.id));

			let insertIndex = siblingsWithoutBlock.length;
			if (update.position !== undefined) {
				const adjustedPosition = Math.max(update.position, 0);
				insertIndex = Math.min(adjustedPosition, siblingsWithoutBlock.length);
			} else {
				const isVoice = target.type === ChannelTypes.GUILD_VOICE;
				if (isVoice) {
					insertIndex = siblingsWithoutBlock.length;
				} else {
					const firstVoice = siblingsWithoutBlock.findIndex((ch) => ch.type === ChannelTypes.GUILD_VOICE);
					insertIndex = firstVoice === -1 ? siblingsWithoutBlock.length : firstVoice;
				}
			}

			precedingSibling = insertIndex === 0 ? null : siblingsWithoutBlock[insertIndex - 1].id;
		}

		await this.executeChannelReorder({
			guildId,
			operation: {
				channelId: target.id,
				parentId: desiredParent,
				precedingSiblingId: precedingSibling,
			},
			requestCache,
		});

		if (update.lockPermissions && desiredParent && desiredParent !== (target.parentId ?? null)) {
			await this.syncPermissionsWithParent({guildId, channelId: target.id, parentId: desiredParent});
		}
	}

	private async syncPermissionsWithParent(params: {
		guildId: GuildID;
		channelId: ChannelID;
		parentId: ChannelID;
	}): Promise<void> {
		const parent = await this.channelRepository.findUnique(params.parentId);
		if (!parent || !parent.permissionOverwrites) return;
		const child = await this.channelRepository.findUnique(params.channelId);
		if (!child) return;

		await this.channelRepository.upsert({
			...child.toRow(),
			permission_overwrites: new Map(
				Array.from(parent.permissionOverwrites.entries()).map(([targetId, overwrite]) => [
					targetId,
					overwrite.toPermissionOverwrite(),
				]),
			),
		});
	}

	private async executeChannelReorder(params: {
		guildId: GuildID;
		operation: ChannelReorderOperation;
		requestCache: RequestCache;
	}): Promise<void> {
		const {guildId, operation, requestCache} = params;
		const allChannels = await this.channelRepository.listGuildChannels(guildId);

		const planResult = computeGuildChannelReorderPlan({channels: allChannels, operation});
		if (!planResult.ok) {
			this.throwReorderPlanError(planResult.code, operation);
		}

		const {plan} = planResult;
		const desiredParentId = plan.desiredParentById.get(operation.channelId) ?? null;
		const targetChannel = allChannels.find((ch) => ch.id === operation.channelId);
		const currentParentId = targetChannel?.parentId ?? null;
		const parentIdsToValidate = new Set<ChannelID>();
		if (currentParentId) {
			parentIdsToValidate.add(currentParentId);
		}
		if (desiredParentId) {
			parentIdsToValidate.add(desiredParentId);
		}
		if (desiredParentId && desiredParentId !== currentParentId) {
			await this.ensureCategoryHasCapacity({guildId, categoryId: desiredParentId});
		}

		ChannelHelpers.validateChannelVoicePlacement(plan.finalChannels, plan.desiredParentById, parentIdsToValidate);

		if (plan.orderUnchanged) {
			return;
		}

		const updatePromises: Array<Promise<void>> = [];
		for (let index = 0; index < plan.finalChannels.length; index++) {
			const channel = plan.finalChannels[index];
			const desiredPosition = index + 1;
			const desiredParent = plan.desiredParentById.get(channel.id) ?? null;
			const currentParent = channel.parentId ?? null;

			if (channel.position !== desiredPosition || currentParent !== desiredParent) {
				updatePromises.push(
					this.channelRepository
						.upsert({...channel.toRow(), position: desiredPosition, parent_id: desiredParent})
						.then(() => {}),
				);
			}
		}
		await Promise.all(updatePromises);

		const updatedChannels = await this.channelRepository.listGuildChannels(guildId);
		await this.dispatchChannelUpdateBulk({guildId, channels: updatedChannels, requestCache});
	}

	private throwReorderPlanError(code: GuildChannelReorderErrorCode, operation: ChannelReorderOperation): never {
		switch (code) {
			case 'TARGET_CHANNEL_NOT_FOUND':
				throw InputValidationError.fromCode('channel_id', ValidationErrorCodes.INVALID_CHANNEL_ID, {
					channelId: operation.channelId.toString(),
				});
			case 'CATEGORIES_CANNOT_HAVE_PARENTS':
				throw InputValidationError.fromCode('parent_id', ValidationErrorCodes.CATEGORIES_CANNOT_HAVE_PARENT_CHANNEL);
			case 'PARENT_NOT_FOUND':
			case 'PARENT_NOT_CATEGORY':
				throw InputValidationError.fromCode('parent_id', ValidationErrorCodes.INVALID_PARENT_CHANNEL);
			case 'PRECEDING_CHANNEL_NOT_FOUND':
				throw InputValidationError.fromCode('preceding_sibling_id', ValidationErrorCodes.INVALID_CHANNEL_ID, {
					channelId: String(operation.precedingSiblingId),
				});
			case 'CANNOT_POSITION_RELATIVE_TO_SELF_BLOCK':
				throw InputValidationError.fromCode(
					'preceding_sibling_id',
					ValidationErrorCodes.CANNOT_POSITION_CHANNEL_RELATIVE_TO_ITSELF,
				);
			case 'PRECEDING_PARENT_MISMATCH':
				throw InputValidationError.fromCode(
					'preceding_sibling_id',
					ValidationErrorCodes.PRECEDING_CHANNEL_MUST_SHARE_PARENT,
				);
			case 'PRECEDING_NOT_IN_GUILD_LIST':
				throw InputValidationError.fromCode(
					'preceding_sibling_id',
					ValidationErrorCodes.PRECEDING_CHANNEL_NOT_IN_GUILD,
				);
			case 'PARENT_NOT_IN_GUILD_LIST':
				throw InputValidationError.fromCode('parent_id', ValidationErrorCodes.PARENT_CHANNEL_NOT_IN_GUILD);
		}
	}

	private async recordAuditLog(params: {
		guildId: GuildID;
		userId: UserID;
		action: AuditLogActionType;
		targetId?: GuildID | ChannelID | RoleID | UserID | EmojiID | StickerID | string | null;
		auditLogReason?: string | null;
		metadata?: Map<string, string> | Record<string, string>;
		changes?: GuildAuditLogChange | null;
		createdAt?: Date;
	}): Promise<void> {
		const targetId =
			params.targetId === undefined || params.targetId === null
				? null
				: typeof params.targetId === 'string'
					? params.targetId
					: params.targetId.toString();

		try {
			const builder = this.guildAuditLogService
				.createBuilder(params.guildId, params.userId)
				.withAction(params.action, targetId)
				.withReason(params.auditLogReason ?? null);

			if (params.metadata) {
				builder.withMetadata(params.metadata);
			}
			if (params.changes) {
				builder.withChanges(params.changes);
			}
			if (params.createdAt) {
				builder.withCreatedAt(params.createdAt);
			}

			await builder.commit();
		} catch (error) {
			Logger.error(
				{
					error,
					guildId: params.guildId.toString(),
					userId: params.userId.toString(),
					action: params.action,
					targetId,
				},
				'Failed to record guild audit log',
			);
		}
	}

	private buildOverwriteChanges(
		previous: ChannelPermissionOverwrite | undefined,
		current: ChannelPermissionOverwrite | undefined,
	): GuildAuditLogChange {
		const changes: GuildAuditLogChange = [];

		const pushChange = (key: string, before?: bigint, after?: bigint) => {
			if (before === after) {
				return;
			}

			const change: AuditLogChange = {key};
			if (before !== undefined) {
				change.old_value = before;
			}
			if (after !== undefined) {
				change.new_value = after;
			}
			changes.push(change);
		};

		pushChange('allow', previous?.allow, current?.allow);
		pushChange('deny', previous?.deny, current?.deny);

		return changes;
	}

	private async dispatchChannelCreate({
		guildId,
		channel,
		requestCache,
	}: {
		guildId: GuildID;
		channel: Channel;
		requestCache: RequestCache;
	}): Promise<void> {
		await this.gatewayService.dispatchGuild({
			guildId,
			event: 'CHANNEL_CREATE',
			data: await mapChannelToResponse({
				channel,
				currentUserId: null,
				userCacheService: this.userCacheService,
				requestCache,
			}),
		});
	}

	private async dispatchChannelUpdateBulk({
		guildId,
		channels,
		requestCache,
	}: {
		guildId: GuildID;
		channels: Array<Channel>;
		requestCache: RequestCache;
	}): Promise<void> {
		const channelResponses = await Promise.all(
			channels.map((channel) =>
				mapChannelToResponse({
					channel,
					currentUserId: null,
					userCacheService: this.userCacheService,
					requestCache,
				}),
			),
		);

		await this.gatewayService.dispatchGuild({
			guildId,
			event: 'CHANNEL_UPDATE_BULK',
			data: {channels: channelResponses},
		});
	}

	private async ensureCategoryHasCapacity(params: {guildId: GuildID; categoryId: ChannelID}): Promise<void> {
		const count = await this.gatewayService.getCategoryChannelCount(params);

		let maxChannels = MAX_CHANNELS_PER_CATEGORY;
		const guild = await this.guildRepository.findUnique(params.guildId);
		const ctx = createLimitMatchContext({user: null, guildFeatures: guild?.features ?? null});
		maxChannels = resolveLimitSafe(
			this.limitConfigService.getConfigSnapshot(),
			ctx,
			'max_channels_per_category',
			maxChannels,
		);

		if (count >= maxChannels) {
			throw new MaxCategoryChannelsError(maxChannels);
		}
	}

	private async ensureGuildHasCapacity(guildId: GuildID): Promise<void> {
		const count = await this.gatewayService.getChannelCount({guildId});

		let maxChannels = MAX_GUILD_CHANNELS;
		const guild = await this.guildRepository.findUnique(guildId);
		const ctx = createLimitMatchContext({user: null, guildFeatures: guild?.features ?? null});
		maxChannels = resolveLimitSafe(this.limitConfigService.getConfigSnapshot(), ctx, 'max_guild_channels', maxChannels);

		if (count >= maxChannels) {
			throw new MaxGuildChannelsError(maxChannels);
		}
	}
}
