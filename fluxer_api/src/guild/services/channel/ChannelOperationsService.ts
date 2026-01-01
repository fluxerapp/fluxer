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

import type {ChannelID, EmojiID, GuildID, RoleID, StickerID, UserID} from '~/BrandedTypes';
import {createChannelID, createRoleID, createUserID} from '~/BrandedTypes';
import {ChannelTypes, GuildFeatures, MAX_CHANNELS_PER_CATEGORY, MAX_GUILD_CHANNELS, Permissions} from '~/Constants';
import type {ChannelCreateRequest, ChannelResponse} from '~/channel/ChannelModel';
import {mapChannelToResponse} from '~/channel/ChannelModel';
import type {IChannelRepository} from '~/channel/IChannelRepository';
import {AuditLogActionType} from '~/constants/AuditLogActionType';
import type {PermissionOverwrite} from '~/database/CassandraTypes';
import {
	InputValidationError,
	MaxCategoryChannelsError,
	MaxGuildChannelsError,
	MissingPermissionsError,
	ResourceLockedError,
} from '~/Errors';
import type {GuildAuditLogChange, GuildAuditLogService} from '~/guild/GuildAuditLogService';
import type {AuditLogChange} from '~/guild/GuildAuditLogTypes';
import type {ICacheService} from '~/infrastructure/ICacheService';
import type {IGatewayService} from '~/infrastructure/IGatewayService';
import type {SnowflakeService} from '~/infrastructure/SnowflakeService';
import type {UserCacheService} from '~/infrastructure/UserCacheService';
import {Logger} from '~/Logger';
import {type Channel, ChannelPermissionOverwrite} from '~/Models';
import type {RequestCache} from '~/middleware/RequestCacheMiddleware';
import {ChannelNameType} from '~/Schema';
import {ChannelHelpers, type ChannelReorderOperation} from './ChannelHelpers';

export class ChannelOperationsService {
	constructor(
		private readonly channelRepository: IChannelRepository,
		private readonly userCacheService: UserCacheService,
		private readonly gatewayService: IGatewayService,
		private readonly cacheService: ICacheService,
		private readonly snowflakeService: SnowflakeService,
		private readonly guildAuditLogService: GuildAuditLogService,
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

		const channelId = createChannelID(this.snowflakeService.generate());
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
			throw InputValidationError.create('channel_id', 'Invalid channel');
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
			throw InputValidationError.create('channel_id', 'Invalid channel');
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
			const viewable = new Set(await this.gatewayService.getViewableChannels({guildId, userId}));

			for (const update of updates) {
				if (!viewable.has(update.channelId)) {
					throw new MissingPermissionsError();
				}
				if (update.parentId && !viewable.has(update.parentId)) {
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
		update: {channelId: ChannelID; position?: number; parentId: ChannelID | null | undefined; lockPermissions: boolean};
		requestCache: RequestCache;
	}): Promise<void> {
		const {guildId, update, requestCache} = params;
		const allChannels = await this.channelRepository.listGuildChannels(guildId);
		const channelMap = new Map(allChannels.map((ch) => [ch.id, ch]));
		const target = channelMap.get(update.channelId);
		if (!target) {
			throw InputValidationError.create('id', 'Channel not found');
		}

		const desiredParent = update.parentId === undefined ? (target.parentId ?? null) : update.parentId;
		if (desiredParent && !channelMap.has(desiredParent)) {
			throw InputValidationError.create('parent_id', 'Invalid parent channel');
		}
		if (desiredParent) {
			const parentChannel = channelMap.get(desiredParent)!;
			if (parentChannel.type !== ChannelTypes.GUILD_CATEGORY) {
				throw InputValidationError.create('parent_id', 'Parent must be a category');
			}
		}
		if (target.type === ChannelTypes.GUILD_CATEGORY && desiredParent) {
			throw InputValidationError.create('parent_id', 'Categories cannot have parents');
		}

		const siblings = allChannels
			.filter((ch) => (ch.parentId ?? null) === desiredParent)
			.sort((a, b) => (a.position === b.position ? String(a.id).localeCompare(String(b.id)) : a.position - b.position));

		const blockIds = new Set<ChannelID>();
		blockIds.add(target.id);
		if (target.type === ChannelTypes.GUILD_CATEGORY) {
			for (const ch of allChannels) {
				if (ch.parentId === target.id) blockIds.add(ch.id);
			}
		}

		const siblingsWithoutBlock = siblings.filter((ch) => !blockIds.has(ch.id));

		let insertIndex = siblingsWithoutBlock.length;
		if (update.position !== undefined) {
			const isChangingParent = desiredParent !== (target.parentId ?? null);
			const adjustedPosition = isChangingParent ? Math.max(update.position - 1, 0) : Math.max(update.position, 0);
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

		const precedingSibling = insertIndex === 0 ? null : siblingsWithoutBlock[insertIndex - 1].id;

		await this.executeChannelReorder({
			guildId,
			operation: {
				channelId: target.id,
				parentId: desiredParent === undefined ? (target.parentId ?? null) : desiredParent,
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
		const channelMap = new Map(allChannels.map((c) => [c.id, c]));

		const targetChannel = channelMap.get(operation.channelId);
		if (!targetChannel) {
			throw InputValidationError.create('channel_id', `Invalid channel ID: ${operation.channelId}`);
		}

		const requestedParentId = operation.parentId;
		const desiredParentId =
			targetChannel.type === ChannelTypes.GUILD_CATEGORY
				? null
				: requestedParentId !== undefined
					? requestedParentId
					: (targetChannel.parentId ?? null);

		if (targetChannel.type === ChannelTypes.GUILD_CATEGORY && operation.parentId) {
			throw InputValidationError.create('parent_id', 'Categories cannot have a parent channel');
		}

		if (desiredParentId) {
			const parentChannel = channelMap.get(desiredParentId);
			if (!parentChannel || parentChannel.type !== ChannelTypes.GUILD_CATEGORY) {
				throw InputValidationError.create('parent_id', 'Invalid parent channel');
			}
		}

		const currentParentId = targetChannel.parentId ?? null;
		if (desiredParentId && desiredParentId !== currentParentId) {
			await this.ensureCategoryHasCapacity({guildId, categoryId: desiredParentId});
		}

		const precedingId: ChannelID | null = operation.precedingSiblingId ?? null;
		if (precedingId && !channelMap.has(precedingId)) {
			throw InputValidationError.create('preceding_sibling_id', `Invalid channel ID: ${precedingId}`);
		}

		const blockIds = new Set<ChannelID>();
		if (targetChannel.type === ChannelTypes.GUILD_CATEGORY) {
			blockIds.add(targetChannel.id);
			for (const channel of allChannels) {
				if (channel.parentId === targetChannel.id) {
					blockIds.add(channel.id);
				}
			}
		} else {
			blockIds.add(targetChannel.id);
		}

		if (precedingId && blockIds.has(precedingId)) {
			throw InputValidationError.create(
				'preceding_sibling_id',
				'Cannot position a channel relative to itself or its descendants',
			);
		}

		const orderedChannels = [...allChannels].sort((a, b) => {
			if (a.position !== b.position) return a.position - b.position;
			return String(a.id).localeCompare(String(b.id));
		});

		const remainingChannels = orderedChannels.filter((ch) => !blockIds.has(ch.id));
		const blockChannels = orderedChannels.filter((ch) => blockIds.has(ch.id));

		const expectedParent = desiredParentId ?? null;
		if (precedingId) {
			const precedingChannel = channelMap.get(precedingId)!;
			const precedingParent = precedingChannel.parentId ?? null;
			if (precedingParent !== expectedParent) {
				throw InputValidationError.create(
					'preceding_sibling_id',
					'Preceding channel must share the same parent as the moved channel',
				);
			}
		}

		const findCategorySpan = (list: Array<Channel>, categoryId: ChannelID) => {
			const start = list.findIndex((ch) => ch.id === categoryId);
			if (start === -1) return {start: -1, end: -1};
			let end = start + 1;
			while (end < list.length && list[end].parentId === categoryId) {
				end++;
			}
			return {start, end};
		};

		let insertIndex = 0;
		if (precedingId) {
			const precedingIndex = remainingChannels.findIndex((ch) => ch.id === precedingId);
			if (precedingIndex === -1) {
				throw InputValidationError.create('preceding_sibling_id', 'Preceding channel is not present in the guild');
			}
			const precedingChannel = channelMap.get(precedingId)!;
			if (precedingChannel.type === ChannelTypes.GUILD_CATEGORY) {
				const span = findCategorySpan(remainingChannels, precedingChannel.id);
				insertIndex = span.end;
			} else {
				insertIndex = precedingIndex + 1;
			}
		} else if (desiredParentId) {
			const parentIndex = remainingChannels.findIndex((ch) => ch.id === desiredParentId);
			if (parentIndex === -1) {
				throw InputValidationError.create('parent_id', 'Parent channel is not present in the guild');
			}
			insertIndex = parentIndex + 1;
		} else {
			insertIndex = 0;
		}

		const finalChannels = [...remainingChannels];
		finalChannels.splice(insertIndex, 0, ...blockChannels);

		const desiredParentMap = new Map<ChannelID, ChannelID | null>();
		for (const channel of finalChannels) {
			if (channel.id === targetChannel.id) {
				desiredParentMap.set(channel.id, desiredParentId ?? null);
			} else {
				desiredParentMap.set(channel.id, channel.parentId ?? null);
			}
		}

		ChannelHelpers.validateChannelVoicePlacement(finalChannels, desiredParentMap);

		const orderUnchanged =
			finalChannels.length === orderedChannels.length &&
			finalChannels.every((channel, index) => channel.id === orderedChannels[index].id) &&
			(targetChannel.parentId ?? null) === (desiredParentMap.get(targetChannel.id) ?? null);

		if (orderUnchanged) {
			return;
		}

		const updatePromises: Array<Promise<void>> = [];
		for (let index = 0; index < finalChannels.length; index++) {
			const channel = finalChannels[index];
			const desiredPosition = index + 1;
			const desiredParent = desiredParentMap.get(channel.id) ?? null;
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
		if (count >= MAX_CHANNELS_PER_CATEGORY) {
			throw new MaxCategoryChannelsError();
		}
	}

	private async ensureGuildHasCapacity(guildId: GuildID): Promise<void> {
		const count = await this.gatewayService.getChannelCount({guildId});
		if (count >= MAX_GUILD_CHANNELS) {
			throw new MaxGuildChannelsError();
		}
	}
}
