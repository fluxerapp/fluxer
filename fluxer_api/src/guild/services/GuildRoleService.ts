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

import type {GuildID, RoleID, UserID} from '~/BrandedTypes';
import {createRoleID, guildIdToRoleId} from '~/BrandedTypes';
import {ALL_PERMISSIONS, DEFAULT_PERMISSIONS, MAX_GUILD_ROLES, Permissions} from '~/Constants';
import {AuditLogActionType} from '~/constants/AuditLogActionType';
import {
	InputValidationError,
	MaxGuildRolesError,
	MissingPermissionsError,
	ResourceLockedError,
	UnknownGuildRoleError,
} from '~/Errors';
import type {ICacheService} from '~/infrastructure/ICacheService';
import type {IGatewayService} from '~/infrastructure/IGatewayService';
import type {SnowflakeService} from '~/infrastructure/SnowflakeService';
import {Logger} from '~/Logger';
import {GuildRole} from '~/Models';
import {computePermissionsDiff} from '~/utils/PermissionUtils';
import type {GuildAuditLogChange, GuildAuditLogService} from '../GuildAuditLogService';
import {
	type GuildRoleCreateRequest,
	type GuildRoleResponse,
	type GuildRoleUpdateRequest,
	mapGuildRoleToResponse,
} from '../GuildModel';
import type {IGuildMemberRepository} from '../repositories/IGuildMemberRepository';
import type {IGuildRoleRepository} from '../repositories/IGuildRoleRepository';

interface RoleReorderOperation {
	roleId: RoleID;
	precedingRoleId: RoleID | null;
}

interface GuildAuth {
	guildData: {owner_id: string};
	checkPermission: (permission: bigint) => Promise<void>;
	getMyPermissions: () => Promise<bigint>;
}

type RoleUpdateData = Partial<{
	name: string;
	color: number;
	position: number;
	hoistPosition: number | null;
	permissions: bigint;
	iconHash: string | null;
	unicodeEmoji: string | null;
	hoist: boolean;
	mentionable: boolean;
}>;

export class GuildRoleService {
	constructor(
		private readonly guildRoleRepository: IGuildRoleRepository,
		private readonly guildMemberRepository: IGuildMemberRepository,
		private readonly snowflakeService: SnowflakeService,
		private readonly cacheService: ICacheService,
		private readonly gatewayService: IGatewayService,
		private readonly guildAuditLogService: GuildAuditLogService,
	) {}

	async createRole(
		params: {userId: UserID; guildId: GuildID; data: GuildRoleCreateRequest},
		auditLogReason?: string | null,
	): Promise<GuildRoleResponse> {
		const {userId, guildId, data} = params;
		const {checkPermission, getMyPermissions, guildData} = await this.getGuildAuthenticated({userId, guildId});
		await checkPermission(Permissions.MANAGE_ROLES);

		const currentRoleCount = await this.guildRoleRepository.countRoles(guildId);
		if (currentRoleCount >= MAX_GUILD_ROLES) throw new MaxGuildRolesError();

		const permissions =
			data.permissions !== undefined
				? await this.resolveRequestedPermissions({
						requestedPermissions: data.permissions,
						guildData,
						userId,
						getMyPermissions,
					})
				: DEFAULT_PERMISSIONS;

		const roleId = createRoleID(this.snowflakeService.generate());
		const position = 1;
		const role = await this.guildRoleRepository.upsertRole({
			guild_id: guildId,
			role_id: roleId,
			name: data.name,
			permissions,
			position,
			hoist_position: null,
			color: data.color || 0,
			icon_hash: null,
			unicode_emoji: null,
			hoist: false,
			mentionable: false,
			version: 1,
		});

		await this.dispatchGuildRoleCreate({guildId, role});

		await this.recordAuditLog({
			guildId,
			userId,
			action: AuditLogActionType.ROLE_CREATE,
			targetId: role.id,
			auditLogReason: auditLogReason ?? null,
			changes: this.guildAuditLogService.computeChanges(null, this.serializeRoleForAudit(role)),
		});

		return mapGuildRoleToResponse(role);
	}

	private async resolveRequestedPermissions(params: {
		requestedPermissions: bigint;
		guildData: {owner_id: string};
		userId: UserID;
		getMyPermissions: () => Promise<bigint>;
	}): Promise<bigint> {
		const {requestedPermissions, guildData, userId, getMyPermissions} = params;
		const sanitizedPermissions = requestedPermissions & ALL_PERMISSIONS;
		const isOwner = guildData && guildData.owner_id === userId.toString();
		if (!isOwner) {
			const myPermissions = await getMyPermissions();
			if ((sanitizedPermissions & ~myPermissions) !== 0n) {
				throw new MissingPermissionsError();
			}
		}
		return sanitizedPermissions;
	}

	async updateRole(
		params: {userId: UserID; guildId: GuildID; roleId: RoleID; data: GuildRoleUpdateRequest},
		auditLogReason?: string | null,
	): Promise<GuildRoleResponse> {
		const {userId, guildId, roleId, data} = params;
		const {guildData, checkPermission, getMyPermissions} = await this.getGuildAuthenticated({userId, guildId});
		await checkPermission(Permissions.MANAGE_ROLES);

		const role = await this.guildRoleRepository.getRole(roleId, guildId);
		if (!role || (role.id === guildIdToRoleId(guildId) && roleId !== guildIdToRoleId(guildId))) {
			throw new UnknownGuildRoleError();
		}
		const isOwner = guildData && guildData.owner_id === userId.toString();
		if (!isOwner) {
			const canManageRole = await this.checkCanManageRole({guildId, userId, targetRole: role});
			if (!canManageRole) {
				throw new MissingPermissionsError();
			}
		}
		const previousSnapshot = this.serializeRoleForAudit(role);

		const updateData = await this.buildRoleUpdateData({
			role,
			guildId,
			guildData,
			userId,
			data,
			getMyPermissions,
		});

		const updatedRoleData = {
			...role.toRow(),
			name: updateData.name ?? role.name,
			color: updateData.color ?? role.color,
			position: updateData.position ?? role.position,
			hoist_position: updateData.hoistPosition !== undefined ? updateData.hoistPosition : role.hoistPosition,
			permissions: updateData.permissions ?? role.permissions,
			icon_hash: updateData.iconHash ?? role.iconHash,
			unicode_emoji: updateData.unicodeEmoji ?? role.unicodeEmoji,
			hoist: updateData.hoist ?? role.isHoisted,
			mentionable: updateData.mentionable ?? role.isMentionable,
		};

		const updatedRole = await this.guildRoleRepository.upsertRole(updatedRoleData);

		await this.dispatchGuildRoleUpdate({guildId, role: updatedRole});

		const changes = this.guildAuditLogService.computeChanges(previousSnapshot, this.serializeRoleForAudit(updatedRole));
		if (role.permissions !== updatedRole.permissions) {
			const permissionsDiff = computePermissionsDiff(role.permissions, updatedRole.permissions);
			changes.push({key: 'permissions_diff', new_value: permissionsDiff});
		}

		await this.recordAuditLog({
			guildId,
			userId,
			action: AuditLogActionType.ROLE_UPDATE,
			targetId: roleId,
			auditLogReason: auditLogReason ?? null,
			changes,
		});
		return mapGuildRoleToResponse(updatedRole);
	}

	async deleteRole(
		params: {userId: UserID; guildId: GuildID; roleId: RoleID},
		auditLogReason?: string | null,
	): Promise<void> {
		const {userId, guildId, roleId} = params;
		const {checkPermission, guildData} = await this.getGuildAuthenticated({userId, guildId});
		await checkPermission(Permissions.MANAGE_ROLES);

		const role = await this.guildRoleRepository.getRole(roleId, guildId);
		if (!role || role.id === guildIdToRoleId(guildId)) {
			throw new UnknownGuildRoleError();
		}
		const isOwner = guildData && guildData.owner_id === userId.toString();
		if (!isOwner) {
			const canManageRole = await this.checkCanManageRole({guildId, userId, targetRole: role});
			if (!canManageRole) {
				throw new MissingPermissionsError();
			}
		}
		const previousSnapshot = this.serializeRoleForAudit(role);

		const memberIds = await this.gatewayService.getMembersWithRole({guildId, roleId});
		await Promise.all(
			memberIds.map(async (memberId) => {
				const member = await this.guildMemberRepository.getMember(guildId, memberId);
				if (member?.roleIds.has(roleId)) {
					const updatedRoleIds = new Set(member.roleIds);
					updatedRoleIds.delete(roleId);
					await this.guildMemberRepository.upsertMember({
						...member.toRow(),
						role_ids: updatedRoleIds.size > 0 ? updatedRoleIds : null,
					});
				}
			}),
		);

		await this.guildRoleRepository.deleteRole(guildId, role.id);
		await this.dispatchGuildRoleDelete({guildId, roleId: role.id});

		await this.recordAuditLog({
			guildId,
			userId,
			action: AuditLogActionType.ROLE_DELETE,
			targetId: roleId,
			auditLogReason: auditLogReason ?? null,
			changes: this.guildAuditLogService.computeChanges(previousSnapshot, null),
		});
	}

	async updateRolePositions(
		params: {userId: UserID; guildId: GuildID; updates: Array<{roleId: RoleID; position?: number}>},
		_auditLogReason?: string | null,
	): Promise<void> {
		const {userId, guildId, updates} = params;
		const {checkPermission} = await this.getGuildAuthenticated({userId, guildId});
		await checkPermission(Permissions.MANAGE_ROLES);

		const lockKey = `guild:${guildId}:role-positions`;
		const lockToken = await this.cacheService.acquireLock(lockKey, 30);
		if (!lockToken) {
			throw new ResourceLockedError();
		}

		try {
			await this.updateRolePositionsByList({userId, guildId, updates});
		} finally {
			await this.cacheService.releaseLock(lockKey, lockToken);
		}
	}

	async listRoles(params: {userId: UserID; guildId: GuildID}): Promise<Array<GuildRoleResponse>> {
		const {userId, guildId} = params;
		const {checkPermission} = await this.getGuildAuthenticated({userId, guildId});
		await checkPermission(Permissions.MANAGE_ROLES);
		const roles = await this.guildRoleRepository.listRoles(guildId);
		const sortedRoles = [...roles].sort((a, b) => {
			if (b.position !== a.position) return b.position - a.position;
			return String(a.id).localeCompare(String(b.id));
		});
		return sortedRoles.map(mapGuildRoleToResponse);
	}

	async updateHoistPositions(
		params: {userId: UserID; guildId: GuildID; updates: Array<{roleId: RoleID; hoistPosition: number}>},
		_auditLogReason?: string | null,
	): Promise<void> {
		const {userId, guildId, updates} = params;
		const {checkPermission, guildData} = await this.getGuildAuthenticated({userId, guildId});
		await checkPermission(Permissions.MANAGE_ROLES);

		const lockKey = `guild:${guildId}:role-hoist-positions`;
		const lockToken = await this.cacheService.acquireLock(lockKey, 30);
		if (!lockToken) {
			throw new ResourceLockedError();
		}

		try {
			const allRoles = await this.guildRoleRepository.listRoles(guildId);
			const roleMap = new Map(allRoles.map((r) => [r.id, r]));
			const everyoneRoleId = guildIdToRoleId(guildId);
			const isOwner = guildData && guildData.owner_id === userId.toString();

			let myHighestRole: GuildRole | null = null;
			if (!isOwner) {
				const member = await this.guildMemberRepository.getMember(guildId, userId);
				if (member) {
					myHighestRole = this.getUserHighestRole(member, allRoles);
				}
			}

			const canManageRole = (role: GuildRole): boolean => {
				if (isOwner) return true;
				if (role.id === everyoneRoleId) return false;
				if (!myHighestRole) return false;
				return this.isRoleHigherThan(myHighestRole, role);
			};

			for (const update of updates) {
				if (update.roleId === everyoneRoleId) {
					throw InputValidationError.create('id', 'Cannot set hoist position for the @everyone role');
				}
				const role = roleMap.get(update.roleId);
				if (!role) {
					throw InputValidationError.create('id', `Invalid role ID: ${update.roleId}`);
				}
				if (!canManageRole(role)) {
					throw new MissingPermissionsError();
				}
			}

			const changedRoles: Array<GuildRole> = [];
			for (const update of updates) {
				const role = roleMap.get(update.roleId)!;
				if (role.hoistPosition === update.hoistPosition) continue;

				const updatedRole = await this.guildRoleRepository.upsertRole({
					...role.toRow(),
					hoist_position: update.hoistPosition,
				});
				changedRoles.push(updatedRole);
			}

			if (changedRoles.length > 0) {
				await this.dispatchGuildRoleUpdateBulk({guildId, roles: changedRoles});
			}
		} finally {
			await this.cacheService.releaseLock(lockKey, lockToken);
		}
	}

	async resetHoistPositions(
		params: {userId: UserID; guildId: GuildID},
		_auditLogReason?: string | null,
	): Promise<void> {
		const {userId, guildId} = params;
		const {checkPermission} = await this.getGuildAuthenticated({userId, guildId});
		await checkPermission(Permissions.MANAGE_ROLES);

		const lockKey = `guild:${guildId}:role-hoist-positions`;
		const lockToken = await this.cacheService.acquireLock(lockKey, 30);
		if (!lockToken) {
			throw new ResourceLockedError();
		}

		try {
			const allRoles = await this.guildRoleRepository.listRoles(guildId);
			const changedRoles: Array<GuildRole> = [];

			for (const role of allRoles) {
				if (role.hoistPosition === null) continue;

				const updatedRole = await this.guildRoleRepository.upsertRole({
					...role.toRow(),
					hoist_position: null,
				});
				changedRoles.push(updatedRole);
			}

			if (changedRoles.length > 0) {
				await this.dispatchGuildRoleUpdateBulk({guildId, roles: changedRoles});
			}
		} finally {
			await this.cacheService.releaseLock(lockKey, lockToken);
		}
	}

	private async getGuildAuthenticated({userId, guildId}: {userId: UserID; guildId: GuildID}): Promise<GuildAuth> {
		const guildData = await this.gatewayService.getGuildData({guildId, userId});

		const checkPermission = async (permission: bigint) => {
			const hasPermission = await this.gatewayService.checkPermission({guildId, userId, permission});
			if (!hasPermission) throw new MissingPermissionsError();
		};

		const getMyPermissions = async () => this.gatewayService.getUserPermissions({guildId, userId});

		return {
			guildData,
			checkPermission,
			getMyPermissions,
		};
	}

	private async checkCanManageRole(params: {
		guildId: GuildID;
		userId: UserID;
		targetRole: GuildRole;
	}): Promise<boolean> {
		const {guildId, userId, targetRole} = params;
		return this.gatewayService.canManageRole({guildId, userId, roleId: targetRole.id});
	}

	private getUserHighestRole(member: {roleIds: Set<RoleID>}, allRoles: Array<GuildRole>): GuildRole | null {
		const roleMap = new Map(allRoles.map((r) => [r.id, r]));

		let highestRole: GuildRole | null = null;
		for (const roleId of member.roleIds) {
			const role = roleMap.get(roleId);
			if (!role) continue;

			if (!highestRole) {
				highestRole = role;
			} else {
				if (this.isRoleHigherThan(role, highestRole)) {
					highestRole = role;
				}
			}
		}

		return highestRole;
	}

	private isRoleHigherThan(roleA: GuildRole, roleB: GuildRole): boolean {
		if (roleA.position > roleB.position) {
			return true;
		}

		if (roleA.position === roleB.position) {
			return String(roleA.id) < String(roleB.id);
		}

		return false;
	}

	private async buildRoleUpdateData(params: {
		role: GuildRole;
		guildId: GuildID;
		guildData: {owner_id: string};
		userId: UserID;
		data: GuildRoleUpdateRequest;
		getMyPermissions: () => Promise<bigint>;
	}): Promise<RoleUpdateData> {
		const {role, guildId, guildData, userId, data, getMyPermissions} = params;
		const updateData: RoleUpdateData = {};
		const isEveryoneRole = role.id === guildIdToRoleId(guildId);

		if (data.name !== undefined && !isEveryoneRole) {
			updateData.name = data.name;
		}
		if (data.color !== undefined) {
			updateData.color = data.color;
		}
		if (data.hoist !== undefined && !isEveryoneRole) {
			updateData.hoist = data.hoist;
		}
		if (data.hoist_position !== undefined && !isEveryoneRole) {
			updateData.hoistPosition = data.hoist_position;
		}
		if (data.mentionable !== undefined && !isEveryoneRole) {
			updateData.mentionable = data.mentionable;
		}
		if (data.permissions !== undefined) {
			updateData.permissions = await this.resolveRequestedPermissions({
				requestedPermissions: data.permissions,
				guildData,
				userId,
				getMyPermissions,
			});
		}
		return updateData;
	}

	private async updateRolePositionsByList(params: {
		userId: UserID;
		guildId: GuildID;
		updates: Array<{roleId: RoleID; position?: number}>;
		auditLogReason?: string | null;
	}): Promise<void> {
		const {userId, guildId, updates} = params;
		const {guildData} = await this.getGuildAuthenticated({userId, guildId});
		const allRoles = await this.guildRoleRepository.listRoles(guildId);
		const roleMap = new Map(allRoles.map((r) => [r.id, r]));

		for (const update of updates) {
			if (update.roleId === guildIdToRoleId(guildId)) {
				throw InputValidationError.create('id', 'Cannot reorder the @everyone role');
			}
			if (!roleMap.has(update.roleId)) {
				throw InputValidationError.create('id', `Invalid role ID: ${update.roleId}`);
			}
		}

		const everyoneRoleId = guildIdToRoleId(guildId);
		const isOwner = guildData && guildData.owner_id === userId.toString();

		let myHighestRole: GuildRole | null = null;
		if (!isOwner) {
			const member = await this.guildMemberRepository.getMember(guildId, userId);
			if (member) {
				myHighestRole = this.getUserHighestRole(member, allRoles);
			}
		}

		const canManageRole = (role: GuildRole): boolean => {
			if (isOwner) return true;
			if (role.id === everyoneRoleId) return false;
			if (!myHighestRole) return false;
			return this.isRoleHigherThan(myHighestRole, role);
		};

		for (const update of updates) {
			const role = roleMap.get(update.roleId)!;
			if (!canManageRole(role)) {
				throw new MissingPermissionsError();
			}
		}

		const explicitPositions = new Map<RoleID, number>();
		for (const update of updates) {
			if (update.position !== undefined) {
				explicitPositions.set(update.roleId, update.position);
			}
		}

		const nonEveryoneRoles = allRoles.filter((r) => r.id !== everyoneRoleId);
		const currentOrder = [...nonEveryoneRoles].sort(
			(a, b) => b.position - a.position || String(a.id).localeCompare(String(b.id)),
		);

		const targetOrder = [...currentOrder].sort((a, b) => {
			const posA = explicitPositions.has(a.id) ? explicitPositions.get(a.id)! : a.position;
			const posB = explicitPositions.has(b.id) ? explicitPositions.get(b.id)! : b.position;
			if (posA !== posB) return posB - posA;
			return 0;
		});

		for (const role of currentOrder) {
			if (!canManageRole(role)) {
				const originalIndex = currentOrder.findIndex((r) => r.id === role.id);
				const newIndex = targetOrder.findIndex((r) => r.id === role.id);
				if (originalIndex !== newIndex) {
					throw new MissingPermissionsError();
				}
			}
		}

		const reorderedIds = targetOrder.map((r) => r.id);
		await this.updateRolePositionsLocked({
			userId,
			guildId,
			operation: {roleId: reorderedIds[0]!, precedingRoleId: null},
			customOrder: reorderedIds,
		});
	}

	private async updateRolePositionsLocked(params: {
		userId: UserID;
		guildId: GuildID;
		operation: RoleReorderOperation;
		customOrder?: Array<RoleID>;
	}): Promise<void> {
		const {userId, guildId, operation, customOrder} = params;
		const {guildData} = await this.getGuildAuthenticated({userId, guildId});

		const allRoles = await this.guildRoleRepository.listRoles(guildId);
		const roleMap = new Map(allRoles.map((r) => [r.id, r]));

		const targetRole = roleMap.get(operation.roleId);
		if (!targetRole) {
			throw InputValidationError.create('role_id', `Invalid role ID: ${operation.roleId}`);
		}

		const everyoneRoleId = guildIdToRoleId(guildId);
		if (targetRole.id === everyoneRoleId) {
			throw InputValidationError.create('role_id', 'Cannot reorder the @everyone role');
		}

		let precedingRole: GuildRole | null = null;
		if (!customOrder) {
			if (operation.precedingRoleId) {
				if (operation.precedingRoleId === targetRole.id) {
					throw InputValidationError.create('preceding_role_id', 'Cannot use the same role as a preceding role');
				}
				precedingRole = roleMap.get(operation.precedingRoleId) ?? null;
				if (!precedingRole) {
					throw InputValidationError.create('preceding_role_id', `Invalid role ID: ${operation.precedingRoleId}`);
				}
			}
		}

		const sortedRoles = [...allRoles].sort((a, b) => {
			if (b.position !== a.position) {
				return b.position - a.position;
			}
			return String(a.id).localeCompare(String(b.id));
		});

		const originalIndex = sortedRoles.findIndex((role) => role.id === targetRole.id);
		if (originalIndex === -1) {
			throw new Error('Role ordering inconsistency detected');
		}

		const baseList = sortedRoles.filter((role) => role.id !== targetRole.id);

		let insertIndex = 0;
		if (!customOrder) {
			if (precedingRole) {
				const precedingIndex = baseList.findIndex((role) => role.id === precedingRole!.id);
				if (precedingIndex === -1) {
					throw InputValidationError.create('preceding_role_id', 'Preceding role is not present in the guild');
				}
				insertIndex = precedingIndex + 1;
			}
		}

		const isOwner = guildData && guildData.owner_id === userId.toString();

		let myHighestRole: GuildRole | null = null;
		if (!isOwner) {
			const member = await this.guildMemberRepository.getMember(guildId, userId);
			if (member) {
				myHighestRole = this.getUserHighestRole(member, allRoles);
			}
		}

		const canManageRole = (role: GuildRole): boolean => {
			if (isOwner) return true;
			if (role.id === everyoneRoleId) return false;
			if (!myHighestRole) return false;
			return this.isRoleHigherThan(myHighestRole, role);
		};

		if (!canManageRole(targetRole)) {
			throw new MissingPermissionsError();
		}

		if (insertIndex < originalIndex) {
			const rolesToCross = sortedRoles.slice(insertIndex, originalIndex);
			for (const role of rolesToCross) {
				if (!canManageRole(role)) {
					throw new MissingPermissionsError();
				}
			}
		}

		if (customOrder) {
			baseList.splice(0, baseList.length, ...sortedRoles.filter((role) => role.id !== everyoneRoleId));
		} else {
			baseList.splice(insertIndex, 0, targetRole);
		}

		const finalOrder = customOrder
			? customOrder.filter((roleId) => roleId !== everyoneRoleId)
			: baseList.filter((role) => role.id !== everyoneRoleId).map((role) => role.id);

		const reorderedRoles = this.reorderRolePositions({
			allRoles,
			reorderedIds: finalOrder,
			guildId,
		});

		const updatePromises = reorderedRoles.map((role) => this.guildRoleRepository.upsertRole(role.toRow()));
		await Promise.all(updatePromises);

		const updatedRoles = await this.guildRoleRepository.listRoles(guildId);
		const changedRoles = updatedRoles.filter((role) => {
			const oldRole = roleMap.get(role.id);
			return oldRole && oldRole.position !== role.position;
		});

		if (changedRoles.length > 0) {
			await this.dispatchGuildRoleUpdateBulk({guildId, roles: changedRoles});
		}
	}

	private reorderRolePositions({
		allRoles,
		reorderedIds,
		guildId,
	}: {
		allRoles: Array<GuildRole>;
		reorderedIds: Array<RoleID>;
		guildId: GuildID;
	}): Array<GuildRole> {
		const roleMap = new Map(allRoles.map((r) => [r.id, r]));
		const everyoneRole = roleMap.get(guildIdToRoleId(guildId));
		const reorderedRoleSet = new Set(reorderedIds);

		const nonReorderedRoles = allRoles
			.filter((role) => role.id !== guildIdToRoleId(guildId) && !reorderedRoleSet.has(role.id))
			.sort((a, b) => a.position - b.position);

		const newRoles: Array<GuildRole> = [];

		if (everyoneRole) {
			newRoles.push(new GuildRole({...everyoneRole.toRow(), position: 0}));
		}

		let currentPosition = reorderedIds.length + nonReorderedRoles.length;
		for (const roleId of reorderedIds) {
			const role = roleMap.get(roleId);
			if (role && roleId !== guildIdToRoleId(guildId)) {
				newRoles.push(new GuildRole({...role.toRow(), position: currentPosition}));
				currentPosition--;
			}
		}

		for (const role of nonReorderedRoles) {
			newRoles.push(new GuildRole({...role.toRow(), position: currentPosition}));
			currentPosition--;
		}

		return newRoles;
	}

	private serializeRoleForAudit(role: GuildRole): Record<string, unknown> {
		return {
			role_id: role.id.toString(),
			name: role.name,
			permissions: role.permissions.toString(),
			position: role.position,
			hoist_position: role.hoistPosition,
			color: role.color,
			icon_hash: role.iconHash ?? null,
			unicode_emoji: role.unicodeEmoji ?? null,
			hoist: role.isHoisted,
			mentionable: role.isMentionable,
		};
	}

	private async dispatchGuildRoleCreate({guildId, role}: {guildId: GuildID; role: GuildRole}): Promise<void> {
		await this.gatewayService.dispatchGuild({
			guildId,
			event: 'GUILD_ROLE_CREATE',
			data: {role: mapGuildRoleToResponse(role)},
		});
	}

	private async dispatchGuildRoleUpdate({guildId, role}: {guildId: GuildID; role: GuildRole}): Promise<void> {
		await this.gatewayService.dispatchGuild({
			guildId,
			event: 'GUILD_ROLE_UPDATE',
			data: {role: mapGuildRoleToResponse(role)},
		});
	}

	private async dispatchGuildRoleDelete({guildId, roleId}: {guildId: GuildID; roleId: RoleID}): Promise<void> {
		await this.gatewayService.dispatchGuild({
			guildId,
			event: 'GUILD_ROLE_DELETE',
			data: {role_id: roleId.toString()},
		});
	}

	private async dispatchGuildRoleUpdateBulk({
		guildId,
		roles,
	}: {
		guildId: GuildID;
		roles: Array<GuildRole>;
	}): Promise<void> {
		const roleResponses = roles.map((role) => mapGuildRoleToResponse(role));

		await this.gatewayService.dispatchGuild({
			guildId,
			event: 'GUILD_ROLE_UPDATE_BULK',
			data: {roles: roleResponses},
		});
	}

	private async recordAuditLog(params: {
		guildId: GuildID;
		userId: UserID;
		action: AuditLogActionType;
		targetId?: RoleID | string | null;
		auditLogReason?: string | null;
		metadata?: Map<string, string> | Record<string, string>;
		changes?: GuildAuditLogChange | null;
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
}
