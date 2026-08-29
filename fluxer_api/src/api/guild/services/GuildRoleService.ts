// SPDX-License-Identifier: AGPL-3.0-or-later

import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import {ALL_PERMISSIONS, DEFAULT_PERMISSIONS, Permissions} from '@fluxer/constants/src/ChannelConstants';
import type {LimitKey} from '@fluxer/constants/src/LimitConfigMetadata';
import {MAX_GUILD_ROLES} from '@fluxer/constants/src/LimitConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {MissingPermissionsError} from '@fluxer/errors/src/domains/core/MissingPermissionsError';
import {ResourceLockedError} from '@fluxer/errors/src/domains/core/ResourceLockedError';
import {MaxGuildRolesError} from '@fluxer/errors/src/domains/guild/MaxGuildRolesError';
import {UnknownGuildRoleError} from '@fluxer/errors/src/domains/guild/UnknownGuildRoleError';
import type {
	GuildRoleCreateRequest,
	GuildRoleUpdateRequest,
} from '@fluxer/schema/src/domains/guild/GuildRequestSchemas';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import type {GuildRoleResponse} from '@fluxer/schema/src/domains/guild/GuildRoleSchemas';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import type {GuildID, RoleID, UserID} from '../../BrandedTypes';
import {createRoleID, guildIdToRoleId} from '../../BrandedTypes';
import type {IGatewayService} from '../../infrastructure/IGatewayService';
import type {ISnowflakeService} from '../../infrastructure/ISnowflakeService';
import {Logger} from '../../Logger';
import type {LimitConfigService} from '../../limits/LimitConfigService';
import {resolveLimitSafe} from '../../limits/LimitConfigUtils';
import {createLimitMatchContext} from '../../limits/LimitMatchContextBuilder';
import {GuildRole} from '../../models/GuildRole';
import type {IUserRepository} from '../../user/IUserRepository';
import {applyProtectedRolePermissions} from '../../utils/featureUtils';
import {computePermissionsDiff} from '../../utils/PermissionUtils';
import type {GuildAuditLogService} from '../GuildAuditLogService';
import type {GuildAuditLogChange} from '../GuildAuditLogTypes';
import {mapGuildRoleToResponse} from '../GuildModel';
import type {IGuildMemberRepository} from '../repositories/IGuildMemberRepository';
import type {IGuildRoleRepository} from '../repositories/IGuildRoleRepository';
import {createGuildMfaEnforcer} from './GuildMfaEnforcement';

interface GuildRoleRepository extends IGuildRoleRepository, IGuildMemberRepository {}

interface GuildAuth {
	guildData: GuildResponse;
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
		private readonly guildRepository: GuildRoleRepository,
		private readonly snowflakeService: ISnowflakeService,
		private readonly cacheService: ICacheService,
		private readonly gatewayService: IGatewayService,
		private readonly guildAuditLogService: GuildAuditLogService,
		private readonly limitConfigService: LimitConfigService,
		private readonly userRepository: IUserRepository,
	) {}

	async systemCreateRole(params: {
		initiatorId: UserID;
		guildId: GuildID;
		data: GuildRoleCreateRequest;
	}): Promise<GuildRoleResponse> {
		const {initiatorId, guildId, data} = params;
		const guildData = await this.gatewayService.getGuildData({guildId, userId: initiatorId, skipMembershipCheck: true});
		const currentRoleCount = await this.guildRepository.countRoles(guildId);
		const roleLimit = this.resolveGuildLimit(guildData.features, 'max_guild_roles', MAX_GUILD_ROLES);
		if (currentRoleCount >= roleLimit) throw new MaxGuildRolesError(roleLimit);
		const permissions = data.permissions !== undefined ? data.permissions & ALL_PERMISSIONS : DEFAULT_PERMISSIONS;
		const roleId = createRoleID(await this.snowflakeService.generate());
		const position = 1;
		const role = await this.guildRepository.upsertRole({
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
			userId: initiatorId,
			action: AuditLogActionType.ROLE_CREATE,
			targetId: role.id,
			auditLogReason: null,
			changes: this.guildAuditLogService.computeChanges(null, this.serializeRoleForAudit(role)),
		});
		return mapGuildRoleToResponse(role);
	}

	async createRole(
		params: {
			userId: UserID;
			guildId: GuildID;
			data: GuildRoleCreateRequest;
			clientFeatures: ReadonlySet<string>;
		},
		auditLogReason?: string | null,
	): Promise<GuildRoleResponse> {
		const {userId, guildId, data, clientFeatures} = params;
		const {checkPermission, getMyPermissions, guildData} = await this.getGuildAuthenticated({userId, guildId});
		await checkPermission(Permissions.MANAGE_ROLES);
		const currentRoleCount = await this.guildRepository.countRoles(guildId);
		const roleLimit = this.resolveGuildLimit(guildData.features, 'max_guild_roles', MAX_GUILD_ROLES);
		if (currentRoleCount >= roleLimit) throw new MaxGuildRolesError(roleLimit);
		const permissions =
			data.permissions !== undefined
				? await this.resolveRequestedPermissions({
						requestedPermissions: data.permissions,
						existingPermissions: 0n,
						clientFeatures,
						guildData,
						userId,
						getMyPermissions,
					})
				: ((await this.guildRepository.getRole(guildIdToRoleId(guildId), guildId))?.permissions ?? DEFAULT_PERMISSIONS);
		const roleId = createRoleID(await this.snowflakeService.generate());
		const position = 1;
		const role = await this.guildRepository.upsertRole({
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
		existingPermissions: bigint;
		clientFeatures: ReadonlySet<string>;
		guildData: {
			owner_id: string;
		};
		userId: UserID;
		getMyPermissions: () => Promise<bigint>;
	}): Promise<bigint> {
		const {requestedPermissions, existingPermissions, clientFeatures, guildData, userId, getMyPermissions} = params;
		const sanitizedPermissions = applyProtectedRolePermissions(
			requestedPermissions & ALL_PERMISSIONS,
			existingPermissions,
			clientFeatures,
		);
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
		params: {
			userId: UserID;
			guildId: GuildID;
			roleId: RoleID;
			data: GuildRoleUpdateRequest;
			clientFeatures: ReadonlySet<string>;
		},
		auditLogReason?: string | null,
	): Promise<GuildRoleResponse> {
		const {userId, guildId, roleId, data, clientFeatures} = params;
		const {guildData, checkPermission, getMyPermissions} = await this.getGuildAuthenticated({userId, guildId});
		await checkPermission(Permissions.MANAGE_ROLES);
		const role = await this.guildRepository.getRole(roleId, guildId);
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
			clientFeatures,
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
		const updatedRole = await this.guildRepository.upsertRole(updatedRoleData, role.toRow());
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
		params: {
			userId: UserID;
			guildId: GuildID;
			roleId: RoleID;
		},
		auditLogReason?: string | null,
	): Promise<void> {
		const {userId, guildId, roleId} = params;
		const {checkPermission, guildData} = await this.getGuildAuthenticated({userId, guildId});
		await checkPermission(Permissions.MANAGE_ROLES);
		const role = await this.guildRepository.getRole(roleId, guildId);
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
				const member = await this.guildRepository.getMember(guildId, memberId);
				if (member?.roleIds.has(roleId)) {
					const updatedRoleIds = new Set(member.roleIds);
					updatedRoleIds.delete(roleId);
					await this.guildRepository.upsertMember({
						...member.toRow(),
						role_ids: updatedRoleIds.size > 0 ? updatedRoleIds : null,
					});
				}
			}),
		);
		await this.guildRepository.deleteRole(guildId, role.id);
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
		params: {
			userId: UserID;
			guildId: GuildID;
			updates: Array<{
				roleId: RoleID;
				position?: number;
			}>;
		},
		auditLogReason?: string | null,
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
			await this.updateRolePositionsByList({userId, guildId, updates, auditLogReason: auditLogReason ?? null});
		} finally {
			await this.cacheService.releaseLock(lockKey, lockToken);
		}
	}

	async listRoles(params: {userId: UserID; guildId: GuildID}): Promise<Array<GuildRoleResponse>> {
		const {userId, guildId} = params;
		await this.getGuildAuthenticated({userId, guildId});
		const roles = await this.guildRepository.listRoles(guildId);
		const sortedRoles = [...roles].sort((a, b) => {
			if (b.position !== a.position) return b.position - a.position;
			return this.compareRoleIds(a, b);
		});
		return sortedRoles.map(mapGuildRoleToResponse);
	}

	async updateHoistPositions(
		params: {
			userId: UserID;
			guildId: GuildID;
			updates: Array<{
				roleId: RoleID;
				hoistPosition: number;
			}>;
		},
		auditLogReason?: string | null,
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
			const allRoles = await this.guildRepository.listRoles(guildId);
			const roleMap = new Map(allRoles.map((r) => [r.id, r]));
			const everyoneRoleId = guildIdToRoleId(guildId);
			const isOwner = guildData && guildData.owner_id === userId.toString();
			let myHighestRole: GuildRole | null = null;
			if (!isOwner) {
				const member = await this.guildRepository.getMember(guildId, userId);
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
					throw InputValidationError.fromCode('id', ValidationErrorCodes.CANNOT_SET_HOIST_FOR_EVERYONE_ROLE);
				}
				const role = roleMap.get(update.roleId);
				if (!role) {
					throw InputValidationError.fromCode('id', ValidationErrorCodes.INVALID_ROLE_ID, {
						roleId: update.roleId.toString(),
					});
				}
				if (!canManageRole(role)) {
					throw new MissingPermissionsError();
				}
			}
			const changedRoles: Array<GuildRole> = [];
			for (const update of updates) {
				const role = roleMap.get(update.roleId)!;
				if (role.hoistPosition === update.hoistPosition) continue;
				const roleRow = role.toRow();
				const updatedRole = await this.guildRepository.upsertRole(
					{
						...roleRow,
						hoist_position: update.hoistPosition,
					},
					roleRow,
				);
				changedRoles.push(updatedRole);
			}
			if (changedRoles.length > 0) {
				await this.dispatchGuildRoleUpdateBulk({guildId, roles: changedRoles});
				await this.recordRolePositionAuditLogs({guildId, userId, roleMap, changedRoles, auditLogReason});
			}
		} finally {
			await this.cacheService.releaseLock(lockKey, lockToken);
		}
	}

	async resetHoistPositions(
		params: {
			userId: UserID;
			guildId: GuildID;
		},
		auditLogReason?: string | null,
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
			const allRoles = await this.guildRepository.listRoles(guildId);
			const roleMap = new Map(allRoles.map((r) => [r.id, r]));
			const changedRoles: Array<GuildRole> = [];
			for (const role of allRoles) {
				if (role.hoistPosition === null) continue;
				const roleRow = role.toRow();
				const updatedRole = await this.guildRepository.upsertRole(
					{
						...roleRow,
						hoist_position: null,
					},
					roleRow,
				);
				changedRoles.push(updatedRole);
			}
			if (changedRoles.length > 0) {
				await this.dispatchGuildRoleUpdateBulk({guildId, roles: changedRoles});
				await this.recordRolePositionAuditLogs({guildId, userId, roleMap, changedRoles, auditLogReason});
			}
		} finally {
			await this.cacheService.releaseLock(lockKey, lockToken);
		}
	}

	private async getGuildAuthenticated({userId, guildId}: {userId: UserID; guildId: GuildID}): Promise<GuildAuth> {
		const guildData = await this.gatewayService.getGuildData({guildId, userId});
		const enforceGuildMfa = await createGuildMfaEnforcer({userRepository: this.userRepository, guildData, userId});
		const checkPermission = async (permission: bigint) => {
			const hasPermission = await this.gatewayService.checkPermission({guildId, userId, permission});
			if (!hasPermission) throw new MissingPermissionsError();
			enforceGuildMfa(permission);
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

	private getUserHighestRole(
		member: {
			roleIds: Set<RoleID>;
		},
		allRoles: Array<GuildRole>,
	): GuildRole | null {
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

	private compareRoleIds(a: GuildRole, b: GuildRole): number {
		const idA = BigInt(a.id);
		const idB = BigInt(b.id);
		return idA < idB ? -1 : idA > idB ? 1 : 0;
	}

	private isRoleHigherThan(roleA: GuildRole, roleB: GuildRole): boolean {
		if (roleA.position > roleB.position) {
			return true;
		}
		if (roleA.position === roleB.position) {
			return BigInt(roleA.id) < BigInt(roleB.id);
		}
		return false;
	}

	private async buildRoleUpdateData(params: {
		role: GuildRole;
		guildId: GuildID;
		guildData: {
			owner_id: string;
		};
		userId: UserID;
		data: GuildRoleUpdateRequest;
		clientFeatures: ReadonlySet<string>;
		getMyPermissions: () => Promise<bigint>;
	}): Promise<RoleUpdateData> {
		const {role, guildId, guildData, userId, data, clientFeatures, getMyPermissions} = params;
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
				existingPermissions: role.permissions,
				clientFeatures,
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
		updates: Array<{
			roleId: RoleID;
			position?: number;
		}>;
		auditLogReason?: string | null;
	}): Promise<void> {
		const {userId, guildId, updates, auditLogReason} = params;
		const {guildData} = await this.getGuildAuthenticated({userId, guildId});
		const allRoles = await this.guildRepository.listRoles(guildId);
		const roleMap = new Map(allRoles.map((r) => [r.id, r]));
		const everyoneRoleId = guildIdToRoleId(guildId);
		for (const update of updates) {
			if (update.roleId === everyoneRoleId) {
				throw InputValidationError.fromCode('id', ValidationErrorCodes.CANNOT_REORDER_EVERYONE_ROLE);
			}
			if (!roleMap.has(update.roleId)) {
				throw InputValidationError.fromCode('id', ValidationErrorCodes.INVALID_ROLE_ID, {
					roleId: update.roleId.toString(),
				});
			}
		}
		const isOwner = guildData && guildData.owner_id === userId.toString();
		let myHighestRole: GuildRole | null = null;
		if (!isOwner) {
			const member = await this.guildRepository.getMember(guildId, userId);
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
			if (canManageRole(role)) {
				continue;
			}
			if (update.position !== undefined && update.position !== role.position) {
				throw new MissingPermissionsError();
			}
		}
		const explicitPositions = new Map<RoleID, number>();
		for (const update of updates) {
			if (update.position !== undefined) {
				explicitPositions.set(update.roleId, update.position);
			}
		}
		const currentOrder = this.getCurrentRoleOrder(allRoles, everyoneRoleId);
		const targetOrder = this.mergeManageableRoleOrder({
			currentOrder,
			canManageRole,
			explicitPositions,
		});
		const reorderedIds = targetOrder.map((r) => r.id);
		const reorderedRoles = this.reorderRolePositions({allRoles, reorderedIds, guildId});
		const updatePromises = reorderedRoles.map((role) => {
			const roleRow = role.toRow();
			const oldRole = roleMap.get(role.id);
			return this.guildRepository.upsertRole(roleRow, oldRole ? oldRole.toRow() : undefined);
		});
		await Promise.all(updatePromises);
		const updatedRoles = await this.guildRepository.listRoles(guildId);
		const changedRoles = updatedRoles.filter((role) => {
			const oldRole = roleMap.get(role.id);
			return oldRole && oldRole.position !== role.position;
		});
		if (changedRoles.length > 0) {
			await this.dispatchGuildRoleUpdateBulk({guildId, roles: changedRoles});
			await this.recordRolePositionAuditLogs({guildId, userId, roleMap, changedRoles, auditLogReason});
		}
	}

	private getCurrentRoleOrder(allRoles: Array<GuildRole>, everyoneRoleId: RoleID): Array<GuildRole> {
		return allRoles
			.filter((role) => role.id !== everyoneRoleId)
			.sort((a, b) => b.position - a.position || this.compareRoleIds(a, b));
	}

	private mergeManageableRoleOrder(params: {
		currentOrder: Array<GuildRole>;
		canManageRole: (role: GuildRole) => boolean;
		explicitPositions: Map<RoleID, number>;
	}): Array<GuildRole> {
		const {currentOrder, canManageRole, explicitPositions} = params;
		const currentIndexById = new Map(currentOrder.map((role, index) => [role.id, index]));
		const manageableSlotIndices: Array<number> = [];
		const manageableRoles: Array<GuildRole> = [];
		for (const [index, role] of currentOrder.entries()) {
			if (!canManageRole(role)) {
				continue;
			}
			manageableSlotIndices.push(index);
			manageableRoles.push(role);
		}
		const sortedManageableRoles = [...manageableRoles].sort((a, b) => {
			const posA = explicitPositions.has(a.id) ? explicitPositions.get(a.id)! : a.position;
			const posB = explicitPositions.has(b.id) ? explicitPositions.get(b.id)! : b.position;
			if (posA !== posB) {
				return posB - posA;
			}
			return currentIndexById.get(a.id)! - currentIndexById.get(b.id)!;
		});
		const targetOrder = [...currentOrder];
		for (const [index, slot] of manageableSlotIndices.entries()) {
			targetOrder[slot] = sortedManageableRoles[index]!;
		}
		return targetOrder;
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
			.sort((a, b) => a.position - b.position || this.compareRoleIds(a, b));
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

	private async recordRolePositionAuditLogs(params: {
		guildId: GuildID;
		userId: UserID;
		roleMap: Map<RoleID, GuildRole>;
		changedRoles: Array<GuildRole>;
		auditLogReason?: string | null;
	}): Promise<void> {
		const {guildId, userId, roleMap, changedRoles, auditLogReason} = params;
		for (const role of changedRoles) {
			const oldRole = roleMap.get(role.id);
			await this.recordAuditLog({
				guildId,
				userId,
				action: AuditLogActionType.ROLE_UPDATE,
				targetId: role.id,
				auditLogReason: auditLogReason ?? null,
				changes: this.guildAuditLogService.computeChanges(
					oldRole ? this.serializeRoleForAudit(oldRole) : null,
					this.serializeRoleForAudit(role),
				),
			});
		}
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

	private resolveGuildLimit(guildFeatures: Iterable<string> | null, key: LimitKey, fallback: number): number {
		const ctx = createLimitMatchContext({guildFeatures});
		return resolveLimitSafe(this.limitConfigService.getConfigSnapshot(), ctx, key, fallback);
	}
}
