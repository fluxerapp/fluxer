// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AdminAuditService} from '@app/api/admin/services/AdminAuditService';
import type {UserID} from '@app/api/BrandedTypes';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {ISnowflakeService} from '@app/api/infrastructure/ISnowflakeService';
import type {InstanceConfigRepository} from '@app/api/instance/InstanceConfigRepository';
import {type BuiltinBadge, MAX_BADGES_PER_TYPE} from '@fluxer/constants/src/BadgeConstants';
import {MaxBadgesError} from '@fluxer/errors/src/domains/badge/MaxBadgesError';
import {UnknownBadgeError} from '@fluxer/errors/src/domains/badge/UnknownBadgeError';
import type {
	BadgeMutationResponse,
	CreateBadgeRequest,
	UpdateBadgeRequest,
} from '@fluxer/schema/src/domains/admin/AdminBadgeSchemas';
import type {
	BadgeResponse,
	BadgesResponse,
	BadgesUpdateDispatchData,
	BuiltinBadgeIconsResponse,
} from '@fluxer/schema/src/domains/badge/BadgeSchemas';

interface AdminBadgeServiceDeps {
	instanceConfigRepository: InstanceConfigRepository;
	snowflakeService: ISnowflakeService;
	gatewayService: IGatewayService;
	auditService: AdminAuditService;
}

interface BadgeAuditParams {
	adminUserId: UserID;
	auditLogReason: string | null;
	targetId: bigint;
	action: string;
	metadata: Map<string, string>;
}

function findBadge(config: BadgesResponse, badgeId: string): BadgeResponse {
	const badge = config.badges.find((existing) => existing.id === badgeId);
	if (!badge) {
		throw new UnknownBadgeError();
	}
	return badge;
}

export class AdminBadgeService {
	constructor(private readonly deps: AdminBadgeServiceDeps) {}

	listBadges(): Promise<BadgesResponse> {
		return this.deps.instanceConfigRepository.getBadgeConfig();
	}

	async createBadge(
		data: CreateBadgeRequest,
		adminUserId: UserID,
		auditLogReason: string | null,
	): Promise<BadgeMutationResponse> {
		const badgeId = (await this.deps.snowflakeService.generate()).toString();
		const config = await this.deps.instanceConfigRepository.updateBadgeConfig((current) => {
			const count = current.badges.filter((badge) => badge.type === data.type).length;
			if (count >= MAX_BADGES_PER_TYPE) {
				throw new MaxBadgesError({maxBadges: MAX_BADGES_PER_TYPE});
			}
			const badge: BadgeResponse = {
				id: badgeId,
				type: data.type,
				name: data.name,
				tooltip: data.tooltip,
				icon: data.icon,
				url: data.url ?? null,
				position: data.position ?? count,
			};
			return {...current, badges: [...current.badges, badge]};
		});
		const badge = findBadge(config, badgeId);
		await this.dispatchBadgesUpdate({version: config.version, badges: [badge]});
		await this.createBadgeAuditLog({
			adminUserId,
			auditLogReason,
			targetId: BigInt(badgeId),
			action: 'create_badge',
			metadata: new Map([
				['type', badge.type],
				['name', badge.name],
			]),
		});
		return {badge};
	}

	async updateBadge(
		badgeId: bigint,
		data: UpdateBadgeRequest,
		adminUserId: UserID,
		auditLogReason: string | null,
	): Promise<BadgeMutationResponse> {
		const id = badgeId.toString();
		const config = await this.deps.instanceConfigRepository.updateBadgeConfig((current) => {
			const existing = findBadge(current, id);
			const updated: BadgeResponse = {
				...existing,
				name: data.name ?? existing.name,
				tooltip: data.tooltip ?? existing.tooltip,
				icon: data.icon ?? existing.icon,
				url: data.url === undefined ? existing.url : data.url,
				position: data.position ?? existing.position,
			};
			return {...current, badges: current.badges.map((badge) => (badge.id === id ? updated : badge))};
		});
		const badge = findBadge(config, id);
		await this.dispatchBadgesUpdate({version: config.version, badges: [badge]});
		await this.createBadgeAuditLog({
			adminUserId,
			auditLogReason,
			targetId: badgeId,
			action: 'update_badge',
			metadata: new Map([['fields', Object.keys(data).join(',')]]),
		});
		return {badge};
	}

	async deleteBadge(badgeId: bigint, adminUserId: UserID, auditLogReason: string | null): Promise<{success: true}> {
		const id = badgeId.toString();
		const {instanceConfigRepository} = this.deps;
		const deleted = findBadge(await instanceConfigRepository.getBadgeConfig(), id);
		const config = await instanceConfigRepository.updateBadgeConfig((current) => ({
			...current,
			badges: current.badges.filter((badge) => badge.id !== id),
		}));
		await this.dispatchBadgesUpdate({version: config.version, deleted_badge_ids: [id]});
		await this.createBadgeAuditLog({
			adminUserId,
			auditLogReason,
			targetId: badgeId,
			action: 'delete_badge',
			metadata: new Map([
				['type', deleted.type],
				['name', deleted.name],
			]),
		});
		return {success: true};
	}

	async updateBuiltinBadgeIcon(
		builtinBadge: BuiltinBadge,
		icon: string | null,
		adminUserId: UserID,
		auditLogReason: string | null,
	): Promise<BuiltinBadgeIconsResponse> {
		const config = await this.deps.instanceConfigRepository.updateBadgeConfig((current) => ({
			...current,
			builtin_icons: {...current.builtin_icons, [builtinBadge]: icon},
		}));
		await this.dispatchBadgesUpdate({version: config.version, builtin_icons: {[builtinBadge]: icon}});
		await this.createBadgeAuditLog({
			adminUserId,
			auditLogReason,
			targetId: 0n,
			action: 'update_builtin_badge_icon',
			metadata: new Map([
				['badge', builtinBadge],
				['reset', String(icon === null)],
			]),
		});
		return config.builtin_icons;
	}

	private async dispatchBadgesUpdate(data: BadgesUpdateDispatchData): Promise<void> {
		await this.deps.gatewayService.broadcastDispatch({event: 'BADGES_UPDATE', data});
	}

	private async createBadgeAuditLog({
		adminUserId,
		auditLogReason,
		targetId,
		action,
		metadata,
	}: BadgeAuditParams): Promise<void> {
		await this.deps.auditService.createAuditLog({
			adminUserId,
			targetType: 'badge',
			targetId,
			action,
			auditLogReason,
			metadata,
		});
	}
}
