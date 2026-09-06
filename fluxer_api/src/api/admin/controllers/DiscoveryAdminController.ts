// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {DiscoveryApplicationStatus, DiscoveryCategoryLabels} from '@fluxer/constants/src/DiscoveryConstants';
import {GuildIdParam} from '@fluxer/schema/src/domains/common/CommonParamSchemas';
import {
	DiscoveryAdminApplicationUpdateRequest,
	DiscoveryAdminCategoryListingQuery,
	DiscoveryAdminListedGuildResponse,
	DiscoveryAdminListingBulkCategoryRequest,
	DiscoveryAdminListingBulkCategoryResponse,
	DiscoveryAdminPendingApplicationResponse,
	DiscoveryAdminRemoveRequest,
	DiscoveryApplicationPatchRequest,
	DiscoveryApplicationResponse,
	DiscoveryCategoryIdParam,
	DiscoveryCategoryListResponse,
} from '@fluxer/schema/src/domains/guild/GuildDiscoverySchemas';
import {z} from 'zod';
import {createGuildID} from '../../BrandedTypes';
import type {GuildDiscoveryRow} from '../../database/types/GuildDiscoveryTypes';
import {mapGuildFeatures} from '../../guild/GuildFeatureUtils';
import type {GuildService} from '../../guild/services/GuildService';
import {requireAdminACL} from '../../middleware/AdminMiddleware';
import {RateLimitMiddleware} from '../../middleware/RateLimitMiddleware';
import {OpenAPI} from '../../middleware/ResponseTypeMiddleware';
import type {User} from '../../models/User';
import {RateLimitConfigs} from '../../RateLimitConfig';
import type {HonoApp} from '../../types/HonoEnv';
import type {IUserRepository} from '../../user/IUserRepository';
import {Validator} from '../../Validator';

function mapRowToApplicationResponse(row: GuildDiscoveryRow) {
	return {
		guild_id: row.guild_id.toString(),
		status: row.status,
		description: row.description,
		category_type: row.category_type,
		primary_language: row.primary_language ?? null,
		custom_tags: row.custom_tags ?? [],
		applied_at: row.applied_at.toISOString(),
		reviewed_at: row.reviewed_at?.toISOString() ?? null,
		review_reason: row.review_reason ?? null,
		removed_at: row.removed_at?.toISOString() ?? null,
		removal_reason: row.removal_reason ?? null,
	};
}

interface GuildEnrichment {
	name: string;
	icon: string | null;
	owner_id: string;
	owner_username: string | null;
	owner_global_name: string | null;
	owner_discriminator: string | null;
	member_count: number;
	nsfw_level: number | null;
	features: Array<string>;
}

async function enrichGuilds(
	rows: ReadonlyArray<GuildDiscoveryRow>,
	guildService: GuildService,
	userRepository: IUserRepository,
): Promise<Map<string, GuildEnrichment>> {
	const map = new Map<string, GuildEnrichment>();
	const guilds = await Promise.all(
		rows.map(async (row) => {
			try {
				return await guildService.data.getGuildSystem(row.guild_id);
			} catch {
				return null;
			}
		}),
	);
	const ownerIds = [...new Set(guilds.filter((g) => g != null).map((g) => g.ownerId))];
	const owners = await userRepository.listUsers(ownerIds);
	const ownerMap = new Map<string, User>();
	for (const owner of owners) {
		ownerMap.set(owner.id.toString(), owner);
	}
	for (let i = 0; i < rows.length; i++) {
		const guild = guilds[i];
		const guildId = rows[i].guild_id.toString();
		if (!guild) continue;
		const owner = ownerMap.get(guild.ownerId.toString()) ?? null;
		map.set(guildId, {
			name: guild.name,
			icon: guild.iconHash,
			owner_id: guild.ownerId.toString(),
			owner_username: owner?.username ?? null,
			owner_global_name: owner?.globalName ?? null,
			owner_discriminator: owner ? String(owner.discriminator).padStart(4, '0') : null,
			member_count: guild.memberCount,
			nsfw_level: guild.nsfwLevel,
			features: mapGuildFeatures(guild.features),
		});
	}
	return map;
}

function mapPendingResponse(row: GuildDiscoveryRow, enrichment: GuildEnrichment | undefined) {
	const guildId = row.guild_id.toString();
	return {
		guild_id: guildId,
		guild_name: enrichment?.name ?? '(unknown guild)',
		guild_icon: enrichment?.icon ?? null,
		guild_owner_id: enrichment?.owner_id ?? '0',
		guild_owner_username: enrichment?.owner_username ?? null,
		guild_owner_global_name: enrichment?.owner_global_name ?? null,
		guild_owner_discriminator: enrichment?.owner_discriminator ?? null,
		guild_member_count: enrichment?.member_count ?? 0,
		guild_nsfw_level: enrichment?.nsfw_level ?? null,
		guild_features: enrichment?.features ?? [],
		description: row.description,
		category_type: row.category_type,
		primary_language: row.primary_language ?? null,
		custom_tags: row.custom_tags ?? [],
		applied_at: row.applied_at.toISOString(),
	};
}

function mapListedResponse(row: GuildDiscoveryRow, enrichment: GuildEnrichment | undefined) {
	const base = mapPendingResponse(row, enrichment);
	return {
		...base,
		approved_at: row.reviewed_at?.toISOString() ?? null,
	};
}

export function DiscoveryAdminController(app: HonoApp) {
	app.get(
		'/admin/discovery/applications',
		RateLimitMiddleware(RateLimitConfigs.DISCOVERY_ADMIN_LIST),
		requireAdminACL(AdminACLs.DISCOVERY_REVIEW),
		OpenAPI({
			operationId: 'list_admin_discovery_applications',
			summary: 'List discovery applications',
			description:
				'Returns every pending discovery application, enriched with guild metadata. No pagination. Requires DISCOVERY_REVIEW permission.',
			responseSchema: z.array(DiscoveryAdminPendingApplicationResponse),
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const discoveryService = ctx.get('discoveryService');
			const guildService = ctx.get('guildService');
			const userRepository = ctx.get('userRepository');
			const rows = await discoveryService.listByStatus({status: DiscoveryApplicationStatus.PENDING});
			const enrichment = await enrichGuilds(rows, guildService, userRepository);
			return ctx.json(rows.map((row) => mapPendingResponse(row, enrichment.get(row.guild_id.toString()))));
		},
	);
	app.patch(
		'/admin/discovery/applications/:guild_id',
		RateLimitMiddleware(RateLimitConfigs.DISCOVERY_ADMIN_ACTION),
		requireAdminACL(AdminACLs.DISCOVERY_REVIEW),
		Validator('param', GuildIdParam),
		Validator('json', DiscoveryAdminApplicationUpdateRequest),
		OpenAPI({
			operationId: 'update_admin_discovery_application',
			summary: 'Review discovery application',
			description: 'Approve or reject a pending discovery application. Requires DISCOVERY_REVIEW permission.',
			responseSchema: DiscoveryApplicationResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const {guild_id} = ctx.req.valid('param');
			const guildId = createGuildID(guild_id);
			const data = ctx.req.valid('json');
			const adminUserId = ctx.get('adminUserId');
			const discoveryService = ctx.get('discoveryService');
			const row =
				data.status === DiscoveryApplicationStatus.APPROVED
					? await discoveryService.approve({guildId, adminUserId, reason: data.reason})
					: await discoveryService.reject({guildId, adminUserId, reason: data.reason});
			return ctx.json(mapRowToApplicationResponse(row));
		},
	);
	app.get(
		'/admin/discovery/categories',
		RateLimitMiddleware(RateLimitConfigs.DISCOVERY_ADMIN_LIST),
		requireAdminACL(AdminACLs.DISCOVERY_REVIEW),
		OpenAPI({
			operationId: 'list_admin_discovery_categories',
			summary: 'List discovery categories',
			description:
				'Returns every discovery category a listing can be filed under. Requires DISCOVERY_REVIEW permission.',
			responseSchema: DiscoveryCategoryListResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			return ctx.json(
				Object.entries(DiscoveryCategoryLabels).map(([id, name]) => ({
					id: Number(id),
					name,
				})),
			);
		},
	);
	app.get(
		'/admin/discovery/categories/:category_id/listings',
		RateLimitMiddleware(RateLimitConfigs.DISCOVERY_ADMIN_LIST),
		requireAdminACL(AdminACLs.DISCOVERY_REVIEW),
		Validator('param', DiscoveryCategoryIdParam),
		Validator('query', DiscoveryAdminCategoryListingQuery),
		OpenAPI({
			operationId: 'list_admin_discovery_category_listings',
			summary: 'List guilds in a discovery category',
			description:
				'Returns an offset page of the guilds listed under one discovery category, most members first, enriched with guild metadata. Requires DISCOVERY_REVIEW permission.',
			responseSchema: z.array(DiscoveryAdminListedGuildResponse),
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const {category_id} = ctx.req.valid('param');
			const {limit, offset} = ctx.req.valid('query');
			const discoveryService = ctx.get('discoveryService');
			const guildService = ctx.get('guildService');
			const userRepository = ctx.get('userRepository');
			const rows = await discoveryService.listByStatus({status: DiscoveryApplicationStatus.APPROVED});
			const inCategory = rows.filter((row) => row.category_type === category_id);
			const enrichment = await enrichGuilds(inCategory, guildService, userRepository);
			const sorted = [...inCategory].sort(
				(left, right) =>
					(enrichment.get(right.guild_id.toString())?.member_count ?? 0) -
					(enrichment.get(left.guild_id.toString())?.member_count ?? 0),
			);
			return ctx.json(
				sorted
					.slice(offset, offset + limit)
					.map((row) => mapListedResponse(row, enrichment.get(row.guild_id.toString()))),
			);
		},
	);
	app.get(
		'/admin/discovery/listings',
		RateLimitMiddleware(RateLimitConfigs.DISCOVERY_ADMIN_LIST),
		requireAdminACL(AdminACLs.DISCOVERY_REVIEW),
		OpenAPI({
			operationId: 'list_admin_discovery_listings',
			summary: 'List discovery listings',
			description:
				'Returns every approved/listed discovery guild, enriched with guild metadata. No pagination. Requires DISCOVERY_REVIEW permission.',
			responseSchema: z.array(DiscoveryAdminListedGuildResponse),
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const discoveryService = ctx.get('discoveryService');
			const guildService = ctx.get('guildService');
			const userRepository = ctx.get('userRepository');
			const rows = await discoveryService.listByStatus({status: DiscoveryApplicationStatus.APPROVED});
			const enrichment = await enrichGuilds(rows, guildService, userRepository);
			return ctx.json(rows.map((row) => mapListedResponse(row, enrichment.get(row.guild_id.toString()))));
		},
	);
	app.patch(
		'/admin/discovery/listings',
		RateLimitMiddleware(RateLimitConfigs.DISCOVERY_ADMIN_ACTION),
		requireAdminACL(AdminACLs.DISCOVERY_REVIEW),
		Validator('json', DiscoveryAdminListingBulkCategoryRequest),
		OpenAPI({
			operationId: 'bulk_update_admin_discovery_listing_category',
			summary: 'Move discovery listings to a category',
			description:
				'Files every named discovery listing under one category. Every guild is attempted and the ones that could not be moved are reported. Requires DISCOVERY_REVIEW permission.',
			responseSchema: DiscoveryAdminListingBulkCategoryResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const data = ctx.req.valid('json');
			const adminUserId = ctx.get('adminUserId');
			const discoveryService = ctx.get('discoveryService');
			const guildIds = [...new Set(data.guild_ids)];
			const failed: Array<string> = [];
			let updated = 0;
			for (const rawGuildId of guildIds) {
				try {
					await discoveryService.editApplication({
						guildId: createGuildID(rawGuildId),
						userId: adminUserId,
						data: {category_type: data.category_type},
					});
					updated += 1;
				} catch {
					failed.push(rawGuildId.toString());
				}
			}
			return ctx.json({updated, failed_guild_ids: failed});
		},
	);
	app.patch(
		'/admin/discovery/listings/:guild_id',
		RateLimitMiddleware(RateLimitConfigs.DISCOVERY_ADMIN_ACTION),
		requireAdminACL(AdminACLs.DISCOVERY_REVIEW),
		Validator('param', GuildIdParam),
		Validator('json', DiscoveryApplicationPatchRequest),
		OpenAPI({
			operationId: 'update_admin_discovery_listing',
			summary: 'Update discovery listing',
			description:
				'Edit the description, category, language, or tags of a discovery listing without delisting the guild. Requires DISCOVERY_REVIEW permission.',
			responseSchema: DiscoveryApplicationResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const {guild_id} = ctx.req.valid('param');
			const guildId = createGuildID(guild_id);
			const data = ctx.req.valid('json');
			const adminUserId = ctx.get('adminUserId');
			const discoveryService = ctx.get('discoveryService');
			const row = await discoveryService.editApplication({guildId, userId: adminUserId, data});
			return ctx.json(mapRowToApplicationResponse(row));
		},
	);
	app.delete(
		'/admin/discovery/listings/:guild_id',
		RateLimitMiddleware(RateLimitConfigs.DISCOVERY_ADMIN_ACTION),
		requireAdminACL(AdminACLs.DISCOVERY_REMOVE),
		Validator('param', GuildIdParam),
		Validator('json', DiscoveryAdminRemoveRequest),
		OpenAPI({
			operationId: 'delete_admin_discovery_listing',
			summary: 'Remove discovery listing',
			description: 'Remove an approved guild from discovery. Requires DISCOVERY_REMOVE permission.',
			responseSchema: DiscoveryApplicationResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const {guild_id} = ctx.req.valid('param');
			const guildId = createGuildID(guild_id);
			const data = ctx.req.valid('json');
			const adminUserId = ctx.get('adminUserId');
			const discoveryService = ctx.get('discoveryService');
			const row = await discoveryService.remove({guildId, adminUserId, reason: data.reason});
			return ctx.json(mapRowToApplicationResponse(row));
		},
	);
}
