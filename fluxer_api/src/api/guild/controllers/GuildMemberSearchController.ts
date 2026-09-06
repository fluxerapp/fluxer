// SPDX-License-Identifier: AGPL-3.0-or-later

import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {MissingPermissionsError} from '@fluxer/errors/src/domains/core/MissingPermissionsError';
import type {SearchableGuildMember} from '@fluxer/schema/src/contracts/search/SearchDocumentTypes';
import {GuildIdParam} from '@fluxer/schema/src/domains/common/CommonParamSchemas';
import {
	GuildMemberSearchRequest,
	GuildMemberSearchResponse,
	type GuildMemberSearchResponse as GuildMemberSearchResponseBody,
	type GuildMemberSearchResult,
} from '@fluxer/schema/src/domains/guild/GuildMemberSearchSchemas';
import {createGuildID} from '../../BrandedTypes';
import {LoginRequired} from '../../middleware/AuthMiddleware';
import {RateLimitMiddleware} from '../../middleware/RateLimitMiddleware';
import {OpenAPI} from '../../middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '../../RateLimitConfig';
import {getGuildMemberSearchService} from '../../SearchFactory';
import {guildMembersNeedReindexing} from '../../search/GuildMemberIndexingUtils';
import type {HonoApp} from '../../types/HonoEnv';
import {Validator} from '../../Validator';
import {GuildRepository} from '../repositories/GuildRepository';
import {createGuildMfaEnforcer} from '../services/GuildMfaEnforcement';

const MEMBERS_PAGE_PERMISSIONS =
	Permissions.MANAGE_GUILD |
	Permissions.MANAGE_ROLES |
	Permissions.MANAGE_NICKNAMES |
	Permissions.BAN_MEMBERS |
	Permissions.MODERATE_MEMBERS |
	Permissions.KICK_MEMBERS;
const guildRepository = new GuildRepository();

function createEmptySearchResponse(guildId: string, indexing: boolean): GuildMemberSearchResponseBody {
	return {
		guild_id: guildId,
		members: [],
		page_result_count: 0,
		total_result_count: 0,
		indexing,
	};
}

function mapSearchableMember(hit: SearchableGuildMember): GuildMemberSearchResult {
	return {
		id: hit.id,
		guild_id: hit.guildId,
		user_id: hit.userId,
		username: hit.username,
		discriminator: hit.discriminator,
		global_name: hit.globalName,
		nickname: hit.nickname,
		role_ids: hit.roleIds,
		joined_at: hit.joinedAt,
		supplemental: {
			join_source_type: hit.joinSourceType,
			source_invite_code: hit.sourceInviteCode,
			inviter_id: hit.inviterId,
		},
		is_bot: hit.isBot,
	};
}

export function GuildMemberSearchController(app: HonoApp) {
	app.post(
		'/guilds/:guild_id/members-search',
		RateLimitMiddleware(RateLimitConfigs.GUILD_MEMBERS),
		LoginRequired,
		Validator('param', GuildIdParam),
		Validator('json', GuildMemberSearchRequest),
		OpenAPI({
			operationId: 'search_guild_members',
			summary: 'Search guild members',
			responseSchema: GuildMemberSearchResponse,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Guilds'],
			description: 'Search and filter guild members with pagination support.',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const guildId = createGuildID(ctx.req.valid('param').guild_id);
			const guildIdString = guildId.toString();
			const body = ctx.req.valid('json');
			const {getMyPermissions, guildData} = await ctx.get('guildService').getGuildAuthenticated({userId, guildId});
			const permissions = await getMyPermissions();
			if ((permissions & MEMBERS_PAGE_PERMISSIONS) === 0n) {
				throw new MissingPermissionsError();
			}
			const enforceGuildMfa = await createGuildMfaEnforcer({
				userRepository: ctx.get('userRepository'),
				guildData,
				userId,
			});
			enforceGuildMfa(permissions & MEMBERS_PAGE_PERMISSIONS);
			const canViewInvites = (permissions & Permissions.MANAGE_GUILD) !== 0n;
			const guild = await guildRepository.findUnique(guildId);
			if (!guild) {
				return ctx.json(createEmptySearchResponse(guildIdString, false));
			}
			const searchService = getGuildMemberSearchService();
			if (!searchService || !searchService.isAvailable()) {
				return ctx.json(createEmptySearchResponse(guildIdString, false));
			}
			const needsIndexing = guildMembersNeedReindexing(guild.membersIndexedAt);
			if (needsIndexing) {
				const workerService = ctx.get('workerService');
				await workerService.addJob(
					'indexGuildMembers',
					{
						guildId: guildIdString,
					},
					{jobKey: `index-guild-members-${guildId}-lazy`, maxAttempts: 3},
				);
				return ctx.json(createEmptySearchResponse(guildIdString, true));
			}
			const query = body.query?.trim() ?? '';
			const limit = body.limit ?? 25;
			const offset = body.offset ?? 0;
			const results = await searchService.searchMembers(
				query,
				{
					guildId: guildIdString,
					roleIds: body.role_ids,
					joinedAtGte: body.joined_at_gte,
					joinedAtLte: body.joined_at_lte,
					joinSourceType: canViewInvites ? body.join_source_type : undefined,
					sourceInviteCode: canViewInvites ? body.source_invite_code : undefined,
					userCreatedAtGte: body.user_created_at_gte,
					userCreatedAtLte: body.user_created_at_lte,
					isBot: body.is_bot,
					sortBy: body.sort_by,
					sortOrder: body.sort_order,
				},
				{limit, offset},
			);
			const members = results.hits.map((hit) => {
				const member = mapSearchableMember(hit);
				if (!canViewInvites) {
					member.supplemental = {
						join_source_type: null,
						source_invite_code: null,
						inviter_id: null,
					};
				}
				return member;
			});
			return ctx.json({
				guild_id: guildIdString,
				members,
				page_result_count: members.length,
				total_result_count: results.total,
				indexing: false,
			});
		},
	);
}
