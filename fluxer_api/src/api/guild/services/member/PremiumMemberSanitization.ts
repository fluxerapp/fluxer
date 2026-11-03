// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildID, UserID} from '@app/api/BrandedTypes';
import {mapGuildMemberToResponse} from '@app/api/guild/GuildModel';
import type {GuildRepository} from '@app/api/guild/repositories/GuildRepository';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {UserCacheService} from '@app/api/infrastructure/UserCacheService';
import {Logger} from '@app/api/Logger';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';

export async function sanitizeExpiredPremiumMemberPerks({
	userId,
	guildIds,
	guildRepository,
	gatewayService,
	userCacheService,
	requestCache,
}: {
	userId: UserID;
	guildIds: ReadonlyArray<GuildID>;
	guildRepository: Pick<GuildRepository, 'getMember' | 'upsertMember'>;
	gatewayService: Pick<IGatewayService, 'dispatchGuild'>;
	userCacheService: Pick<UserCacheService, 'getUserPartialResponse'>;
	requestCache: RequestCache;
}): Promise<boolean> {
	if (guildIds.length === 0) return false;
	const members = await Promise.all(
		guildIds.map(async (guildId) => {
			try {
				const member = await guildRepository.getMember(guildId, userId);
				return {guildId, member, error: null};
			} catch (error) {
				Logger.error(
					{userId: userId.toString(), guildId: guildId.toString(), error},
					'Failed to fetch guild member for expired premium sweep sanitization',
				);
				return {guildId, member: null, error};
			}
		}),
	);
	const membersToSanitize = members.filter(
		({member, error}) =>
			!error &&
			member &&
			!member.isPremiumSanitized &&
			(member.avatarHash || member.bannerHash || member.bio || member.accentColor !== null),
	);
	if (membersToSanitize.length === 0) {
		return false;
	}
	const updatePromises = membersToSanitize.map(({guildId, member}) =>
		guildRepository
			.upsertMember({
				...member!.toRow(),
				is_premium_sanitized: true,
			})
			.then((updatedMember) => ({guildId, updatedMember, error: null}))
			.catch((error) => {
				Logger.error(
					{userId: userId.toString(), guildId: guildId.toString(), error},
					'Failed to upsert guild member during expired premium sweep sanitization',
				);
				return {guildId, updatedMember: null, error};
			}),
	);
	const updatedResults = await Promise.all(updatePromises);
	const dispatchPromises = updatedResults.map(async ({guildId, updatedMember, error}) => {
		if (error || !updatedMember) return;
		try {
			await gatewayService.dispatchGuild({
				guildId,
				event: 'GUILD_MEMBER_UPDATE',
				data: await mapGuildMemberToResponse(updatedMember, userCacheService, requestCache),
			});
		} catch (error) {
			Logger.error(
				{userId: userId.toString(), guildId: guildId.toString(), error},
				'Failed to dispatch guild member update during expired premium sweep sanitization',
			);
		}
	});
	await Promise.all(dispatchPromises);
	return true;
}
