// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildID, UserID} from '@app/api/BrandedTypes';
import {mapGuildMemberToResponse} from '@app/api/guild/GuildModel';
import type {IGuildRepositoryAggregate} from '@app/api/guild/repositories/IGuildRepositoryAggregate';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {UserCacheService} from '@app/api/infrastructure/UserCacheService';
import {createRequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import type {User} from '@app/api/models/User';
import {updateUserCache} from '@app/api/user/UserCacheHelpers';

interface UserGuildIdReader {
	getUserGuildIds(userId: UserID): Promise<Array<GuildID>>;
}

export interface PartialUserChangePropagationDeps {
	userCacheService: UserCacheService;
	gatewayService: IGatewayService;
	userRepository: UserGuildIdReader;
	guildRepository: Pick<IGuildRepositoryAggregate, 'getMember'>;
}

export async function propagatePartialUserChange(deps: PartialUserChangePropagationDeps, user: User): Promise<void> {
	const {userCacheService, gatewayService, userRepository, guildRepository} = deps;
	await updateUserCache({user, userCacheService});
	const guildIds = await userRepository.getUserGuildIds(user.id);
	if (guildIds.length === 0) {
		return;
	}
	const requestCache = createRequestCache();
	for (const guildId of guildIds) {
		const member = await guildRepository.getMember(guildId, user.id);
		if (!member) {
			continue;
		}
		const memberResponse = await mapGuildMemberToResponse(member, userCacheService, requestCache);
		await gatewayService.dispatchGuild({
			guildId,
			event: 'GUILD_MEMBER_UPDATE',
			data: memberResponse,
		});
	}
	requestCache.clear();
}
