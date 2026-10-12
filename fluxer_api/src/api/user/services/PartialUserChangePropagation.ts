// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildID, UserID} from '@app/api/BrandedTypes';
import {mapGuildMemberToResponse} from '@app/api/guild/GuildModel';
import type {GuildRepository} from '@app/api/guild/repositories/GuildRepository';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {UserCacheService} from '@app/api/infrastructure/UserCacheService';
import {createRequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import type {User} from '@app/api/models/User';
import {dispatchUserUpdate, hasPartialUserFieldsChanged} from '@app/api/user/UserMappers';
import {runAllInOrder} from '@app/api/utils/ConcurrencyUtils';

interface UserGuildIdReader {
	getUserGuildIds(userId: UserID): Promise<Array<GuildID>>;
}

export interface PartialUserChangePropagationDeps {
	userCacheService: UserCacheService;
	gatewayService: IGatewayService;
	userRepository: UserGuildIdReader;
	guildRepository: Pick<GuildRepository, 'getMember'>;
}

export async function propagatePartialUserChange(deps: PartialUserChangePropagationDeps, user: User): Promise<void> {
	const {userCacheService, gatewayService, userRepository, guildRepository} = deps;
	await userCacheService.setUserPartialResponseFromUser(user);
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

export async function propagateUserUpdate(
	deps: PartialUserChangePropagationDeps,
	oldUser: User,
	updatedUser: User,
	options: {continueAfterDispatchFailure?: boolean} = {},
): Promise<void> {
	const dispatch = () => dispatchUserUpdate(deps.gatewayService, updatedUser);
	const partial = async () => {
		if (hasPartialUserFieldsChanged(oldUser, updatedUser)) await propagatePartialUserChange(deps, updatedUser);
	};
	if (options.continueAfterDispatchFailure) {
		await runAllInOrder([dispatch, partial], 'Failed to propagate user update');
	} else {
		await dispatch();
		await partial();
	}
}
