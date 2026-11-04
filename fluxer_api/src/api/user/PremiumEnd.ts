// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserRow} from '@app/api/database/types/UserTypes';
import type {GuildRepository} from '@app/api/guild/repositories/GuildRepository';
import {sanitizeExpiredPremiumMemberPerks} from '@app/api/guild/services/member/PremiumMemberSanitization';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {UserCacheService} from '@app/api/infrastructure/UserCacheService';
import {Logger} from '@app/api/Logger';
import {createRequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import type {User} from '@app/api/models/User';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {createPremiumClearPatch} from '@app/api/user/UserHelpers';
import {dispatchUserUpdate} from '@app/api/user/UserMappers';
import {PremiumFlags} from '@fluxer/constants/src/UserConstants';

interface EndPremiumDeps {
	userRepository: Pick<UserRepository, 'patchUpsert' | 'getUserGuildIds'>;
	guildRepository: Pick<GuildRepository, 'getMember' | 'upsertMember'>;
	userCacheService: UserCacheService;
	gatewayService: IGatewayService;
}

export async function endPremium(
	user: User,
	deps: EndPremiumDeps,
	patch: Partial<UserRow> = createPremiumClearPatch(),
): Promise<{user: User; sanitized: boolean}> {
	let currentUser = await deps.userRepository.patchUpsert(user.id, patch, user.toRow());
	deps.userCacheService.setUserPartialResponseFromUserInBackground(currentUser);
	await dispatchUserUpdate(deps.gatewayService, currentUser);
	if (currentUser.premiumFlags & PremiumFlags.PERKS_SANITIZED) {
		return {user: currentUser, sanitized: false};
	}
	let sanitized = false;
	try {
		sanitized = await sanitizeExpiredPremiumMemberPerks({
			userId: user.id,
			guildIds: await deps.userRepository.getUserGuildIds(user.id),
			guildRepository: deps.guildRepository,
			gatewayService: deps.gatewayService,
			userCacheService: deps.userCacheService,
			requestCache: createRequestCache(),
		});
	} catch (error) {
		Logger.error({userId: user.id.toString(), error}, 'Failed to sanitize guild member perks after premium ended');
	}
	currentUser = await deps.userRepository.patchUpsert(
		currentUser.id,
		{premium_flags: currentUser.premiumFlags | PremiumFlags.PERKS_SANITIZED},
		currentUser.toRow(),
	);
	deps.userCacheService.setUserPartialResponseFromUserInBackground(currentUser);
	await dispatchUserUpdate(deps.gatewayService, currentUser);
	return {user: currentUser, sanitized};
}
