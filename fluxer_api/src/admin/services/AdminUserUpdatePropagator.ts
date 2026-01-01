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

import type {UserID} from '~/BrandedTypes';
import {mapGuildMemberToResponse} from '~/guild/GuildModel';
import type {IGuildRepository} from '~/guild/IGuildRepository';
import type {IGatewayService} from '~/infrastructure/IGatewayService';
import type {UserCacheService} from '~/infrastructure/UserCacheService';
import type {User} from '~/Models';
import type {RequestCache} from '~/middleware/RequestCacheMiddleware';
import type {IUserRepository} from '~/user/IUserRepository';
import {BaseUserUpdatePropagator} from '~/user/services/BaseUserUpdatePropagator';
import {hasPartialUserFieldsChanged} from '~/user/UserMappers';

interface AdminUserUpdatePropagatorDeps {
	userCacheService: UserCacheService;
	userRepository: IUserRepository;
	guildRepository: IGuildRepository;
	gatewayService: IGatewayService;
}

export class AdminUserUpdatePropagator extends BaseUserUpdatePropagator {
	constructor(private readonly deps: AdminUserUpdatePropagatorDeps) {
		super({
			userCacheService: deps.userCacheService,
			gatewayService: deps.gatewayService,
		});
	}

	async propagateUserUpdate({
		userId,
		oldUser,
		updatedUser,
	}: {
		userId: UserID;
		oldUser: User;
		updatedUser: User;
	}): Promise<void> {
		await this.dispatchUserUpdate(updatedUser);

		if (hasPartialUserFieldsChanged(oldUser, updatedUser)) {
			await this.invalidateUserCache(userId);
			await this.propagateToGuilds(userId);
		}
	}

	private async propagateToGuilds(userId: UserID): Promise<void> {
		const {userRepository, guildRepository, gatewayService, userCacheService} = this.deps;

		const guildIds = await userRepository.getUserGuildIds(userId);
		if (guildIds.length === 0) {
			return;
		}

		const requestCache: RequestCache = {
			userPartials: new Map(),
			clear() {
				this.userPartials.clear();
			},
		};

		for (const guildId of guildIds) {
			const member = await guildRepository.getMember(guildId, userId);
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
}
