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

import type {GuildID, UserID} from '~/BrandedTypes';
import {mapGuildMemberToResponse} from '~/guild/GuildModel';
import type {IGatewayService} from '~/infrastructure/IGatewayService';
import type {UserCacheService} from '~/infrastructure/UserCacheService';
import type {GuildMember} from '~/Models';
import type {RequestCache} from '~/middleware/RequestCacheMiddleware';

export class GuildMemberEventService {
	constructor(
		private readonly gatewayService: IGatewayService,
		private readonly userCacheService: UserCacheService,
	) {}

	async dispatchGuildMemberAdd({
		member,
		requestCache,
	}: {
		member: GuildMember;
		requestCache: RequestCache;
	}): Promise<void> {
		await this.gatewayService.dispatchGuild({
			guildId: member.guildId,
			event: 'GUILD_MEMBER_ADD',
			data: await mapGuildMemberToResponse(member, this.userCacheService, requestCache),
		});
	}

	async dispatchGuildMemberUpdate({
		guildId,
		member,
		requestCache,
	}: {
		guildId: GuildID;
		member: GuildMember;
		requestCache: RequestCache;
	}): Promise<void> {
		const memberResponse = await mapGuildMemberToResponse(member, this.userCacheService, requestCache);
		await this.gatewayService.dispatchGuild({
			guildId,
			event: 'GUILD_MEMBER_UPDATE',
			data: memberResponse,
		});
	}

	async dispatchGuildMemberRemove({guildId, userId}: {guildId: GuildID; userId: UserID}): Promise<void> {
		await this.gatewayService.dispatchGuild({
			guildId,
			event: 'GUILD_MEMBER_REMOVE',
			data: {user: {id: userId.toString()}},
		});
	}
}
