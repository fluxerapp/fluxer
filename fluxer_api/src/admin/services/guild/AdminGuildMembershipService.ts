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

import {createGuildID, createUserID, type UserID} from '~/BrandedTypes';
import {UnknownUserError} from '~/Errors';
import type {GuildService} from '~/guild/services/GuildService';
import type {RequestCache} from '~/middleware/RequestCacheMiddleware';
import type {IUserRepository} from '~/user/IUserRepository';
import type {BulkAddGuildMembersRequest, ForceAddUserToGuildRequest} from '../../AdminModel';
import type {AdminAuditService} from '../AdminAuditService';

interface AdminGuildMembershipServiceDeps {
	userRepository: IUserRepository;
	guildService: GuildService;
	auditService: AdminAuditService;
}

export class AdminGuildMembershipService {
	constructor(private readonly deps: AdminGuildMembershipServiceDeps) {}

	async forceAddUserToGuild({
		data,
		requestCache,
		adminUserId,
		auditLogReason,
	}: {
		data: ForceAddUserToGuildRequest;
		requestCache: RequestCache;
		adminUserId: UserID;
		auditLogReason: string | null;
	}) {
		const {userRepository, guildService, auditService} = this.deps;
		const userId = createUserID(data.user_id);
		const guildId = createGuildID(data.guild_id);

		const user = await userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}

		await guildService.addUserToGuild({
			userId,
			guildId,
			sendJoinMessage: true,
			skipBanCheck: true,
			requestCache,
			initiatorId: adminUserId,
		});

		await auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: BigInt(userId),
			action: 'force_add_to_guild',
			auditLogReason,
			metadata: new Map([['guild_id', String(guildId)]]),
		});

		return {success: true};
	}

	async bulkAddGuildMembers(data: BulkAddGuildMembersRequest, adminUserId: UserID, auditLogReason: string | null) {
		const {guildService, auditService} = this.deps;
		const successful: Array<string> = [];
		const failed: Array<{id: string; error: string}> = [];
		const guildId = createGuildID(data.guild_id);

		for (const userIdBigInt of data.user_ids) {
			try {
				const userId = createUserID(userIdBigInt);
				await guildService.addUserToGuild({
					userId,
					guildId,
					sendJoinMessage: false,
					skipBanCheck: true,
					requestCache: {} as RequestCache,
					initiatorId: adminUserId,
				});
				successful.push(userId.toString());
			} catch (error) {
				failed.push({
					id: userIdBigInt.toString(),
					error: error instanceof Error ? error.message : 'Unknown error',
				});
			}
		}

		await auditService.createAuditLog({
			adminUserId,
			targetType: 'guild',
			targetId: BigInt(guildId),
			action: 'bulk_add_guild_members',
			auditLogReason,
			metadata: new Map([
				['guild_id', guildId.toString()],
				['user_count', data.user_ids.length.toString()],
			]),
		});

		return {
			successful,
			failed,
		};
	}
}
