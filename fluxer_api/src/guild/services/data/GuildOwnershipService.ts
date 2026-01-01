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
import {AuditLogActionType} from '~/constants/AuditLogActionType';
import {MissingAccessError, MissingPermissionsError, UnknownGuildError, UnknownGuildMemberError} from '~/Errors';
import type {GuildResponse} from '~/guild/GuildModel';
import {mapGuildToGuildResponse} from '~/guild/GuildModel';
import type {IGuildRepository} from '~/guild/IGuildRepository';
import type {Guild, GuildMember, User} from '~/Models';
import type {IUserRepository} from '~/user/IUserRepository';
import {checkGuildVerificationWithGuildModel} from '~/utils/GuildVerificationUtils';
import type {GuildDataHelpers} from './GuildDataHelpers';

export class GuildOwnershipService {
	constructor(
		private readonly guildRepository: IGuildRepository,
		private readonly userRepository: IUserRepository,
		private readonly helpers: GuildDataHelpers,
	) {}

	async transferOwnership(
		params: {userId: UserID; guildId: GuildID; newOwnerId: UserID},
		auditLogReason?: string | null,
	): Promise<GuildResponse> {
		const {userId, guildId, newOwnerId} = params;
		const {guildData} = await this.helpers.getGuildAuthenticated({userId, guildId});

		if (guildData.owner_id !== userId.toString()) {
			throw new MissingPermissionsError();
		}

		const user = await this.userRepository.findUnique(userId);
		if (!user) throw new MissingAccessError();

		const newOwner = await this.guildRepository.getMember(guildId, newOwnerId);
		if (!newOwner) {
			throw new UnknownGuildMemberError();
		}

		const guild = await this.guildRepository.findUnique(guildId);
		if (!guild) throw new UnknownGuildError();
		const previousSnapshot = this.helpers.serializeGuildForAudit(guild);

		const updatedGuild = await this.guildRepository.upsert({
			...guild.toRow(),
			owner_id: newOwnerId,
		});

		await this.helpers.dispatchGuildUpdate(updatedGuild);

		await this.helpers.recordAuditLog({
			guildId,
			userId,
			action: AuditLogActionType.GUILD_UPDATE,
			targetId: guildId,
			auditLogReason: auditLogReason ?? null,
			metadata: {new_owner_id: newOwnerId.toString()},
			changes: this.helpers.computeGuildChanges(previousSnapshot, updatedGuild),
		});
		return mapGuildToGuildResponse(updatedGuild);
	}

	async checkGuildVerification(params: {user: User; guild: Guild; member: GuildMember}): Promise<void> {
		const {user, guild, member} = params;
		checkGuildVerificationWithGuildModel({user, guild, member});
	}
}
