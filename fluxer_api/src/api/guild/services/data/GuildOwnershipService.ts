// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildID, UserID} from '@app/api/BrandedTypes';
import {mapGuildToGuildResponse} from '@app/api/guild/GuildModel';
import type {IGuildRepositoryAggregate} from '@app/api/guild/repositories/IGuildRepositoryAggregate';
import type {GuildDataHelpers} from '@app/api/guild/services/data/GuildDataHelpers';
import type {Guild} from '@app/api/models/Guild';
import type {GuildMember} from '@app/api/models/GuildMember';
import type {User} from '@app/api/models/User';
import type {IUserRepository} from '@app/api/user/IUserRepository';
import {checkGuildVerificationWithGuildModel} from '@app/api/utils/GuildVerificationUtils';
import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import {MissingAccessError} from '@fluxer/errors/src/domains/core/MissingAccessError';
import {MissingPermissionsError} from '@fluxer/errors/src/domains/core/MissingPermissionsError';
import {CannotTransferOwnershipToBotError} from '@fluxer/errors/src/domains/guild/CannotTransferOwnershipToBotError';
import {UnknownGuildError} from '@fluxer/errors/src/domains/guild/UnknownGuildError';
import {UnknownGuildMemberError} from '@fluxer/errors/src/domains/guild/UnknownGuildMemberError';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';

const OWNERSHIP_AUDIT_KEYS: ReadonlySet<string> = new Set(['owner_id']);

export class GuildOwnershipService {
	constructor(
		private readonly guildRepository: IGuildRepositoryAggregate,
		private readonly userRepository: IUserRepository,
		private readonly helpers: GuildDataHelpers,
	) {}

	async transferOwnership(
		params: {
			userId: UserID;
			guildId: GuildID;
			newOwnerId: UserID;
		},
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
		const newOwnerUser = await this.userRepository.findUnique(newOwnerId);
		if (newOwnerUser?.isBot) {
			throw new CannotTransferOwnershipToBotError();
		}
		const guild = await this.guildRepository.findUnique(guildId);
		if (!guild) throw new UnknownGuildError();
		const previousSnapshot = this.helpers.serializeGuildForAudit(guild);
		const previousOwnerId = guild.ownerId;
		const updatedGuild = await this.guildRepository.upsertPartial(
			guildId,
			{owner_id: newOwnerId},
			guild.toRow(),
			previousOwnerId,
		);
		await this.helpers.dispatchGuildUpdate(updatedGuild);
		await this.helpers.recordAuditLog({
			guildId,
			userId,
			action: AuditLogActionType.GUILD_UPDATE,
			targetId: guildId,
			auditLogReason: auditLogReason ?? null,
			changes: this.helpers.computeGuildChanges(previousSnapshot, updatedGuild, OWNERSHIP_AUDIT_KEYS),
		});
		return mapGuildToGuildResponse(updatedGuild);
	}

	async checkGuildVerification(params: {user: User; guild: Guild; member: GuildMember}): Promise<void> {
		const {user, guild, member} = params;
		checkGuildVerificationWithGuildModel({user, guild, member});
	}
}
