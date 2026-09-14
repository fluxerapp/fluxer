// SPDX-License-Identifier: AGPL-3.0-or-later

import {mapGuildToAdminResponse} from '@app/api/admin/models/GuildTypes';
import type {AdminAuditService} from '@app/api/admin/services/AdminAuditService';
import type {AdminGuildUpdatePropagator} from '@app/api/admin/services/guild/AdminGuildUpdatePropagator';
import {
	createGuildID,
	createInviteCode,
	createVanityURLCode,
	type UserID,
	vanityCodeToInviteCode,
} from '@app/api/BrandedTypes';
import type {IGuildRepositoryAggregate} from '@app/api/guild/repositories/IGuildRepositoryAggregate';
import type {InviteRepository} from '@app/api/invite/InviteRepository';
import {InviteTypes} from '@fluxer/constants/src/ChannelConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {UnknownGuildError} from '@fluxer/errors/src/domains/guild/UnknownGuildError';
import type {UpdateGuildVanityRequest} from '@fluxer/schema/src/domains/admin/AdminGuildSchemas';

interface AdminGuildVanityServiceDeps {
	guildRepository: IGuildRepositoryAggregate;
	inviteRepository: InviteRepository;
	auditService: AdminAuditService;
	updatePropagator: AdminGuildUpdatePropagator;
}

export class AdminGuildVanityService {
	constructor(private readonly deps: AdminGuildVanityServiceDeps) {}

	async updateGuildVanity(data: UpdateGuildVanityRequest, adminUserId: UserID, auditLogReason: string | null) {
		const {guildRepository, inviteRepository, auditService, updatePropagator} = this.deps;
		const guildId = createGuildID(data.guild_id);
		const guild = await guildRepository.findUnique(guildId);
		if (!guild) {
			throw new UnknownGuildError();
		}
		const oldVanity = guild.vanityUrlCode;
		if (data.vanity_url_code) {
			const inviteCode = createInviteCode(data.vanity_url_code);
			const existingInvite = await inviteRepository.findUnique(inviteCode);
			if (existingInvite) {
				throw InputValidationError.fromCode('vanity_url_code', ValidationErrorCodes.THIS_VANITY_URL_IS_ALREADY_TAKEN);
			}
			if (oldVanity) {
				await inviteRepository.delete(vanityCodeToInviteCode(oldVanity));
			}
			await inviteRepository.create({
				code: inviteCode,
				type: InviteTypes.GUILD,
				guild_id: guildId,
				channel_id: null,
				inviter_id: null,
				uses: 0,
				max_uses: 0,
				max_age: 0,
				temporary: false,
			});
		} else if (oldVanity) {
			await inviteRepository.delete(vanityCodeToInviteCode(oldVanity));
		}
		const updatedGuild = await guildRepository.upsertPartial(
			guildId,
			{vanity_url_code: data.vanity_url_code ? createVanityURLCode(data.vanity_url_code) : null},
			guild.toRow(),
		);
		await updatePropagator.dispatchGuildUpdate(guildId, updatedGuild);
		await auditService.createAuditLog({
			adminUserId,
			targetType: 'guild',
			targetId: BigInt(guildId),
			action: 'update_vanity',
			auditLogReason,
			metadata: new Map([
				['old_vanity', oldVanity ?? ''],
				['new_vanity', data.vanity_url_code ?? ''],
			]),
		});
		return {
			guild: mapGuildToAdminResponse(updatedGuild),
		};
	}
}
