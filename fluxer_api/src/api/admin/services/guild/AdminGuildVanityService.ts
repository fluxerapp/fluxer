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
import type {GuildRepository} from '@app/api/guild/repositories/GuildRepository';
import {enqueueEndGuildVisitsForCode} from '@app/api/guild/visits/GuildVisitLifecycle';
import type {InviteRepository} from '@app/api/invite/InviteRepository';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {UnknownGuildError} from '@fluxer/errors/src/domains/guild/UnknownGuildError';
import type {UpdateGuildVanityRequest} from '@fluxer/schema/src/domains/admin/AdminGuildSchemas';

interface AdminGuildVanityServiceDeps {
	guildRepository: GuildRepository;
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
			const claimed = await inviteRepository.claimVanityInvite(inviteCode, guildId);
			if (!claimed) {
				throw InputValidationError.fromCode('vanity_url_code', ValidationErrorCodes.THIS_VANITY_URL_IS_ALREADY_TAKEN);
			}
			if (oldVanity) {
				await inviteRepository.releaseVanityInvite(vanityCodeToInviteCode(oldVanity), guildId);
				await enqueueEndGuildVisitsForCode(guildId, vanityCodeToInviteCode(oldVanity));
			}
		} else if (oldVanity) {
			await inviteRepository.releaseVanityInvite(vanityCodeToInviteCode(oldVanity), guildId);
			await enqueueEndGuildVisitsForCode(guildId, vanityCodeToInviteCode(oldVanity));
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
