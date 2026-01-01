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

import {
	createGuildID,
	createInviteCode,
	createVanityURLCode,
	type UserID,
	vanityCodeToInviteCode,
} from '~/BrandedTypes';
import {InviteTypes} from '~/Constants';
import {InputValidationError, UnknownGuildError} from '~/Errors';
import type {IGuildRepository} from '~/guild/IGuildRepository';
import type {InviteRepository} from '~/invite/InviteRepository';
import type {UpdateGuildVanityRequest} from '../../AdminModel';
import {mapGuildToAdminResponse} from '../../AdminModel';
import type {AdminAuditService} from '../AdminAuditService';
import type {AdminGuildUpdatePropagator} from './AdminGuildUpdatePropagator';

interface AdminGuildVanityServiceDeps {
	guildRepository: IGuildRepository;
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
		const guildRow = guild.toRow();

		if (data.vanity_url_code) {
			const inviteCode = createInviteCode(data.vanity_url_code);
			const existingInvite = await inviteRepository.findUnique(inviteCode);
			if (existingInvite) {
				throw InputValidationError.create('vanity_url_code', 'This vanity URL is already taken');
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

		const updatedGuild = await guildRepository.upsert({
			...guildRow,
			vanity_url_code: data.vanity_url_code ? createVanityURLCode(data.vanity_url_code) : null,
		});

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
