// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createInviteCode,
	createVanityURLCode,
	type GuildID,
	type UserID,
	vanityCodeToInviteCode,
} from '@app/api/BrandedTypes';
import type {IGuildRepositoryAggregate} from '@app/api/guild/repositories/IGuildRepositoryAggregate';
import type {GuildDataHelpers} from '@app/api/guild/services/data/GuildDataHelpers';
import type {InviteRepository} from '@app/api/invite/InviteRepository';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import {InviteTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {UnknownGuildError} from '@fluxer/errors/src/domains/guild/UnknownGuildError';
import type {GuildVanityURLResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';

const VANITY_AUDIT_KEYS: ReadonlySet<string> = new Set(['vanity_url_code']);

export class GuildVanityService {
	constructor(
		private readonly guildRepository: IGuildRepositoryAggregate,
		private readonly inviteRepository: InviteRepository,
		private readonly helpers: GuildDataHelpers,
	) {}

	async getVanityURL(params: {userId: UserID; guildId: GuildID}): Promise<GuildVanityURLResponse> {
		const {userId, guildId} = params;
		const {guildData, checkPermission} = await this.helpers.getGuildAuthenticated({userId, guildId});
		await checkPermission(Permissions.MANAGE_GUILD);
		if (!guildData) throw new UnknownGuildError();
		const vanityCodeString = guildData.vanity_url_code;
		if (!vanityCodeString) {
			return {code: null, uses: 0};
		}
		const vanityCode = createVanityURLCode(vanityCodeString);
		const invite = await this.inviteRepository.findUnique(vanityCodeToInviteCode(vanityCode));
		return {
			code: vanityCodeString,
			uses: invite?.uses ?? 0,
		};
	}

	async updateVanityURL(
		params: {
			userId: UserID;
			guildId: GuildID;
			code: string | null;
			requestCache: RequestCache;
		},
		auditLogReason?: string | null,
	): Promise<{
		code: string | null;
	}> {
		const {userId, guildId, code} = params;
		const {checkPermission} = await this.helpers.getGuildAuthenticated({userId, guildId});
		await checkPermission(Permissions.MANAGE_GUILD);
		const guild = await this.guildRepository.findUnique(guildId);
		if (!guild) throw new UnknownGuildError();
		const previousSnapshot = this.helpers.serializeGuildForAudit(guild);
		if (code && !guild.features.has(GuildFeatures.VANITY_URL)) {
			throw InputValidationError.fromCode('code', ValidationErrorCodes.VANITY_URL_REQUIRES_FEATURE);
		}
		if (code?.includes('fluxer')) {
			throw InputValidationError.fromCode('code', ValidationErrorCodes.VANITY_URL_CODE_CANNOT_CONTAIN_FLUXER);
		}
		if (code != null && guild.vanityUrlCode === code) {
			return {code};
		}
		if (code == null) {
			if (guild.vanityUrlCode != null) {
				const oldInvite = await this.inviteRepository.findUnique(vanityCodeToInviteCode(guild.vanityUrlCode));
				if (oldInvite) {
					await this.inviteRepository.delete(oldInvite.code);
				}
				const updatedGuild = await this.guildRepository.upsertPartial(guildId, {vanity_url_code: null}, guild.toRow());
				await this.helpers.dispatchGuildUpdate(updatedGuild);
				await this.helpers.recordAuditLog({
					guildId,
					userId,
					action: AuditLogActionType.GUILD_UPDATE,
					targetId: guildId,
					auditLogReason: auditLogReason ?? null,
					changes: this.helpers.computeGuildChanges(previousSnapshot, updatedGuild, VANITY_AUDIT_KEYS),
				});
				return {code: null};
			}
			return {code: null};
		}
		const existingInvite = await this.inviteRepository.findUnique(createInviteCode(code));
		if (existingInvite != null) {
			throw InputValidationError.fromCode('code', ValidationErrorCodes.VANITY_URL_CODE_ALREADY_TAKEN);
		}
		if (guild.vanityUrlCode != null) {
			const oldInvite = await this.inviteRepository.findUnique(vanityCodeToInviteCode(guild.vanityUrlCode));
			if (oldInvite) {
				await this.inviteRepository.delete(oldInvite.code);
			}
		}
		await this.inviteRepository.create({
			code: createInviteCode(code),
			type: InviteTypes.GUILD,
			guild_id: guildId,
			channel_id: null,
			inviter_id: null,
			uses: 0,
			max_uses: 0,
			max_age: 0,
		});
		const updatedGuild = await this.guildRepository.upsertPartial(
			guildId,
			{vanity_url_code: createVanityURLCode(code)},
			guild.toRow(),
		);
		await this.helpers.dispatchGuildUpdate(updatedGuild);
		await this.helpers.recordAuditLog({
			guildId,
			userId,
			action: AuditLogActionType.GUILD_UPDATE,
			targetId: guildId,
			auditLogReason: auditLogReason ?? null,
			changes: this.helpers.computeGuildChanges(previousSnapshot, updatedGuild, VANITY_AUDIT_KEYS),
		});
		return {code};
	}
}
