// SPDX-License-Identifier: AGPL-3.0-or-later

import {createRoleID, type GuildID, guildIdToRoleId, type RoleID, type UserID} from '@app/api/BrandedTypes';
import type {GuildAuditLogService} from '@app/api/guild/GuildAuditLogService';
import {mapGuildSelfAssignableRoleSectionToResponse} from '@app/api/guild/GuildModel';
import {type GuildUpdateLocks, withGuildUpdateLock} from '@app/api/guild/GuildUpdateLock';
import type {GuildRepository} from '@app/api/guild/repositories/GuildRepository';
import {GuildDataHelpers} from '@app/api/guild/services/data/GuildDataHelpers';
import type {GuildMemberService} from '@app/api/guild/services/GuildMemberService';
import {getGuildAuth} from '@app/api/guild/services/GuildMfaEnforcement';
import {contentModerationService} from '@app/api/infrastructure/ContentModerationService';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {ISnowflakeService} from '@app/api/infrastructure/ISnowflakeService';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import type {Guild} from '@app/api/models/Guild';
import type {GuildMember} from '@app/api/models/GuildMember';
import {
	GuildSelfAssignableRoleSection,
	selfAssignableRoleSectionsToRow,
} from '@app/api/models/GuildSelfAssignableRoleSection';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {serializeGuildForAudit} from '@app/api/utils/AuditSerializationUtils';
import {checkGuildVerificationWithGuildModel} from '@app/api/utils/GuildVerificationUtils';
import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {GuildFeatures, SELF_ASSIGNABLE_ROLE_SECTION_AUDIT_KEYS} from '@fluxer/constants/src/GuildConstants';
import {canUseSelfAssignableRoleSection} from '@fluxer/constants/src/SelfAssignableRoleRules';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {MissingPermissionsError} from '@fluxer/errors/src/domains/core/MissingPermissionsError';
import {UnknownGuildError} from '@fluxer/errors/src/domains/guild/UnknownGuildError';
import type {GuildSelfAssignableRoleSectionsUpdateRequest} from '@fluxer/schema/src/domains/guild/GuildRequestSchemas';
import type {GuildSelfAssignableRoleSectionListResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';

const SECTION_AUDIT_KEYS: ReadonlySet<string> = new Set(SELF_ASSIGNABLE_ROLE_SECTION_AUDIT_KEYS);

interface UsableSection {
	readonly section: GuildSelfAssignableRoleSection;
	readonly guild: Guild;
	readonly member: GuildMember;
}

function findSelfAssignableSection(guild: Guild, roleId: RoleID): GuildSelfAssignableRoleSection | null {
	if (!guild.features.has(GuildFeatures.SELF_ASSIGNABLE_ROLES_ENABLED)) return null;
	return guild.selfAssignableRoleSections.find((section) => sectionHasRole(section, roleId)) ?? null;
}

function sectionHasRole(section: GuildSelfAssignableRoleSection, roleId: RoleID): boolean {
	return section.roles.some((role) => role.roleId === roleId);
}

function widensAccess(previous: ReadonlyArray<RoleID>, next: ReadonlyArray<RoleID>): boolean {
	if (previous.length === 0) return false;
	const previousSet = new Set(previous);
	return next.length === 0 || next.some((roleId) => !previousSet.has(roleId));
}

export async function pruneRoleFromSelfAssignableSections(
	deps: {guildRepository: GuildRepository; helpers: GuildDataHelpers; locks: GuildUpdateLocks},
	guildId: GuildID,
	roleId: RoleID,
): Promise<void> {
	const updatedGuild = await withGuildUpdateLock(deps.locks, guildId, async () => {
		const guild = await deps.guildRepository.findUnique(guildId);
		if (!guild?.selfAssignableRoleSections.some((section) => sectionHasRole(section, roleId))) return null;
		const sections = guild.selfAssignableRoleSections.map((section) => section.withoutRole(roleId));
		return deps.guildRepository.upsertPartial(
			guildId,
			{self_assignable_role_sections: selfAssignableRoleSectionsToRow(sections)},
			guild.toRow(),
		);
	});
	if (updatedGuild) await deps.helpers.dispatchGuildUpdate(updatedGuild);
}

export class GuildSelfAssignableRoleService {
	private readonly helpers: GuildDataHelpers;

	constructor(
		private readonly guildRepository: GuildRepository,
		private readonly gatewayService: IGatewayService,
		private readonly userRepository: UserRepository,
		private readonly snowflakeService: ISnowflakeService,
		private readonly guildAuditLogService: GuildAuditLogService,
		private readonly locks: GuildUpdateLocks,
		private readonly members: GuildMemberService,
	) {
		this.helpers = new GuildDataHelpers(gatewayService, guildAuditLogService, userRepository);
	}

	async updateSections(
		params: {
			userId: UserID;
			guildId: GuildID;
			data: GuildSelfAssignableRoleSectionsUpdateRequest;
			requestCache: RequestCache;
		},
		auditLogReason?: string | null,
	): Promise<GuildSelfAssignableRoleSectionListResponse> {
		const {userId, guildId, data, requestCache} = params;
		const {checkPermission, guildData} = await getGuildAuth(this.gatewayService, this.userRepository, {
			userId,
			guildId,
		});
		await checkPermission(Permissions.MANAGE_GUILD);
		await checkPermission(Permissions.MANAGE_ROLES);
		const isOwner = guildData.owner_id === userId.toString();
		const existingRoleIds = new Set((await this.guildRepository.listRoles(guildId)).map((role) => role.id));
		this.validateRequestedRoleIds(guildId, data, existingRoleIds);
		const modCtx = {userId, guildId, channelId: null, messageId: null, surface: 'profile_field' as const};
		for (const section of data.sections) {
			for (const role of section.roles) contentModerationService.scanText(role.description, modCtx);
			contentModerationService.scanText(section.content, modCtx);
		}
		const {previousSnapshot, updatedGuild} = await withGuildUpdateLock(this.locks, guildId, async () => {
			requestCache.guilds.delete(guildId);
			const currentGuild = await this.guildRepository.findUnique(guildId);
			if (!currentGuild) throw new UnknownGuildError();
			this.validateRequiredRoleIds(guildId, data, existingRoleIds, currentGuild);
			if (!isOwner) {
				await this.ensureCanOpenRoles({guildId, userId, currentGuild, data});
			}
			const sections = await this.buildSections(currentGuild, data);
			const snapshot = serializeGuildForAudit(currentGuild);
			const updated = await this.guildRepository.upsertPartial(
				guildId,
				{self_assignable_role_sections: selfAssignableRoleSectionsToRow(sections)},
				currentGuild.toRow(),
			);
			return {previousSnapshot: snapshot, updatedGuild: updated};
		});
		await this.helpers.dispatchGuildUpdate(updatedGuild);
		const changes = this.helpers.computeGuildChanges(previousSnapshot, updatedGuild, SECTION_AUDIT_KEYS);
		if (changes.length > 0) {
			await this.guildAuditLogService.record({
				guildId,
				userId,
				action: AuditLogActionType.GUILD_UPDATE,
				targetId: guildId,
				auditLogReason: auditLogReason ?? null,
				changes,
			});
		}
		return updatedGuild.selfAssignableRoleSections.map(mapGuildSelfAssignableRoleSectionToResponse);
	}

	async addRoleToSelf(
		params: {userId: UserID; guildId: GuildID; roleId: RoleID; requestCache: RequestCache},
		auditLogReason?: string | null,
	): Promise<void> {
		const {userId, guildId, roleId, requestCache} = params;
		const usable = await this.findUsableSection(params);
		if (usable) {
			const {section, guild, member} = usable;
			const user = await this.userRepository.findUniqueAssert(userId);
			checkGuildVerificationWithGuildModel({user, guild, member});
			await this.members.selfAddMemberRole(
				{
					userId,
					guildId,
					roleId,
					requestCache,
					section: {roleIds: section.roles.map((role) => role.roleId), maxRoles: section.maxRoles},
				},
				auditLogReason,
			);
			return;
		}
		await this.members.addMemberRole({userId, targetId: userId, guildId, roleId, requestCache}, auditLogReason);
	}

	async removeRoleFromSelf(
		params: {userId: UserID; guildId: GuildID; roleId: RoleID; requestCache: RequestCache},
		auditLogReason?: string | null,
	): Promise<void> {
		const {userId, guildId, roleId, requestCache} = params;
		if (await this.findUsableSection(params)) {
			await this.members.selfRemoveMemberRole({userId, guildId, roleId, requestCache}, auditLogReason);
			return;
		}
		await this.members.removeMemberRole({userId, targetId: userId, guildId, roleId, requestCache}, auditLogReason);
	}

	private async findUsableSection(params: {
		userId: UserID;
		guildId: GuildID;
		roleId: RoleID;
	}): Promise<UsableSection | null> {
		const {userId, guildId, roleId} = params;
		if (roleId === guildIdToRoleId(guildId)) return null;
		const guild = await this.guildRepository.findUnique(guildId);
		const section = guild ? findSelfAssignableSection(guild, roleId) : null;
		if (!guild || !section) return null;
		if ((await this.guildRepository.getRole(roleId, guildId)) == null) return null;
		const member = await this.guildRepository.getMember(guildId, userId);
		if (!canUseSelfAssignableRoleSection(section.requiredRoleIds, member?.roleIds)) return null;
		await getGuildAuth(this.gatewayService, this.userRepository, {userId, guildId});
		return member ? {section, guild, member} : null;
	}

	private validateRequestedRoleIds(
		guildId: GuildID,
		data: GuildSelfAssignableRoleSectionsUpdateRequest,
		existingRoleIds: ReadonlySet<RoleID>,
	): void {
		const everyoneRoleId = guildIdToRoleId(guildId);
		const seen = new Set<RoleID>();
		data.sections.forEach((section, sectionIndex) => {
			section.roles.forEach((role, roleIndex) => {
				const roleId = createRoleID(role.id);
				const path = `sections.${sectionIndex}.roles.${roleIndex}.id`;
				if (roleId === everyoneRoleId || !existingRoleIds.has(roleId)) {
					throw InputValidationError.fromCode(path, ValidationErrorCodes.INVALID_ROLE_ID, {
						roleId: roleId.toString(),
					});
				}
				if (seen.has(roleId)) {
					throw InputValidationError.fromCode(path, ValidationErrorCodes.DUPLICATE_SELF_ASSIGNABLE_ROLE);
				}
				seen.add(roleId);
			});
		});
	}

	private validateRequiredRoleIds(
		guildId: GuildID,
		data: GuildSelfAssignableRoleSectionsUpdateRequest,
		existingRoleIds: ReadonlySet<RoleID>,
		currentGuild: Guild,
	): void {
		const everyoneRoleId = guildIdToRoleId(guildId);
		const storedRequired = new Map(
			currentGuild.selfAssignableRoleSections.map((section) => [section.id, new Set(section.requiredRoleIds)]),
		);
		data.sections.forEach((section, sectionIndex) => {
			const stored = section.id == null ? undefined : storedRequired.get(section.id);
			(section.required_role_ids ?? []).forEach((rawRoleId, roleIndex) => {
				const roleId = createRoleID(rawRoleId);
				const known = existingRoleIds.has(roleId) || stored?.has(roleId) === true;
				if (roleId === everyoneRoleId || !known) {
					throw InputValidationError.fromCode(
						`sections.${sectionIndex}.required_role_ids.${roleIndex}`,
						ValidationErrorCodes.INVALID_ROLE_ID,
						{roleId: roleId.toString()},
					);
				}
			});
		});
	}

	private async ensureCanOpenRoles(params: {
		guildId: GuildID;
		userId: UserID;
		currentGuild: Guild;
		data: GuildSelfAssignableRoleSectionsUpdateRequest;
	}): Promise<void> {
		const {guildId, userId, currentGuild, data} = params;
		const currentRequirements = new Map<RoleID, ReadonlyArray<RoleID>>();
		for (const section of currentGuild.selfAssignableRoleSections) {
			for (const role of section.roles) {
				currentRequirements.set(role.roleId, section.requiredRoleIds);
			}
		}
		const openedRoleIds = new Set<RoleID>();
		for (const section of data.sections) {
			const requiredRoleIds = (section.required_role_ids ?? []).map((roleId) => createRoleID(roleId));
			for (const role of section.roles) {
				const roleId = createRoleID(role.id);
				const previous = currentRequirements.get(roleId);
				if (previous === undefined || widensAccess(previous, requiredRoleIds)) {
					openedRoleIds.add(roleId);
				}
			}
		}
		const results = await Promise.all(
			[...openedRoleIds].map((roleId) => this.gatewayService.canManageRole({guildId, userId, roleId})),
		);
		if (results.some((allowed) => !allowed)) {
			throw new MissingPermissionsError();
		}
	}

	private async buildSections(
		currentGuild: Guild,
		data: GuildSelfAssignableRoleSectionsUpdateRequest,
	): Promise<Array<GuildSelfAssignableRoleSection>> {
		const existingSectionIds = new Set(currentGuild.selfAssignableRoleSections.map((section) => section.id));
		const usedSectionIds = new Set<bigint>();
		const sections: Array<GuildSelfAssignableRoleSection> = [];
		for (const section of data.sections) {
			let sectionId = section.id ?? null;
			if (sectionId == null || !existingSectionIds.has(sectionId) || usedSectionIds.has(sectionId)) {
				sectionId = await this.snowflakeService.generate();
			}
			usedSectionIds.add(sectionId);
			sections.push(
				new GuildSelfAssignableRoleSection({
					section_id: sectionId,
					content: section.content,
					roles: section.roles.map((role) => ({role_id: createRoleID(role.id), description: role.description})),
					max_roles: section.max_roles ?? null,
					required_role_ids: [...new Set((section.required_role_ids ?? []).map((roleId) => createRoleID(roleId)))],
				}),
			);
		}
		return sections;
	}
}
