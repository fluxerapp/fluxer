// SPDX-License-Identifier: AGPL-3.0-or-later

import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {AccessDeniedError} from '@fluxer/errors/src/domains/core/AccessDeniedError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {TagAlreadyTakenError} from '@fluxer/errors/src/domains/user/TagAlreadyTakenError';
import {UnknownUserError} from '@fluxer/errors/src/domains/user/UnknownUserError';
import type {
	ChangeDobRequest,
	ChangeEmailRequest,
	ChangeUsernameRequest,
	ClearUserFieldsRequest,
	SetUserBotStatusRequest,
	SetUserSystemStatusRequest,
	VerifyUserEmailRequest,
} from '@fluxer/schema/src/domains/admin/AdminUserSchemas';
import {types} from 'cassandra-driver';
import type {ApiContext} from '../../ApiContext';
import {EMAIL_CLEARABLE_SUSPICIOUS_ACTIVITY_FLAGS} from '../../auth/AuthEmail';
import {createUserID, type UserID} from '../../BrandedTypes';
import type {IGuildRepositoryAggregate} from '../../guild/repositories/IGuildRepositoryAggregate';
import {GuildMemberSearchIndexService} from '../../guild/services/member/GuildMemberSearchIndexService';
import type {IDiscriminatorService} from '../../infrastructure/DiscriminatorService';
import type {EntityAssetService, PreparedAssetUpload} from '../../infrastructure/EntityAssetService';
import {Logger} from '../../Logger';
import type {User} from '../../models/User';
import {mapUserToAdminResponse} from '../models/UserTypes';
import type {AdminAuditService} from './AdminAuditService';
import type {AdminUserUpdatePropagator} from './AdminUserUpdatePropagator';

interface AdminUserProfileServiceDeps {
	apiContext: ApiContext;
	discriminatorService: IDiscriminatorService;
	entityAssetService: EntityAssetService;
	auditService: AdminAuditService;
	updatePropagator: AdminUserUpdatePropagator;
	guildRepository: IGuildRepositoryAggregate;
}

export class AdminUserProfileService {
	private readonly searchIndexService: GuildMemberSearchIndexService;

	constructor(private readonly deps: AdminUserProfileServiceDeps) {
		this.searchIndexService = new GuildMemberSearchIndexService();
	}

	async clearUserFields(
		data: ClearUserFieldsRequest,
		adminUserId: UserID,
		auditLogReason: string | null,
		acls: ReadonlySet<string>,
	) {
		const {users: userRepository, cache: cacheService, gateway: gatewayService} = this.deps.apiContext.services;
		const {entityAssetService, auditService, updatePropagator} = this.deps;
		const userId = createUserID(data.user_id);
		const user = await userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}
		const updates: Record<string, null | string> = {};
		const preparedAssets: Array<PreparedAssetUpload> = [];
		for (const field of data.fields) {
			if (field === 'avatar') {
				const prepared = await entityAssetService.prepareAssetUpload({
					assetType: 'avatar',
					entityType: 'user',
					entityId: userId,
					previousHash: user.avatarHash,
					base64Image: null,
					errorPath: 'avatar',
				});
				preparedAssets.push(prepared);
				updates['avatar_hash'] = prepared.newHash;
			} else if (field === 'banner') {
				const prepared = await entityAssetService.prepareAssetUpload({
					assetType: 'banner',
					entityType: 'user',
					entityId: userId,
					previousHash: user.bannerHash,
					base64Image: null,
					errorPath: 'banner',
				});
				preparedAssets.push(prepared);
				updates['banner_hash'] = prepared.newHash;
			} else if (field === 'bio') {
				updates['bio'] = null;
			} else if (field === 'pronouns') {
				updates['pronouns'] = null;
			} else if (field === 'activity') {
				await gatewayService.clearCurrentActivities(userId);
			} else if (field === 'global_name') {
				updates['global_name'] = null;
			}
		}
		let updatedUser = user;
		if (Object.keys(updates).length > 0) {
			try {
				updatedUser = await userRepository.patchUpsert(userId, updates, user.toRow());
			} catch (error) {
				await Promise.all(preparedAssets.map((p) => entityAssetService.rollbackAssetUpload(p)));
				throw error;
			}
			await Promise.all(preparedAssets.map((p) => entityAssetService.commitAssetChange({prepared: p})));
			await updatePropagator.propagateUserUpdate({userId, oldUser: user, updatedUser: updatedUser});
		}
		await auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: BigInt(userId),
			action: 'clear_fields',
			auditLogReason,
			metadata: new Map([['fields', data.fields.join(',')]]),
		});
		return {
			user: await mapUserToAdminResponse(updatedUser, cacheService, acls, gatewayService),
		};
	}

	async setUserBotStatus(
		data: SetUserBotStatusRequest,
		adminUserId: UserID,
		auditLogReason: string | null,
		acls: ReadonlySet<string>,
	) {
		const {users: userRepository, cache: cacheService, gateway: gatewayService} = this.deps.apiContext.services;
		const {auditService, updatePropagator} = this.deps;
		const userId = createUserID(data.user_id);
		const user = await userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}
		if (data.bot && user.acls.size > 0) {
			throw new AccessDeniedError();
		}
		const updates: Record<string, boolean> = {bot: data.bot};
		if (!data.bot) {
			updates['system'] = false;
		}
		const updatedUser = await userRepository.patchUpsert(userId, updates, user.toRow());
		await updatePropagator.propagateUserUpdate({userId, oldUser: user, updatedUser: updatedUser});
		await auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: BigInt(userId),
			action: 'set_bot_status',
			auditLogReason,
			metadata: new Map([['bot', data.bot.toString()]]),
		});
		return {
			user: await mapUserToAdminResponse(updatedUser, cacheService, acls, gatewayService),
		};
	}

	async setUserSystemStatus(
		data: SetUserSystemStatusRequest,
		adminUserId: UserID,
		auditLogReason: string | null,
		acls: ReadonlySet<string>,
	) {
		const {users: userRepository, cache: cacheService, gateway: gatewayService} = this.deps.apiContext.services;
		const {auditService, updatePropagator} = this.deps;
		const userId = createUserID(data.user_id);
		const user = await userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}
		if (data.system && !user.isBot) {
			throw InputValidationError.fromCode(
				'system',
				ValidationErrorCodes.USER_MUST_BE_A_BOT_TO_BE_MARKED_AS_A_SYSTEM_USER,
			);
		}
		const updatedUser = await userRepository.patchUpsert(userId, {system: data.system}, user.toRow());
		await updatePropagator.propagateUserUpdate({userId, oldUser: user, updatedUser: updatedUser});
		await auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: BigInt(userId),
			action: 'set_system_status',
			auditLogReason,
			metadata: new Map([['system', data.system.toString()]]),
		});
		return {
			user: await mapUserToAdminResponse(updatedUser, cacheService, acls, gatewayService),
		};
	}

	async verifyUserEmail(
		data: VerifyUserEmailRequest,
		adminUserId: UserID,
		auditLogReason: string | null,
		acls: ReadonlySet<string>,
	) {
		const {users: userRepository, cache: cacheService, gateway: gatewayService} = this.deps.apiContext.services;
		const {auditService, updatePropagator} = this.deps;
		const userId = createUserID(data.user_id);
		const user = await userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}
		const updates: {
			email_verified: boolean;
			email_bounced: boolean;
			suspicious_activity_flags?: number;
		} = {
			email_verified: true,
			email_bounced: false,
		};
		if (user.suspiciousActivityFlags !== null && user.suspiciousActivityFlags !== 0) {
			const newFlags = user.suspiciousActivityFlags & ~EMAIL_CLEARABLE_SUSPICIOUS_ACTIVITY_FLAGS;
			if (newFlags !== user.suspiciousActivityFlags) {
				updates.suspicious_activity_flags = newFlags;
			}
		}
		const updatedUser = await userRepository.patchUpsert(userId, updates, user.toRow());
		await updatePropagator.propagateUserUpdate({userId, oldUser: user, updatedUser: updatedUser});
		await auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: BigInt(userId),
			action: 'verify_email',
			auditLogReason,
			metadata: new Map([['email', user.email ?? 'null']]),
		});
		return {
			user: await mapUserToAdminResponse(updatedUser, cacheService, acls, gatewayService),
		};
	}

	async changeUsername(
		data: ChangeUsernameRequest,
		adminUserId: UserID,
		auditLogReason: string | null,
		acls: ReadonlySet<string>,
	) {
		const {
			users: userRepository,
			cache: cacheService,
			gateway: gatewayService,
			contactChangeLog: contactChangeLogService,
		} = this.deps.apiContext.services;
		const {discriminatorService, auditService, updatePropagator} = this.deps;
		const userId = createUserID(data.user_id);
		const user = await userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}
		const discriminatorResult = await discriminatorService.generateDiscriminator({
			username: data.username,
			requestedDiscriminator: data.discriminator,
			user,
		});
		if (!discriminatorResult.available || discriminatorResult.discriminator === -1) {
			throw new TagAlreadyTakenError();
		}
		const updatedUser = await userRepository.patchUpsert(
			userId,
			{
				username: data.username,
				discriminator: discriminatorResult.discriminator,
			},
			user.toRow(),
		);
		await updatePropagator.propagateUserUpdate({userId, oldUser: user, updatedUser: updatedUser});
		await contactChangeLogService.recordDiff({
			oldUser: user,
			newUser: updatedUser,
			reason: 'admin_action',
			actorUserId: adminUserId,
		});
		await auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: BigInt(userId),
			action: 'change_username',
			auditLogReason,
			metadata: new Map([
				['old_username', user.username],
				['new_username', data.username],
				['discriminator', discriminatorResult.discriminator.toString()],
			]),
		});
		void this.reindexGuildMembersForUser(updatedUser);
		return {
			user: await mapUserToAdminResponse(updatedUser, cacheService, acls, gatewayService),
		};
	}

	async changeEmail(
		data: ChangeEmailRequest,
		adminUserId: UserID,
		auditLogReason: string | null,
		acls: ReadonlySet<string>,
	) {
		const {
			users: userRepository,
			cache: cacheService,
			gateway: gatewayService,
			contactChangeLog: contactChangeLogService,
		} = this.deps.apiContext.services;
		const {auditService, updatePropagator} = this.deps;
		const userId = createUserID(data.user_id);
		const user = await userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}
		const updatedUser = await userRepository.patchUpsert(
			userId,
			{
				email: data.email,
				email_verified: false,
			},
			user.toRow(),
		);
		await updatePropagator.propagateUserUpdate({userId, oldUser: user, updatedUser: updatedUser});
		await contactChangeLogService.recordDiff({
			oldUser: user,
			newUser: updatedUser,
			reason: 'admin_action',
			actorUserId: adminUserId,
		});
		await auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: BigInt(userId),
			action: 'change_email',
			auditLogReason,
			metadata: new Map([
				['old_email', user.email ?? 'null'],
				['new_email', data.email],
			]),
		});
		return {
			user: await mapUserToAdminResponse(updatedUser, cacheService, acls, gatewayService),
		};
	}

	private async reindexGuildMembersForUser(updatedUser: User): Promise<void> {
		try {
			const {users: userRepository} = this.deps.apiContext.services;
			const {guildRepository} = this.deps;
			const guildIds = await userRepository.getUserGuildIds(updatedUser.id);
			if (guildIds.length === 0) return;
			const guilds = await guildRepository.listGuilds(guildIds);
			const indexedGuilds = guilds.filter((guild) => guild.membersIndexedAt != null);
			if (indexedGuilds.length === 0) return;
			const members = await Promise.all(
				indexedGuilds.map((guild) => guildRepository.getMember(guild.id, updatedUser.id)),
			);
			for (let i = 0; i < members.length; i++) {
				const member = members[i];
				if (member) {
					const guild = indexedGuilds[i]!;
					const includeDefault = guild.membersIndexedAt != null;
					void this.searchIndexService.updateMember(member, updatedUser, {includeDefault});
				}
			}
		} catch (error) {
			Logger.error(
				{userId: updatedUser.id.toString(), error},
				'Failed to reindex guild members after admin user update',
			);
		}
	}

	async changeDob(
		data: ChangeDobRequest,
		adminUserId: UserID,
		auditLogReason: string | null,
		acls: ReadonlySet<string>,
	) {
		const {users: userRepository, cache: cacheService, gateway: gatewayService} = this.deps.apiContext.services;
		const {auditService, updatePropagator} = this.deps;
		const userId = createUserID(data.user_id);
		const user = await userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}
		const updatedUser = await userRepository.patchUpsert(
			userId,
			{
				date_of_birth: types.LocalDate.fromString(data.date_of_birth),
			},
			user.toRow(),
		);
		await updatePropagator.propagateUserUpdate({userId, oldUser: user, updatedUser: updatedUser});
		await auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: BigInt(userId),
			action: 'change_dob',
			auditLogReason,
			metadata: new Map([
				['old_dob', user.dateOfBirth ?? 'null'],
				['new_dob', data.date_of_birth],
			]),
		});
		return {
			user: await mapUserToAdminResponse(updatedUser, cacheService, acls, gatewayService),
		};
	}
}
