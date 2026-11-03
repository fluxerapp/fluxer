// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiContext} from '@app/api/ApiContext';
import {mapUserToAdminResponse} from '@app/api/admin/models/UserTypes';
import type {AdminAuditService} from '@app/api/admin/services/AdminAuditService';
import {confirmEmailOwnership} from '@app/api/auth/EmailOwnership';
import {createUserID, type UserID} from '@app/api/BrandedTypes';
import {LocalDate} from '@app/api/database/LocalDate';
import type {EnforcementDecisionRow} from '@app/api/database/types/EnforcementTypes';
import type {EnforcementActor} from '@app/api/enforcement/EnforcementActor';
import {decisionAuditMetadata, type EnforcementDecisionService} from '@app/api/enforcement/EnforcementDecisionService';
import type {GuildRepository} from '@app/api/guild/repositories/GuildRepository';
import type {DiscriminatorService} from '@app/api/infrastructure/DiscriminatorService';
import type {EntityAssetService, PreparedAssetUpload} from '@app/api/infrastructure/EntityAssetService';
import {getEmailDeliveryState} from '@app/api/instance/AccountsConfigCache';
import {getInstanceConfigRepository, getKVAccountDeletionQueue} from '@app/api/middleware/ServiceSingletons';
import type {User} from '@app/api/models/User';
import {enqueueStripeCustomerEmailSync} from '@app/api/stripe/StripeCustomer';
import {
	type PartialUserChangePropagationDeps,
	propagateUserUpdate,
} from '@app/api/user/services/PartialUserChangePropagation';
import {clearPendingDeletion} from '@app/api/user/services/PendingDeletionCoordinator';
import {assertNoDiscriminatorChange} from '@app/api/user/UniqueUsernames';
import {renameUser} from '@app/api/user/UserRename';
import {BELOW_MINIMUM_AGE_TRAIT, DOB_DEFERRED_TRAIT} from '@app/api/user/UserTraits';
import {EmailDeliveryStates} from '@fluxer/constants/src/AccountConfigConstants';
import {DeletionReasons} from '@fluxer/constants/src/Core';
import {isPlausibleDateOfBirth} from '@fluxer/constants/src/RegionalMinimumAge';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {AccessDeniedError} from '@fluxer/errors/src/domains/core/AccessDeniedError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import type {
	AdminUserClearFieldsRequest,
	ChangeDobRequest,
	ChangeEmailRequest,
	ChangeUsernameRequest,
	SetUserBotStatusRequest,
	SetUserSystemStatusRequest,
	UserProfileField,
	VerifyUserEmailRequest,
} from '@fluxer/schema/src/domains/admin/AdminUserSchemas';

interface AdminUserProfileServiceDeps {
	apiContext: ApiContext;
	discriminatorService: DiscriminatorService;
	entityAssetService: EntityAssetService;
	auditService: AdminAuditService;
	userUpdateDeps: PartialUserChangePropagationDeps;
	guildRepository: GuildRepository;
	enforcement: EnforcementDecisionService;
}

function profileValues(user: User, fields: ReadonlyArray<UserProfileField>): Map<UserProfileField, string> {
	const current: Record<UserProfileField, string | null> = {
		avatar: user.avatarHash,
		banner: user.bannerHash,
		bio: user.bio,
		pronouns: user.pronouns,
		global_name: user.globalName,
	};
	return new Map(fields.flatMap((field) => (current[field] ? [[field, current[field]] as const] : [])));
}

function isPlausibleAdminDateOfBirth(value: string): boolean {
	const [year, month, day] = value.split('-').map(Number);
	const now = new Date();
	return isPlausibleDateOfBirth(
		{year: year!, month: month!, day: day!},
		{year: now.getUTCFullYear(), month: now.getUTCMonth() + 1, day: now.getUTCDate()},
	);
}

export class AdminUserProfileService {
	constructor(private readonly deps: AdminUserProfileServiceDeps) {}

	async clearUserFields(userId: UserID, {fields, basis}: AdminUserClearFieldsRequest, actor: EnforcementActor) {
		const {users: userRepository, cache: cacheService} = this.deps.apiContext.services;
		const user = await userRepository.findUniqueAssert(userId);
		const resetFields = [...new Set(fields)];
		let updatedUser = user;
		const clear = async () => {
			updatedUser = await this.clearFields(user, resetFields);
		};
		let decision: EnforcementDecisionRow | null = null;
		if (basis.type === 'enforcement') {
			({decision} = await this.deps.enforcement.record(
				{
					subject: {type: 'user', user},
					kind: 'profile_reset',
					input: basis.decision,
					profileReset: {fields: resetFields, values: profileValues(user, resetFields)},
					actor,
				},
				clear,
			));
		} else {
			await clear();
		}
		await this.deps.auditService.createAuditLog({
			adminUserId: actor.adminUserId,
			targetType: 'user',
			targetId: BigInt(userId),
			action: 'clear_fields',
			auditLogReason: actor.internalNote,
			metadata: new Map([
				['fields', resetFields.join(',')],
				['basis', basis.type],
				...(decision ? decisionAuditMetadata(decision) : []),
			]),
		});
		return {
			user: await mapUserToAdminResponse(updatedUser, cacheService, actor.acls),
		};
	}

	private async clearFields(user: User, fields: ReadonlyArray<UserProfileField>): Promise<User> {
		const {entityAssetService, userUpdateDeps} = this.deps;
		const updates: Record<string, null | string> = {};
		const preparedAssets: Array<PreparedAssetUpload> = [];
		for (const field of fields) {
			if (field === 'avatar' || field === 'banner') {
				const prepared = await entityAssetService.prepareAssetUpload({
					assetType: field,
					entityType: 'user',
					entityId: user.id,
					previousHash: field === 'avatar' ? user.avatarHash : user.bannerHash,
					base64Image: null,
					errorPath: field,
				});
				preparedAssets.push(prepared);
				updates[`${field}_hash`] = prepared.newHash;
			} else {
				updates[field] = null;
			}
		}
		const updatedUser = await this.deps.apiContext.services.users.patchUpsert(user.id, updates, user.toRow());
		await entityAssetService.commitAssetChanges(preparedAssets);
		await propagateUserUpdate(userUpdateDeps, user, updatedUser);
		return updatedUser;
	}

	async setUserBotStatus(
		data: SetUserBotStatusRequest,
		adminUserId: UserID,
		auditLogReason: string | null,
		acls: ReadonlySet<string>,
	) {
		const {users: userRepository, cache: cacheService} = this.deps.apiContext.services;
		const {auditService, userUpdateDeps} = this.deps;
		const userId = createUserID(data.user_id);
		const user = await userRepository.findUniqueAssert(userId);
		if (data.bot && user.acls.size > 0) {
			throw new AccessDeniedError();
		}
		const updates: Record<string, boolean> = {bot: data.bot};
		if (!data.bot) {
			updates['system'] = false;
		}
		const updatedUser = await userRepository.patchUpsert(userId, updates, user.toRow());
		await propagateUserUpdate(userUpdateDeps, user, updatedUser);
		await auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: BigInt(userId),
			action: 'set_bot_status',
			auditLogReason,
			metadata: new Map([['bot', data.bot.toString()]]),
		});
		return {
			user: await mapUserToAdminResponse(updatedUser, cacheService, acls),
		};
	}

	async setUserSystemStatus(
		data: SetUserSystemStatusRequest,
		adminUserId: UserID,
		auditLogReason: string | null,
		acls: ReadonlySet<string>,
	) {
		const {users: userRepository, cache: cacheService} = this.deps.apiContext.services;
		const {auditService, userUpdateDeps} = this.deps;
		const userId = createUserID(data.user_id);
		const user = await userRepository.findUniqueAssert(userId);
		if (data.system && !user.isBot) {
			throw InputValidationError.fromCode(
				'system',
				ValidationErrorCodes.USER_MUST_BE_A_BOT_TO_BE_MARKED_AS_A_SYSTEM_USER,
			);
		}
		const updatedUser = await userRepository.patchUpsert(userId, {system: data.system}, user.toRow());
		await propagateUserUpdate(userUpdateDeps, user, updatedUser);
		await auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: BigInt(userId),
			action: 'set_system_status',
			auditLogReason,
			metadata: new Map([['system', data.system.toString()]]),
		});
		return {
			user: await mapUserToAdminResponse(updatedUser, cacheService, acls),
		};
	}

	async verifyUserEmail(
		data: VerifyUserEmailRequest,
		adminUserId: UserID,
		auditLogReason: string | null,
		acls: ReadonlySet<string>,
	) {
		const {users: userRepository, cache: cacheService} = this.deps.apiContext.services;
		const {auditService, userUpdateDeps} = this.deps;
		const userId = createUserID(data.user_id);
		const user = await userRepository.findUniqueAssert(userId);
		const updatedUser = await confirmEmailOwnership(this.deps.apiContext, user);
		await propagateUserUpdate(userUpdateDeps, user, updatedUser);
		await enqueueStripeCustomerEmailSync(this.deps.apiContext.services.worker, user, updatedUser);
		await auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: BigInt(userId),
			action: 'verify_email',
			auditLogReason,
			metadata: new Map([['email', user.email ?? 'null']]),
		});
		return {
			user: await mapUserToAdminResponse(updatedUser, cacheService, acls),
		};
	}

	async changeUsername(
		data: ChangeUsernameRequest,
		adminUserId: UserID,
		auditLogReason: string | null,
		acls: ReadonlySet<string>,
	) {
		const {users: userRepository, cache: cacheService} = this.deps.apiContext.services;
		const userId = createUserID(data.user_id);
		const user = await userRepository.findUniqueAssert(userId);
		if (!user.isBot && (await getInstanceConfigRepository().usesUniqueUsernames())) {
			assertNoDiscriminatorChange(data.discriminator, user.discriminator);
		}
		const updatedUser = await renameUser(
			this.deps,
			user,
			{username: data.username, discriminator: data.discriminator},
			{reason: 'admin_action', actorUserId: adminUserId},
		);
		await this.deps.auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: BigInt(userId),
			action: 'change_username',
			auditLogReason,
			metadata: new Map([
				['old_username', user.username],
				['new_username', data.username],
				['discriminator', updatedUser.discriminator.toString()],
			]),
		});
		return {
			user: await mapUserToAdminResponse(updatedUser, cacheService, acls),
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
			contactChangeLog: contactChangeLogService,
			worker: workerService,
		} = this.deps.apiContext.services;
		const {auditService, userUpdateDeps} = this.deps;
		const userId = createUserID(data.user_id);
		const user = await userRepository.findUniqueAssert(userId);
		const updatedUser = await userRepository.patchUpsert(
			userId,
			{
				email: data.email,
				email_verified: getEmailDeliveryState() !== EmailDeliveryStates.ON,
			},
			user.toRow(),
		);
		await propagateUserUpdate(userUpdateDeps, user, updatedUser);
		await contactChangeLogService.recordDiff({
			oldUser: user,
			newUser: updatedUser,
			reason: 'admin_action',
			actorUserId: adminUserId,
		});
		await enqueueStripeCustomerEmailSync(workerService, user, updatedUser);
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
			user: await mapUserToAdminResponse(updatedUser, cacheService, acls),
		};
	}

	async changeDob(
		data: ChangeDobRequest,
		adminUserId: UserID,
		auditLogReason: string | null,
		acls: ReadonlySet<string>,
	) {
		const {users: userRepository, cache: cacheService} = this.deps.apiContext.services;
		const {auditService, userUpdateDeps} = this.deps;
		const userId = createUserID(data.user_id);
		const user = await userRepository.findUniqueAssert(userId);
		const traits = user.traits;
		traits.delete(DOB_DEFERRED_TRAIT);
		const clearedBelowMinimumAge = traits.delete(BELOW_MINIMUM_AGE_TRAIT);
		let updatedUser = await userRepository.patchUpsert(
			userId,
			{
				date_of_birth: LocalDate.fromString(data.date_of_birth),
				traits: traits.size > 0 ? traits : null,
			},
			user.toRow(),
		);
		if (clearedBelowMinimumAge && updatedUser.deletionReasonCode === DeletionReasons.BELOW_MINIMUM_AGE) {
			updatedUser = await userRepository.updateDeletionSchedule(updatedUser, {
				flags: updatedUser.flags & ~UserFlags.DELETED,
				pending_deletion_at: null,
				deletion_reason_code: null,
				deletion_public_reason: null,
				deletion_audit_log_reason: null,
			});
			await clearPendingDeletion({userId, deletionQueue: getKVAccountDeletionQueue()});
		}
		await propagateUserUpdate(userUpdateDeps, user, updatedUser);
		await auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: BigInt(userId),
			action: 'change_dob',
			auditLogReason,
			metadata: new Map([
				['old_dob', user.dateOfBirth ?? 'null'],
				['new_dob', data.date_of_birth],
				['cleared_below_minimum_age', clearedBelowMinimumAge ? 'true' : 'false'],
				['outside_plausible_range', isPlausibleAdminDateOfBirth(data.date_of_birth) ? 'false' : 'true'],
			]),
		});
		return {
			user: await mapUserToAdminResponse(updatedUser, cacheService, acls),
		};
	}
}
