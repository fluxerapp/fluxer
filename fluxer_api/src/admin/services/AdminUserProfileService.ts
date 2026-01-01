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

import {types} from 'cassandra-driver';
import {createUserID, type UserID} from '~/BrandedTypes';
import {InputValidationError, TagAlreadyTakenError, UnknownUserError} from '~/Errors';
import type {IDiscriminatorService} from '~/infrastructure/DiscriminatorService';
import type {EntityAssetService, PreparedAssetUpload} from '~/infrastructure/EntityAssetService';
import type {User} from '~/Models';
import type {IUserRepository} from '~/user/IUserRepository';
import type {UserContactChangeLogService} from '~/user/services/UserContactChangeLogService';
import type {
	ChangeDobRequest,
	ChangeEmailRequest,
	ChangeUsernameRequest,
	ClearUserFieldsRequest,
	SetUserBotStatusRequest,
	SetUserSystemStatusRequest,
	VerifyUserEmailRequest,
} from '../AdminModel';
import type {AdminAuditService} from './AdminAuditService';
import type {AdminUserUpdatePropagator} from './AdminUserUpdatePropagator';

interface AdminUserProfileServiceDeps {
	userRepository: IUserRepository;
	discriminatorService: IDiscriminatorService;
	entityAssetService: EntityAssetService;
	auditService: AdminAuditService;
	updatePropagator: AdminUserUpdatePropagator;
	contactChangeLogService: UserContactChangeLogService;
}

export class AdminUserProfileService {
	constructor(private readonly deps: AdminUserProfileServiceDeps) {}

	async clearUserFields(data: ClearUserFieldsRequest, adminUserId: UserID, auditLogReason: string | null) {
		const {userRepository, entityAssetService, auditService, updatePropagator} = this.deps;
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
				updates.avatar_hash = prepared.newHash;
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
				updates.banner_hash = prepared.newHash;
			} else if (field === 'bio') {
				updates.bio = null;
			} else if (field === 'pronouns') {
				updates.pronouns = null;
			} else if (field === 'global_name') {
				updates.global_name = null;
			}
		}

		let updatedUser: User | null = null;
		try {
			updatedUser = await userRepository.patchUpsert(userId, updates);
		} catch (error) {
			await Promise.allSettled(preparedAssets.map((p) => entityAssetService.rollbackAssetUpload(p)));
			throw error;
		}

		await Promise.allSettled(preparedAssets.map((p) => entityAssetService.commitAssetChange({prepared: p})));

		await updatePropagator.propagateUserUpdate({userId, oldUser: user, updatedUser: updatedUser!});

		await auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: BigInt(userId),
			action: 'clear_fields',
			auditLogReason,
			metadata: new Map([['fields', data.fields.join(',')]]),
		});
	}

	async setUserBotStatus(data: SetUserBotStatusRequest, adminUserId: UserID, auditLogReason: string | null) {
		const {userRepository, auditService, updatePropagator} = this.deps;
		const userId = createUserID(data.user_id);
		const user = await userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}

		const updates: Record<string, boolean> = {bot: data.bot};
		if (!data.bot) {
			updates.system = false;
		}

		const updatedUser = await userRepository.patchUpsert(userId, updates);
		await updatePropagator.propagateUserUpdate({userId, oldUser: user, updatedUser: updatedUser!});

		await auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: BigInt(userId),
			action: 'set_bot_status',
			auditLogReason,
			metadata: new Map([['bot', data.bot.toString()]]),
		});
	}

	async setUserSystemStatus(data: SetUserSystemStatusRequest, adminUserId: UserID, auditLogReason: string | null) {
		const {userRepository, auditService, updatePropagator} = this.deps;
		const userId = createUserID(data.user_id);
		const user = await userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}

		if (data.system && !user.isBot) {
			throw InputValidationError.create('system', 'User must be a bot to be marked as a system user');
		}

		const updatedUser = await userRepository.patchUpsert(userId, {system: data.system});
		await updatePropagator.propagateUserUpdate({userId, oldUser: user, updatedUser: updatedUser!});

		await auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: BigInt(userId),
			action: 'set_system_status',
			auditLogReason,
			metadata: new Map([['system', data.system.toString()]]),
		});
	}

	async verifyUserEmail(data: VerifyUserEmailRequest, adminUserId: UserID, auditLogReason: string | null) {
		const {userRepository, auditService, updatePropagator} = this.deps;
		const userId = createUserID(data.user_id);
		const user = await userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}

		const updatedUser = await userRepository.patchUpsert(userId, {email_verified: true});
		await updatePropagator.propagateUserUpdate({userId, oldUser: user, updatedUser: updatedUser!});

		await auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: BigInt(userId),
			action: 'verify_email',
			auditLogReason,
			metadata: new Map([['email', user.email ?? 'null']]),
		});
	}

	async changeUsername(data: ChangeUsernameRequest, adminUserId: UserID, auditLogReason: string | null) {
		const {userRepository, discriminatorService, auditService, updatePropagator, contactChangeLogService} = this.deps;
		const userId = createUserID(data.user_id);
		const user = await userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}

		const discriminatorResult = await discriminatorService.generateDiscriminator({
			username: data.username,
			requestedDiscriminator: data.discriminator,
			isPremium: true,
		});

		if (!discriminatorResult.available || discriminatorResult.discriminator === -1) {
			throw new TagAlreadyTakenError();
		}

		const updatedUser = await userRepository.patchUpsert(userId, {
			username: data.username,
			discriminator: discriminatorResult.discriminator,
		});
		await updatePropagator.propagateUserUpdate({userId, oldUser: user, updatedUser: updatedUser!});

		await contactChangeLogService.recordDiff({
			oldUser: user,
			newUser: updatedUser!,
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
	}

	async changeEmail(data: ChangeEmailRequest, adminUserId: UserID, auditLogReason: string | null) {
		const {userRepository, auditService, updatePropagator, contactChangeLogService} = this.deps;
		const userId = createUserID(data.user_id);
		const user = await userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}

		const updatedUser = await userRepository.patchUpsert(userId, {
			email: data.email,
			email_verified: false,
		});
		await updatePropagator.propagateUserUpdate({userId, oldUser: user, updatedUser: updatedUser!});

		await contactChangeLogService.recordDiff({
			oldUser: user,
			newUser: updatedUser!,
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
	}

	async changeDob(data: ChangeDobRequest, adminUserId: UserID, auditLogReason: string | null) {
		const {userRepository, auditService, updatePropagator} = this.deps;
		const userId = createUserID(data.user_id);
		const user = await userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}

		const updatedUser = await userRepository.patchUpsert(userId, {
			date_of_birth: types.LocalDate.fromString(data.date_of_birth),
		});
		await updatePropagator.propagateUserUpdate({userId, oldUser: user, updatedUser: updatedUser!});

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
	}
}
