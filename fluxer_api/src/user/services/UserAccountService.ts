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

import type {AuthService} from '~/auth/AuthService';
import type {SudoVerificationResult} from '~/auth/services/SudoVerificationService';
import type {GuildID, UserID} from '~/BrandedTypes';
import type {IGuildRepository} from '~/guild/IGuildRepository';
import type {GuildService} from '~/guild/services/GuildService';
import type {IDiscriminatorService} from '~/infrastructure/DiscriminatorService';
import type {EntityAssetService} from '~/infrastructure/EntityAssetService';
import type {IEmailService} from '~/infrastructure/IEmailService';
import type {IGatewayService} from '~/infrastructure/IGatewayService';
import type {IMediaService} from '~/infrastructure/IMediaService';
import type {IRateLimitService} from '~/infrastructure/IRateLimitService';
import type {RedisAccountDeletionQueueService} from '~/infrastructure/RedisAccountDeletionQueueService';
import type {UserCacheService} from '~/infrastructure/UserCacheService';
import {Logger} from '~/Logger';
import type {AuthSession, User, UserGuildSettings, UserSettings} from '~/Models';
import type {RequestCache} from '~/middleware/RequestCacheMiddleware';
import type {PackService} from '~/pack/PackService';
import {hasPartialUserFieldsChanged} from '~/user/UserMappers';
import type {UserGuildSettingsUpdateRequest, UserSettingsUpdateRequest, UserUpdateRequest} from '~/user/UserModel';
import type {IUserAccountRepository} from '../repositories/IUserAccountRepository';
import type {IUserChannelRepository} from '../repositories/IUserChannelRepository';
import type {IUserRelationshipRepository} from '../repositories/IUserRelationshipRepository';
import type {IUserSettingsRepository} from '../repositories/IUserSettingsRepository';
import {UserAccountLifecycleService} from './UserAccountLifecycleService';
import {UserAccountLookupService} from './UserAccountLookupService';
import {UserAccountNotesService} from './UserAccountNotesService';
import {UserAccountProfileService} from './UserAccountProfileService';
import {UserAccountSecurityService} from './UserAccountSecurityService';
import {UserAccountSettingsService} from './UserAccountSettingsService';
import {UserAccountUpdatePropagator} from './UserAccountUpdatePropagator';
import type {UserContactChangeLogService} from './UserContactChangeLogService';

interface UpdateUserParams {
	user: User;
	oldAuthSession: AuthSession;
	data: UserUpdateRequest;
	request: Request;
	sudoContext?: SudoVerificationResult;
	emailVerifiedViaToken?: boolean;
}

export class UserAccountService {
	private readonly lookupService: UserAccountLookupService;
	private readonly profileService: UserAccountProfileService;
	private readonly securityService: UserAccountSecurityService;
	private readonly settingsService: UserAccountSettingsService;
	private readonly notesService: UserAccountNotesService;
	private readonly lifecycleService: UserAccountLifecycleService;
	private readonly updatePropagator: UserAccountUpdatePropagator;

	constructor(
		private readonly userAccountRepository: IUserAccountRepository,
		userSettingsRepository: IUserSettingsRepository,
		userRelationshipRepository: IUserRelationshipRepository,
		userChannelRepository: IUserChannelRepository,
		authService: AuthService,
		userCacheService: UserCacheService,
		guildService: GuildService,
		gatewayService: IGatewayService,
		entityAssetService: EntityAssetService,
		mediaService: IMediaService,
		packService: PackService,
		emailService: IEmailService,
		rateLimitService: IRateLimitService,
		guildRepository: IGuildRepository,
		discriminatorService: IDiscriminatorService,
		redisDeletionQueue: RedisAccountDeletionQueueService,
		private readonly contactChangeLogService: UserContactChangeLogService,
	) {
		this.updatePropagator = new UserAccountUpdatePropagator({
			userCacheService,
			gatewayService,
			mediaService,
		});

		this.lookupService = new UserAccountLookupService({
			userAccountRepository,
			userRelationshipRepository,
			userChannelRepository,
			guildRepository,
			guildService,
			discriminatorService,
		});

		this.profileService = new UserAccountProfileService({
			userAccountRepository,
			guildRepository,
			entityAssetService,
			rateLimitService,
			updatePropagator: this.updatePropagator,
		});

		this.securityService = new UserAccountSecurityService({
			userAccountRepository,
			authService,
			discriminatorService,
			rateLimitService,
		});

		this.settingsService = new UserAccountSettingsService({
			userAccountRepository,
			userSettingsRepository,
			updatePropagator: this.updatePropagator,
			guildRepository,
			packService,
		});

		this.notesService = new UserAccountNotesService({
			userAccountRepository,
			userRelationshipRepository,
			updatePropagator: this.updatePropagator,
		});

		this.lifecycleService = new UserAccountLifecycleService({
			userAccountRepository,
			guildRepository,
			authService,
			emailService,
			updatePropagator: this.updatePropagator,
			redisDeletionQueue,
		});
	}

	async findUnique(userId: UserID): Promise<User | null> {
		return this.lookupService.findUnique(userId);
	}

	async findUniqueAssert(userId: UserID): Promise<User> {
		return this.lookupService.findUniqueAssert(userId);
	}

	async getUserProfile(params: {
		userId: UserID;
		targetId: UserID;
		guildId?: GuildID;
		withMutualFriends?: boolean;
		withMutualGuilds?: boolean;
		requestCache: RequestCache;
	}) {
		return this.lookupService.getUserProfile(params);
	}

	async generateUniqueDiscriminator(username: string): Promise<number> {
		return this.lookupService.generateUniqueDiscriminator(username);
	}

	async checkUsernameDiscriminatorAvailability(params: {username: string; discriminator: number}): Promise<boolean> {
		return this.lookupService.checkUsernameDiscriminatorAvailability(params);
	}

	async update(params: UpdateUserParams): Promise<User> {
		const {user, oldAuthSession, data, request, sudoContext, emailVerifiedViaToken = false} = params;

		const profileResult = await this.profileService.processProfileUpdates({user, data});
		const securityUpdates = await this.securityService.processSecurityUpdates({user, data, sudoContext});

		const updates = {
			...securityUpdates,
			...profileResult.updates,
		};

		const updatedUserRow = {
			...user.toRow(),
			...updates,
		};

		if (updates.avatar_hash === null) {
			updatedUserRow.avatar_hash = null;
		}
		if (updates.banner_hash === null) {
			updatedUserRow.banner_hash = null;
		}

		const emailChanged = data.email !== undefined;
		if (emailChanged) {
			updatedUserRow.email_verified = !!emailVerifiedViaToken;
		}

		let updatedUser: User;
		try {
			updatedUser = await this.userAccountRepository.upsert(updatedUserRow, user.toRow());
		} catch (error) {
			await this.profileService.rollbackAssetChanges(profileResult);
			Logger.error({error, userId: user.id}, 'User update failed, rolled back asset uploads');
			throw error;
		}

		await this.contactChangeLogService.recordDiff({
			oldUser: user,
			newUser: updatedUser,
			reason: 'user_requested',
			actorUserId: user.id,
		});

		try {
			await this.profileService.commitAssetChanges(profileResult);
		} catch (error) {
			Logger.error({error, userId: user.id}, 'Failed to commit asset changes after successful DB update');
		}

		await this.updatePropagator.dispatchUserUpdate(updatedUser);

		if (hasPartialUserFieldsChanged(user, updatedUser)) {
			await this.updatePropagator.invalidateUserCache(updatedUser.id);
		}

		if (updates.invalidateAuthSessions) {
			await this.securityService.invalidateAndRecreateSessions({user, oldAuthSession, request});
		}

		return updatedUser;
	}

	async findSettings(userId: UserID): Promise<UserSettings> {
		return this.settingsService.findSettings(userId);
	}

	async updateSettings(params: {userId: UserID; data: UserSettingsUpdateRequest}): Promise<UserSettings> {
		return this.settingsService.updateSettings(params);
	}

	async findGuildSettings(userId: UserID, guildId: GuildID | null): Promise<UserGuildSettings | null> {
		return this.settingsService.findGuildSettings(userId, guildId);
	}

	async updateGuildSettings(params: {
		userId: UserID;
		guildId: GuildID | null;
		data: UserGuildSettingsUpdateRequest;
	}): Promise<UserGuildSettings> {
		return this.settingsService.updateGuildSettings(params);
	}

	async getUserNote(params: {userId: UserID; targetId: UserID}): Promise<{note: string} | null> {
		return this.notesService.getUserNote(params);
	}

	async getUserNotes(userId: UserID): Promise<Record<string, string>> {
		return this.notesService.getUserNotes(userId);
	}

	async setUserNote(params: {userId: UserID; targetId: UserID; note: string | null}): Promise<void> {
		return this.notesService.setUserNote(params);
	}

	async selfDisable(userId: UserID): Promise<void> {
		return this.lifecycleService.selfDisable(userId);
	}

	async selfDelete(userId: UserID): Promise<void> {
		return this.lifecycleService.selfDelete(userId);
	}

	async dispatchUserUpdate(user: User): Promise<void> {
		return this.updatePropagator.dispatchUserUpdate(user);
	}

	async dispatchUserSettingsUpdate({userId, settings}: {userId: UserID; settings: UserSettings}): Promise<void> {
		return this.updatePropagator.dispatchUserSettingsUpdate({userId, settings});
	}

	async dispatchUserGuildSettingsUpdate({
		userId,
		settings,
	}: {
		userId: UserID;
		settings: UserGuildSettings;
	}): Promise<void> {
		return this.updatePropagator.dispatchUserGuildSettingsUpdate({userId, settings});
	}

	async dispatchUserNoteUpdate(params: {userId: UserID; targetId: UserID; note: string}): Promise<void> {
		return this.updatePropagator.dispatchUserNoteUpdate(params);
	}
}
