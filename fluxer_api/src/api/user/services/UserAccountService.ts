// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiContext} from '@app/api/ApiContext';
import {EMAIL_CLEARABLE_SUSPICIOUS_ACTIVITY_FLAGS} from '@app/api/auth/AuthEmail';
import type {SudoVerificationResult} from '@app/api/auth/services/SudoVerificationService';
import type {IConnectionRepository} from '@app/api/connection/IConnectionRepository';
import type {IGuildRepositoryAggregate} from '@app/api/guild/repositories/IGuildRepositoryAggregate';
import type {GuildService} from '@app/api/guild/services/GuildService';
import {GuildMemberSearchIndexService} from '@app/api/guild/services/member/GuildMemberSearchIndexService';
import type {IDiscriminatorService} from '@app/api/infrastructure/DiscriminatorService';
import type {EntityAssetService} from '@app/api/infrastructure/EntityAssetService';
import type {KVAccountDeletionQueueService} from '@app/api/infrastructure/KVAccountDeletionQueueService';
import type {UserCacheService} from '@app/api/infrastructure/UserCacheService';
import {Logger} from '@app/api/Logger';
import type {LimitConfigService} from '@app/api/limits/LimitConfigService';
import type {AuthSession} from '@app/api/models/AuthSession';
import type {User} from '@app/api/models/User';
import type {IUserAccountRepository} from '@app/api/user/repositories/IUserAccountRepository';
import type {IUserChannelRepository} from '@app/api/user/repositories/IUserChannelRepository';
import type {IUserRelationshipRepository} from '@app/api/user/repositories/IUserRelationshipRepository';
import type {IUserSettingsRepository} from '@app/api/user/repositories/IUserSettingsRepository';
import {UserAccountLifecycleService} from '@app/api/user/services/UserAccountLifecycleService';
import {UserAccountLookupService} from '@app/api/user/services/UserAccountLookupService';
import {UserAccountNotesService} from '@app/api/user/services/UserAccountNotesService';
import {UserAccountProfileService} from '@app/api/user/services/UserAccountProfileService';
import {UserAccountSecurityService} from '@app/api/user/services/UserAccountSecurityService';
import {UserAccountSettingsService} from '@app/api/user/services/UserAccountSettingsService';
import {UserAccountUpdatePropagator} from '@app/api/user/services/UserAccountUpdatePropagator';
import type {UserContactChangeLogService} from '@app/api/user/services/UserContactChangeLogService';
import {createPremiumClearPatch} from '@app/api/user/UserHelpers';
import {hasPartialUserFieldsChanged} from '@app/api/user/UserMappers';
import {runAllInOrder} from '@app/api/utils/ConcurrencyUtils';
import {PremiumFlags} from '@fluxer/constants/src/UserConstants';
import type {UserUpdateRequest} from '@fluxer/schema/src/domains/user/UserRequestSchemas';

interface UpdateUserParams {
	user: User;
	oldAuthSession: AuthSession;
	data: UserUpdateRequest;
	request: Request;
	sudoContext?: SudoVerificationResult;
	emailVerifiedViaToken?: boolean;
}

interface UserAccountRepository
	extends IUserAccountRepository,
		IUserSettingsRepository,
		IUserRelationshipRepository,
		IUserChannelRepository {}

export class UserAccountService {
	readonly lookupService: UserAccountLookupService;
	private readonly profileService: UserAccountProfileService;
	private readonly securityService: UserAccountSecurityService;
	readonly settingsService: UserAccountSettingsService;
	readonly notesService: UserAccountNotesService;
	readonly lifecycleService: UserAccountLifecycleService;
	readonly updatePropagator: UserAccountUpdatePropagator;
	private readonly userAccountRepository: UserAccountRepository;
	private readonly guildRepository: IGuildRepositoryAggregate;
	private readonly searchIndexService: GuildMemberSearchIndexService;

	constructor(
		private readonly apiContext: ApiContext,
		userCacheService: UserCacheService,
		guildService: GuildService,
		entityAssetService: EntityAssetService,
		guildRepository: IGuildRepositoryAggregate,
		discriminatorService: IDiscriminatorService,
		kvDeletionQueue: KVAccountDeletionQueueService,
		private readonly contactChangeLogService: UserContactChangeLogService,
		connectionRepository: IConnectionRepository,
		readonly limitConfigService: LimitConfigService,
	) {
		const {
			users: userAccountRepository,
			gateway: gatewayService,
			media: mediaService,
			email: emailService,
			rateLimit: rateLimitService,
		} = this.apiContext.services;
		this.userAccountRepository = userAccountRepository;
		this.guildRepository = guildRepository;
		this.searchIndexService = new GuildMemberSearchIndexService();
		this.updatePropagator = new UserAccountUpdatePropagator({
			userCacheService,
			gatewayService,
			mediaService,
			userRepository: userAccountRepository,
		});
		this.lookupService = new UserAccountLookupService({
			userAccountRepository,
			userRelationshipRepository: userAccountRepository,
			userChannelRepository: userAccountRepository,
			userSettingsRepository: userAccountRepository,
			guildRepository,
			guildService,
			discriminatorService,
			connectionRepository,
		});
		this.profileService = new UserAccountProfileService({
			userAccountRepository,
			guildRepository,
			entityAssetService,
			rateLimitService,
			limitConfigService,
		});
		this.securityService = new UserAccountSecurityService({
			apiContext: this.apiContext,
			userAccountRepository,
			discriminatorService,
			rateLimitService,
			limitConfigService,
		});
		this.settingsService = new UserAccountSettingsService({
			userAccountRepository,
			userSettingsRepository: userAccountRepository,
			userRelationshipRepository: userAccountRepository,
			updatePropagator: this.updatePropagator,
			gatewayService,
			userCacheService,
			guildRepository,
			limitConfigService,
		});
		this.notesService = new UserAccountNotesService({
			userAccountRepository,
			userRelationshipRepository: userAccountRepository,
			updatePropagator: this.updatePropagator,
		});
		this.lifecycleService = new UserAccountLifecycleService({
			apiContext: this.apiContext,
			userAccountRepository,
			guildRepository,
			guildService,
			emailService,
			updatePropagator: this.updatePropagator,
			kvDeletionQueue,
		});
	}

	async update(params: UpdateUserParams): Promise<User> {
		const {user, oldAuthSession, data, request, sudoContext, emailVerifiedViaToken = false} = params;
		const profileResult = await this.profileService.processProfileUpdates({user, data});
		const securityResult = await this.securityService.processSecurityUpdates({user, data, sudoContext});
		const updates = {
			...securityResult.updates,
			...profileResult.updates,
		};
		if (securityResult.updates.flags !== undefined && securityResult.updates.flags !== null) {
			const profileFlags = profileResult.updates.flags ?? user.flags;
			updates.flags = profileFlags | (securityResult.updates.flags & ~user.flags);
		}
		const securityPremiumFlags = securityResult.updates.premium_flags;
		if (securityPremiumFlags !== undefined && securityPremiumFlags !== null) {
			const profilePremiumFlags = profileResult.updates.premium_flags ?? user.premiumFlags;
			updates.premium_flags = profilePremiumFlags | (securityPremiumFlags & ~user.premiumFlags);
		}
		const emailChanged = data.email !== undefined;
		if (emailChanged) {
			updates.email_verified = !!emailVerifiedViaToken;
			if (emailVerifiedViaToken && user.suspiciousActivityFlags !== null && user.suspiciousActivityFlags !== 0) {
				const newFlags = user.suspiciousActivityFlags & ~EMAIL_CLEARABLE_SUSPICIOUS_ACTIVITY_FLAGS;
				if (newFlags !== user.suspiciousActivityFlags) {
					updates.suspicious_activity_flags = newFlags;
				}
			}
		}
		let updatedUser: User;
		try {
			updatedUser = await this.userAccountRepository.patchUpsert(user.id, updates, user.toRow());
		} catch (error) {
			Logger.error(
				{error, userId: user.id},
				'User profile update failed with unknown commit status; retaining uploaded assets',
			);
			throw error;
		}
		const finalizationSteps: Array<() => Promise<unknown>> = [
			() =>
				this.contactChangeLogService.recordDiff({
					oldUser: user,
					newUser: updatedUser,
					reason: 'user_requested',
					actorUserId: user.id,
				}),
			async () => {
				try {
					await this.profileService.commitAssetChanges(profileResult);
				} catch (error) {
					Logger.error({error, userId: user.id}, 'Failed to commit asset changes after successful DB update');
				}
			},
			() => this.updatePropagator.dispatchUserUpdate(updatedUser),
			async () => {
				if (hasPartialUserFieldsChanged(user, updatedUser)) {
					await this.updatePropagator.updateUserCache(updatedUser);
				}
			},
			async () => {
				const nameChanged =
					user.username !== updatedUser.username ||
					user.discriminator !== updatedUser.discriminator ||
					user.globalName !== updatedUser.globalName;
				if (nameChanged) {
					void this.reindexGuildMembersForUser(updatedUser);
				}
			},
		];
		if (securityResult.metadata.invalidateAuthSessions) {
			finalizationSteps.push(
				() => this.securityService.invalidateAndRecreateSessions({user, oldAuthSession, request}),
				() => this.userAccountRepository.deleteAllPasswordResetTokens(user.id),
			);
		}
		await runAllInOrder(finalizationSteps, 'Failed to finalize user update');
		return updatedUser;
	}

	private async reindexGuildMembersForUser(updatedUser: User): Promise<void> {
		try {
			const guildIds = await this.userAccountRepository.getUserGuildIds(updatedUser.id);
			await this.searchIndexService.updateUserMembers(updatedUser, guildIds, this.guildRepository);
		} catch (error) {
			Logger.error({userId: updatedUser.id.toString(), error}, 'Failed to reindex guild members after user update');
		}
	}

	async resetCurrentUserPremiumState(user: User): Promise<void> {
		const updates = {
			...createPremiumClearPatch(),
			premium_lifetime_sequence: null,
			stripe_subscription_id: null,
			stripe_customer_id: null,
			has_ever_purchased: null,
			first_refund_at: null,
			gift_inventory_server_seq: null,
			gift_inventory_client_seq: null,
			premium_flags: user.premiumFlags & ~PremiumFlags.ENABLED_OVERRIDE,
		};
		const updatedUser = await this.userAccountRepository.patchUpsert(user.id, updates, user.toRow());
		await this.updatePropagator.dispatchUserUpdate(updatedUser);
		if (hasPartialUserFieldsChanged(user, updatedUser)) {
			await this.updatePropagator.updateUserCache(updatedUser);
		}
	}
}
