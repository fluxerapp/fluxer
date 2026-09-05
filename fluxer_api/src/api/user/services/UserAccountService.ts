// SPDX-License-Identifier: AGPL-3.0-or-later

import {PremiumFlags} from '@fluxer/constants/src/UserConstants';
import type {UserUpdateRequest} from '@fluxer/schema/src/domains/user/UserRequestSchemas';
import type {ApiContext} from '../../ApiContext';
import {EMAIL_CLEARABLE_SUSPICIOUS_ACTIVITY_FLAGS} from '../../auth/AuthEmail';
import type {SudoVerificationResult} from '../../auth/services/SudoVerificationService';
import type {IConnectionRepository} from '../../connection/IConnectionRepository';
import type {IGuildRepositoryAggregate} from '../../guild/repositories/IGuildRepositoryAggregate';
import type {GuildService} from '../../guild/services/GuildService';
import {GuildMemberSearchIndexService} from '../../guild/services/member/GuildMemberSearchIndexService';
import type {IDiscriminatorService} from '../../infrastructure/DiscriminatorService';
import type {EntityAssetService} from '../../infrastructure/EntityAssetService';
import type {KVAccountDeletionQueueService} from '../../infrastructure/KVAccountDeletionQueueService';
import type {UserCacheService} from '../../infrastructure/UserCacheService';
import {Logger} from '../../Logger';
import type {LimitConfigService} from '../../limits/LimitConfigService';
import type {AuthSession} from '../../models/AuthSession';
import type {User} from '../../models/User';
import type {IUserAccountRepository} from '../repositories/IUserAccountRepository';
import type {IUserChannelRepository} from '../repositories/IUserChannelRepository';
import type {IUserRelationshipRepository} from '../repositories/IUserRelationshipRepository';
import type {IUserSettingsRepository} from '../repositories/IUserSettingsRepository';
import {createPremiumClearPatch} from '../UserHelpers';
import {hasPartialUserFieldsChanged} from '../UserMappers';
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
			updatePropagator: this.updatePropagator,
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
		const metadata = {
			...securityResult.metadata,
			...profileResult.metadata,
		};
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
		await this.profileService.commitAssetChanges(profileResult).catch((error) => {
			Logger.error({error, userId: user.id}, 'Failed to commit asset changes after successful DB update');
		});
		await this.updatePropagator.dispatchUserUpdate(updatedUser);
		if (hasPartialUserFieldsChanged(user, updatedUser)) {
			await this.updatePropagator.updateUserCache(updatedUser);
		}
		const nameChanged =
			user.username !== updatedUser.username ||
			user.discriminator !== updatedUser.discriminator ||
			user.globalName !== updatedUser.globalName;
		if (nameChanged) {
			void this.reindexGuildMembersForUser(updatedUser);
		}
		if (metadata.invalidateAuthSessions) {
			await this.securityService.invalidateAndRecreateSessions({user, oldAuthSession, request});
			await this.userAccountRepository.deleteAllPasswordResetTokens(user.id);
		}
		return updatedUser;
	}

	private async reindexGuildMembersForUser(updatedUser: User): Promise<void> {
		try {
			const guildIds = await this.userAccountRepository.getUserGuildIds(updatedUser.id);
			if (guildIds.length === 0) return;
			const guilds = await this.guildRepository.listGuilds(guildIds);
			const indexedGuilds = guilds.filter((guild) => guild.membersIndexedAt != null);
			if (indexedGuilds.length === 0) return;
			const members = await Promise.all(
				indexedGuilds.map((guild) => this.guildRepository.getMember(guild.id, updatedUser.id)),
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
